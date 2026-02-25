#!/usr/bin/env Rscript
# assignment1/run_all.R
# -----------------------------------------------------------------------------
# End-to-end pipeline for Assignment 1:
# 1) simulate a tournament (WSLS vs k-ToM-inspired belief learning)
# 2) bootstrap uncertainty for tournament summaries
# 3) parameter recovery + Stan fitting
# 4) LOO model comparison
# -----------------------------------------------------------------------------

get_script_dir <- function() {
  # RStudio (interactive)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(p) && nzchar(p)) return(dirname(p))
  }

  # Rscript --file=...
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(sub("^--file=", "", file_arg[1])))
  }

  # source() from a file
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(sys.frames()[[1]]$ofile))
  }

  getwd()
}

setwd(get_script_dir())

REPO_ROOT <- normalizePath("..", winslash = "/", mustWork = FALSE)

# --- renv restore (recommended for reproducibility) ---------------------------
if (requireNamespace("renv", quietly = TRUE) && file.exists(file.path(REPO_ROOT, "renv.lock"))) {
  message("renv.lock found -> renv::restore()")
  renv::restore(project = REPO_ROOT, prompt = FALSE)
}

# --- Packages ----------------------------------------------------------------
source(file.path("R", "dependencies.R"))
# missing <- ACM_PACKAGES[!vapply(ACM_PACKAGES, requireNamespace, logical(1), quietly = TRUE)]
# if (length(missing) > 0) {
#   stop(
#     "Missing packages: ", paste(missing, collapse = ", "), "\n",
#     "If you use renv: renv::restore()\n",
#     "Otherwise install them with install.packages(...).\n"
#   )
# }
# 
# suppressPackageStartupMessages({
#   library(dplyr)
#   library(tidyr)
#   library(readr)
#   library(ggplot2)
#   library(patchwork)
# })

# --- Source project code ------------------------------------------------------
source(file.path("R", "config.R"))
source(file.path("R", "strategies.R"))
source(file.path("R", "game.R"))
source(file.path("R", "bootstrap.R"))
source(file.path("R", "stan_utils.R"))
source(file.path("R", "tournament.R"))
source(file.path("R", "plots.R"))
source(file.path("R", "fit_models.R"))
source(file.path("R", "recovery.R"))

set.seed(SEED)

# --- Compile Stan models (once) ----------------------------------------------
message("\n=== Compiling Stan models (CmdStan) ===")
models <- compile_models()

# --- 1) Tournament (behavioural comparison) ----------------------------------
message("\n=== 1) Tournament simulations ===")
res <- run_tournament(
  models = models,
  n_sims = N_SIMS,
  T = T_TRIALS,
  seed = SEED,
  return_trials = TRUE
)

matches <- res$matches   # one row per sim x pairing
players <- res$players   # one row per sim x pairing x role
trials  <- res$trials    # one row per sim x pairing x trial

write_csv(trials,  file.path(OUT_DATA_DIR, "trials.csv"))
write_csv(matches, file.path(OUT_DATA_DIR, "matches.csv"))
write_csv(players, file.path(OUT_DATA_DIR, "players.csv"))

message("Saved tournament data:")
message(" - trials rows:  ", nrow(trials))
message(" - matches rows: ", nrow(matches))
message(" - players rows: ", nrow(players))

plot_fig0_dynamics(
  trials_df  = trials,
  out_file   = file.path(OUT_FIG_DIR, "fig0_dynamics.png"),
  swap_trial = HALF_TRIAL
)

# --- 2) Bootstrap summaries + tournament plots --------------------------------
message("\n=== 2) Bootstrap summaries + tournament plots ===")

matchup_sum <- summarise_matchups(matches, R = BOOT_R, level = CI_LEVEL, seed = SEED)
role_sum    <- summarise_role_advantage(players, R = BOOT_R, level = CI_LEVEL, seed = SEED)

write_csv(matchup_sum, file.path(OUT_DATA_DIR, "matchup_summary.csv"))
write_csv(role_sum,    file.path(OUT_DATA_DIR, "role_advantage_summary.csv"))

plot_fig1_matchups(matchup_sum, file.path(OUT_FIG_DIR, "fig1_matchups.png"))
plot_fig2_role_advantage(role_sum, file.path(OUT_FIG_DIR, "fig2_role_advantage.png"))
plot_fig4_payoff_distribution(matches, file.path(OUT_FIG_DIR, "fig4_payoff_distribution.png"))

# --- 3) Parameter recovery + model comparison --------------------------------
message("\n=== 3) Parameter recovery + Stan fits ===")

# Subject parameters (drawn from the models' generative priors)
wsls_params <- sample_priors(models$priors_wsls, n_draws = N_SUBJECTS_RECOVERY, seed = SEED)
wsls_params <- wsls_params[, c("p_repeat_win_gq", "p_repeat_loss_gq", "lapse_gq")]
names(wsls_params) <- c("p_repeat_win", "p_repeat_loss", "lapse")

bel_params <- sample_priors(models$priors_belief, n_draws = N_SUBJECTS_RECOVERY, seed = SEED + 1)
bel_params <- bel_params[, c("alpha_gq", "beta_gq", "lapse_gq")]
names(bel_params) <- c("alpha", "beta", "lapse")

# Opponent parameters (fresh opponent per subject)
wsls_opp <- sample_priors(models$priors_wsls, n_draws = N_SUBJECTS_RECOVERY, seed = SEED + 2)
wsls_opp <- wsls_opp[, c("p_repeat_win_gq", "p_repeat_loss_gq", "lapse_gq")]
names(wsls_opp) <- c("p_repeat_win", "p_repeat_loss", "lapse")  # important for simulate_dataset()

bel_opp <- sample_priors(models$priors_belief, n_draws = N_SUBJECTS_RECOVERY, seed = SEED + 3)
bel_opp <- bel_opp[, c("alpha_gq", "beta_gq", "lapse_gq")]
names(bel_opp) <- c("alpha", "beta", "lapse")

# Datasets
wsls_data <- simulate_dataset(
  strategy_A = "WSLS",
  strategy_B = "kToM",
  params_A   = wsls_params,
  params_B   = bel_opp,
  T          = T_TRIALS,
  payoff_win = PAYOFF_WIN,
  payoff_loss = PAYOFF_LOSS
)

bel_data <- simulate_dataset(
  strategy_A = "kToM",
  strategy_B = "WSLS",
  params_A   = bel_params,
  params_B   = wsls_opp,
  T          = T_TRIALS,
  payoff_win = PAYOFF_WIN,
  payoff_loss = PAYOFF_LOSS
)

# Save true generating parameters
write_csv(as.data.frame(wsls_params) %>% mutate(subject = row_number()),
          file.path(OUT_DATA_DIR, "true_params_wsls.csv"))
write_csv(as.data.frame(bel_params) %>% mutate(subject = row_number()),
          file.path(OUT_DATA_DIR, "true_params_belief.csv"))

# Fit both models to both datasets
wsls_stan <- prepare_stan_data(wsls_data)
bel_stan  <- prepare_stan_data(bel_data)

message("Fitting WSLS model to WSLS-generated data")
fit_wsls_on_wsls <- fit_model(
  models$fit_wsls, wsls_stan,
  seed = SEED, chains = STAN_CHAINS,
  iter_warmup = STAN_WARMUP, iter_sampling = STAN_SAMPLES
)

message("Fitting Belief model to WSLS-generated data")
fit_bel_on_wsls <- fit_model(
  models$fit_belief, wsls_stan,
  seed = SEED + 10, chains = STAN_CHAINS,
  iter_warmup = STAN_WARMUP, iter_sampling = STAN_SAMPLES
)

message("Fitting WSLS model to Belief-generated data")
fit_wsls_on_bel <- fit_model(
  models$fit_wsls, bel_stan,
  seed = SEED + 20, chains = STAN_CHAINS,
  iter_warmup = STAN_WARMUP, iter_sampling = STAN_SAMPLES
)

message("Fitting Belief model to Belief-generated data")
fit_bel_on_bel <- fit_model(
  models$fit_belief, bel_stan,
  seed = SEED + 30, chains = STAN_CHAINS,
  iter_warmup = STAN_WARMUP, iter_sampling = STAN_SAMPLES
)

# --- Recovery summaries (WSLS on WSLS; Belief on Belief) ----------------------
draws_wsls_on_wsls <- posterior::as_draws_df(fit_wsls_on_wsls$draws())
draws_bel_on_bel   <- posterior::as_draws_df(fit_bel_on_bel$draws())

true_wsls_df <- as.data.frame(wsls_params) %>% mutate(subject = row_number())
true_bel_df  <- as.data.frame(bel_params)  %>% mutate(subject = row_number())

rec_win   <- summarise_subject_params(draws_wsls_on_wsls, "^p_repeat_win\\[",  true_values = true_wsls_df$p_repeat_win)
rec_loss  <- summarise_subject_params(draws_wsls_on_wsls, "^p_repeat_loss\\[", true_values = true_wsls_df$p_repeat_loss)
rec_lapW  <- summarise_subject_params(draws_wsls_on_wsls, "^lapse\\[",         true_values = true_wsls_df$lapse)

rec_alpha <- summarise_subject_params(draws_bel_on_bel,   "^alpha\\[",         true_values = true_bel_df$alpha)
rec_beta  <- summarise_subject_params(draws_bel_on_bel,   "^beta\\[",          true_values = true_bel_df$beta)
rec_lapB  <- summarise_subject_params(draws_bel_on_bel,   "^lapse\\[",         true_values = true_bel_df$lapse)

write_csv(rec_win,   file.path(OUT_DATA_DIR, "recovery_wsls_p_repeat_win.csv"))
write_csv(rec_loss,  file.path(OUT_DATA_DIR, "recovery_wsls_p_repeat_loss.csv"))
write_csv(rec_lapW,  file.path(OUT_DATA_DIR, "recovery_wsls_lapse.csv"))
write_csv(rec_alpha, file.path(OUT_DATA_DIR, "recovery_belief_alpha.csv"))
write_csv(rec_beta,  file.path(OUT_DATA_DIR, "recovery_belief_beta.csv"))
write_csv(rec_lapB,  file.path(OUT_DATA_DIR, "recovery_belief_lapse.csv"))

# Posterior densities (example subjects) + recovery scatter
example_subj <- pick_example_subjects(N_SUBJECTS_RECOVERY, n = 3)

p_post_wsls <- plot_posterior_densities_with_truth(
  draws_df = draws_wsls_on_wsls,
  params   = c("p_repeat_win", "p_repeat_loss", "lapse"),
  true_df  = true_wsls_df,
  subjects = example_subj,
  out_file = file.path(OUT_FIG_DIR, "fig3a_posteriors_wsls.png")
)

p_post_bel <- plot_posterior_densities_with_truth(
  draws_df = draws_bel_on_bel,
  params   = c("alpha", "beta", "lapse"),
  true_df  = true_bel_df,
  subjects = example_subj,
  out_file = file.path(OUT_FIG_DIR, "fig3b_posteriors_belief.png")
)

p_rec_wsls <- plot_recovery_scatter(
  rec_win, "WSLS: p_repeat_win",
  file.path(OUT_FIG_DIR, "fig3c_recovery_wsls_p_repeat_win.png")
)

p_rec_bel <- plot_recovery_scatter(
  rec_alpha, "Belief: alpha",
  file.path(OUT_FIG_DIR, "fig3e_recovery_belief_alpha.png")
)

combined <- (p_post_wsls / p_post_bel) / (p_rec_wsls | p_rec_bel)
ggsave(
  file.path(OUT_FIG_DIR, "fig3_interpretation_combined.png"),
  combined, width = 10, height = 10, dpi = 200
)

# --- 4) LOO model comparison --------------------------------------------------
message("\n=== 4) LOO model comparison ===")

loo_wsls_on_wsls <- compute_loo(fit_wsls_on_wsls)
loo_bel_on_wsls  <- compute_loo(fit_bel_on_wsls)
loo_wsls_on_bel  <- compute_loo(fit_wsls_on_bel)
loo_bel_on_bel   <- compute_loo(fit_bel_on_bel)

cmp_wsls <- loo_table_and_dot(loo_wsls_on_wsls, loo_bel_on_wsls, label_a = "WSLS", label_b = "Belief")
cmp_bel  <- loo_table_and_dot(loo_wsls_on_bel,  loo_bel_on_bel,  label_a = "WSLS", label_b = "Belief")

cmp_wsls$table$dataset <- "WSLS-generated"
cmp_bel$table$dataset  <- "Belief-generated"

loo_table <- bind_rows(cmp_wsls$table, cmp_bel$table)
write_csv(loo_table, file.path(OUT_DATA_DIR, "loo_comparison_table.csv"))

loo_dot <- bind_rows(
  transform(cmp_wsls$dot, dataset = "WSLS-generated"),
  transform(cmp_bel$dot,  dataset = "Belief-generated")
)
write_csv(loo_dot, file.path(OUT_DATA_DIR, "loo_dot.csv"))
plot_loo_dot(loo_dot, file.path(OUT_FIG_DIR, "fig5_loo_dot.png"))

message("\nDone.")
message("Outputs:")
message(" - data: ", OUT_DATA_DIR)
message(" - figs: ", OUT_FIG_DIR)

 quarto::quarto_render("assignment1.qmd") # render over here if u want the nice diagram; it doesn't work over the render button :P
