#!/usr/bin/env Rscript
# assignment1/run_all.R
# -----------------------------------------------------------------------------
# One-click pipeline (RStudio-first):
#  1) simulate a tournament (WSLS vs k-ToM-inspired)
#  2) bootstrap uncertainty for tournament plots
#  3) parameter recovery + hierarchical Stan fitting
#  4) LOO model comparison (table + dot plot)
#
# -----------------------------------------------------------------------------

# --- Working directory -------------------------------------------------------
# If you run this in RStudio, it will set the working directory to the script's location.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# If you run from the terminal with Rscript, the above may not fire.
# In that case, you can just setwd("assignment1") manually.

# Repo root is one level up from assignment1/
REPO_ROOT <- normalizePath("..", winslash = "/", mustWork = FALSE)

# --- renv (optional) ----------------------------------------------------------
# If you use renv and have a lockfile, restoring gives you the same package set.
if (requireNamespace("renv", quietly = TRUE) && file.exists(file.path(REPO_ROOT, "renv.lock"))) {
  message("renv.lock found -> running renv::restore()")
  renv::restore(project = REPO_ROOT, prompt = FALSE)
}

# --- Packages ----------------------------------------------------------------
# We keep the dependency list in one place (R/dependencies.R).
source(file.path("R", "dependencies.R"))
missing <- ACM_PACKAGES[!vapply(ACM_PACKAGES, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  stop(
    "Missing packages: ", paste(missing, collapse = ", "), "\n",
    "If you use renv, run: renv::restore()\n",
    "Otherwise install them, e.g.: install.packages(c(...))\n"
  )
}

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)

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

set.seed(1) # For reproducibility of non-Stan random processes

# --- Compile Stan models once -------------------------------------------------
message("\n=== Compiling Stan models (CmdStan) ===")
models <- compile_models()

# --- 1) Tournament (behavioural comparison) -----------------------------------
message("\n=== 1) Tournament simulations ===")
res <- run_tournament(
  models = models,
  n_sims = N_SIMS,
  T = T_TRIALS,
  seed = SEED,
  return_trials = TRUE
)

matches <- res$matches # one row per sim x pairing (summary of final payoffs)
players <- res$players # one row per sim x pairing x role (summary of role advantage)
trials  <- res$trials # one row per sim x pairing x trial (full sequences; can be huge!)

write_csv(trials, file.path(OUT_DATA_DIR, "trials.csv"))

# plot0 doesn't work when the code is concise so...
message("trials rows: ", nrow(trials)) # taking forever; nrow(trials) = 4 * N_SIMS * T = 4 * 240000 * 100 = 96 million rows

trials_small <- dplyr::filter(trials, sim_id <= 2000) # this is just for testing the plotting code on a smaller subset of the data (2000 sims instead of 240000), to make it faster to iterate on the plot design. The full dataset is still saved as trials.csv, and the final plot will be generated from the full dataset (but it takes a long time to run, so we can use this smaller subset for development).

message("trials_small dim: ", paste(dim(trials_small), collapse = " x ")) # trials_small dim: 240000 x 10
message("unique pairings: ", dplyr::n_distinct(trials_small$pairing)) # unique pairings: 4
message("max sim_id: ", max(trials_small$sim_id)) # max sim_id: 600

print(system.time(
  plot_fig0_dynamics(trials_small, file.path(OUT_FIG_DIR, "fig0_dynamics.png"), swap_trial = HALF_TRIAL)
))

write_csv(matches, file.path(OUT_DATA_DIR, "matches.csv"))
write_csv(players, file.path(OUT_DATA_DIR, "players.csv"))

# Summaries + plots
message("\n=== 2) Bootstrap summaries + tournament plots ===")
matchup_sum <- summarise_matchups(matches, R = BOOT_R, level = CI_LEVEL, seed = SEED) # this is the slow step, because of the bootstrapping (R = 400 resamples per matchup)
role_sum    <- summarise_role_advantage(players, R = BOOT_R, level = CI_LEVEL, seed = SEED) # this is also slow, because of the bootstrapping (R = 400 resamples per strategy)

write_csv(matchup_sum, file.path(OUT_DATA_DIR, "matchup_summary.csv"))
write_csv(role_sum,    file.path(OUT_DATA_DIR, "role_advantage_summary.csv"))

plot_fig1_matchups(matchup_sum, file.path(OUT_FIG_DIR, "fig1_matchups.png"))
plot_fig2_role_advantage(role_sum, file.path(OUT_FIG_DIR, "fig2_role_advantage.png"))
plot_fig4_payoff_distribution(matches, file.path(OUT_FIG_DIR, "fig4_payoff_distribution.png"))

# --- 3) Parameter recovery + model comparison (the "ACM" core) ---------------
message("\n=== 3) Parameter recovery + hierarchical model fits ===")

# Simulate two datasets:
# - WSLS-generated subjects vs belief opponents
# - Belief-generated subjects vs WSLS opponents
#
# Why? It lets us test whether our fitted models recover the mechanisms.

# Sample subject parameters (Stan priors)
wsls_params <- sample_priors(models$priors_wsls, n_draws = N_SUBJECTS_RECOVERY, seed = SEED) # this is a bit slow, because of the sampling (N_SUBJECTS_RECOVERY = 1000 draws from the priors)
wsls_params <- wsls_params[, c("p_repeat_win_gq", "p_repeat_loss_gq", "lapse_gq")] # we only keep the "gq" parameters, which are the ones used for data generation (the non-gq parameters are just for fitting)
names(wsls_params) <- c("p_repeat_win", "p_repeat_loss", "lapse") # we rename them to match the parameter names in the Stan model (this makes it easier to pass them to the simulate_dataset function)

bel_params <- sample_priors(models$priors_belief, n_draws = N_SUBJECTS_RECOVERY, seed = SEED + 1)
bel_params <- bel_params[, c("alpha_gq", "beta_gq", "lapse_gq")]
names(bel_params) <- c("alpha", "beta", "lapse")

# Opponent parameters (draw a fresh opponent for each subject)
wsls_opp <- sample_priors(models$priors_wsls, n_draws = N_SUBJECTS_RECOVERY, seed = SEED + 2) # we sample a fresh set of opponent parameters for each subject, to make the task more realistic (if all subjects faced the same opponent, it would be easier to fit the models, but less ecologically valid)
wsls_opp <- wsls_opp[, c("p_repeat_win_gq", "p_repeat_loss_gq", "lapse_gq")] # 
names(wsls_opp) <- c("p_repeat_win", "p_repeat_loss", "lapse")

bel_opp <- sample_priors(models$priors_belief, n_draws = N_SUBJECTS_RECOVERY, seed = SEED + 3)
bel_opp <- bel_opp[, c("alpha_gq", "beta_gq", "lapse_gq")]
names(bel_opp) <- c("alpha", "beta", "lapse")

# Datasets
wsls_data <- simulate_dataset(
  strategy_A = "WSLS", strategy_B = "kToM",
  params_A = wsls_params, params_B = bel_opp,
  T = T_TRIALS, payoff_win = PAYOFF_WIN, payoff_loss = PAYOFF_LOSS
)

bel_data <- simulate_dataset(
  strategy_A = "kToM", strategy_B = "WSLS",
  params_A = bel_params, params_B = wsls_opp,
  T = T_TRIALS, payoff_win = PAYOFF_WIN, payoff_loss = PAYOFF_LOSS
)

# Save raw sequences (useful for debugging / transparency)
write_csv(as.data.frame(wsls_params) |> mutate(subject = row_number()), file.path(OUT_DATA_DIR, "true_params_wsls.csv"))
write_csv(as.data.frame(bel_params)  |> mutate(subject = row_number()), file.path(OUT_DATA_DIR, "true_params_belief.csv"))

# Fit both models to both datasets
wsls_stan <- prepare_stan_data(wsls_data)
bel_stan  <- prepare_stan_data(bel_data)

message("Fitting WSLS model to WSLS-generated data")
fit_wsls_on_wsls <- fit_model(models$fit_wsls, wsls_stan, seed = SEED, chains = STAN_CHAINS, iter_warmup = STAN_WARMUP, iter_sampling = STAN_SAMPLES)

message("Fitting Belief model to WSLS-generated data")
fit_bel_on_wsls  <- fit_model(models$fit_belief, wsls_stan, seed = SEED + 10, chains = STAN_CHAINS, iter_warmup = STAN_WARMUP, iter_sampling = STAN_SAMPLES)

message("Fitting WSLS model to Belief-generated data")
fit_wsls_on_bel  <- fit_model(models$fit_wsls, bel_stan, seed = SEED + 20, chains = STAN_CHAINS, iter_warmup = STAN_WARMUP, iter_sampling = STAN_SAMPLES)

message("Fitting Belief model to Belief-generated data")
fit_bel_on_bel   <- fit_model(models$fit_belief, bel_stan, seed = SEED + 30, chains = STAN_CHAINS, iter_warmup = STAN_WARMUP, iter_sampling = STAN_SAMPLES)

# Posterior draws
# We convert the CmdStanR draws to data frames for easier manipulation (the as_draws_df function also adds the parameter names as columns, which is useful for summarising and plotting).
draws_wsls_on_wsls <- posterior::as_draws_df(fit_wsls_on_wsls$draws())
draws_bel_on_bel   <- posterior::as_draws_df(fit_bel_on_bel$draws())

# Parameter recovery summaries (subject-level)
# We summarise the posterior draws for each subject and parameter, and compare them to the true values used for data generation. The summarise_subject_params function computes the mean and 95% credible interval for each subject's parameter, and also includes the true value for reference.
true_wsls_df <- as.data.frame(wsls_params) |> mutate(subject = row_number())
true_bel_df  <- as.data.frame(bel_params)  |> mutate(subject = row_number())

# We do this separately for each parameter, because the parameter names are different in the two models (e.g., p_repeat_win vs alpha). The regex patterns "^p_repeat_win\\[" etc. are used to select the relevant parameters from the posterior draws (the square brackets indicate that these are subject-level parameters, e.g., p_repeat_win[1], p_repeat_win[2], etc.).
rec_win  <- summarise_subject_params(draws_wsls_on_wsls, "^p_repeat_win\\[",  true_values = true_wsls_df$p_repeat_win)
rec_loss <- summarise_subject_params(draws_wsls_on_wsls, "^p_repeat_loss\\[", true_values = true_wsls_df$p_repeat_loss)
rec_lapW <- summarise_subject_params(draws_wsls_on_wsls, "^lapse\\[",         true_values = true_wsls_df$lapse)

rec_alpha <- summarise_subject_params(draws_bel_on_bel, "^alpha\\[", true_values = true_bel_df$alpha)
rec_beta  <- summarise_subject_params(draws_bel_on_bel, "^beta\\[",  true_values = true_bel_df$beta)
rec_lapB  <- summarise_subject_params(draws_bel_on_bel, "^lapse\\[", true_values = true_bel_df$lapse)

write_csv(rec_win,   file.path(OUT_DATA_DIR, "recovery_wsls_p_repeat_win.csv"))
write_csv(rec_loss,  file.path(OUT_DATA_DIR, "recovery_wsls_p_repeat_loss.csv"))
write_csv(rec_lapW,  file.path(OUT_DATA_DIR, "recovery_wsls_lapse.csv"))
write_csv(rec_alpha, file.path(OUT_DATA_DIR, "recovery_belief_alpha.csv"))
write_csv(rec_beta,  file.path(OUT_DATA_DIR, "recovery_belief_beta.csv"))
write_csv(rec_lapB,  file.path(OUT_DATA_DIR, "recovery_belief_lapse.csv"))

# Figure 3 (multi-panel): densities + recovery
example_subj <- pick_example_subjects(N_SUBJECTS_RECOVERY, n = 3)

# A combined interpretation figure (single file) using patchwork.

# Capture ggplot objects (the helper functions return them invisibly).
p_post_wsls <- plot_posterior_densities_with_truth(
  draws_df = draws_wsls_on_wsls,
  params = c("p_repeat_win", "p_repeat_loss", "lapse"),
  true_df = true_wsls_df,
  subjects = example_subj,
  out_file = file.path(OUT_FIG_DIR, "fig3a_posteriors_wsls.png")
)

p_post_bel <- plot_posterior_densities_with_truth(
  draws_df = draws_bel_on_bel,
  params = c("alpha", "beta", "lapse"),
  true_df = true_bel_df,
  subjects = example_subj,
  out_file = file.path(OUT_FIG_DIR, "fig3b_posteriors_belief.png")
)

p_rec_wsls <- plot_recovery_scatter(rec_win,  "WSLS: p_repeat_win",  file.path(OUT_FIG_DIR, "fig3c_recovery_wsls_p_repeat_win.png"))
p_rec_bel  <- plot_recovery_scatter(rec_alpha,"Belief: alpha",       file.path(OUT_FIG_DIR, "fig3e_recovery_belief_alpha.png"))

combined <- (p_post_wsls / p_post_bel) / (p_rec_wsls | p_rec_bel)

ggplot2::ggsave(file.path(OUT_FIG_DIR, "fig3_interpretation_combined.png"), combined, width = 10, height = 10, dpi = 200)

# --- 4) LOO model comparison (table + dot plot) -------------------------------
# We compare the models using leave-one-out cross-validation (LOO), which estimates the expected log predictive density for new data. The compute_loo function computes the LOO values for each fitted model, and the loo_table_and_dot function creates a table of LOO differences and a dot plot of the LOO values for each dataset.
message("\n=== 4) LOO model comparison ===")

loo_wsls_on_wsls <- compute_loo(fit_wsls_on_wsls)
loo_bel_on_wsls  <- compute_loo(fit_bel_on_wsls)
loo_wsls_on_bel  <- compute_loo(fit_wsls_on_bel)
loo_bel_on_bel   <- compute_loo(fit_bel_on_bel)

# Table + dot
cmp_wsls <- loo_table_and_dot(loo_wsls_on_wsls, loo_bel_on_wsls, label_a = "WSLS", label_b = "Belief")
cmp_bel  <- loo_table_and_dot(loo_wsls_on_bel,  loo_bel_on_bel,  label_a = "WSLS", label_b = "Belief")

cmp_wsls$table$dataset <- "WSLS-generated"
cmp_bel$table$dataset  <- "Belief-generated"

loo_table <- dplyr::bind_rows(cmp_wsls$table, cmp_bel$table)
write_csv(loo_table, file.path(OUT_DATA_DIR, "loo_comparison_table.csv"))

# Dot data: one point per dataset
loo_dot <- dplyr::bind_rows(
  transform(cmp_wsls$dot, dataset = "WSLS-generated"),
  transform(cmp_bel$dot,  dataset = "Belief-generated")
)
write_csv(loo_dot, file.path(OUT_DATA_DIR, "loo_dot.csv"))
plot_loo_dot(loo_dot, file.path(OUT_FIG_DIR, "fig5_loo_dot.png"))

message("\nDone. Outputs are in:")
message(" - ", OUT_DATA_DIR)
message(" - ", OUT_FIG_DIR)

# quarto::quarto_render("assignment1.qmd") # render over here if u want the nice diagram; it doesn't work over the render button :P
