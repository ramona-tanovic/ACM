# 06_empirical_fit.R
# Optional extension: fit multilevel WSLS model to an empirical dataset.

source("R/05_recovery_multilevel.R")  # for prep_transitions() and fit_wsls_hier()
source("R/data_prep.R")

run_empirical_fit <- function(mod) {
  dir.create("outputs/figs", showWarnings = FALSE, recursive = TRUE)
  dir.create("outputs/data", showWarnings = FALSE, recursive = TRUE)

  # Choose one dataset by default (students).
  fp <- file.path("data", "raw", "mp_students.csv")
  if (!file.exists(fp)) {
    message("Empirical dataset not found at: ", fp)
    message("Place the dataset in data/raw/ and rerun.")
    return(invisible(NULL))
  }

  df <- read_csv(fp, show_col_types = FALSE)

  # Standardise column names, map to 0/1 actions and win
  std <- standardise_matching_pennies(df)

  # Filter to a single bot type (present in all datasets): 0-ToM
  if ("BotStrategy" %in% names(std)) {
    std <- std %>% filter(BotStrategy == "0-ToM")
  }

  # Build transitions within participant (and within block if Block exists)
  trans <- build_transitions(std)

  # Fit hierarchical model
  fit <- fit_wsls_hier(mod, trans, prior_only = 0, seed = 910)

  # Posterior summaries for population-level (mu, sigma)
  pop_summ <- posterior::summarise_draws(
    posterior::as_draws_df(fit$draws(variables = c("mu","sigma"))),
    mean, median,
    ~posterior::quantile2(.x, probs = c(0.025, 0.975))
  )
  write_csv(pop_summ, "outputs/data/empirical_population_posterior.csv")

  # Posterior predictive check on simple stats: repeat-rate and conditional repeat rates
  # We use generated quantities from the model: rep_rate, rep_win_rate, rep_loss_rate (per dataset)
  ppc <- posterior::as_draws_df(fit$draws(variables = c("rep_rate","rep_win_rate","rep_loss_rate")))
  obs <- empirical_stats_from_transitions(trans)

  p1 <- ggplot(ppc, aes(x = rep_rate)) +
    geom_histogram(bins = 30, alpha = 0.5) +
    geom_vline(xintercept = obs$repeat_rate, linewidth = 1) +
    labs(title = "Empirical PPC: overall repeat-rate",
         subtitle = "Histogram = posterior predictive; line = observed",
         x = "repeat-rate", y = "count")
  ggsave("outputs/figs/figH_empirical_ppc_repeat_rate.png", p1, width = 8, height = 5, dpi = 200)

  # LOO (optional but requested): predictive comparison is more meaningful when you have multiple models.
  # Here we compute LOO for this fitted model as a quality check and for later comparisons.
  log_lik <- fit$draws("log_lik")
  loo_obj <- loo::loo(log_lik)
  saveRDS(loo_obj, "outputs/data/empirical_loo.rds")

  message("Empirical fit completed (dataset, BotStrategy==0-ToM).")
}
