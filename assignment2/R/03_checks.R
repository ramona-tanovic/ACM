# 03_checks.R
# Prior predictive, posterior predictive, and prior→posterior update checks.

make_single_agent_checks <- function(mod, fit, sim) {
  dir.create("outputs/figs", showWarnings = FALSE, recursive = TRUE)
  dir.create("outputs/data", showWarnings = FALSE, recursive = TRUE)

  # --- Observed summary statistics (from the simulated dataset) ----
  obs <- summarise_behavior(sim$a, sim$win)

  # --- Prior predictive: sample from priors only (skip likelihood) ----
  # "I’d run a thousand simulations just to reproduce that smile."
  fit_prior <- fit_wsls_single(
    mod = mod,
    a = sim$a,
    win = sim$win,
    prior_only = 1,
    priors = default_priors_single("medium"),
    seed = 1234
  )

  # Extract generated quantities summaries
  post_draws  <- fit$draws(variables = c("rep_rate", "rep_win_rate", "rep_loss_rate",
                                        "p_win", "p_loss", "lapse"))
  prior_draws <- fit_prior$draws(variables = c("rep_rate", "rep_win_rate", "rep_loss_rate",
                                              "p_win", "p_loss", "lapse"))

  post_df  <- posterior::as_draws_df(post_draws)
  prior_df <- posterior::as_draws_df(prior_draws)

  # --- Plot 1: Prior predictive vs posterior predictive for repeat-rate ----
  p1 <- ggplot() +
    geom_histogram(data = prior_df, aes(x = rep_rate, y = after_stat(density)),
                   bins = 30, alpha = 0.4) +
    geom_histogram(data = post_df, aes(x = rep_rate, y = after_stat(density)),
                   bins = 30, alpha = 0.4) +
    geom_vline(xintercept = obs$repeat_rate, linewidth = 1) +
    labs(
      title = "Prior vs posterior predictive check: overall repeat-rate",
      x = "repeat-rate (t>=2)",
      y = "density",
      subtitle = "vertical line = observed (simulated) statistic"
    )

  ggsave("outputs/figs/figA_prior_post_pred_repeat_rate.png", p1, width = 8, height = 5, dpi = 200)

  # --- Plot 2: Prior → posterior update for parameters ----
  # overlay densities to see if data updated beliefs
  long_prior <- prior_df %>%
    select(p_win, p_loss, lapse) %>%
    pivot_longer(everything(), names_to = "param", values_to = "value") %>%
    mutate(source = "prior")

  long_post <- post_df %>%
    select(p_win, p_loss, lapse) %>%
    pivot_longer(everything(), names_to = "param", values_to = "value") %>%
    mutate(source = "posterior")

  long_all <- bind_rows(long_prior, long_post)

  p2 <- ggplot(long_all, aes(x = value, fill = source)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~param, scales = "free") +
    geom_vline(data = tibble::tibble(
      param = c("p_win","p_loss","lapse"),
      true = c(sim$true$p_win, sim$true$p_loss, sim$true$lapse)
    ), aes(xintercept = true), linewidth = 1, inherit.aes = FALSE) +
    labs(
      title = "Prior → posterior update check (single agent)",
      x = "parameter value",
      y = "density",
      subtitle = "vertical line = true generating value (for this simulated dataset)"
    )

  ggsave("outputs/figs/figB_prior_posterior_update.png", p2, width = 9, height = 4.5, dpi = 200)

  # Save a small table used in the QMD
  summ <- posterior::summarise_draws(
    posterior::as_draws_df(fit$draws(variables = c("p_win","p_loss","lapse"))),
    mean, median,
    ~posterior::quantile2(.x, probs = c(0.025, 0.975))
  )
  write_csv(summ, "outputs/data/single_agent_posterior_summary.csv")
  write_csv(obs,  "outputs/data/single_agent_observed_summary.csv")
}
