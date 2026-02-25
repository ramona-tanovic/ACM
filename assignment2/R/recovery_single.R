# recovery_single.R
# Parameter recovery for the single-agent model, across trial lengths.
#
# Why: to validate that parameters are identifiable from finite, noisy behaviour.
# How: simulate with known parameters → fit → compare posterior to truth.

run_recovery_single <- function(mod,
                                Ts = c(30, 60, 120, 240),
                                N = 40,
                                priors_strength = c("weak", "medium", "strong"),
                                seed = 1) {

  set.seed(seed)
  dir.create("outputs/figs", showWarnings = FALSE, recursive = TRUE)
  dir.create("outputs/data", showWarnings = FALSE, recursive = TRUE)

  # Helper: draw "true" parameters from generative priors (medium by default)
  draw_true <- function() {
    list(
      p_win = rbeta(1, 8, 2),
      p_loss = rbeta(1, 2, 8),
      lapse = rbeta(1, 2, 10)
    )
  }

  results <- list()

  for (T in Ts) {
    message("Recovery: T = ", T)

    for (i in 1:N) {
      tru <- draw_true()
      sim <- simulate_wsls_game(T = T, true = tru, seed = 1000 + 10*T + i)

      # Fit under the default (medium) prior
      fit <- fit_wsls_single(mod, sim$a, sim$win, prior_only = 0,
                             priors = default_priors_single("medium"),
                             seed = 2000 + 10*T + i)

      draws <- posterior::as_draws_df(fit$draws(variables = c("p_win","p_loss","lapse")))

      est <- posterior::summarise_draws(draws, mean,
                                        ~posterior::quantile2(.x, probs = c(0.025, 0.975)))

      est <- est %>%
        transmute(
          param = variable,
          mean = mean,
          q025 = q2.5,
          q975 = q97.5
        ) %>%
        mutate(
          T = T,
          sim_id = i,
          true = c(tru$p_win, tru$p_loss, tru$lapse)
        )

      results[[length(results) + 1]] <- est
    }
  }

  rec <- bind_rows(results)
  write_csv(rec, "outputs/data/recovery_single_long.csv")

  # --- Plot: recovery scatter with 95% intervals ----
  p <- ggplot(rec, aes(x = true, y = mean)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    geom_errorbar(aes(ymin = q025, ymax = q975), width = 0) +
    geom_point(alpha = 0.7) +
    facet_grid(param ~ T, scales = "free") +
    labs(
      title = "Single-agent parameter recovery across trial lengths",
      x = "true value",
      y = "posterior mean ± 95% interval",
      subtitle = "Dashed line = perfect recovery"
    )

  ggsave("outputs/figs/fig3_recovery_single.png", p, width = 11, height = 6, dpi = 200)

  # --- Coverage vs T (minimal “how many trials?” answer) ----
  cov <- rec %>%
    mutate(covered = (true >= q025 & true <= q975)) %>%
    group_by(T, param) %>%
    summarise(coverage = mean(covered), .groups = "drop")

  write_csv(cov, "outputs/data/recovery_single_coverage.csv")

  p_cov <- ggplot(cov, aes(x = T, y = coverage)) +
    geom_line() + geom_point() +
    facet_wrap(~param) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Recovery quality (95% coverage) vs trial length",
      x = "T (trials)",
      y = "coverage"
    )

  ggsave("outputs/figs/fig4_recovery_single_coverage.png", p_cov, width = 9, height = 4.8, dpi = 200)

  # --- Prior sensitivity (role of priors) on ONE representative dataset ----
  # We show how posteriors change when T is small vs large.
  for (T0 in c(min(Ts), max(Ts))) {
    tru <- draw_true()
    sim <- simulate_wsls_game(T = T0, true = tru, seed = 999 + T0)

    sens <- list()
    for (ps in priors_strength) {
      fit <- fit_wsls_single(mod, sim$a, sim$win, prior_only = 0,
                             priors = default_priors_single(ps),
                             seed = 888 + T0)

      df <- posterior::as_draws_df(fit$draws(variables = c("p_win","p_loss","lapse"))) %>%
        pivot_longer(everything(), names_to = "param", values_to = "value") %>%
        mutate(prior = ps, T = T0)

      sens[[ps]] <- df
    }

    sens_df <- bind_rows(sens)

    p_sens <- ggplot(sens_df, aes(x = value, fill = prior)) +
      geom_density(alpha = 0.35) +
      facet_wrap(~param, scales = "free") +
      geom_vline(data = tibble::tibble(
        param = c("p_win","p_loss","lapse"),
        true = c(tru$p_win, tru$p_loss, tru$lapse)
      ), aes(xintercept = true), linewidth = 1, inherit.aes = FALSE) +
      labs(
        title = paste0("Prior sensitivity (single agent), T=", T0),
        subtitle = "How much do posteriors depend on priors at this trial length?"
      )

    ggsave(paste0("outputs/figs/fig5_prior_sensitivity_T", T0, ".png"),
           p_sens, width = 9, height = 4.5, dpi = 200)
  }
}
