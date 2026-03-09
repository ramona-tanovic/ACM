# ============================================================
# Assignment 2 -- parameter recovery
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))

mod <- cmdstan_model(model_file)

trial_grid <- c(50, 100, 200)
recovery_n <- 5

simulate_one_dataset_f <- function(trials, alpha, beta, bias, m0) {
  opponent <- rbinom(trials, 1, 0.6)
  memory_now <- m0
  choice <- rep(NA, trials)

  for (t in 1:trials) {
    p_choice <- plogis(bias + beta * (2 * (memory_now - 0.5)))
    choice[t] <- rbinom(1, 1, p_choice)
    memory_now <- memory_now + alpha * (opponent[t] - memory_now)
  }

  tibble(trial = 1:trials, choice = choice, opponent = opponent)
}

recovery_results <- NULL

for (trials in trial_grid) {
  for (i in 1:recovery_n) {
    alpha_true <- rbeta(1, 2, 2)
    beta_true <- rlnorm(1, 0, 0.5)
    bias_true <- rnorm(1, 0, 1)
    m0_true <- rbeta(1, 2, 2)

    df <- simulate_one_dataset_f(trials, alpha_true, beta_true, bias_true, m0_true)

    stan_data <- list(
      T = nrow(df),
      y = as.integer(df$choice),
      opponent = as.integer(df$opponent)
    )

    fit <- mod$sample(
      data = stan_data,
      seed = sampling_seed + i + trials,
      chains = chains_quick,
      parallel_chains = chains_quick,
      iter_warmup = iter_warmup_quick,
      iter_sampling = iter_sampling_quick,
      adapt_delta = adapt_delta_quick,
      refresh = 0
    )

    s <- fit$summary(variables = c("alpha", "beta", "bias", "m0"))

    recovery_results <- bind_rows(
      recovery_results,
      tibble(
        trials = trials,
        dataset = i,
        alpha_true = alpha_true,
        beta_true = beta_true,
        bias_true = bias_true,
        m0_true = m0_true,
        alpha_est = s$mean[s$variable == "alpha"],
        beta_est = s$mean[s$variable == "beta"],
        bias_est = s$mean[s$variable == "bias"],
        m0_est = s$mean[s$variable == "m0"]
      )
    )
  }
}

write_csv(recovery_results, file.path(derived_dir, "parameter_recovery_results.csv"))

plot_alpha <- ggplot(recovery_results, aes(alpha_true, alpha_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~ trials) +
  labs(title = "Recovery of alpha", x = "True", y = "Recovered")

plot_beta <- ggplot(recovery_results, aes(beta_true, beta_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~ trials) +
  labs(title = "Recovery of beta", x = "True", y = "Recovered")

plot_bias <- ggplot(recovery_results, aes(bias_true, bias_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~ trials) +
  labs(title = "Recovery of bias", x = "True", y = "Recovered")

plot_m0 <- ggplot(recovery_results, aes(m0_true, m0_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~ trials) +
  labs(title = "Recovery of m0", x = "True", y = "Recovered")

save_plot(plot_alpha, file.path(plots_dir, "recovery_alpha.png"), width = 8, height = 5)
save_plot(plot_beta, file.path(plots_dir, "recovery_beta.png"), width = 8, height = 5)
save_plot(plot_bias, file.path(plots_dir, "recovery_bias.png"), width = 8, height = 5)
save_plot(plot_m0, file.path(plots_dir, "recovery_m0.png"), width = 8, height = 5)

print(plot_alpha)
print(plot_beta)
print(plot_bias)
print(plot_m0)

# Are you my recovery plot? Because I only trust us when the points stay close to the diagonal.
