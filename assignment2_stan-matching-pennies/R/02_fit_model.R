# ============================================================
# Assignment 2 -- fit the Stan model and make the main checks
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))

sim_file <- file.path(sim_dir, "simulated_single_agent_data.csv")

if (!file.exists(sim_file)) {
  source(file.path("assignment2_stan-matching-pennies", "R", "01_simulate_data.R"))
}

df <- read_csv(sim_file, show_col_types = FALSE)

stan_data <- list(
  T = nrow(df),
  y = as.integer(df$choice),
  opponent = as.integer(df$opponent)
)

mod <- cmdstan_model(model_file)

fit <- mod$sample(
  data = stan_data,
  seed = sampling_seed,
  chains = chains_main,
  parallel_chains = chains_main,
  iter_warmup = iter_warmup_main,
  iter_sampling = iter_sampling_main,
  adapt_delta = adapt_delta_main,
  refresh = 200
)

fit$save_object(file.path(fits_dir, "fit_simulated.rds"))

summary_main <- fit$summary(variables = c("alpha", "beta", "bias", "m0"))
write_csv(summary_main, file.path(derived_dir, "fit_summary_simulated.csv"))
print(summary_main)

# prior predictive simulation in R
sample_prior_and_simulate_f <- function(trials, n_sims = 200) {
  out <- rep(NA, n_sims)

  for (i in 1:n_sims) {
    alpha <- rbeta(1, 2, 2)
    beta <- rlnorm(1, 0, 0.5)
    bias <- rnorm(1, 0, 1)
    m0 <- rbeta(1, 2, 2)
    opponent <- rbinom(trials, 1, 0.6)
    memory_now <- m0
    choice <- rep(NA, trials)

    for (t in 1:trials) {
      p_choice <- plogis(bias + beta * (2 * (memory_now - 0.5)))
      choice[t] <- rbinom(1, 1, p_choice)
      memory_now <- memory_now + alpha * (opponent[t] - memory_now)
    }

    out[i] <- mean(choice)
  }

  tibble(mean_choice = out)
}

prior_pred_df <- sample_prior_and_simulate_f(trials = nrow(df), n_sims = 300)
write_csv(prior_pred_df, file.path(derived_dir, "prior_predictive_summary.csv"))

posterior_draws_df <- as_draws_df(fit$draws(c("alpha", "beta", "bias", "m0")))

prior_compare_df <- bind_rows(
  tibble(parameter = "alpha", value = rbeta(2000, 2, 2), source = "Prior"),
  tibble(parameter = "beta", value = rlnorm(2000, 0, 0.5), source = "Prior"),
  tibble(parameter = "bias", value = rnorm(2000, 0, 1), source = "Prior"),
  tibble(parameter = "m0", value = rbeta(2000, 2, 2), source = "Prior"),
  tibble(parameter = "alpha", value = posterior_draws_df$alpha, source = "Posterior"),
  tibble(parameter = "beta", value = posterior_draws_df$beta, source = "Posterior"),
  tibble(parameter = "bias", value = posterior_draws_df$bias, source = "Posterior"),
  tibble(parameter = "m0", value = posterior_draws_df$m0, source = "Posterior")
)

yrep <- fit$draws("y_rep", format = "matrix")

plot_prior <- ggplot(prior_pred_df, aes(mean_choice)) +
  geom_histogram(bins = 30, fill = "grey80", color = "black") +
  geom_vline(xintercept = mean(df$choice), linewidth = 1) +
  labs(
    title = "Prior predictive check",
    x = "Mean simulated choice",
    y = "Count"
  )

plot_update <- ggplot(prior_compare_df, aes(value, color = source, fill = source)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ parameter, scales = "free") +
  labs(
    title = "Prior to posterior update",
    x = NULL,
    y = "Density"
  )

plot_ppc <- ppc_bars(df$choice, yrep[1:100, ]) +
  ggtitle("Posterior predictive check")

save_plot(plot_prior, file.path(plots_dir, "prior_predictive_check.png"), width = 7, height = 5)
save_plot(plot_update, file.path(plots_dir, "prior_posterior_update.png"), width = 9, height = 6)
save_plot(plot_ppc, file.path(plots_dir, "posterior_predictive_check.png"), width = 7, height = 5)

print(plot_prior)
print(plot_update)
print(plot_ppc)

# You must be my adaptive delta, because when things get difficult you help me avoid divergences.
