source(file.path("assignment2_stan-matching-pennies", "R", "03_parameter_recovery.R"))

trial_grid <- c(50, 100, 200, 400)
n_datasets_per_condition <- 20

# Store all recovery results here
recovery_all <- NULL

for (T in trial_grid) {
  for (i in 1:n_datasets_per_condition) {
    
    sim_data <- simulate_single_agent_data(
      trials = T,
      alpha = runif(1, 0.1, 0.9),
      beta = runif(1, 0.5, 4),
      bias = rnorm(1, 0, 1),
      m0 = runif(1, 0.2, 0.8)
    )
    
    stan_data <- list(
      T = nrow(sim_data),
      y = sim_data$choice,
      opponent = sim_data$opponent
    )
    
    fit_tmp <- mod$sample(
      data = stan_data,
      seed = 1234 + i + T,
      chains = 4,
      parallel_chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      refresh = 0
    )
    
    summ <- fit_tmp$summary(variables = c("alpha", "beta", "bias", "m0"))
    
    recovery_row <- tibble(
      trials = T,
      dataset = i,
      alpha_true = unique(sim_data$alpha_true),
      beta_true = unique(sim_data$beta_true),
      bias_true = unique(sim_data$bias_true),
      m0_true = unique(sim_data$m0_true),
      alpha_est = summ$mean[summ$variable == "alpha"],
      beta_est = summ$mean[summ$variable == "beta"],
      bias_est = summ$mean[summ$variable == "bias"],
      m0_est = summ$mean[summ$variable == "m0"]
    )
    
    recovery_all <- bind_rows(recovery_all, recovery_row)
  }
}

readr::write_csv(recovery_all, "parameter_recovery_results_extended.csv")

source(file.path("assignment2_stan-matching-pennies", "R", "03_parameter_recovery.R"))

trial_grid <- c(50, 100, 200, 400)
n_datasets_per_condition <- 20

# Store all recovery results here
recovery_all <- NULL

for (T in trial_grid) {
  for (i in 1:n_datasets_per_condition) {
    
    sim_data <- simulate_single_agent_data(
      trials = T,
      alpha = runif(1, 0.1, 0.9),
      beta = runif(1, 0.5, 4),
      bias = rnorm(1, 0, 1),
      m0 = runif(1, 0.2, 0.8)
    )
    
    stan_data <- list(
      T = nrow(sim_data),
      y = sim_data$choice,
      opponent = sim_data$opponent
    )
    
    fit_tmp <- mod$sample(
      data = stan_data,
      seed = 1234 + i + T,
      chains = 4,
      parallel_chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      refresh = 0
    )
    
    summ <- fit_tmp$summary(variables = c("alpha", "beta", "bias", "m0"))
    
    recovery_row <- tibble(
      trials = T,
      dataset = i,
      alpha_true = unique(sim_data$alpha_true),
      beta_true = unique(sim_data$beta_true),
      bias_true = unique(sim_data$bias_true),
      m0_true = unique(sim_data$m0_true),
      alpha_est = summ$mean[summ$variable == "alpha"],
      beta_est = summ$mean[summ$variable == "beta"],
      bias_est = summ$mean[summ$variable == "bias"],
      m0_est = summ$mean[summ$variable == "m0"]
    )
    
    recovery_all <- bind_rows(recovery_all, recovery_row)
  }
}

readr::write_csv(recovery_all, "parameter_recovery_results_extended.csv")

recovery_summary <- recovery_all |>
  summarise(
    alpha_cor = cor(alpha_true, alpha_est),
    beta_cor = cor(beta_true, beta_est),
    bias_cor = cor(bias_true, bias_est),
    m0_cor = cor(m0_true, m0_est)
  )

print(recovery_summary)

recovery_by_trials <- recovery_all |>
  group_by(trials) |>
  summarise(
    alpha_cor = cor(alpha_true, alpha_est),
    beta_cor = cor(beta_true, beta_est),
    bias_cor = cor(bias_true, bias_est),
    m0_cor = cor(m0_true, m0_est),
    .groups = "drop"
  )

print(recovery_by_trials)
