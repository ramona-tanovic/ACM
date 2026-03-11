# ============================================================
# Assignment 2 -- improvement 2:
# extended recovery study with quantitative summaries
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "05_improvement_helpers.R"))

trial_grid <- c(50, 100, 200)
recovery_n <- 10

recovery_rows <- vector("list", length = length(trial_grid) * recovery_n)
row_i <- 1

set.seed(simulation_seed)

for (trials in trial_grid) {
  for (dataset_i in seq_len(recovery_n)) {
    
    true_pars <- sample_true_parameters()
    
    df_sim <- simulate_single_agent_dataset(
      trials = trials,
      alpha = true_pars$alpha,
      beta = true_pars$beta,
      bias = true_pars$bias,
      m0 = true_pars$m0,
      p_opponent = 0.60
    )
    
    fit <- fit_model_to_df(
      df = df_sim,
      stan_filename = basename(model_file),
      seed = sampling_seed + trials * 100 + dataset_i,
      quick = TRUE
    )
    
    s <- fit$summary(variables = c("alpha", "beta", "bias", "m0"))
    
    recovery_rows[[row_i]] <- tibble(
      trials = trials,
      dataset = dataset_i,
      
      alpha_true = true_pars$alpha,
      beta_true  = true_pars$beta,
      bias_true  = true_pars$bias,
      m0_true    = true_pars$m0,
      
      alpha_est = s$mean[s$variable == "alpha"],
      beta_est  = s$mean[s$variable == "beta"],
      bias_est  = s$mean[s$variable == "bias"],
      m0_est    = s$mean[s$variable == "m0"],
      
      alpha_q5 = s$q5[s$variable == "alpha"],
      beta_q5  = s$q5[s$variable == "beta"],
      bias_q5  = s$q5[s$variable == "bias"],
      m0_q5    = s$q5[s$variable == "m0"],
      
      alpha_q95 = s$q95[s$variable == "alpha"],
      beta_q95  = s$q95[s$variable == "beta"],
      bias_q95  = s$q95[s$variable == "bias"],
      m0_q95    = s$q95[s$variable == "m0"]
    )
    
    cat("Finished extended recovery fit for trials =", trials,
        "dataset =", dataset_i, "\n")
    
    row_i <- row_i + 1
  }
}

recovery_results <- bind_rows(recovery_rows)

write_csv(
  recovery_results,
  file.path(derived_dir, "parameter_recovery_results_extended.csv")
)

recovery_long <- bind_rows(
  tibble(
    trials = recovery_results$trials,
    dataset = recovery_results$dataset,
    parameter = "alpha",
    true = recovery_results$alpha_true,
    est = recovery_results$alpha_est,
    q5 = recovery_results$alpha_q5,
    q95 = recovery_results$alpha_q95
  ),
  tibble(
    trials = recovery_results$trials,
    dataset = recovery_results$dataset,
    parameter = "beta",
    true = recovery_results$beta_true,
    est = recovery_results$beta_est,
    q5 = recovery_results$beta_q5,
    q95 = recovery_results$beta_q95
  ),
  tibble(
    trials = recovery_results$trials,
    dataset = recovery_results$dataset,
    parameter = "bias",
    true = recovery_results$bias_true,
    est = recovery_results$bias_est,
    q5 = recovery_results$bias_q5,
    q95 = recovery_results$bias_q95
  ),
  tibble(
    trials = recovery_results$trials,
    dataset = recovery_results$dataset,
    parameter = "m0",
    true = recovery_results$m0_true,
    est = recovery_results$m0_est,
    q5 = recovery_results$m0_q5,
    q95 = recovery_results$m0_q95
  )
) %>%
  mutate(
    error = est - true,
    abs_error = abs(error),
    covered_90 = true >= q5 & true <= q95
  )

write_csv(
  recovery_long,
  file.path(derived_dir, "parameter_recovery_results_extended_long.csv")
)

recovery_summary <- recovery_long %>%
  group_by(trials, parameter) %>%
  summarise(
    n = n(),
    correlation = safe_cor_f(true, est),
    mean_true = mean(true),
    mean_est = mean(est),
    mean_error = mean(error),
    mae = mean(abs_error),
    rmse = rmse_f(error),
    coverage_90 = mean(covered_90),
    .groups = "drop"
  )

recovery_summary_overall <- recovery_long %>%
  group_by(parameter) %>%
  summarise(
    n = n(),
    correlation = safe_cor_f(true, est),
    mean_true = mean(true),
    mean_est = mean(est),
    mean_error = mean(error),
    mae = mean(abs_error),
    rmse = rmse_f(error),
    coverage_90 = mean(covered_90),
    .groups = "drop"
  )

write_csv(
  recovery_summary,
  file.path(derived_dir, "parameter_recovery_summary_extended.csv")
)

write_csv(
  recovery_summary_overall,
  file.path(derived_dir, "parameter_recovery_summary_extended_overall.csv")
)

cat("\nExtended recovery summary:\n")
print(recovery_summary)

plot_alpha <- plot_recovery_parameter(recovery_long, "alpha")
plot_beta  <- plot_recovery_parameter(recovery_long, "beta")
plot_bias  <- plot_recovery_parameter(recovery_long, "bias")
plot_m0    <- plot_recovery_parameter(recovery_long, "m0")

plot_recovery_cor <- ggplot(
  recovery_summary,
  aes(x = factor(trials), y = correlation, color = parameter, group = parameter)
) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Recovery correlation by trial count",
    x = "Number of trials",
    y = "Correlation between true and estimated values"
  )

plot_recovery_mae <- ggplot(
  recovery_summary,
  aes(x = factor(trials), y = mae, color = parameter, group = parameter)
) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Mean absolute error by trial count",
    x = "Number of trials",
    y = "Mean absolute error"
  )

save_plot(plot_alpha, file.path(plots_dir, "recovery_alpha_extended.png"), 8, 5)
save_plot(plot_beta, file.path(plots_dir, "recovery_beta_extended.png"), 8, 5)
save_plot(plot_bias, file.path(plots_dir, "recovery_bias_extended.png"), 8, 5)
save_plot(plot_m0, file.path(plots_dir, "recovery_m0_extended.png"), 8, 5)
save_plot(plot_recovery_cor, file.path(plots_dir, "recovery_summary_correlation_extended.png"), 8, 5)
save_plot(plot_recovery_mae, file.path(plots_dir, "recovery_summary_mae_extended.png"), 8, 5)

print(plot_alpha)
print(plot_beta)
print(plot_bias)
print(plot_m0)
print(plot_recovery_cor)
print(plot_recovery_mae)

# The extended recovery study supported the baseline conclusion that parameter identifiability was uneven. Bias showed the strongest and most consistent recovery, with very high true-estimated correlations across trial counts. Alpha showed moderate recovery, while beta was only partly recoverable and remained somewhat imprecise. The weakest parameter was m0, whose estimates were compressed toward the middle and did not reliably track the true values. Increasing the number of trials improved some recovery measures, especially for bias and beta, but even at 200 trials recovery was not uniformly strong across all parameters.

# You must be my recovery correlation, because I do not trust a parameter until higher truths really lead to higher estimates.
