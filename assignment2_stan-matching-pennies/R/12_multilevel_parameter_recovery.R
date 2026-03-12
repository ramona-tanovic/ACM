# ============================================================
# Assignment 2 -- multilevel parameter recovery
# ============================================================

# This recovery script focuses on population-level recovery.
#
# Why?
# Because in a multilevel model, the cleanest first question is:
# can I recover the population means and population SDs?
#
# Individual-level recovery exists too, but it is harder and more
# affected by shrinkage, so this script starts with the group level.

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "05_improvement_helpers.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "11_multilevel_helpers.R"))

subject_grid <- c(20, 40, 80)
trials_per_subject <- 20
recovery_n <- 6

set.seed(simulation_seed)

recovery_rows <- vector("list", length = length(subject_grid) * recovery_n)
row_i <- 1

for (n_subjects in subject_grid) {
  for (dataset_i in seq_len(recovery_n)) {
    
    # Sample one population-level truth for this synthetic dataset
    alpha_mean_true <- runif(1, 0.15, 0.35)
    beta_mean_true  <- runif(1, 1.5, 3.5)
    bias_mean_true  <- rnorm(1, 0, 0.4)
    m0_mean_true    <- runif(1, 0.35, 0.65)
    
    sigma_alpha_true    <- runif(1, 0.10, 0.40)
    sigma_log_beta_true <- runif(1, 0.10, 0.35)
    sigma_bias_true     <- runif(1, 0.10, 0.60)
    sigma_m0_true       <- runif(1, 0.10, 0.35)
    
    sim_obj <- simulate_multilevel_memory_dataset(
      n_subjects = n_subjects,
      trials_per_subject = trials_per_subject,
      alpha_mean = alpha_mean_true,
      beta_mean = beta_mean_true,
      bias_mean = bias_mean_true,
      m0_mean = m0_mean_true,
      alpha_sd = sigma_alpha_true,
      beta_sd = sigma_log_beta_true,
      bias_sd = sigma_bias_true,
      m0_sd = sigma_m0_true,
      p_opponent = 0.60
    )
    
    prep_obj <- prepare_multilevel_stan_data(sim_obj$data)
    
    fit <- fit_multilevel_model(
      stan_data = prep_obj$stan_data,
      stan_filename = "exponential_forgetting_multilevel_singleblock.stan",
      seed = sampling_seed + n_subjects * 100 + dataset_i,
      quick = TRUE
    )
    
    pop_summary <- extract_multilevel_population_summary(fit)
    
    get_mean <- function(par_name) {
      pop_summary$mean[pop_summary$parameter == par_name]
    }
    
    recovery_rows[[row_i]] <- tibble(
      n_subjects = n_subjects,
      trials_per_subject = trials_per_subject,
      dataset = dataset_i,
      
      alpha_mean_true = alpha_mean_true,
      beta_mean_true  = beta_mean_true,
      bias_mean_true  = bias_mean_true,
      m0_mean_true    = m0_mean_true,
      
      sigma_alpha_true    = sigma_alpha_true,
      sigma_log_beta_true = sigma_log_beta_true,
      sigma_bias_true     = sigma_bias_true,
      sigma_m0_true       = sigma_m0_true,
      
      alpha_mean_est = get_mean("alpha_mean_nat"),
      beta_mean_est  = get_mean("beta_mean_nat"),
      bias_mean_est  = get_mean("bias_mean_nat"),
      m0_mean_est    = get_mean("m0_mean_nat"),
      
      sigma_alpha_est    = get_mean("sigma_alpha"),
      sigma_log_beta_est = get_mean("sigma_log_beta"),
      sigma_bias_est     = get_mean("sigma_bias"),
      sigma_m0_est       = get_mean("sigma_m0")
    )
    
    cat("Finished multilevel recovery fit:",
        "subjects =", n_subjects,
        "dataset =", dataset_i, "\n")
    
    row_i <- row_i + 1
  }
}

recovery_results <- bind_rows(recovery_rows)

write_csv(
  recovery_results,
  file.path(derived_dir, "multilevel_parameter_recovery_results.csv")
)

# summary

recovery_long <- bind_rows(
  tibble(
    n_subjects = recovery_results$n_subjects,
    dataset = recovery_results$dataset,
    parameter = "alpha_mean_nat",
    true = recovery_results$alpha_mean_true,
    est = recovery_results$alpha_mean_est,
    type = "Population mean"
  ),
  tibble(
    n_subjects = recovery_results$n_subjects,
    dataset = recovery_results$dataset,
    parameter = "beta_mean_nat",
    true = recovery_results$beta_mean_true,
    est = recovery_results$beta_mean_est,
    type = "Population mean"
  ),
  tibble(
    n_subjects = recovery_results$n_subjects,
    dataset = recovery_results$dataset,
    parameter = "bias_mean_nat",
    true = recovery_results$bias_mean_true,
    est = recovery_results$bias_mean_est,
    type = "Population mean"
  ),
  tibble(
    n_subjects = recovery_results$n_subjects,
    dataset = recovery_results$dataset,
    parameter = "m0_mean_nat",
    true = recovery_results$m0_mean_true,
    est = recovery_results$m0_mean_est,
    type = "Population mean"
  ),
  tibble(
    n_subjects = recovery_results$n_subjects,
    dataset = recovery_results$dataset,
    parameter = "sigma_alpha",
    true = recovery_results$sigma_alpha_true,
    est = recovery_results$sigma_alpha_est,
    type = "Population SD"
  ),
  tibble(
    n_subjects = recovery_results$n_subjects,
    dataset = recovery_results$dataset,
    parameter = "sigma_log_beta",
    true = recovery_results$sigma_log_beta_true,
    est = recovery_results$sigma_log_beta_est,
    type = "Population SD"
  ),
  tibble(
    n_subjects = recovery_results$n_subjects,
    dataset = recovery_results$dataset,
    parameter = "sigma_bias",
    true = recovery_results$sigma_bias_true,
    est = recovery_results$sigma_bias_est,
    type = "Population SD"
  ),
  tibble(
    n_subjects = recovery_results$n_subjects,
    dataset = recovery_results$dataset,
    parameter = "sigma_m0",
    true = recovery_results$sigma_m0_true,
    est = recovery_results$sigma_m0_est,
    type = "Population SD"
  )
) %>%
  mutate(
    error = est - true,
    abs_error = abs(error)
  )

write_csv(
  recovery_long,
  file.path(derived_dir, "multilevel_parameter_recovery_results_long.csv")
)

recovery_summary <- recovery_long %>%
  group_by(n_subjects, parameter, type) %>%
  summarise(
    n = n(),
    correlation = safe_cor_f(true, est),
    mean_true = mean(true),
    mean_est = mean(est),
    mean_error = mean(error),
    mae = mean(abs_error),
    rmse = rmse_f(error),
    .groups = "drop"
  )

write_csv(
  recovery_summary,
  file.path(derived_dir, "multilevel_parameter_recovery_summary.csv")
)

cat("\nMultilevel recovery summary:\n")
print(recovery_summary)

# plots

plot_pop_means <- ggplot(
  recovery_long %>% filter(type == "Population mean"),
  aes(x = true, y = est)
) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_grid(parameter ~ n_subjects, scales = "free") +
  labs(
    title = "Multilevel recovery: population means",
    x = "True value",
    y = "Estimated value"
  )

plot_pop_sds <- ggplot(
  recovery_long %>% filter(type == "Population SD"),
  aes(x = true, y = est)
) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_grid(parameter ~ n_subjects, scales = "free") +
  labs(
    title = "Multilevel recovery: population SDs",
    x = "True value",
    y = "Estimated value"
  )

plot_recovery_cor <- ggplot(
  recovery_summary,
  aes(x = factor(n_subjects), y = correlation, color = parameter, group = parameter)
) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ type, scales = "free_y") +
  labs(
    title = "Multilevel recovery correlation by number of subjects",
    x = "Number of subjects",
    y = "Correlation"
  )

plot_recovery_mae <- ggplot(
  recovery_summary,
  aes(x = factor(n_subjects), y = mae, color = parameter, group = parameter)
) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ type, scales = "free_y") +
  labs(
    title = "Multilevel recovery MAE by number of subjects",
    x = "Number of subjects",
    y = "Mean absolute error"
  )

save_plot(
  plot_pop_means,
  file.path(plots_dir, "multilevel_recovery_population_means.png"),
  width = 10,
  height = 8
)

save_plot(
  plot_pop_sds,
  file.path(plots_dir, "multilevel_recovery_population_sds.png"),
  width = 10,
  height = 8
)

save_plot(
  plot_recovery_cor,
  file.path(plots_dir, "multilevel_recovery_correlation.png"),
  width = 9,
  height = 6
)

save_plot(
  plot_recovery_mae,
  file.path(plots_dir, "multilevel_recovery_mae.png"),
  width = 9,
  height = 6
)

print(plot_pop_means)
print(plot_pop_sds)
print(plot_recovery_cor)
print(plot_recovery_mae)

# You must be my population mean, because even when everyone differs, there is still a pattern I can come back to.
