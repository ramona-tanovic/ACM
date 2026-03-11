# ============================================================
# Assignment 2 -- improvement 5:
# lapse-model comparison
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "05_improvement_helpers.R"))

df <- read_simulated_data()
true_values <- make_true_values_table(include_lapse = TRUE, lapse_value = 0)

baseline_fit <- load_or_fit_model(
  df = df,
  stan_filename = basename(model_file),
  save_path = file.path(fits_dir, "fit_simulated_baseline.rds"),
  seed = sampling_seed,
  quick = FALSE
)

lapse_fit <- load_or_fit_model(
  df = df,
  stan_filename = "exponential_forgetting_single_agent_lapse.stan",
  save_path = file.path(fits_dir, "fit_simulated_lapse.rds"),
  seed = sampling_seed,
  quick = FALSE
)

baseline_summary <- extract_parameter_summary(
  baseline_fit,
  variables = c("alpha", "beta", "bias", "m0"),
  model_name = "Baseline"
)

lapse_summary <- extract_parameter_summary(
  lapse_fit,
  variables = c("alpha", "beta", "bias", "m0", "lapse"),
  model_name = "Lapse model"
)

common_compare_summary <- bind_rows(
  baseline_summary,
  lapse_summary %>% filter(parameter %in% c("alpha", "beta", "bias", "m0"))
)

write_csv(
  common_compare_summary,
  file.path(derived_dir, "lapse_compare_common_parameters.csv")
)

write_csv(
  lapse_summary,
  file.path(derived_dir, "lapse_compare_full_lapse_summary.csv")
)

diagnostic_summary <- bind_rows(
  extract_diagnostic_summary(baseline_summary, baseline_fit, "Baseline"),
  extract_diagnostic_summary(lapse_summary, lapse_fit, "Lapse model")
)

write_csv(
  diagnostic_summary,
  file.path(derived_dir, "lapse_compare_diagnostics.csv")
)

cat("\nLapse comparison summary:\n")
print(lapse_summary)

cat("\nLapse comparison diagnostics:\n")
print(diagnostic_summary)

plot_parameter_compare_obj <- plot_parameter_compare(
  compare_summary = common_compare_summary,
  true_values = true_values %>% filter(parameter %in% c("alpha", "beta", "bias", "m0")),
  title = "Baseline vs lapse model"
)

# Lapse posterior on its own:
lapse_draws <- as_draws_df(lapse_fit$draws("lapse"))

plot_lapse_density <- ggplot(lapse_draws, aes(x = lapse)) +
  geom_density(fill = "grey80", color = "black") +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(
    title = "Posterior for lapse parameter",
    subtitle = "Dashed line = true value (0 in the baseline simulated dataset)",
    x = "Lapse",
    y = "Density"
  )

baseline_ppc <- make_cumulative_payoff_ppc(baseline_fit, df, n_draws = 200)
lapse_ppc <- make_cumulative_payoff_ppc(lapse_fit, df, n_draws = 200)

ppc_compare <- combine_ppc_for_models(list(
  Baseline = baseline_ppc,
  `Lapse model` = lapse_ppc
))

plot_ppc_compare <- plot_ppc_band_compare(
  ppc_compare_df = ppc_compare,
  title = "Baseline vs lapse model: cumulative-payoff PPC"
)

coverage_compare <- tibble(
  model = c("Baseline", "Lapse model"),
  coverage_rate = c(baseline_ppc$coverage_rate, lapse_ppc$coverage_rate)
)

write_csv(
  coverage_compare,
  file.path(derived_dir, "lapse_compare_ppc_coverage.csv")
)

save_plot(
  plot_parameter_compare_obj,
  file.path(plots_dir, "lapse_compare_parameters.png"),
  width = 9,
  height = 6
)

save_plot(
  plot_lapse_density,
  file.path(plots_dir, "lapse_compare_lapse_density.png"),
  width = 7,
  height = 5
)

save_plot(
  plot_ppc_compare,
  file.path(plots_dir, "lapse_compare_ppc.png"),
  width = 9,
  height = 5
)

print(plot_parameter_compare_obj)
print(plot_lapse_density)
print(plot_ppc_compare)

# Parameter comparison: Adding a lapse parameter barely changes the estimates for alpha, beta, bias, or m0.
# Lapse posterior: The lapse estimate is concentrated near zero, which is what we would expect because the simulated baseline data were generated with no lapse.
# Cumulative-payoff PPC: The lapse model fits the learning trajectory almost the same as the baseline model.

# You must be my lapse density, because if you sit near zero, I know the extra complexity was mostly just flirting.
