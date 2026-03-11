# ============================================================
# Assignment 2 -- improvement 4:
# parameterization comparison
# ============================================================

# Current baseline = constrained model
# Comparison model = legacy unconstrained version

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "05_improvement_helpers.R"))

df <- read_simulated_data()
true_values <- make_true_values_table()

baseline_fit <- load_or_fit_model(
  df = df,
  stan_filename = basename(model_file),
  save_path = file.path(fits_dir, "fit_simulated_baseline.rds"),
  seed = sampling_seed,
  quick = FALSE
)

unconstrained_fit <- load_or_fit_model(
  df = df,
  stan_filename = "exponential_forgetting_single_agent_unconstrained.stan",
  save_path = file.path(fits_dir, "fit_simulated_unconstrained.rds"),
  seed = sampling_seed,
  quick = FALSE
)

baseline_summary <- extract_parameter_summary(baseline_fit, model_name = "Constrained baseline")
unconstrained_summary <- extract_parameter_summary(unconstrained_fit, model_name = "Unconstrained legacy")

compare_summary <- bind_rows(
  baseline_summary,
  unconstrained_summary
)

write_csv(
  compare_summary,
  file.path(derived_dir, "parameterization_compare_summary.csv")
)

diagnostic_summary <- bind_rows(
  extract_diagnostic_summary(baseline_summary, baseline_fit, "Constrained baseline"),
  extract_diagnostic_summary(unconstrained_summary, unconstrained_fit, "Unconstrained legacy")
)

write_csv(
  diagnostic_summary,
  file.path(derived_dir, "parameterization_compare_diagnostics.csv")
)

cat("\nParameterization comparison summary:\n")
print(compare_summary)

cat("\nParameterization comparison diagnostics:\n")
print(diagnostic_summary)

plot_parameter_compare_obj <- plot_parameter_compare(
  compare_summary = compare_summary,
  true_values = true_values,
  title = "Constrained baseline vs unconstrained legacy"
)

baseline_ppc <- make_cumulative_payoff_ppc(baseline_fit, df, n_draws = 200)
unconstrained_ppc <- make_cumulative_payoff_ppc(unconstrained_fit, df, n_draws = 200)

ppc_compare <- combine_ppc_for_models(list(
  `Constrained baseline` = baseline_ppc,
  `Unconstrained legacy` = unconstrained_ppc
))

plot_ppc_compare <- plot_ppc_band_compare(
  ppc_compare_df = ppc_compare,
  title = "Parameterization comparison: cumulative-payoff PPC"
)

coverage_compare <- tibble(
  model = c("Constrained baseline", "Unconstrained legacy"),
  coverage_rate = c(baseline_ppc$coverage_rate, unconstrained_ppc$coverage_rate)
)

write_csv(
  coverage_compare,
  file.path(derived_dir, "parameterization_compare_ppc_coverage.csv")
)

save_plot(
  plot_parameter_compare_obj,
  file.path(plots_dir, "parameterization_compare_parameters.png"),
  width = 9,
  height = 6
)

save_plot(
  plot_ppc_compare,
  file.path(plots_dir, "parameterization_compare_ppc.png"),
  width = 9,
  height = 5
)

print(plot_parameter_compare_obj)
# The constrained and unconstrained versions give almost identical posterior estimates and intervals for all four parameters.
print(plot_ppc_compare)
# The PPC bands are also very similar, and the coverage rates are almost identical (0.88 vs 0.89). The unconstrained version has a slightly wider band in the early trials, but this is not a huge difference.

# Parameterization made essentially no practical difference here, so the main issue is not how the model was written in Stan but the identifiability of the parameters themselves.

# You must be my parameterization comparison, because before I blame the theory, I should first check the math plumbing.
