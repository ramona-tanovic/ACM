# ============================================================
# Assignment 2 -- improvement 3:
# prior sensitivity analysis
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "05_improvement_helpers.R"))

df <- read_simulated_data()
true_values <- make_true_values_table()

# Compare

set.seed(simulation_seed)

prior_pred_baseline <- simulate_from_priors(
  trials = nrow(df),
  n_sims = 300,
  p_opponent = 0.60,
  prior_type = "baseline"
)

prior_pred_tighter <- simulate_from_priors(
  trials = nrow(df),
  n_sims = 300,
  p_opponent = 0.60,
  prior_type = "tighter"
)

prior_pred_compare <- bind_rows(
  prior_pred_baseline %>% mutate(model = "Baseline priors"),
  prior_pred_tighter %>% mutate(model = "Tighter priors")
)

write_csv(
  prior_pred_compare,
  file.path(derived_dir, "prior_sensitivity_prior_predictive_compare.csv")
)

plot_prior_choice_compare <- ggplot(
  prior_pred_compare,
  aes(x = mean_choice, color = model, fill = model)
) +
  geom_density(alpha = 0.20) +
  geom_vline(xintercept = mean(df$choice), linewidth = 1) +
  labs(
    title = "Prior sensitivity: prior predictive mean choice",
    subtitle = "Vertical line = observed mean choice",
    x = "Mean simulated choice",
    y = "Density"
  )

plot_prior_payoff_compare <- ggplot(
  prior_pred_compare,
  aes(x = mean_payoff, color = model, fill = model)
) +
  geom_density(alpha = 0.20) +
  geom_vline(xintercept = mean(df$payoff), linewidth = 1) +
  labs(
    title = "Prior sensitivity: prior predictive mean payoff",
    subtitle = "Vertical line = observed mean payoff",
    x = "Mean simulated payoff",
    y = "Density"
  )

# Fit

baseline_fit <- load_or_fit_model(
  df = df,
  stan_filename = basename(model_file),
  save_path = file.path(fits_dir, "fit_simulated_baseline.rds"),
  seed = sampling_seed,
  quick = FALSE
)

tighter_fit <- load_or_fit_model(
  df = df,
  stan_filename = "exponential_forgetting_single_agent_tighterpriors.stan",
  save_path = file.path(fits_dir, "fit_simulated_tighterpriors.rds"),
  seed = sampling_seed,
  quick = FALSE
)

baseline_summary <- extract_parameter_summary(baseline_fit, model_name = "Baseline priors")
tighter_summary <- extract_parameter_summary(tighter_fit, model_name = "Tighter priors")

compare_summary <- bind_rows(baseline_summary, tighter_summary)

write_csv(
  compare_summary,
  file.path(derived_dir, "prior_sensitivity_parameter_compare.csv")
)

diagnostic_summary <- bind_rows(
  extract_diagnostic_summary(baseline_summary, baseline_fit, "Baseline priors"),
  extract_diagnostic_summary(tighter_summary, tighter_fit, "Tighter priors")
)

write_csv(
  diagnostic_summary,
  file.path(derived_dir, "prior_sensitivity_diagnostics.csv")
)

cat("\nPrior sensitivity parameter summary:\n")
print(compare_summary)

cat("\nPrior sensitivity diagnostics:\n")
print(diagnostic_summary)

plot_parameter_priors_compare <- plot_parameter_compare(
  compare_summary = compare_summary,
  true_values = true_values,
  title = "Baseline priors vs tighter priors"
)

# Compare cp ppc

baseline_ppc <- make_cumulative_payoff_ppc(baseline_fit, df, n_draws = 200)
tighter_ppc <- make_cumulative_payoff_ppc(tighter_fit, df, n_draws = 200)

ppc_compare <- combine_ppc_for_models(list(
  `Baseline priors` = baseline_ppc,
  `Tighter priors` = tighter_ppc
))

plot_ppc_compare <- plot_ppc_band_compare(
  ppc_compare_df = ppc_compare,
  title = "Prior sensitivity: cumulative-payoff PPC"
)

coverage_compare <- tibble(
  model = c("Baseline priors", "Tighter priors"),
  coverage_rate = c(baseline_ppc$coverage_rate, tighter_ppc$coverage_rate)
)

write_csv(
  coverage_compare,
  file.path(derived_dir, "prior_sensitivity_ppc_coverage.csv")
)

# Plots

save_plot(plot_prior_choice_compare, file.path(plots_dir, "prior_sensitivity_prior_choice.png"), 8, 5)
save_plot(plot_prior_payoff_compare, file.path(plots_dir, "prior_sensitivity_prior_payoff.png"), 8, 5)
save_plot(plot_parameter_priors_compare, file.path(plots_dir, "prior_sensitivity_parameter_compare.png"), 9, 6)
save_plot(plot_ppc_compare, file.path(plots_dir, "prior_sensitivity_ppc_compare.png"), 9, 5)

print(plot_prior_choice_compare)
print(plot_prior_payoff_compare)
print(plot_parameter_priors_compare)
print(plot_ppc_compare)

# Reflection:
# Prior predictive mean choice: Tighter priors produce a narrower, more centered distribution. The observed mean choice is plausible under both, but the tighter priors are more restrictive.
# Prior predictive mean payoff: Same pattern. Tighter priors allow a narrower range of payoff behaviour, but the observed payoff is still plausible under both.
# Parameter comparison: Posterior estimates are very similar under baseline and tighter priors. The main visible effect is slightly more shrinkage and slightly narrower intervals with tighter priors, especially for beta and m0.
# Cumulative-payoff PPC: Both prior settings give almost the same fit to the learning trajectory. So changing the priors did not materially improve the behavioural fit.

# You must be my prior sensitivity analysis, because you tell me whether the story is in the data or in the assumptions.
