# ============================================================
# Assignment 2 -- empirical example with the multilevel model
# ============================================================

# I use the multilevel model here, not the single-agent model, because the empirical dataset contains many participants.

# I also keep the empirical subset simple:
# students data, Human framing, 0-ToM bot, first 20 trials per participant.

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "05_improvement_helpers.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "11_multilevel_helpers.R"))

# load data

df_emp <- prepare_empirical_students_subset(
  framing = "Human",
  bot_strategy = "0-ToM",
  trials_keep = 20
)

prep_obj <- prepare_multilevel_stan_data(df_emp)

df_model <- prep_obj$df_model
subject_lookup <- prep_obj$subject_lookup
stan_data <- prep_obj$stan_data

empirical_data_summary <- tibble(
  n_subjects = n_distinct(df_model$ID),
  n_rows = nrow(df_model),
  trials_per_subject = median(table(df_model$ID)),
  mean_choice = mean(df_model$Decision),
  mean_payoff = mean(df_model$Payoff == 1)
)

write_csv(
  empirical_data_summary,
  file.path(derived_dir, "empirical_students_data_summary.csv")
)

cat("\nEmpirical data summary:\n")
print(empirical_data_summary)

# fit

fit_empirical <- load_or_fit_multilevel_model(
  stan_data = stan_data,
  save_path = file.path(fits_dir, "fit_empirical_multilevel_students_0tom_human.rds"),
  stan_filename = "exponential_forgetting_multilevel_singleblock.stan",
  seed = sampling_seed,
  quick = FALSE
)

# diagnostic/summary

population_summary <- extract_multilevel_population_summary(
  fit_empirical,
  model_name = "Empirical multilevel"
)

diagnostic_summary <- extract_diagnostic_summary(
  param_summary = population_summary,
  fit_object = fit_empirical,
  model_name = "Empirical multilevel"
)

subject_summary <- extract_multilevel_subject_summary(
  fit_object = fit_empirical,
  subject_lookup = subject_lookup
)

write_csv(
  population_summary,
  file.path(derived_dir, "empirical_multilevel_population_summary.csv")
)

write_csv(
  diagnostic_summary,
  file.path(derived_dir, "empirical_multilevel_diagnostics.csv")
)

write_csv(
  subject_summary,
  file.path(derived_dir, "empirical_multilevel_subject_summary.csv")
)

cat("\nPopulation summary:\n")
print(population_summary)

cat("\nDiagnostic summary:\n")
print(diagnostic_summary)

# Population-level plot

population_plot_df <- population_summary %>%
  filter(parameter %in% c("alpha_mean_nat", "beta_mean_nat", "bias_mean_nat", "m0_mean_nat")) %>%
  mutate(
    parameter = factor(
      parameter,
      levels = c("alpha_mean_nat", "beta_mean_nat", "bias_mean_nat", "m0_mean_nat"),
      labels = c("alpha_mean", "beta_mean", "bias_mean", "m0_mean")
    )
  )

plot_population <- ggplot(population_plot_df, aes(x = parameter, y = mean)) +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.15) +
  geom_point(size = 2) +
  facet_wrap(~ parameter, scales = "free_y") +
  labs(
    title = "Empirical multilevel fit: population-level parameters",
    subtitle = "Points = posterior means, bars = 90% intervals",
    x = NULL,
    y = "Posterior summary"
  )

# Subject-level parameter distributions

plot_subject_distributions <- ggplot(subject_summary, aes(x = mean)) +
  geom_histogram(bins = 20, fill = "grey80", color = "black") +
  facet_wrap(~ parameter, scales = "free") +
  labs(
    title = "Empirical multilevel fit: subject-level parameter estimates",
    x = "Posterior mean",
    y = "Count"
  )

# Trialwise mean-choice PPC

# For the empirical data I use a choice-based PPC rather than payoff, because the observed dataset gives me direct choices for everyone, and this is the most straightforward multilevel check.
ppc_trial_choice <- make_trialwise_mean_choice_ppc(
  fit_object = fit_empirical,
  df_model = df_model,
  n_draws = 200
)

write_csv(
  ppc_trial_choice$band_df,
  file.path(derived_dir, "empirical_multilevel_trialwise_choice_ppc.csv")
)

write_csv(
  ppc_trial_choice$coverage_df,
  file.path(derived_dir, "empirical_multilevel_trialwise_choice_ppc_coverage.csv")
)

ppc_summary <- tibble(
  coverage_rate = ppc_trial_choice$coverage_rate
)

write_csv(
  ppc_summary,
  file.path(derived_dir, "empirical_multilevel_ppc_summary.csv")
)

cat("\nEmpirical PPC summary:\n")
print(ppc_summary)

plot_trialwise_ppc <- plot_trialwise_mean_choice_ppc(
  ppc_object = ppc_trial_choice,
  title = "Empirical multilevel PPC: trialwise mean choice"
)

# plots

save_plot(
  plot_population,
  file.path(plots_dir, "empirical_multilevel_population_parameters.png"),
  width = 9,
  height = 6
)

save_plot(
  plot_subject_distributions,
  file.path(plots_dir, "empirical_multilevel_subject_distributions.png"),
  width = 9,
  height = 6
)

save_plot(
  plot_trialwise_ppc,
  file.path(plots_dir, "empirical_multilevel_trialwise_choice_ppc.png"),
  width = 8,
  height = 5
)

print(plot_population)
print(plot_subject_distributions)
print(plot_trialwise_ppc)
   
# You must be my empirical dataset, because after all the simulations, you are where the model has to stop flirting and start proving itself.
