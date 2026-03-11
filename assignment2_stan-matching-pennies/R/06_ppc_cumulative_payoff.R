# ============================================================
# Assignment 2 -- improvement 1:
# stronger PPC with cumulative payoff
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "05_improvement_helpers.R"))

df <- read_simulated_data()

baseline_fit <- load_or_fit_model(
  df = df,
  stan_filename = basename(model_file),
  save_path = file.path(fits_dir, "fit_simulated.rds"),
  seed = sampling_seed,
  quick = FALSE
)

ppc_cum <- make_cumulative_payoff_ppc(
  fit_object = baseline_fit,
  df = df,
  n_draws = 200
)

write_csv(
  ppc_cum$band_df,
  file.path(derived_dir, "posterior_predictive_cumulative_payoff_summary.csv")
)

write_csv(
  ppc_cum$coverage_df,
  file.path(derived_dir, "posterior_predictive_cumulative_payoff_coverage.csv")
)

coverage_summary <- tibble(
  coverage_rate = ppc_cum$coverage_rate
)

write_csv(
  coverage_summary,
  file.path(derived_dir, "posterior_predictive_cumulative_payoff_overall.csv")
)

cat("\nCumulative-payoff PPC summary:\n")
print(coverage_summary)

plot_ppc_cum_cloud <- plot_cumulative_ppc_cloud(
  ppc_object = ppc_cum,
  title = "Improvement PPC: cumulative mean payoff"
)

plot_ppc_cum_band <- plot_cumulative_ppc_band(
  ppc_object = ppc_cum,
  title = "Improvement PPC: cumulative payoff envelope"
)

save_plot(
  plot_ppc_cum_cloud,
  file.path(plots_dir, "improvement_ppc_cumulative_payoff_cloud.png"),
  width = 8,
  height = 5
)

save_plot(
  plot_ppc_cum_band,
  file.path(plots_dir, "improvement_ppc_cumulative_payoff_band.png"),
  width = 8,
  height = 5
)

print(plot_ppc_cum_cloud)
# The curve generally lives inside the range of trajectories the fitted model considers plausible - good. It means the model is not obviously failing to reproduce the broad pattern of success across trials
print(plot_ppc_cum_band)
# decent, not perfect. The model captures the broad trajectory reasonably well, but not flawlessly

# The main mismatch is early in the task. The observed curve dips quite low in the early trials, more than the model’s predictive median expects. That suggests the model smooths the learning dynamics a bit

# You must be my cumulative PPC, because you do not just ask whether I chose, you ask whether I learned.
