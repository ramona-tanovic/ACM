source(file.path("assignment2_stan-matching-pennies", "R", "02_fit_model.R"))

# Since we already have:
# - fit
# - stan_data
# - sim_data or empirical_data with columns trial and payoff

y_rep <- fit$draws("y_rep", format = "matrix")

# Convert posterior predictive draws into cumulative mean payoff curves
make_cumulative_curve <- function(choice_vec, opponent_vec) {
  payoff <- as.numeric(choice_vec == opponent_vec)
  cumsum(payoff) / seq_along(payoff)
}

# Use at most 100 posterior draws for plotting
n_draws_plot <- min(100, nrow(y_rep))
draw_ids <- sample(seq_len(nrow(y_rep)), n_draws_plot)

ppc_curves <- lapply(draw_ids, function(i) {
  tibble(
    trial = seq_along(stan_data$opponent),
    cumulative = make_cumulative_curve(y_rep[i, ], stan_data$opponent),
    draw = i
  )
}) |> bind_rows()

observed_curve <- tibble(
  trial = seq_along(stan_data$opponent),
  cumulative = cumsum(as.numeric(stan_data$y == stan_data$opponent)) / seq_along(stan_data$opponent)
)

p_ppc_cumulative <- ggplot() +
  geom_line(data = ppc_curves, aes(trial, cumulative, group = draw), alpha = 0.1) +
  geom_line(data = observed_curve, aes(trial, cumulative), linewidth = 1.2) +
  theme_classic() +
  labs(
    title = "Posterior predictive check: cumulative payoff",
    x = "Trial",
    y = "Cumulative mean payoff"
  )

print(p_ppc_cumulative)

# You must be my posterior predictive check, because you tell me whether my story still works after seeing the data.
