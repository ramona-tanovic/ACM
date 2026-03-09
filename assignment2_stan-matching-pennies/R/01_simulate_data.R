# ============================================================
# Assignment 2 -- simulate data from the model
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))

make_opponent_f <- function(trials, rate = 0.60) {
  rbinom(trials, 1, rate)
}

make_block_opponent_f <- function(trials) {
  out <- rep(NA, trials)
  for (t in 1:trials) {
    block <- floor((t - 1) / 20)
    if (block %% 2 == 0) {
      out[t] <- rbinom(1, 1, 0.80)
    } else {
      out[t] <- rbinom(1, 1, 0.20)
    }
  }
  out
}

simulate_memory_model_f <- function(trials = sim_trials,
                                    alpha = sim_alpha,
                                    beta = sim_beta,
                                    bias = sim_bias,
                                    m0 = sim_m0,
                                    opponent_type = "block") {
  choice <- rep(NA, trials)
  opponent <- rep(NA, trials)
  memory_before <- rep(NA, trials)
  memory_after <- rep(NA, trials)
  p_choice <- rep(NA, trials)

  if (opponent_type == "block") {
    opponent <- make_block_opponent_f(trials)
  } else {
    opponent <- make_opponent_f(trials, rate = 0.60)
  }

  memory_now <- m0

  for (t in 1:trials) {
    memory_before[t] <- memory_now
    p_choice[t] <- plogis(bias + beta * (2 * (memory_now - 0.5)))
    choice[t] <- rbinom(1, 1, p_choice[t])
    memory_now <- memory_now + alpha * (opponent[t] - memory_now)
    memory_after[t] <- memory_now
  }

  tibble(
    trial = 1:trials,
    choice = choice,
    opponent = opponent,
    payoff = as.numeric(choice == opponent),
    memory_before = memory_before,
    memory_after = memory_after,
    p_choice = p_choice,
    alpha_true = alpha,
    beta_true = beta,
    bias_true = bias,
    m0_true = m0
  )
}

sim_df <- simulate_memory_model_f()

write_csv(sim_df, file.path(sim_dir, "simulated_single_agent_data.csv"))

plot_sim_1 <- ggplot(sim_df, aes(trial, memory_before)) +
  geom_line() +
  geom_point(aes(y = opponent), alpha = 0.35) +
  labs(
    title = "Simulated memory state over trials",
    x = "Trial",
    y = "Memory / opponent choice"
  )

plot_sim_2 <- ggplot(sim_df, aes(trial, cumsum(payoff) / trial)) +
  geom_line() +
  labs(
    title = "Cumulative mean payoff in the simulated data",
    x = "Trial",
    y = "Cumulative mean payoff"
  )

save_plot(plot_sim_1, file.path(plots_dir, "simulated_memory_trajectory.png"), width = 8, height = 5)
save_plot(plot_sim_2, file.path(plots_dir, "simulated_cumulative_payoff.png"), width = 8, height = 5)

print(plot_sim_1)
print(plot_sim_2)

# You must be my latent state, because even when I cannot see you directly, you still explain my choices.
