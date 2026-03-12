# ============================================================
# Assignment 2 -- simulate one dataset from the model
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))

# Opponent generator
# stable learning environment 
simulate_opponent_sequence <- function(trials, p_one = 0.60) {
  rbinom(trials, 1, p_one)
}

# Single-agent memory model simulator
# The agent starts with an initial memory m0. On each trial, memory influences choice probability. After observing the opponent's action, the agent updates memory.

# Update rule:
# new_memory = old_memory + alpha * (opponent - old_memory)

# Interpretation:
# - if alpha is small, memory moves slowly
# - if alpha is large, memory moves quickly toward the new observation

# This is an exponential-forgetting / recency-weighted learner because recent observations matter more than older ones.

simulate_single_agent_memory <- function(
    opponent,
    alpha,
    beta,
    bias,
    m0
) {
  trials <- length(opponent)
  
  # Pre-allocate vectors to store the full trial-by-trial process.
  memory_before <- numeric(trials)
  memory_after <- numeric(trials)
  p_choice <- numeric(trials)
  choice <- integer(trials)
  payoff <- integer(trials)
  
  # Start memory at the initial state.
  memory_now <- m0
  
  for (t in seq_len(trials)) {
    
    # Save the memory state before this trial's update.
    memory_before[t] <- memory_now
    
    # Convert memory into a choice probability
    # Multiplying by 2 just puts the centred memory on a slightly more convenient scale before beta acts on it. 
    eta_t <- bias + beta * (2 * (memory_now - 0.5))
    p_choice[t] <- plogis(eta_t)
    
    # Sample the actual choice from that probability.
    choice[t] <- rbinom(n = 1, size = 1, prob = p_choice[t])
    
    payoff[t] <- as.integer(choice[t] == opponent[t])
    
    # Update memory toward the opponent's latest action
    # Prediction error idea:
    # (opponent[t] - memory_now) is the gap between what the agent currently expects and what actually happened
    # Alpha controls how much of that gap is used to update memory
    memory_now <- memory_now + alpha * (opponent[t] - memory_now)
    
    memory_after[t] <- memory_now
  }
  
  tibble(
    trial = seq_len(trials),
    opponent = opponent,
    choice = choice,
    payoff = payoff,
    memory_before = memory_before,
    memory_after = memory_after,
    p_choice = p_choice
  )
}

# Dataset simulation

set.seed(simulation_seed)

opponent_seq <- simulate_opponent_sequence(
  trials = sim_trials,
  p_one = 0.60
)

df_sim <- simulate_single_agent_memory(
  opponent = opponent_seq,
  alpha = sim_alpha,
  beta = sim_beta,
  bias = sim_bias,
  m0 = sim_m0
)

sim_file <- file.path(sim_dir, "simulated_single_agent_data.csv")
write_csv(df_sim, sim_file)

true_values <- tibble(
  parameter = c("alpha", "beta", "bias", "m0"),
  true_value = c(sim_alpha, sim_beta, sim_bias, sim_m0)
)

write_csv(
  true_values,
  file.path(sim_dir, "simulated_single_agent_true_values.csv")
)

# Plots

# Plot 1: memory trajectory
plot_memory <- ggplot(df_sim, aes(x = trial, y = memory_after)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Simulated memory trajectory",
    subtitle = "How the agent's memory changes across trials",
    x = "Trial",
    y = "Updated memory"
  )

# Plot 2: cumulative mean payoff
df_sim <- df_sim %>%
  mutate(cum_mean_payoff = cumsum(payoff) / trial)

plot_payoff <- ggplot(df_sim, aes(x = trial, y = cum_mean_payoff)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Simulated cumulative mean payoff",
    subtitle = "Broad performance trend across trials",
    x = "Trial",
    y = "Cumulative mean payoff"
  )

# Plot 3: choice probability through time
plot_choice_prob <- ggplot(df_sim, aes(x = trial, y = p_choice)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Simulated choice probability",
    subtitle = "How memory translates into behaviour",
    x = "Trial",
    y = "Probability of choosing 1"
  )

save_plot(
  plot_memory,
  file.path(plots_dir, "simulated_memory_trajectory.png"),
  width = 7,
  height = 5
)

save_plot(
  plot_payoff,
  file.path(plots_dir, "simulated_cumulative_payoff.png"),
  width = 7,
  height = 5
)

save_plot(
  plot_choice_prob,
  file.path(plots_dir, "simulated_choice_probability.png"),
  width = 7,
  height = 5
)

# Summary tables

cat("\nSimulated dataset saved to:\n", sim_file, "\n")
cat("\nTrue generating values:\n")
print(true_values)

cat("\nQuick behavioural summary:\n")
print(
  df_sim %>%
    summarise(
      mean_choice = mean(choice),
      mean_opponent = mean(opponent),
      mean_payoff = mean(payoff),
      final_memory = dplyr::last(memory_after)
    )
)

print(plot_memory) # The memory trajectory shows that the agent is updating continuously rather than staying fixed. It moves up and down because each new opponent action pulls memory a bit in that direction, and alpha = 0.20 makes those updates gradual rather than extreme.
print(plot_payoff) # The cumulative-payoff plot shows that one simulated run can still be noisy. The agent is learning, but this particular run does not end up especially successful overall
print(plot_choice_prob) # The choice-probability plot shows that memory is actually driving behaviour. When memory rises above the neutral point, the probability of choosing 1 rises too; when memory drops, that probability falls. That is the effect of beta.

# You must be my posterior mean, because even when the noise is messy, you still give me something to hold on to.
