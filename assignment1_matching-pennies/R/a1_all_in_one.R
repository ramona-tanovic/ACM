# ============================================================
# Assignment 1 -- all in one script
# ============================================================

source(file.path("R", "00_global_setup.R"))

# The matching pennies game -> parameters into data?

# Opponents

# Random agent, no learning, no feedback
# I use this as a baseline, to see how the other agents do compared to pure chance.
RandomAgent_f <- function(rate) {
  choice <- rbinom(1, 1, rate)
  return(choice)
}

# Agent without noise
WSLSAgent_f <- function(prevChoice, feedback) {
  if (feedback == 1) {
    choice <- prevChoice
  } else {
    choice <- 1 - prevChoice
  }
  return(choice)
}

# WSLS with noise
WSLSAgentNoise_f <- function(prevChoice, feedback, noise) {
  choice <- WSLSAgent_f(prevChoice, feedback)

  if (rbinom(1, 1, noise) == 1) {
    choice <- rbinom(1, 1, 0.5)
  }

  return(choice)
}

# Recency-weighted memory choice rule
# The agent looks at its memory, turns that into a tendency toward option 1 using beta and bias, converts that tendency into a probability, and then makes a random binary choice based on that probability.
MemoryAgent_f <- function(memory, beta, bias = 0) {
  p_choice_1 <- plogis(bias + beta * (2 * (memory - 0.5))) # 
  choice <- rbinom(1, 1, p_choice_1)
  return(choice)
}

# Recency-weighted memory update
# The agent updates its memory by moving it a fraction alpha of the way toward the opponent's last choice. So if alpha is 0, the memory never changes, and if alpha is 1, the memory just becomes the opponent's last choice.
MemoryUpdate_f <- function(memory, otherChoice, alpha) {
  memory_new <- memory + alpha * (otherChoice - memory)
  return(memory_new)
}

# Block opponent
# This opponent plays 0 for 10 trials, then 1 for 10 trials, and so on. So it has a predictable pattern that changes every 10 trials.
BlockAgent_f <- function(t) {
  block <- floor((t - 1) / 10)
  choice <- block %% 2
  return(choice)
}

# You must be my block structure, because you change in steps but I still try to follow you.

# Simulation

trials <- 100
agents <- 100 # number of simulated agents per strategy
noise <- 0.10
alpha <- 0.20 # gradual updating, so the agent doesn't just copy the opponent's last choice but integrates over time
beta <- 4.00 # memory has a strong effect on choice, so the agent is quite sensitive to its memory of the opponent's behaviour
bias <- 0.00 # no stable side preference, so the agent doesn't have an inherent bias toward one option or the other

simulate_game_wsls <- function(trials, noise) {
  
  Self <- rep(NA, trials)
  Other <- rep(NA, trials)
  Feedback <- rep(NA, trials)

  Self[1] <- RandomAgent_f(0.5)
  Other[1] <- BlockAgent_f(1)
  Feedback[1] <- as.numeric(Self[1] == Other[1])

  for (t in 2:trials) {
    Self[t] <- WSLSAgentNoise_f(Self[t - 1], Feedback[t - 1], noise)
    Other[t] <- BlockAgent_f(t)
    Feedback[t] <- as.numeric(Self[t] == Other[t])
  }

  tibble(
    trial = 1:trials,
    Self = Self,
    Other = Other,
    Feedback = Feedback,
    strategy = "WSLS"
  )
}

simulate_game_memory <- function(trials, alpha, beta, bias = 0) {
  
  Self <- rep(NA, trials)
  Other <- rep(NA, trials)
  Feedback <- rep(NA, trials)
  Memory <- rep(NA, trials)

  memory_now <- 0.5
  Other[1] <- BlockAgent_f(1)
  Memory[1] <- memory_now
  Self[1] <- MemoryAgent_f(memory_now, beta = beta, bias = bias)
  Feedback[1] <- as.numeric(Self[1] == Other[1])
  memory_now <- MemoryUpdate_f(memory_now, Other[1], alpha)

  for (t in 2:trials) {
    Other[t] <- BlockAgent_f(t)
    Memory[t] <- memory_now
    Self[t] <- MemoryAgent_f(memory_now, beta = beta, bias = bias)
    Feedback[t] <- as.numeric(Self[t] == Other[t])
    memory_now <- MemoryUpdate_f(memory_now, Other[t], alpha)
  }

  tibble(
    trial = 1:trials,
    Self = Self,
    Other = Other,
    Feedback = Feedback,
    Memory = Memory,
    strategy = "Memory"
  )
}

# You must be my learning rate, because the more weight I give you, the faster everything else fades.

df_all <- NULL

for (agent in 1:agents) {
  df_wsls <- simulate_game_wsls(trials = trials, noise = noise)
  df_wsls$agent_id <- agent

  df_memory <- simulate_game_memory(trials = trials, alpha = alpha, beta = beta, bias = bias)
  df_memory$agent_id <- agent

  df_all <- bind_rows(df_all, df_wsls, df_memory)
}

# cumulative average per agent
# this shows how the average win rate develops over time

df_all <- df_all %>%
  group_by(strategy, agent_id) %>%
  mutate(cumulative = cumsum(Feedback) / trial) %>%
  ungroup()

summary_df <- df_all %>%
  group_by(strategy, trial) %>%
  summarise(
    mean_cumulative = mean(cumulative),
    .groups = "drop"
  )

final_df <- df_all %>%
  group_by(strategy, agent_id) %>%
  summarise(final_win_rate = mean(Feedback), .groups = "drop")

plot_1 <- ggplot(df_all, aes(trial, cumulative, group = interaction(strategy, agent_id), color = strategy)) +
  geom_line(alpha = 0.15) +
  geom_line(
    data = summary_df,
    aes(x = trial, y = mean_cumulative, color = strategy, group = strategy),
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Cumulative win rate against the block opponent",
    x = "Trial",
    y = "Cumulative average payoff"
  )

plot_2 <- ggplot(final_df, aes(strategy, final_win_rate, fill = strategy)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Final win rate across simulated agents",
    x = NULL,
    y = "Mean payoff"
  ) +
  guides(fill = "none")

save_plot(plot_1, file.path("assignment1_matching-pennies", "plots", "a1_cumulative.png"), width = 8, height = 5)
save_plot(plot_2, file.path("assignment1_matching-pennies", "plots", "a1_final_boxplot.png"), width = 6, height = 5)

write_csv(df_all, file.path("assignment1_matching-pennies", "report", "a1_simulated_data.csv"))

print(plot_1)
print(plot_2)

# extra
# calculate cumulative performance for each simulation run
df_scaled_vs_random <- df_all %>%
  filter(strategy == "Memory") %>%
  group_by(agent_id) %>%
  mutate(opponent_rate = mean(Other)) %>%
  ungroup()

df_scaled_vs_random <- df_scaled_vs_random %>%
  group_by(agent, opponent_rate) %>%
  # CHANGED 'Feedback' to 'win' here:
  mutate(Cumulative_Performance = cumsum(win) / row_number()) %>%
  ungroup()

# plot average performance over time, grouped by opponent bias rate
df_scaled_vs_random <- df_scaled_vs_random %>%
  group_by(trial, opponent_rate) %>%
  summarise(mean_cumulative_performance = mean(Cumulative_Performance), .groups = "drop")
plot_scaled_vs_random <- ggplot(df_scaled_vs_random, aes(x = trial, y = Cumulative_Performance, color = as.factor(opponent_rate))) +
  geom_line(stat = "summary", fun = "mean") +
  labs(
    title = "Cumulative performance of scaled memory agent vs random opponent",
    x = "Trial",
    y = "Cumulative average payoff",
    color = "Opponent bias rate"
  )

