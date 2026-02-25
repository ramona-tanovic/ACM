# simulate.R
# Simulation functions.
#
# Key idea: simulate a full Matching Pennies interaction (opponent random),
# then fit a WSLS policy model to the agent's actions conditional on wins/losses.

simulate_wsls_game <- function(T = 60,
                              true = list(p_win = 0.85, p_loss = 0.15, lapse = 0.08),
                              seed = 1) {
  set.seed(seed)

  p_win <- true$p_win
  p_loss <- true$p_loss
  lapse <- true$lapse

  # Role schedule: first half matcher, second half mismatcher (diagnostic manipulation)
  role <- c(rep(1L, floor(T/2)), rep(0L, T - floor(T/2)))  # 1=Matcher, 0=Mismatcher

  # Opponent actions: simplest baseline, random 0.5 each trial
  b <- rbinom(T, 1, 0.5)

  a <- integer(T)
  win <- integer(T)

  # First action: random
  a[1] <- rbinom(1, 1, 0.5)

  # Compute win at t=1 (depends on role and match/mismatch)
  win[1] <- if (role[1] == 1L) as.integer(a[1] == b[1]) else as.integer(a[1] != b[1])

  for (t in 2:T) {
    # Choose whether to repeat previous action based on previous outcome
    p_rep <- if (win[t-1] == 1L) p_win else p_loss
    p_repeat_obs <- (1 - lapse) * p_rep + lapse * 0.5

    repeat_t <- rbinom(1, 1, p_repeat_obs)
    if (repeat_t == 1L) {
      a[t] <- a[t-1]
    } else {
      a[t] <- 1L - a[t-1]
    }

    win[t] <- if (role[t] == 1L) as.integer(a[t] == b[t]) else as.integer(a[t] != b[t])
  }

  list(
    T = T,
    a = a,
    b = b,
    role = role,
    win = win,
    payoff = ifelse(win == 1L, 1L, -1L),
    true = true
  )
}

summarise_behavior <- function(a, win) {
  # Simple human-readable stats we can use in checks
  T <- length(a)
  rep <- a[2:T] == a[1:(T-1)]
  prev_win <- win[1:(T-1)] == 1

  tibble::tibble(
    T = T,
    repeat_rate = mean(rep),
    repeat_after_win = mean(rep[prev_win]),
    repeat_after_loss = mean(rep[!prev_win])
  )
}
