# R/game.R
# Implements the course protocol:
# - 30 trials, then swap roles (Matcher <-> Hider), then 30 more (60 total).
# - Payoffs: matcher gets +1 if actions match else -1; hider gets opposite.

source(here::here("assignment1/R/utils.R"))

compute_payoffs <- function(a_matcher, a_hider) {
  match <- (a_matcher == a_hider)
  payoff_matcher <- ifelse(match, 1, -1)
  payoff_hider   <- -payoff_matcher
  c(matcher = payoff_matcher, hider = payoff_hider)
}

simulate_match <- function(strat_A, strat_B, n_block = 30, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Initialize agent states
  sA <- strat_A$init()
  sB <- strat_B$init()

  n_total <- 2 * n_block
  out <- vector("list", n_total)

  for (t in seq_len(n_total)) {

    # Roles: A starts as Matcher, then swaps at t = n_block + 1
    if (t <= n_block) {
      role_A <- "Matcher"
      role_B <- "Hider"
    } else {
      role_A <- "Hider"
      role_B <- "Matcher"
    }

    aA <- strat_A$act(sA, role_A)
    aB <- strat_B$act(sB, role_B)

    # Compute payoffs based on roles
    if (role_A == "Matcher") {
      pay <- compute_payoffs(a_matcher = aA, a_hider = aB)
      rA <- pay["matcher"]; rB <- pay["hider"]
    } else {
      pay <- compute_payoffs(a_matcher = aB, a_hider = aA)
      rA <- pay["hider"]; rB <- pay["matcher"]
    }

    # Learning (both get to observe opponent action + own payoff)
    strat_A$learn(sA, self_action = aA, opp_action = aB, reward = rA, role = role_A)
    strat_B$learn(sB, self_action = aB, opp_action = aA, reward = rB, role = role_B)

    out[[t]] <- tibble(
      trial = t,
      block = ifelse(t <= n_block, 1L, 2L),
      role_A = role_A,
      role_B = role_B,
      action_A = aA,
      action_B = aB,
      payoff_A = as.numeric(rA),
      payoff_B = as.numeric(rB)
    )
  }

  bind_rows(out)
}
