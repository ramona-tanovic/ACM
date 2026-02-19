# R/tournament.R
# -----------------------------------------------------------------------------
# Tournament runner for two strategies (WSLS vs k-ToM-inspired belief learner).
#
# Why add TRIAL-LEVEL output?
# - End-of-match payoff is a single number; it hides *dynamics*.
# - The course plots often show learning curves over trials (line + ribbon),
#   because that is where cognitive constraints show up (adaptation, inertia).
# -----------------------------------------------------------------------------

transition_counts <- function(actions, wins, half) {
  # Counts for repeat-after-win/loss signatures (mechanistic summary of WSLS-like tendencies).
  cts <- list(
    n_win_first = 0L, rep_win_first = 0L,
    n_loss_first = 0L, rep_loss_first = 0L,
    n_win_second = 0L, rep_win_second = 0L,
    n_loss_second = 0L, rep_loss_second = 0L
  )
  
  for (t in 2:length(actions)) {
    prev <- t - 1L
    is_first <- prev <= half
    prev_win <- wins[prev]
    repeated <- actions[t] == actions[prev]
    
    if (is_first) {
      if (prev_win) {
        cts$n_win_first <- cts$n_win_first + 1L
        if (repeated) cts$rep_win_first <- cts$rep_win_first + 1L
      } else {
        cts$n_loss_first <- cts$n_loss_first + 1L
        if (repeated) cts$rep_loss_first <- cts$rep_loss_first + 1L
      }
    } else {
      if (prev_win) {
        cts$n_win_second <- cts$n_win_second + 1L
        if (repeated) cts$rep_win_second <- cts$rep_win_second + 1L
      } else {
        cts$n_loss_second <- cts$n_loss_second + 1L
        if (repeated) cts$rep_loss_second <- cts$rep_loss_second + 1L
      }
    }
  }
  
  unlist(cts)
}

run_tournament <- function(models, n_sims, T, seed = 123, return_trials = TRUE) {
  # models: list returned by compile_models()
  # return_trials: if TRUE, also return a big data.frame with trial-by-trial outcomes
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Need dplyr")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Need tidyr")
  
  pairings <- tidyr::expand_grid(A = STRATEGIES, B = STRATEGIES) |>
    dplyr::mutate(pairing = paste0(A, " vs ", B))
  
  half <- T / 2
  set.seed(seed)
  
  # Pre-sample heterogeneous agents from Stan priors ONCE (fast + principled).
  wsls_needed <- 0
  belief_needed <- 0
  for (p in seq_len(nrow(pairings))) {
    wsls_needed   <- wsls_needed   + (pairings$A[p] == "WSLS") * n_sims + (pairings$B[p] == "WSLS") * n_sims
    belief_needed <- belief_needed + (pairings$A[p] == "kToM")  * n_sims + (pairings$B[p] == "kToM")  * n_sims
  }
  
  wsls_draws <- sample_priors(models$priors_wsls, n_draws = wsls_needed, seed = seed)
  bel_draws  <- sample_priors(models$priors_belief, n_draws = belief_needed, seed = seed + 1)
  
  wsls_draws <- wsls_draws[, c("p_repeat_win_gq", "p_repeat_loss_gq", "lapse_gq")]
  names(wsls_draws) <- c("p_repeat_win", "p_repeat_loss", "lapse")
  
  bel_draws <- bel_draws[, c("alpha_gq", "beta_gq", "lapse_gq")]
  names(bel_draws) <- c("alpha", "beta", "lapse")
  
  i_wsls <- 1L
  i_bel  <- 1L
  
  match_rows  <- vector("list", nrow(pairings) * n_sims)
  player_rows <- vector("list", nrow(pairings) * n_sims * 2)
  
  # Trial-level rows: store one data.frame per sim (T rows each), then bind_rows at the end.
  trial_rows <- if (isTRUE(return_trials)) vector("list", nrow(pairings) * n_sims) else NULL
  
  mi <- 1L
  pi <- 1L
  ti <- 1L
  
  for (p in seq_len(nrow(pairings))) {
    A <- pairings$A[p]
    B <- pairings$B[p]
    pairing_label <- pairings$pairing[p]
    
    message("Simulating pairing: ", pairing_label)
    
    for (s in seq_len(n_sims)) {
      
      # Draw params for A
      paramsA <- if (A == "WSLS") wsls_draws[i_wsls, , drop = FALSE] else bel_draws[i_bel, , drop = FALSE]
      if (A == "WSLS") i_wsls <- i_wsls + 1L else i_bel <- i_bel + 1L
      
      # Draw params for B
      paramsB <- if (B == "WSLS") wsls_draws[i_wsls, , drop = FALSE] else bel_draws[i_bel, , drop = FALSE]
      if (B == "WSLS") i_wsls <- i_wsls + 1L else i_bel <- i_bel + 1L
      
      agentA <- make_agent(A, paramsA[1, ])
      agentB <- make_agent(B, paramsB[1, ])
      
      sim <- simulate_match(agentA, agentB, T = T, payoff_win = PAYOFF_WIN, payoff_loss = PAYOFF_LOSS)
      
      # Match-level summary (for “who beats whom?”)
      match_rows[[mi]] <- data.frame(
        pairing = pairing_label, A = A, B = B, sim_id = s,
        total_payoff_A  = sim$total_payoff,
        payoff_first_A  = sim$payoff_first,
        payoff_second_A = sim$payoff_second,
        swing_A         = sim$swing,
        winrate_A       = sim$winrate,
        stringsAsFactors = FALSE
      )
      mi <- mi + 1L
      
      # Trial-level output (for learning curves / role swap dynamics)
      if (isTRUE(return_trials)) {
        trial_rows[[ti]] <- data.frame(
          pairing = pairing_label, A = A, B = B, sim_id = s,
          trial = seq_len(T),
          roleA = sim$roleA,
          a = sim$a,
          b = sim$b,
          winA = as.integer(sim$winA),
          payoffA = sim$payoffA,
          stringsAsFactors = FALSE
        )
        ti <- ti + 1L
      }
      
      # Player-level outputs (role advantage + WSLS signatures)
      payoff_matcher_A    <- sim$payoff_first
      payoff_mismatcher_A <- sim$payoff_second
      role_adv_A          <- payoff_matcher_A - payoff_mismatcher_A
      
      cts_A <- transition_counts(sim$a, sim$winA, half)
      
      player_rows[[pi]] <- data.frame(
        pairing = pairing_label, opponent = B, strategy = A, position = "A", sim_id = s,
        payoff_total = sim$total_payoff,
        payoff_matcher = payoff_matcher_A,
        payoff_mismatcher = payoff_mismatcher_A,
        role_advantage = role_adv_A,
        winrate = sim$winrate,
        t(cts_A),
        stringsAsFactors = FALSE
      )
      pi <- pi + 1L
      
      # For B: zero-sum, so flip signs / roles
      payoff_total_B      <- -sim$total_payoff
      payoff_matcher_B    <- -sim$payoff_second
      payoff_mismatcher_B <- -sim$payoff_first
      role_adv_B          <- payoff_matcher_B - payoff_mismatcher_B
      
      win_B <- !sim$winA
      cts_B <- transition_counts(sim$b, win_B, half)
      
      player_rows[[pi]] <- data.frame(
        pairing = pairing_label, opponent = A, strategy = B, position = "B", sim_id = s,
        payoff_total = payoff_total_B,
        payoff_matcher = payoff_matcher_B,
        payoff_mismatcher = payoff_mismatcher_B,
        role_advantage = role_adv_B,
        winrate = 1 - sim$winrate,
        t(cts_B),
        stringsAsFactors = FALSE
      )
      pi <- pi + 1L
    }
  }
  
  out <- list(
    matches = dplyr::bind_rows(match_rows),
    players = dplyr::bind_rows(player_rows)
  )
  
  if (isTRUE(return_trials)) {
    out$trials <- dplyr::bind_rows(trial_rows)
  }
  
  out
}
