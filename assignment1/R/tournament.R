# R/tournament.R
# Runs many matches across strategy pairs and produces tidy outputs.

source(here::here("assignment1/R/utils.R"))
source(here::here("assignment1/R/game.R"))

run_tournament <- function(strategies, n_sims = 200, n_block = 30, seed = 1) {
  set.seed(seed)

  names_strats <- names(strategies)
  pairs <- expand_grid(A = names_strats, B = names_strats)

  all_trials <- vector("list", nrow(pairs))

  for (i in seq_len(nrow(pairs))) {
    A_nm <- pairs$A[i]; B_nm <- pairs$B[i]
    strat_A <- strategies[[A_nm]]
    strat_B <- strategies[[B_nm]]

    sims <- vector("list", n_sims)
    for (s in seq_len(n_sims)) {
      sims[[s]] <- simulate_match(strat_A, strat_B, n_block = n_block) %>%
        mutate(sim = s, strat_A = strat_A$name, strat_B = strat_B$name, stratA_key = A_nm, stratB_key = B_nm)
    }
    all_trials[[i]] <- bind_rows(sims)
  }

  trials <- bind_rows(all_trials)

  # Summaries for final payoff (total over 60 trials)
  summary_final <- trials %>%
    group_by(strat_A, strat_B, stratA_key, stratB_key, sim) %>%
    summarise(
      total_payoff_A = sum(payoff_A),
      total_payoff_B = sum(payoff_B),
      .groups = "drop"
    )

  list(trials = trials, summary_final = summary_final)
}
