# 05_recovery_multilevel.R
# Optional extension: multilevel (hierarchical) WSLS model + recovery.
#
# Why: partial pooling stabilises individual estimates, especially with short sequences.
# How: simulate many subjects → fit hierarchical model → evaluate recovery for:
#  - population (mu) parameters
#  - subject-level parameters

prep_transitions <- function(df) {
  # df must contain columns: id, trial, a, win
  df <- df %>% arrange(id, trial)
  
  df %>%
    group_by(id) %>%
    mutate(
      a_prev   = dplyr::lag(a),
      win_prev = dplyr::lag(win),
      y_rep    = as.integer(a == a_prev)   # 1 if repeat previous action
    ) %>%
    filter(!is.na(a_prev), !is.na(win_prev)) %>%
    ungroup()
}

simulate_population <- function(S = 40, T = 60, seed = 1) {
  set.seed(seed)
  
  # Draw subject parameters from broad-ish priors (same as single-agent generative)
  pars <- tibble::tibble(
    id = 1:S,
    p_win  = rbeta(S, 8, 2),
    p_loss = rbeta(S, 2, 8),
    lapse  = rbeta(S, 2, 10)
  )
  
  sims <- vector("list", S)
  for (i in 1:S) {
    tru <- list(p_win = pars$p_win[i], p_loss = pars$p_loss[i], lapse = pars$lapse[i])
    sim <- simulate_wsls_game(T = T, true = tru, seed = seed + i)
    sims[[i]] <- tibble::tibble(
      id    = i,
      trial = 1:T,
      a     = sim$a,
      win   = sim$win
    )
  }
  
  list(
    df    = bind_rows(sims),
    truth = pars
  )
}

fit_wsls_hier <- function(mod, trans_df, prior_only = 0, seed = 1) {
  # Prepare arrays for Stan
  ids <- sort(unique(trans_df$id))
  trans_df <- trans_df %>% mutate(subj = match(id, ids))
  
  data_list <- list(
    N = nrow(trans_df),
    S = length(ids),
    y_rep    = as.integer(trans_df$y_rep),
    prev_win = as.integer(trans_df$win_prev),
    subj     = as.integer(trans_df$subj),
    prior_only = as.integer(prior_only),
    
    # hyper-priors (kept mild)
    mu_loc     = c(0, 0, -2),
    mu_scale   = c(1.5, 1.5, 1.5),
    sigma_scale = c(1, 1, 1)
  )
  
  mod$sample(
    data = data_list,
    chains = 2,
    parallel_chains = 2,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = seed,
    refresh = 0
  )
}

run_recovery_multilevel <- function(mod, S = 40, Ts = c(30, 60, 120), seed = 1) {
  dir.create("outputs/figs", showWarnings = FALSE, recursive = TRUE)
  dir.create("outputs/data", showWarnings = FALSE, recursive = TRUE)
  
  all_rec <- list()
  
  for (T in Ts) {
    message("Hier recovery: T = ", T)
    pop   <- simulate_population(S = S, T = T, seed = 5000 + T)
    trans <- prep_transitions(pop$df)
    
    fit <- fit_wsls_hier(mod, trans, prior_only = 0, seed = 6000 + T)
    
    # Extract subject-level parameters (already on probability scale in transformed parameters)
    draws <- posterior::as_draws_df(fit$draws(variables = c("p_win", "p_loss", "lapse")))
    
    # reshape: one row per draw per subject per parameter
    long <- draws %>%
      pivot_longer(everything(), names_to = "name", values_to = "value") %>%
      mutate(
        param = sub("\\[.*$", "", name),
        subj  = readr::parse_number(name),
        T = T
      )
    
    # Summarise posterior per subject/param
    summ <- long %>%
      group_by(T, param, subj) %>%
      summarise(
        mean = mean(value),
        q025 = quantile(value, 0.025),
        q975 = quantile(value, 0.975),
        .groups = "drop"
      ) %>%
      mutate(true = case_when(
        param == "p_win"  ~ pop$truth$p_win[subj],
        param == "p_loss" ~ pop$truth$p_loss[subj],
        param == "lapse"  ~ pop$truth$lapse[subj]
      ))
    
    all_rec[[as.character(T)]] <- summ
  }
  
  rec <- bind_rows(all_rec)
  write_csv(rec, "outputs/data/recovery_multilevel_long.csv")
  
  p <- ggplot(rec, aes(x = true, y = mean)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    geom_errorbar(aes(ymin = q025, ymax = q975), width = 0) +
    geom_point(alpha = 0.7) +
    facet_grid(param ~ T, scales = "free") +
    labs(
      title = "Multilevel recovery: subject parameters across T",
      x = "true value",
      y = "posterior mean ± 95% interval",
      subtitle = "Partial pooling should reduce noise in short sequences"
    )
  
  ggsave("outputs/figs/figF_recovery_multilevel.png", p, width = 11, height = 6, dpi = 200)
  
  cov <- rec %>%
    mutate(covered = (true >= q025 & true <= q975)) %>%
    group_by(T, param) %>%
    summarise(coverage = mean(covered), .groups = "drop")
  
  write_csv(cov, "outputs/data/recovery_multilevel_coverage.csv")
  
  p_cov <- ggplot(cov, aes(x = T, y = coverage)) +
    geom_line() + geom_point() +
    facet_wrap(~param) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Multilevel recovery quality (95% coverage) vs trial length",
      x = "T (trials)",
      y = "coverage"
    )
  
  ggsave("outputs/figs/figG_recovery_multilevel_coverage.png", p_cov, width = 9, height = 4.8, dpi = 200)
}
