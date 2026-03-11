# ============================================================
# Assignment 2 -- shared helpers for improvement scripts
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))

# Data

read_simulated_data <- function() {
  sim_file <- file.path(sim_dir, "simulated_single_agent_data.csv")
  
  if (!file.exists(sim_file)) {
    source(file.path("assignment2_stan-matching-pennies", "R", "01_simulate_data.R"))
  }
  
  read_csv(sim_file, show_col_types = FALSE)
}

make_stan_data <- function(df) {
  list(
    T = nrow(df),
    y = as.integer(df$choice),
    opponent = as.integer(df$opponent)
  )
}

make_true_values_table <- function(include_lapse = FALSE, lapse_value = 0) {
  out <- tibble(
    parameter = c("alpha", "beta", "bias", "m0"),
    true = c(sim_alpha, sim_beta, sim_bias, sim_m0)
  )
  
  if (include_lapse) {
    out <- bind_rows(
      out,
      tibble(parameter = "lapse", true = lapse_value)
    )
  }
  
  out
}

# Model

compile_model <- function(stan_filename) {
  cmdstan_model(file.path(stan_dir, stan_filename))
}

fit_model_to_df <- function(
    df,
    stan_filename = basename(model_file),
    seed = sampling_seed,
    quick = FALSE,
    save_path = NULL
) {
  mod <- compile_model(stan_filename)
  stan_data <- make_stan_data(df)
  
  if (quick) {
    chains_use <- chains_quick
    iter_warmup_use <- iter_warmup_quick
    iter_sampling_use <- iter_sampling_quick
    adapt_delta_use <- adapt_delta_quick
    refresh_use <- 0
  } else {
    chains_use <- chains_main
    iter_warmup_use <- iter_warmup_main
    iter_sampling_use <- iter_sampling_main
    adapt_delta_use <- adapt_delta_main
    refresh_use <- 200
  }
  
  fit <- mod$sample(
    data = stan_data,
    seed = seed,
    chains = chains_use,
    parallel_chains = chains_use,
    iter_warmup = iter_warmup_use,
    iter_sampling = iter_sampling_use,
    adapt_delta = adapt_delta_use,
    refresh = refresh_use
  )
  
  if (!is.null(save_path)) {
    fit$save_object(save_path)
  }
  
  fit
}

load_or_fit_model <- function(
    df,
    stan_filename = basename(model_file),
    save_path,
    seed = sampling_seed,
    quick = FALSE
) {
  if (file.exists(save_path)) {
    readRDS(save_path)
  } else {
    fit_model_to_df(
      df = df,
      stan_filename = stan_filename,
      seed = seed,
      quick = quick,
      save_path = save_path
    )
  }
}

count_divergences <- function(fit_object) {
  diag_array <- fit_object$sampler_diagnostics()
  var_names <- dimnames(diag_array)[[3]]
  
  if (!"divergent__" %in% var_names) {
    return(NA_integer_)
  }
  
  sum(diag_array[, , "divergent__"])
}

extract_parameter_summary <- function(
    fit_object,
    variables = c("alpha", "beta", "bias", "m0", "lapse"),
    model_name = NULL
) {
  s <- fit_object$summary()
  vars_present <- intersect(variables, s$variable)
  
  out <- s %>%
    filter(variable %in% vars_present) %>%
    select(variable, mean, median, sd, q5, q95, rhat, ess_bulk, ess_tail) %>%
    rename(parameter = variable)
  
  if (!is.null(model_name)) {
    out <- out %>% mutate(model = model_name, .before = 1)
  }
  
  out
}

extract_diagnostic_summary <- function(param_summary, fit_object, model_name) {
  tibble(
    model = model_name,
    max_rhat = max(param_summary$rhat, na.rm = TRUE),
    min_ess_bulk = min(param_summary$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(param_summary$ess_tail, na.rm = TRUE),
    divergences = count_divergences(fit_object)
  )
}

# Simulation

sample_prior_parameters <- function(
    prior_type = c("baseline", "tighter"),
    include_lapse = FALSE
) {
  prior_type <- match.arg(prior_type)
  
  if (prior_type == "baseline") {
    out <- list(
      alpha = rbeta(1, 2, 2),
      beta  = rlnorm(1, 0, 0.5),
      bias  = rnorm(1, 0, 1),
      m0    = rbeta(1, 2, 2)
    )
  } else {
    out <- list(
      alpha = rbeta(1, 4, 4),
      beta  = rlnorm(1, log(2), 0.25),
      bias  = rnorm(1, 0, 0.5),
      m0    = rbeta(1, 4, 4)
    )
  }
  
  if (include_lapse) {
    out$lapse <- rbeta(1, 1, 15)
  }
  
  out
}

sample_true_parameters <- function(include_lapse = FALSE) {
  sample_prior_parameters(
    prior_type = "baseline",
    include_lapse = include_lapse
  )
}

simulate_single_agent_dataset <- function(
    trials,
    alpha,
    beta,
    bias,
    m0,
    p_opponent = 0.60,
    lapse = 0
) {
  opponent <- rbinom(trials, 1, p_opponent)
  
  memory_now <- m0
  memory_before <- numeric(trials)
  memory_after <- numeric(trials)
  p_choice <- numeric(trials)
  choice <- integer(trials)
  payoff <- integer(trials)
  
  for (t in seq_len(trials)) {
    memory_before[t] <- memory_now
    
    p_model <- plogis(bias + beta * (2 * (memory_now - 0.5)))
    
    # Optional lapse:
    # with probability = lapse, the choice is random (0.5 / 0.5)
    p_choice[t] <- (1 - lapse) * p_model + lapse * 0.5
    
    choice[t] <- rbinom(1, 1, p_choice[t])
    payoff[t] <- as.integer(choice[t] == opponent[t])
    
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

simulate_from_priors <- function(
    trials,
    n_sims = 300,
    p_opponent = 0.60,
    prior_type = c("baseline", "tighter"),
    include_lapse = FALSE
) {
  prior_type <- match.arg(prior_type)
  
  out <- vector("list", length = n_sims)
  
  for (i in seq_len(n_sims)) {
    pars <- sample_prior_parameters(
      prior_type = prior_type,
      include_lapse = include_lapse
    )
    
    df_sim <- simulate_single_agent_dataset(
      trials = trials,
      alpha = pars$alpha,
      beta = pars$beta,
      bias = pars$bias,
      m0 = pars$m0,
      p_opponent = p_opponent,
      lapse = if (include_lapse) pars$lapse else 0
    )
    
    out[[i]] <- tibble(
      sim = i,
      prior_type = prior_type,
      mean_choice = mean(df_sim$choice),
      mean_payoff = mean(df_sim$payoff)
    )
  }
  
  bind_rows(out)
}

# PPC

make_cumulative_payoff_ppc <- function(fit_object, df, n_draws = 200) {
  yrep <- fit_object$draws("y_rep", format = "matrix")
  
  if (is.null(dim(yrep))) {
    yrep <- matrix(yrep, nrow = 1)
  }
  
  n_use <- min(n_draws, nrow(yrep))
  
  rep_df <- bind_rows(lapply(seq_len(n_use), function(i) {
    yrep_i <- as.numeric(yrep[i, ])
    payoff_i <- as.integer(yrep_i == df$opponent)
    
    tibble(
      draw = i,
      trial = seq_len(nrow(df)),
      cum_payoff = cumsum(payoff_i) / seq_len(nrow(df))
    )
  }))
  
  band_df <- rep_df %>%
    group_by(trial) %>%
    summarise(
      q10 = quantile(cum_payoff, 0.10),
      q50 = quantile(cum_payoff, 0.50),
      q90 = quantile(cum_payoff, 0.90),
      .groups = "drop"
    )
  
  observed_df <- tibble(
    trial = seq_len(nrow(df)),
    cum_payoff = cumsum(df$payoff) / seq_len(nrow(df))
  )
  
  coverage_df <- observed_df %>%
    left_join(band_df, by = "trial") %>%
    mutate(inside_band = cum_payoff >= q10 & cum_payoff <= q90)
  
  list(
    rep_df = rep_df,
    band_df = band_df,
    observed_df = observed_df,
    coverage_df = coverage_df,
    coverage_rate = mean(coverage_df$inside_band)
  )
}

plot_cumulative_ppc_cloud <- function(
    ppc_object,
    title = "Posterior predictive check: cumulative mean payoff",
    subtitle = "Grey lines = replicated datasets, black line = observed data"
) {
  ggplot() +
    geom_line(
      data = ppc_object$rep_df,
      aes(x = trial, y = cum_payoff, group = draw),
      alpha = 0.08
    ) +
    geom_line(
      data = ppc_object$observed_df,
      aes(x = trial, y = cum_payoff),
      linewidth = 1
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Trial",
      y = "Cumulative mean payoff"
    )
}

plot_cumulative_ppc_band <- function(
    ppc_object,
    title = "Posterior predictive check: cumulative payoff envelope"
) {
  coverage_pct <- round(100 * ppc_object$coverage_rate, 1)
  
  ggplot() +
    geom_ribbon(
      data = ppc_object$band_df,
      aes(x = trial, ymin = q10, ymax = q90),
      alpha = 0.25
    ) +
    geom_line(
      data = ppc_object$band_df,
      aes(x = trial, y = q50),
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_line(
      data = ppc_object$observed_df,
      aes(x = trial, y = cum_payoff),
      linewidth = 1
    ) +
    labs(
      title = title,
      subtitle = paste0(
        "Band = 10th to 90th percentile; observed curve inside band on ",
        coverage_pct,
        "% of trials"
      ),
      x = "Trial",
      y = "Cumulative mean payoff"
    )
}

# Recovery

safe_cor_f <- function(x, y) {
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(NA_real_)
  }
  cor(x, y)
}

rmse_f <- function(x) {
  sqrt(mean(x^2))
}

plot_recovery_parameter <- function(data, parameter_name) {
  ggplot(
    data %>% filter(parameter == parameter_name),
    aes(x = true, y = est)
  ) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    facet_wrap(~ trials, scales = "free") +
    labs(
      title = paste("Recovery of", parameter_name),
      x = "True value",
      y = "Posterior mean estimate"
    )
}

# Comparison

plot_parameter_compare <- function(compare_summary, true_values, title) {
  ggplot(compare_summary, aes(x = model, y = mean)) +
    geom_hline(
      data = true_values,
      aes(yintercept = true),
      linetype = 2,
      inherit.aes = FALSE
    ) +
    geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.15) +
    geom_point(size = 2) +
    facet_wrap(~ parameter, scales = "free_y") +
    labs(
      title = title,
      subtitle = "Points = posterior means, bars = 90% intervals, dashed line = true value",
      x = NULL,
      y = "Posterior summary"
    )
}

combine_ppc_for_models <- function(ppc_list_named) {
  band_df <- bind_rows(lapply(names(ppc_list_named), function(model_name) {
    ppc_list_named[[model_name]]$band_df %>%
      mutate(model = model_name)
  }))
  
  observed_df <- bind_rows(lapply(names(ppc_list_named), function(model_name) {
    ppc_list_named[[model_name]]$observed_df %>%
      mutate(model = model_name)
  }))
  
  list(
    band_df = band_df,
    observed_df = observed_df
  )
}

plot_ppc_band_compare <- function(ppc_compare_df, title) {
  ggplot() +
    geom_ribbon(
      data = ppc_compare_df$band_df,
      aes(x = trial, ymin = q10, ymax = q90),
      alpha = 0.25
    ) +
    geom_line(
      data = ppc_compare_df$band_df,
      aes(x = trial, y = q50),
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_line(
      data = ppc_compare_df$observed_df,
      aes(x = trial, y = cum_payoff),
      linewidth = 1
    ) +
    facet_wrap(~ model) +
    labs(
      title = title,
      x = "Trial",
      y = "Cumulative mean payoff"
    )
}
