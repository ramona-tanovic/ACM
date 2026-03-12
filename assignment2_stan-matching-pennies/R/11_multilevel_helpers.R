# ============================================================
# Assignment 2 -- helpers for the multilevel model
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "05_improvement_helpers.R"))

# choose data

find_students_data_path <- function() {
  candidates <- c(
    file.path(a2_dir, "data", "raw", "mp_students.csv"),
    file.path(a2_dir, "data", "mp_students.csv"),
    "/mnt/data/mp_students.csv"
  )
  
  hit <- candidates[file.exists(candidates)][1]
  
  if (is.na(hit)) {
    stop("Could not find mp_students.csv. Put it in assignment2_stan-matching-pennies/data/raw/ or check the path.")
  }
  
  hit
}

# prep data

# I keep the empirical example simple:
# - one dataset
# - one framing
# - one bot strategy
# - one short sequence per participant
#
# This is not the only possible choice.

prepare_empirical_students_subset <- function(
    data_path = NULL,
    framing = "Human",
    bot_strategy = "0-ToM",
    trials_keep = 20
) {
  if (is.null(data_path)) {
    data_path <- find_students_data_path()
  }
  
  df <- read_csv(data_path, show_col_types = FALSE)
  
  df_clean <- df %>%
    filter(
      Framing == framing,
      BotStrategy == bot_strategy
    ) %>%
    transmute(
      ID = as.character(ID),
      Trial = as.integer(Trial),
      Decision = as.integer(Decision),
      BotDecision = as.integer(BotDecision),
      Payoff = as.integer(Payoff)
    ) %>%
    drop_na() %>%
    arrange(ID, Trial) %>%
    group_by(ID) %>%
    filter(n() >= trials_keep) %>%
    slice_head(n = trials_keep) %>%
    ungroup()
  
  df_clean
}

# Stan data

prepare_multilevel_stan_data <- function(
    df,
    id_col = "ID",
    trial_col = "Trial",
    choice_col = "Decision",
    opponent_col = "BotDecision"
) {
  df_model <- df %>%
    arrange(.data[[id_col]], .data[[trial_col]]) %>%
    mutate(
      subject = dense_rank(.data[[id_col]])
    )
  
  subject_lookup <- df_model %>%
    distinct(subject, !!sym(id_col)) %>%
    rename(ID = !!sym(id_col))
  
  stan_data <- list(
    N = nrow(df_model),
    S = n_distinct(df_model$subject),
    subj = as.integer(df_model$subject),
    trial = as.integer(df_model[[trial_col]]),
    y = as.integer(df_model[[choice_col]]),
    opponent = as.integer(df_model[[opponent_col]])
  )
  
  list(
    df_model = df_model,
    subject_lookup = subject_lookup,
    stan_data = stan_data
  )
}

# simulate

simulate_multilevel_memory_dataset <- function(
    n_subjects = 40,
    trials_per_subject = 20,
    alpha_mean = 0.20,
    beta_mean = 2.50,
    bias_mean = 0.00,
    m0_mean = 0.50,
    alpha_sd = 0.25,      # latent logit scale
    beta_sd = 0.25,       # latent log scale
    bias_sd = 0.40,       # natural scale
    m0_sd = 0.25,         # latent logit scale
    p_opponent = 0.60
) {
  subject_ids <- paste0("S", seq_len(n_subjects))
  
  alpha_subject <- plogis(qlogis(alpha_mean) + alpha_sd * rnorm(n_subjects))
  beta_subject  <- exp(log(beta_mean) + beta_sd * rnorm(n_subjects))
  bias_subject  <- bias_mean + bias_sd * rnorm(n_subjects)
  m0_subject    <- plogis(qlogis(m0_mean) + m0_sd * rnorm(n_subjects))
  
  df_list <- vector("list", length = n_subjects)
  
  for (s in seq_len(n_subjects)) {
    df_s <- simulate_single_agent_dataset(
      trials = trials_per_subject,
      alpha = alpha_subject[s],
      beta = beta_subject[s],
      bias = bias_subject[s],
      m0 = m0_subject[s],
      p_opponent = p_opponent,
      lapse = 0
    ) %>%
      transmute(
        ID = subject_ids[s],
        Trial = trial,
        Decision = choice,
        BotDecision = opponent,
        Payoff = if_else(payoff == 1, 1L, -1L)
      )
    
    df_list[[s]] <- df_s
  }
  
  df_sim <- bind_rows(df_list)
  
  truth_population <- tibble(
    parameter = c(
      "alpha_mean_nat", "beta_mean_nat", "bias_mean_nat", "m0_mean_nat",
      "sigma_alpha", "sigma_log_beta", "sigma_bias", "sigma_m0"
    ),
    true = c(
      alpha_mean, beta_mean, bias_mean, m0_mean,
      alpha_sd, beta_sd, bias_sd, m0_sd
    )
  )
  
  truth_subject <- tibble(
    ID = subject_ids,
    alpha = alpha_subject,
    beta = beta_subject,
    bias = bias_subject,
    m0 = m0_subject
  )
  
  list(
    data = df_sim,
    truth_population = truth_population,
    truth_subject = truth_subject
  )
}

# fit

fit_multilevel_model <- function(
    stan_data,
    stan_filename = "exponential_forgetting_multilevel_singleblock.stan",
    seed = sampling_seed,
    quick = FALSE,
    save_path = NULL
) {
  mod <- compile_model(stan_filename)
  
  if (quick) {
    fit <- mod$sample(
      data = stan_data,
      seed = seed,
      chains = chains_quick,
      parallel_chains = chains_quick,
      iter_warmup = iter_warmup_quick,
      iter_sampling = iter_sampling_quick,
      adapt_delta = 0.97,
      refresh = 0
    )
  } else {
    fit <- mod$sample(
      data = stan_data,
      seed = seed,
      chains = chains_main,
      parallel_chains = chains_main,
      iter_warmup = iter_warmup_main,
      iter_sampling = iter_sampling_main,
      adapt_delta = 0.98,
      refresh = 200
    )
  }
  
  if (!is.null(save_path)) {
    fit$save_object(save_path)
  }
  
  fit
}

load_or_fit_multilevel_model <- function(
    stan_data,
    save_path,
    stan_filename = "exponential_forgetting_multilevel_singleblock.stan",
    seed = sampling_seed,
    quick = FALSE
) {
  if (file.exists(save_path)) {
    readRDS(save_path)
  } else {
    fit_multilevel_model(
      stan_data = stan_data,
      stan_filename = stan_filename,
      seed = seed,
      quick = quick,
      save_path = save_path
    )
  }
}

# summary

extract_multilevel_population_summary <- function(fit_object, model_name = NULL) {
  wanted <- c(
    "alpha_mean_nat", "beta_mean_nat", "bias_mean_nat", "m0_mean_nat",
    "sigma_alpha", "sigma_log_beta", "sigma_bias", "sigma_m0"
  )
  
  s <- fit_object$summary() %>%
    filter(variable %in% wanted) %>%
    select(variable, mean, median, sd, q5, q95, rhat, ess_bulk, ess_tail) %>%
    rename(parameter = variable)
  
  if (!is.null(model_name)) {
    s <- s %>% mutate(model = model_name, .before = 1)
  }
  
  s
}

extract_multilevel_subject_summary <- function(fit_object, subject_lookup) {
  s <- fit_object$summary()
  
  keep <- c("^alpha\\[", "^beta\\[", "^bias\\[", "^m0\\[")
  
  out <- bind_rows(lapply(keep, function(pattern_now) {
    s %>%
      filter(str_detect(variable, pattern_now)) %>%
      mutate(
        parameter = str_remove(variable, "\\[.*\\]"),
        subject = as.integer(str_extract(variable, "\\d+"))
      ) %>%
      select(subject, parameter, mean, median, sd, q5, q95)
  })) %>%
    left_join(subject_lookup, by = "subject") %>%
    select(ID, subject, parameter, mean, median, sd, q5, q95)
  
  out
}

# Trialwise mean-choice PPC across subjects

make_trialwise_mean_choice_ppc <- function(fit_object, df_model, n_draws = 200) {
  yrep <- fit_object$draws("y_rep", format = "matrix")
  n_use <- min(n_draws, nrow(yrep))
  
  observed_df <- df_model %>%
    group_by(Trial) %>%
    summarise(
      mean_choice = mean(Decision),
      .groups = "drop"
    ) %>%
    rename(trial = Trial)
  
  rep_df <- bind_rows(lapply(seq_len(n_use), function(i) {
    yrep_i <- as.numeric(yrep[i, ])
    
    tibble(
      Trial = df_model$Trial,
      choice = yrep_i
    ) %>%
      group_by(Trial) %>%
      summarise(
        mean_choice = mean(choice),
        .groups = "drop"
      ) %>%
      rename(trial = Trial) %>%
      mutate(draw = i)
  }))
  
  band_df <- rep_df %>%
    group_by(trial) %>%
    summarise(
      q10 = quantile(mean_choice, 0.10),
      q50 = quantile(mean_choice, 0.50),
      q90 = quantile(mean_choice, 0.90),
      .groups = "drop"
    )
  
  coverage_df <- observed_df %>%
    left_join(band_df, by = "trial") %>%
    mutate(inside_band = mean_choice >= q10 & mean_choice <= q90)
  
  list(
    observed_df = observed_df,
    rep_df = rep_df,
    band_df = band_df,
    coverage_df = coverage_df,
    coverage_rate = mean(coverage_df$inside_band)
  )
}

plot_trialwise_mean_choice_ppc <- function(ppc_object, title) {
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
      aes(x = trial, y = mean_choice),
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
      y = "Mean choice of 1 across subjects"
    )
}

# You must be my multilevel helper, because once there are many learners, somebody has to keep the indexing emotionally stable.
