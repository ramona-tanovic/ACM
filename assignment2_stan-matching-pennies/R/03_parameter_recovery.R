# ============================================================
# Assignment 2 -- baseline parameter recovery
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))

mod <- cmdstan_model(model_file)

# Trials
trial_grid <- c(50, 100, 200)
recovery_n <- 5

# Helpers

# priors-inspired
sample_true_parameters <- function() {
  tibble(
    alpha_true = rbeta(1, 2, 2),
    beta_true  = rlnorm(1, 0, 0.5),
    bias_true  = rnorm(1, 0, 1),
    m0_true    = rbeta(1, 2, 2)
  )
}

# Simulate one dataset from the single-agent memory model
simulate_single_recovery_dataset <- function(trials, alpha, beta, bias, m0, p_opponent = 0.60) {
  opponent <- rbinom(trials, 1, p_opponent)
  
  memory_now <- m0
  choice <- integer(trials)
  
  for (t in seq_len(trials)) {
    eta_t <- bias + beta * (2 * (memory_now - 0.5))
    p_choice_t <- plogis(eta_t)
    
    choice[t] <- rbinom(1, 1, p_choice_t)
    
    # Learning step:
    # the new memory moves part of the way toward the newest observation
    memory_now <- memory_now + alpha * (opponent[t] - memory_now)
  }
  
  tibble(
    trial = seq_len(trials),
    choice = choice,
    opponent = opponent
  )
}

# Fit the Stan model
fit_one_recovery_dataset <- function(df, seed) {
  stan_data <- list(
    T = nrow(df),
    y = as.integer(df$choice),
    opponent = as.integer(df$opponent)
  )
  
  fit <- mod$sample(
    data = stan_data,
    seed = seed,
    chains = chains_quick,
    parallel_chains = chains_quick,
    iter_warmup = iter_warmup_quick,
    iter_sampling = iter_sampling_quick,
    adapt_delta = adapt_delta_quick,
    refresh = 0
  )
  
  s <- fit$summary(variables = c("alpha", "beta", "bias", "m0"))
  
  tibble(
    alpha_est = s$mean[s$variable == "alpha"],
    beta_est  = s$mean[s$variable == "beta"],
    bias_est  = s$mean[s$variable == "bias"],
    m0_est    = s$mean[s$variable == "m0"]
  )
}

# Recovery

set.seed(simulation_seed)

recovery_rows <- list()
row_i <- 1

for (trials in trial_grid) {
  for (dataset_i in seq_len(recovery_n)) {
    
    # Step 1: sample true generating values
    true_pars <- sample_true_parameters()
    
    # Step 2: simulate one dataset from those truths
    df_sim <- simulate_single_recovery_dataset(
      trials = trials,
      alpha = true_pars$alpha_true,
      beta = true_pars$beta_true,
      bias = true_pars$bias_true,
      m0 = true_pars$m0_true,
      p_opponent = 0.60
    )
    
    # Step 3: fit the model back to the simulated dataset
    est_pars <- fit_one_recovery_dataset(
      df = df_sim,
      seed = sampling_seed + trials * 100 + dataset_i
    )
    
    # Step 4: store truth and estimate side by side
    recovery_rows[[row_i]] <- bind_cols(
      tibble(
        trials = trials,
        dataset = dataset_i
      ),
      true_pars,
      est_pars
    )
    
    cat("Finished recovery fit for trials =", trials,
        "dataset =", dataset_i, "\n")
    
    row_i <- row_i + 1
  }
}

recovery_results <- bind_rows(recovery_rows)

write_csv(
  recovery_results,
  file.path(derived_dir, "parameter_recovery_results.csv")
)

recovery_summary <- bind_rows(
  recovery_results %>%
    group_by(trials) %>%
    summarise(
      parameter = "alpha",
      mean_true = mean(alpha_true),
      mean_est = mean(alpha_est),
      .groups = "drop"
    ),
  recovery_results %>%
    group_by(trials) %>%
    summarise(
      parameter = "beta",
      mean_true = mean(beta_true),
      mean_est = mean(beta_est),
      .groups = "drop"
    ),
  recovery_results %>%
    group_by(trials) %>%
    summarise(
      parameter = "bias",
      mean_true = mean(bias_true),
      mean_est = mean(bias_est),
      .groups = "drop"
    ),
  recovery_results %>%
    group_by(trials) %>%
    summarise(
      parameter = "m0",
      mean_true = mean(m0_true),
      mean_est = mean(m0_est),
      .groups = "drop"
    )
)

write_csv(
  recovery_summary,
  file.path(derived_dir, "parameter_recovery_summary.csv")
)

cat("\nRecovery summary:\n")
print(recovery_summary)

# Plots

plot_alpha <- ggplot(recovery_results, aes(x = alpha_true, y = alpha_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~ trials, scales = "free") +
  labs(
    title = "Recovery of alpha",
    x = "True value",
    y = "Posterior mean estimate"
  )

plot_beta <- ggplot(recovery_results, aes(x = beta_true, y = beta_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~ trials, scales = "free") +
  labs(
    title = "Recovery of beta",
    x = "True value",
    y = "Posterior mean estimate"
  )

plot_bias <- ggplot(recovery_results, aes(x = bias_true, y = bias_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~ trials, scales = "free") +
  labs(
    title = "Recovery of bias",
    x = "True value",
    y = "Posterior mean estimate"
  )

plot_m0 <- ggplot(recovery_results, aes(x = m0_true, y = m0_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~ trials, scales = "free") +
  labs(
    title = "Recovery of m0",
    x = "True value",
    y = "Posterior mean estimate"
  )

save_plot(
  plot_alpha,
  file.path(plots_dir, "recovery_alpha.png"),
  width = 8,
  height = 5
)

save_plot(
  plot_beta,
  file.path(plots_dir, "recovery_beta.png"),
  width = 8,
  height = 5
)

save_plot(
  plot_bias,
  file.path(plots_dir, "recovery_bias.png"),
  width = 8,
  height = 5
)

save_plot(
  plot_m0,
  file.path(plots_dir, "recovery_m0.png"),
  width = 8,
  height = 5
)

# Recovery summary
# recovery is uneven across parameters, with m0 showing the weakest identification

print(plot_alpha)
# partial recovery; estimates generally move in the right direction but remain noisy
print(plot_beta)
# moderate to weak recovery; broad differences are captured, but estimates are imprecise
print(plot_bias)
# best recovered parameter; estimates track the true values relatively clearly, though with some shrinkage toward zero
print(plot_m0)
# weakest recovered parameter; estimates are compressed toward the middle and do not track true values closely

# Trial count
# 50 trials seems too short; 100 and 200 help somewhat, but recovery is still not strong for all parameters

# You must be my recovery plot, because I only trust the story when the estimates come back close to the truth.
