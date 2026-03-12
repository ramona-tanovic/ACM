# ============================================================
# Assignment 2 -- baseline fit script
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))

# Simulated data

sim_file <- file.path(sim_dir, "simulated_single_agent_data.csv")

df <- read_csv(sim_file, show_col_types = FALSE)

# stan likes it raw
stan_data <- list(
  T = nrow(df),
  y = as.integer(df$choice),
  opponent = as.integer(df$opponent)
)

# Prior predictive check
simulate_from_priors <- function(trials, n_sims = 300, p_opponent = 0.60) {
  out <- vector("list", length = n_sims)
  
  for (i in seq_len(n_sims)) {
    alpha <- rbeta(1, 2, 2) # memory learning rate between 0 and 1
    beta  <- rlnorm(1, 0, 0.5) # positive influence of memory on choice, with some variability
    bias  <- rnorm(1, 0, 1) # stable side preference, centered at 0 but with wide variability
    m0    <- rbeta(1, 2, 2) # initial memory between 0 and 1
    
    opponent <- rbinom(trials, 1, p_opponent)
    memory_now <- m0
    
    choice <- integer(trials)
    payoff <- integer(trials)
    
    for (t in seq_len(trials)) {
      eta_t <- bias + beta * (2 * (memory_now - 0.5))
      p_choice_t <- plogis(eta_t)
      choice[t] <- rbinom(1, 1, p_choice_t)
      payoff[t] <- as.integer(choice[t] == opponent[t])
      memory_now <- memory_now + alpha * (opponent[t] - memory_now)
    }
    
    out[[i]] <- tibble(
      sim = i,
      mean_choice = mean(choice),
      mean_payoff = mean(payoff)
    )
  }
  
  bind_rows(out)
}

set.seed(simulation_seed)

prior_pred_df <- simulate_from_priors(
  trials = nrow(df),
  n_sims = 300,
  p_opponent = 0.60
)

write_csv(
  prior_pred_df,
  file.path(derived_dir, "prior_predictive_summary.csv")
)

plot_prior_choice <- ggplot(prior_pred_df, aes(x = mean_choice)) +
  geom_histogram(bins = 30, fill = "grey80", color = "black") +
  geom_vline(xintercept = mean(df$choice), linewidth = 1) +
  labs(
    title = "Prior predictive check: mean choice",
    subtitle = "Vertical line = observed mean choice",
    x = "Mean simulated choice",
    y = "Count"
  )

plot_prior_payoff <- ggplot(prior_pred_df, aes(x = mean_payoff)) +
  geom_histogram(bins = 30, fill = "grey80", color = "black") +
  geom_vline(xintercept = mean(df$payoff), linewidth = 1) +
  labs(
    title = "Prior predictive check: mean payoff",
    subtitle = "Vertical line = observed mean payoff",
    x = "Mean simulated payoff",
    y = "Count"
  )

# fit the Stan model

# from "what the priors allow" to "what the data teach the model."
mod <- cmdstan_model(model_file)

fit <- mod$sample(
  data = stan_data,
  seed = sampling_seed,
  chains = chains_main,
  parallel_chains = chains_main,
  iter_warmup = iter_warmup_main,
  iter_sampling = iter_sampling_main,
  adapt_delta = adapt_delta_main,
  refresh = 200
)

fit$save_object(file.path(fits_dir, "fit_simulated.rds"))

# Parameter summaries and diagnostics
param_summary <- fit$summary(variables = c("alpha", "beta", "bias", "m0"))

write_csv(
  param_summary,
  file.path(derived_dir, "fit_summary_simulated.csv")
)

cat("\nParameter summary:\n")
print(param_summary)

# Helper function to count divergences.
count_divergences <- function(fit_object) {
  diag_array <- fit_object$sampler_diagnostics()
  var_names <- dimnames(diag_array)[[3]]
  
  if (!"divergent__" %in% var_names) {
    return(NA_integer_)
  }
  
  sum(diag_array[, , "divergent__"])
}

diagnostic_summary <- tibble(
  max_rhat = max(param_summary$rhat, na.rm = TRUE),
  min_ess_bulk = min(param_summary$ess_bulk, na.rm = TRUE),
  min_ess_tail = min(param_summary$ess_tail, na.rm = TRUE),
  divergences = count_divergences(fit)
)

write_csv(
  diagnostic_summary,
  file.path(derived_dir, "fit_diagnostics_simulated.csv")
)

cat("\nDiagnostic summary:\n")
print(diagnostic_summary)

# Diagnostics tell me whether estimation was technically trustworthy.
# They do NOT by themselves prove the model is psychologically good.
# R-hat near 1 is good; because it means the chains mixed well.
# higher ESS is better; because it means more effective samples.
# zero divergences is what I want; because divergences indicate potential issues with the geometry of the posterior.

# Prior-to-posterior update check
# "Did the data actually move the parameter beliefs?"
#
# If prior and posterior look very similar, then the data were weakly informative.
# If the posterior shifts or tightens clearly, then the data taught the model something.
posterior_draws_df <- as_draws_df(
  fit$draws(c("alpha", "beta", "bias", "m0"))
)

prior_compare_df <- bind_rows(
  tibble(parameter = "alpha", value = rbeta(2000, 2, 2), source = "Prior"),
  tibble(parameter = "beta",  value = rlnorm(2000, 0, 0.5), source = "Prior"),
  tibble(parameter = "bias",  value = rnorm(2000, 0, 1), source = "Prior"),
  tibble(parameter = "m0",    value = rbeta(2000, 2, 2), source = "Prior"),
  
  tibble(parameter = "alpha", value = posterior_draws_df$alpha, source = "Posterior"),
  tibble(parameter = "beta",  value = posterior_draws_df$beta,  source = "Posterior"),
  tibble(parameter = "bias",  value = posterior_draws_df$bias,  source = "Posterior"),
  tibble(parameter = "m0",    value = posterior_draws_df$m0,    source = "Posterior")
)

plot_update <- ggplot(prior_compare_df, aes(x = value, color = source, fill = source)) +
  geom_density(alpha = 0.20) +
  facet_wrap(~ parameter, scales = "free") +
  labs(
    title = "Prior to posterior update",
    x = NULL,
    y = "Density"
  )

# Basic posterior predictive check
# observed vs replicated choices.
yrep <- fit$draws("y_rep", format = "matrix")
n_ppc <- min(100, nrow(yrep))

plot_ppc_choice <- ppc_bars(
  y = df$choice,
  yrep = yrep[seq_len(n_ppc), ]
) +
  ggtitle("Posterior predictive check: observed vs replicated choices")

# Plots

save_plot(
  plot_prior_choice,
  file.path(plots_dir, "prior_predictive_mean_choice.png"),
  width = 7,
  height = 5
)

save_plot(
  plot_prior_payoff,
  file.path(plots_dir, "prior_predictive_mean_payoff.png"),
  width = 7,
  height = 5
)

save_plot(
  plot_update,
  file.path(plots_dir, "prior_posterior_update.png"),
  width = 9,
  height = 6
)

save_plot(
  plot_ppc_choice,
  file.path(plots_dir, "posterior_predictive_choices.png"),
  width = 7,
  height = 5
)

# Parameter summary
# alpha recovered well, beta was somewhat underestimated, bias was slightly above zero, and m0 was underestimated...

# Diagnostic summary
# R-hat was near 1, ESS was high, and there were no divergences, so the fit looked technically reliable. A model can sample beautifully and still be psychologically weak. Diagnostics only tell you that Stan did its job well, not that the model is necessarily the best model.

print(plot_prior_choice)
# the observed mean choice fell comfortably inside the prior predictive distribution
print(plot_prior_payoff)
# the observed mean payoff was somewhat low but still plausible under the priors
print(plot_update)
# the data clearly informed alpha, beta, and bias, while m0 showed weaker updating and remained the least identifiable parameter.
print(plot_ppc_choice)
# the fitted model reproduced the overall choice distribution reasonably well

# You must be my posterior, because after all that uncertainty, you are where my beliefs finally settle.
