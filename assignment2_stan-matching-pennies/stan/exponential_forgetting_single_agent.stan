data {
  // Number of trials
  int<lower=1> T;

  // Observed agent choices (0 or 1)
  array[T] int<lower=0, upper=1> y;

  // Observed opponent actions (0 or 1)
  array[T] int<lower=0, upper=1> opponent;
}

parameters {
  // alpha: learning rate / recency weight
  // Must be between 0 and 1 because it is a weighting parameter.
  real<lower=0, upper=1> alpha;

  // beta: how strongly memory affects choice
  // Must be positive because it is a sensitivity / slope parameter.
  real<lower=0> beta;

  // bias: stable side tendency
  // This can be positive or negative, so it stays unconstrained.
  real bias;

  // m0: initial memory state before any trial
  // Must lie between 0 and 1 because memory represents expected tendency.
  real<lower=0, upper=1> m0;
}

transformed parameters {
  // These vectors store the latent learning process across trials.
  vector[T] memory_before;
  vector[T] memory_after;
  vector[T] eta;
  vector[T] p;

  // Start memory at the initial state.
  real m_now;
  m_now = m0;

  for (t in 1:T) {
    // Save the memory state before the current update.
    memory_before[t] = m_now;
    eta[t] = bias + beta * (2 * (m_now - 0.5));

    // Convert the linear predictor into a choice probability.
    p[t] = inv_logit(eta[t]);

    // new_memory = old_memory + alpha * (observation - old_memory)
    m_now = m_now + alpha * (opponent[t] - m_now);

    // Save memory after the update.
    memory_after[t] = m_now;
  }

  // You must be my latent state, because even when I cannot observe you directly, you still explain everything I do.
}

model {
  // Priors

  // Mildly regularising baseline priors
  alpha ~ beta(2, 2);
  beta  ~ lognormal(0, 0.5);
  bias  ~ normal(0, 1);
  m0    ~ beta(2, 2);

  // Likelihood

  // Observed choices are generated from the current choice tendency.
  for (t in 1:T) {
    y[t] ~ bernoulli_logit(eta[t]);
  }

  // Are you my prior, because you shape what I expect before the data get their say.
}

generated quantities {
  // Pointwise log-likelihood for possible later model comparison
  vector[T] log_lik;

  // Posterior predictive replicated choices
  array[T] int y_rep;

  for (t in 1:T) {
    log_lik[t] = bernoulli_logit_lpmf(y[t] | eta[t]);
    y_rep[t] = bernoulli_logit_rng(eta[t]);
  }

  // You must be my posterior predictive check, because you tell me whether my theory can imitate reality.
}
