data {
  int<lower=1> T;
  array[T] int<lower=0, upper=1> y;
  array[T] int<lower=0, upper=1> opponent;
}

parameters {
  real<lower=0, upper=1> alpha;
  real<lower=0> beta;
  real bias;
  real<lower=0, upper=1> m0;
}

transformed parameters {
  vector[T] memory_before;
  vector[T] memory_after;
  vector[T] eta;
  vector[T] p;

  real m_now;
  m_now = m0;

  for (t in 1:T) {
    memory_before[t] = m_now;
    eta[t] = bias + beta * (2 * (m_now - 0.5));
    p[t] = inv_logit(eta[t]);
    m_now = m_now + alpha * (opponent[t] - m_now);
    memory_after[t] = m_now;
  }
}

model {
  // Tighter priors:
  alpha ~ beta(4, 4);
  beta  ~ lognormal(log(2), 0.25);
  bias  ~ normal(0, 0.5);
  m0    ~ beta(4, 4);

  for (t in 1:T) {
    y[t] ~ bernoulli_logit(eta[t]);
  }

  // You must be my regularizing prior, because when the data whisper, you stop me from hallucinating confidence.
}

generated quantities {
  vector[T] log_lik;
  array[T] int y_rep;

  for (t in 1:T) {
    log_lik[t] = bernoulli_logit_lpmf(y[t] | eta[t]);
    y_rep[t] = bernoulli_logit_rng(eta[t]);
  }
}
