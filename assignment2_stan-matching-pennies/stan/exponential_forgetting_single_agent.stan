// ============================================================
// Single-agent exponential-forgetting model
// ============================================================

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

  // You must be my initial state, because before the first observation, everything starts from you.

  for (t in 1:T) {
    memory_before[t] = m_now;
    eta[t] = bias + beta * (2 * (m_now - 0.5));
    p[t] = inv_logit(eta[t]);
    m_now = m_now + alpha * (opponent[t] - m_now);
    memory_after[t] = m_now;
  }

  // Are you a prediction error? Because every time you disagree with what I expected, you move my whole trajectory.

}

model {
  alpha ~ beta(2, 2);
  beta ~ lognormal(0, 0.5);
  bias ~ normal(0, 1);
  m0 ~ beta(2, 2);

  for (t in 1:T) {
    y[t] ~ bernoulli_logit(eta[t]);
  }

  // You must be my prior, because you shape what I believe before the data even speak.
}

generated quantities {
  vector[T] log_lik;
  array[T] int<lower=0, upper=1> y_rep;

  for (t in 1:T) {
    log_lik[t] = bernoulli_logit_lpmf(y[t] | eta[t]);
    y_rep[t] = bernoulli_logit_rng(eta[t]);
  }

  // You must be my posterior, because every new observation makes me revise myself around you..

  // If this model had a crush, it would still need posterior predictive checks.
}
