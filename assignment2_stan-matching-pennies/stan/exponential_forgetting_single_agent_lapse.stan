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
  real<lower=0, upper=1> lapse;
}

transformed parameters {
  vector[T] memory_before;
  vector[T] memory_after;
  vector[T] eta;
  vector[T] p_model;
  vector[T] p;

  real m_now;
  m_now = m0;

  for (t in 1:T) {
    memory_before[t] = m_now;

    eta[t] = bias + beta * (2 * (m_now - 0.5));
    p_model[t] = inv_logit(eta[t]);

    // With probability lapse, the choice is random.
    p[t] = (1 - lapse) * p_model[t] + lapse * 0.5;

    m_now = m_now + alpha * (opponent[t] - m_now);
    memory_after[t] = m_now;
  }
}

model {
  alpha ~ beta(2, 2);
  beta  ~ lognormal(0, 0.5);
  bias  ~ normal(0, 1);
  m0    ~ beta(2, 2);

  // Small-lapse prior:
  // random responding is possible but expected to be uncommon.
  lapse ~ beta(1, 15);

  for (t in 1:T) {
    y[t] ~ bernoulli(p[t]);
  }

  // You must be my lapse parameter, because you admit that sometimes behaviour is just noise in a trench coat.
}

generated quantities {
  vector[T] log_lik;
  array[T] int y_rep;

  for (t in 1:T) {
    log_lik[t] = bernoulli_lpmf(y[t] | p[t]);
    y_rep[t] = bernoulli_rng(p[t]);
  }
}
