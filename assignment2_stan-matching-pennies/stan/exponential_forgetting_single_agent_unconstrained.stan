data {
  int<lower=1> T;
  array[T] int<lower=0, upper=1> y;
  array[T] int<lower=0, upper=1> opponent;
}

parameters {
  // This version leaves parameter support implicit.
  // It is useful only as a comparison against the constrained baseline.
  real alpha;
  real beta;
  real bias;
  real m0;
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
  alpha ~ beta(2, 2);
  beta  ~ lognormal(0, 0.5);
  bias  ~ normal(0, 1);
  m0    ~ beta(2, 2);

  for (t in 1:T) {
    y[t] ~ bernoulli_logit(eta[t]);
  }

  // You must be my unconstrained parameter, because you let me wander into places the theory never invited me to.
}

generated quantities {
  vector[T] log_lik;
  array[T] int y_rep;

  for (t in 1:T) {
    log_lik[t] = bernoulli_logit_lpmf(y[t] | eta[t]);
    y_rep[t] = bernoulli_logit_rng(eta[t]);
  }
}
