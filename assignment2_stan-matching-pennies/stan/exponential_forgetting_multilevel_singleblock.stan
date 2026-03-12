// ============================================================
// Assignment 2 -- multilevel exponential-forgetting model
// ============================================================

// This is the hierarchical version of the memory model.
// Main idea:
// - each participant has their own alpha, beta, bias, and m0
// - those individual parameters are partially pooled
//   around population-level means and SDs

// Why multilevel?
// Because participants are not identical, but they are also
// not fully unrelated. Partial pooling lets the model learn
// from both the individual and the group.

// In the single-agent model I estimated one learner.
// Here I estimate many learners at once, plus the population
// structure that links them together.

data {
  int<lower=1> N;                       // total number of observations
  int<lower=1> S;                       // number of subjects

  array[N] int<lower=1, upper=S> subj;  // subject id for each row
  array[N] int<lower=1> trial;          // trial number within subject
  array[N] int<lower=0, upper=1> y;     // observed participant choice
  array[N] int<lower=0, upper=1> opponent; // observed bot choice
}

parameters {
  // Population-level means
  real mu_alpha_logit;
  real mu_log_beta;
  real mu_bias;
  real mu_m0_logit;

  // Population-level SDs
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_log_beta;
  real<lower=0> sigma_bias;
  real<lower=0> sigma_m0;

  // Non-centered individual deviations
  vector[S] z_alpha;
  vector[S] z_log_beta;
  vector[S] z_bias;
  vector[S] z_m0;
}

transformed parameters {
  vector[S] alpha;
  vector[S] beta;
  vector[S] bias;
  vector[S] m0;

  vector[N] eta;
  vector[N] p;
  vector[N] memory_before;
  vector[N] memory_after;

  real m_now;

  // Subject-level parameters on their natural scales
  alpha = inv_logit(mu_alpha_logit + sigma_alpha * z_alpha);
  beta  = exp(mu_log_beta + sigma_log_beta * z_log_beta);
  bias  = mu_bias + sigma_bias * z_bias;
  m0    = inv_logit(mu_m0_logit + sigma_m0 * z_m0);

  for (n in 1:N) {
    // Reset memory at the start of each subject sequence
    if (n == 1 || subj[n] != subj[n - 1] || trial[n] == 1) {
      m_now = m0[subj[n]];
    }

    memory_before[n] = m_now;

    eta[n] = bias[subj[n]] + beta[subj[n]] * (2 * (m_now - 0.5));
    p[n] = inv_logit(eta[n]);

    m_now = m_now + alpha[subj[n]] * (opponent[n] - m_now);

    memory_after[n] = m_now;
  }
}

model {
  // Population-level priors
  mu_alpha_logit ~ normal(logit(0.2), 1); // maybe qlogis?
  mu_log_beta    ~ normal(log(2), 0.7);
  mu_bias        ~ normal(0, 1);
  mu_m0_logit    ~ normal(0, 1);

  sigma_alpha    ~ normal(0, 0.5);
  sigma_log_beta ~ normal(0, 0.4);
  sigma_bias     ~ normal(0, 0.5);
  sigma_m0       ~ normal(0, 0.5);

  // Non-centered deviations
  z_alpha    ~ std_normal();
  z_log_beta ~ std_normal();
  z_bias     ~ std_normal();
  z_m0       ~ std_normal();

  // Likelihood
  for (n in 1:N) {
    y[n] ~ bernoulli_logit(eta[n]);
  }

  // You must be my group-level prior, because you remind each participant that they are unique, but not from another universe.
}

generated quantities {
  array[N] int y_rep;
  vector[N] log_lik;

  // Population means on interpretable scales
  real<lower=0, upper=1> alpha_mean_nat;
  real<lower=0> beta_mean_nat;
  real bias_mean_nat;
  real<lower=0, upper=1> m0_mean_nat;

  alpha_mean_nat = inv_logit(mu_alpha_logit);
  beta_mean_nat  = exp(mu_log_beta);
  bias_mean_nat  = mu_bias;
  m0_mean_nat    = inv_logit(mu_m0_logit);

  for (n in 1:N) {
    y_rep[n] = bernoulli_logit_rng(eta[n]);
    log_lik[n] = bernoulli_logit_lpmf(y[n] | eta[n]);
  }
}
