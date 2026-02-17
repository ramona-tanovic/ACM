// wsls_fit_hier.stan
// -----------------------------------------------------------------------------
// Hierarchical WSLS policy model.
//
// We fit subject-level parameters, with population-level pooling.
// This mirrors the "simulate -> fit -> interpret" workflow from the course.
//
// Data: multiple sequences (subjects) of repeated Matching Pennies.
// We condition on the opponent actions and on the role (Matcher/Mismatcher).
// -----------------------------------------------------------------------------

data {
  int<lower=1> S;                          // number of subjects
  int<lower=2> T;                          // trials per subject

  array[S, T] int<lower=0, upper=1> a;     // subject actions
  array[S, T] int<lower=0, upper=1> b;     // opponent actions
  array[S, T] int<lower=0, upper=1> roleA; // 1=Matcher, 0=Mismatcher
}

transformed data {
  // Pre-compute whether subject won each trial (deterministic given data)
  array[S, T] int<lower=0, upper=1> winA;
  for (s in 1:S) {
    for (t in 1:T) {
      int match = (a[s, t] == b[s, t]);
      winA[s, t] = (roleA[s, t] == 1) ? match : (1 - match);
    }
  }
}

parameters {
  // Population-level means (on unconstrained scales)
  real mu_win;
  real mu_loss;
  real mu_lapse;

  // Population-level SDs (half-normal via <lower=0>)
  real<lower=0> sigma_win;
  real<lower=0> sigma_loss;
  real<lower=0> sigma_lapse;

  // Subject-level deviations (non-centered parameterisation)
  vector[S] z_win;
  vector[S] z_loss;
  vector[S] z_lapse;
}

transformed parameters {
  // Subject-level parameters on interpretable scales
  vector[S] p_repeat_win;
  vector[S] p_repeat_loss;
  vector[S] lapse;

  for (s in 1:S) {
    p_repeat_win[s]  = inv_logit(mu_win  + sigma_win  * z_win[s]);
    p_repeat_loss[s] = inv_logit(mu_loss + sigma_loss * z_loss[s]);
    lapse[s]         = inv_logit(mu_lapse + sigma_lapse * z_lapse[s]);
  }
}

model {
  // Weakly-informative priors (the data are short sequences)
  mu_win   ~ normal(0, 1.5);
  mu_loss  ~ normal(0, 1.5);
  mu_lapse ~ normal(-2, 1.5);   // encourages small lapse but doesn't force it

  sigma_win   ~ normal(0, 1);
  sigma_loss  ~ normal(0, 1);
  sigma_lapse ~ normal(0, 1);

  z_win   ~ normal(0, 1);
  z_loss  ~ normal(0, 1);
  z_lapse ~ normal(0, 1);

  // Likelihood
  for (s in 1:S) {
    // Trial 1: no previous outcome, so we set an unbiased starting choice.
    a[s, 1] ~ bernoulli(0.5);

    for (t in 2:T) {
      int prev = t - 1;

      // WSLS: repetition probability depends on previous outcome
      real p_rep = (winA[s, prev] == 1) ? p_repeat_win[s] : p_repeat_loss[s];

      // Lapse mixes the policy toward random responding
      real p_same = (1 - lapse[s]) * p_rep + lapse[s] * 0.5;

      // Convert repetition probability into probability of action=1
      // (depends on what the previous action was)
      real p1 = (a[s, prev] == 1) ? p_same : (1 - p_same);

      a[s, t] ~ bernoulli(p1);
    }
  }
}

generated quantities {
  // Pointwise log-likelihood for LOO (one value per observation)
  array[S, T] real log_lik;

  for (s in 1:S) {
    log_lik[s, 1] = bernoulli_lpmf(a[s, 1] | 0.5);

    for (t in 2:T) {
      int prev = t - 1;
      real p_rep  = (winA[s, prev] == 1) ? p_repeat_win[s] : p_repeat_loss[s];
      real p_same = (1 - lapse[s]) * p_rep + lapse[s] * 0.5;
      real p1     = (a[s, prev] == 1) ? p_same : (1 - p_same);
      log_lik[s, t] = bernoulli_lpmf(a[s, t] | p1);
    }
  }
}
