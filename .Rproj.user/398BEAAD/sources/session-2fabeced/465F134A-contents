// belief_fit_hier.stan
// -----------------------------------------------------------------------------
// Hierarchical belief-learning ("k-ToM-inspired") model.
//
// The agent maintains a belief about the opponent's probability of action=1.
// It updates this belief with a delta rule (alpha), and chooses a role-
// dependent best response, softened by beta and lapse.
// -----------------------------------------------------------------------------

data {
  int<lower=1> S;
  int<lower=2> T;

  array[S, T] int<lower=0, upper=1> a;
  array[S, T] int<lower=0, upper=1> b;
  array[S, T] int<lower=0, upper=1> roleA;  // 1=Matcher, 0=Mismatcher
}

parameters {
  // Population-level means (unconstrained)
  real mu_alpha;
  real mu_log_beta;
  real mu_lapse;

  // Population-level SDs
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_log_beta;
  real<lower=0> sigma_lapse;

  // Subject-level deviations
  vector[S] z_alpha;
  vector[S] z_log_beta;
  vector[S] z_lapse;
}

transformed parameters {
  vector[S] alpha;
  vector[S] beta;
  vector[S] lapse;

  for (s in 1:S) {
    alpha[s] = inv_logit(mu_alpha + sigma_alpha * z_alpha[s]);
    beta[s]  = exp(mu_log_beta + sigma_log_beta * z_log_beta[s]);
    lapse[s] = inv_logit(mu_lapse + sigma_lapse * z_lapse[s]);
  }
}

model {
  // Weakly-informative priors
  mu_alpha     ~ normal(0, 1.5);
  mu_log_beta  ~ normal(0, 1.5);
  mu_lapse     ~ normal(-2, 1.5);

  sigma_alpha    ~ normal(0, 1);
  sigma_log_beta ~ normal(0, 1);
  sigma_lapse    ~ normal(0, 1);

  z_alpha     ~ normal(0, 1);
  z_log_beta  ~ normal(0, 1);
  z_lapse     ~ normal(0, 1);

  // Likelihood
  for (s in 1:S) {
    real belief = 0.5; // prior belief before seeing any opponent actions

    for (t in 1:T) {
      // belief -> value difference between choosing action 1 vs 0
      // If Matcher: prefer matching the more likely opponent action.
      // If Mismatcher: prefer mismatching.
      real v = 2 * belief - 1;               // opponent bias toward 1
      if (roleA[s, t] == 0) v = -v;          // flip if Mismatcher

      // Soft best response: logistic; then add lapse toward 0.5
      real p1 = (1 - lapse[s]) * inv_logit(beta[s] * v) + lapse[s] * 0.5;

      a[s, t] ~ bernoulli(p1);

      // Update belief *after* observing opponent action at t
      belief = belief + alpha[s] * (b[s, t] - belief);
    }
  }
}

generated quantities {
  array[S, T] real log_lik;

  for (s in 1:S) {
    real belief = 0.5;

    for (t in 1:T) {
      real v = 2 * belief - 1;
      if (roleA[s, t] == 0) v = -v;
      real p1 = (1 - lapse[s]) * inv_logit(beta[s] * v) + lapse[s] * 0.5;

      log_lik[s, t] = bernoulli_lpmf(a[s, t] | p1);

      belief = belief + alpha[s] * (b[s, t] - belief);
    }
  }
}
