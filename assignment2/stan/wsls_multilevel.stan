// wsls_multilevel.stan
//
// Hierarchical (multilevel) WSLS + lapse model.
//
// We model transitions (t>=2 within each subject/block):
// repeat[n] ~ Bernoulli( (1-lapse_i)*p_{win/loss,i} + lapse_i*0.5 )
//
// Each subject i has 3 latent parameters:
//   p_win[i], p_loss[i], lapse[i]
// Parameterisation:
//   theta_{k,i} = mu_k + sigma_k * z_{k,i}, with z ~ Normal(0,1)
//   p = inv_logit(theta) to map real line -> (0,1)
//
// This is the “partial pooling” story: subject parameters are shrunk toward
// the population mean depending on sigma and available data.

data {
  int<lower=1> N;                 // number of transitions (rows)
  int<lower=1> S;                 // number of subjects
  array[N] int<lower=0, upper=1> y_rep;  // did the subject repeat previous action?
  array[N] int<lower=0, upper=1> prev_win; // was previous trial a win?
  array[N] int<lower=1, upper=S> subj;     // subject index per row

  int<lower=0, upper=1> prior_only;

  // Hyperpriors (passed as data to keep the model easy to tweak)
  vector[3] mu_loc;     // location of mu prior (on logit scale)
  vector[3] mu_scale;   // scale of mu prior
  vector<lower=0>[3] sigma_scale; // scale for sigma prior
}

parameters {
  vector[3] mu;                 // population mean (logit scale): [p_win, p_loss, lapse]
  vector<lower=0>[3] sigma;     // population SD (logit scale)
  matrix[3, S] z;               // standardised subject effects
}

transformed parameters {
  matrix[3, S] theta;
  array[S] real<lower=0, upper=1> p_win;
  array[S] real<lower=0, upper=1> p_loss;
  array[S] real<lower=0, upper=1> lapse;

  for (i in 1:S) {
  theta[, i] = mu + sigma .* z[, i];
  };

  for (i in 1:S) {
    p_win[i]  = inv_logit(theta[1, i]);
    p_loss[i] = inv_logit(theta[2, i]);
    lapse[i]  = inv_logit(theta[3, i]);
  }
}

model {
  // Priors on population parameters
  // "Are you a hierarchical model? Because you operate on every level of my thoughts."
  mu ~ normal(mu_loc, mu_scale);

  // Mildly regularising prior on sigma (exponential is common; scale passed via data)
  sigma ~ exponential(sigma_scale);

  // Standard normal for subject deviations
  to_vector(z) ~ normal(0, 1);

  // Likelihood (skip for prior predictive)
  if (prior_only == 0) {
    for (n in 1:N) {
      int i = subj[n];
      real p_rep = (prev_win[n] == 1) ? p_win[i] : p_loss[i];
      real p_repeat = (1 - lapse[i]) * p_rep + lapse[i] * 0.5;
      y_rep[n] ~ bernoulli(p_repeat);
    }
  }
}

generated quantities {
  vector[N] log_lik;
  array[N] int<lower=0, upper=1> repeat_rep;

  // Simple global PPC summaries (for the whole dataset)
  real rep_rate;
  real rep_win_rate;
  real rep_loss_rate;

  {
    int n_all = 0;
    int n_win = 0;
    int n_loss = 0;
    int s_all = 0;
    int s_win = 0;
    int s_loss = 0;

    for (n in 1:N) {
      int i = subj[n];
      real p_rep = (prev_win[n] == 1) ? p_win[i] : p_loss[i];
      real p_repeat = (1 - lapse[i]) * p_rep + lapse[i] * 0.5;

      if (prior_only == 0) {
        log_lik[n] = bernoulli_lpmf(y_rep[n] | p_repeat);
      } else {
        log_lik[n] = 0;
      }

      repeat_rep[n] = bernoulli_rng(p_repeat);

      n_all += 1; s_all += repeat_rep[n];
      if (prev_win[n] == 1) { n_win += 1; s_win += repeat_rep[n]; }
      else { n_loss += 1; s_loss += repeat_rep[n]; }
    }

    rep_rate = s_all * 1.0 / n_all;
    rep_win_rate = (n_win > 0) ? (s_win * 1.0 / n_win) : negative_infinity();
    rep_loss_rate = (n_loss > 0) ? (s_loss * 1.0 / n_loss) : negative_infinity();
  }
}
