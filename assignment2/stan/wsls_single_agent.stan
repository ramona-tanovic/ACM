// wsls_single_agent.stan
//
// Single-agent WSLS + lapse policy model for Matching Pennies behaviour.
//
// The key modelling decision (for interpretability):
// We model *repeating the previous action* as a function of whether the agent
// won the previous trial. This is the “Win–Stay / Lose–Shift” signature.
//
// We also include a lapse parameter: with probability lapse the agent behaves
// randomly (repeat probability becomes 0.5). This captures errors/distraction.
//
// IMPORTANT: We pass Beta prior hyperparameters in data so we can do
// prior-sensitivity analyses without editing the Stan file.

data {
  int<lower=2> T;  // number of trials (must be >= 2 because we use repeats)

  // Observed actions (0/1). We do not model a[1] directly; it seeds the repeat process.
  array[T] int<lower=0, upper=1> a;

  // Observed win outcome (0/1) for each trial.
  // win[t] = 1 means payoff was +1 at trial t.
  array[T] int<lower=0, upper=1> win;

  // prior_only = 1: ignore likelihood (prior predictive simulation)
  // prior_only = 0: fit to data (posterior)
  int<lower=0, upper=1> prior_only;

  // Prior hyperparameters
  real<lower=0> a_pwin;
  real<lower=0> b_pwin;
  real<lower=0> a_ploss;
  real<lower=0> b_ploss;
  real<lower=0> a_lapse;
  real<lower=0> b_lapse;
}

transformed data {
  // Derived observation: did the agent repeat its previous action?
  // repeat[t] is only meaningful for t>=2; we set repeat[1]=0 and ignore it.
  array[T] int<lower=0, upper=1> repeat_obs;
  repeat_obs[1] = 0;
  for (t in 2:T) {
    repeat_obs[t] = (a[t] == a[t-1]);
  }
}

parameters {
  // WSLS parameters on probability scale (simple and interpretable)
  real<lower=0, upper=1> p_win;   // P(repeat | previous trial was a win)
  real<lower=0, upper=1> p_loss;  // P(repeat | previous trial was a loss)
  real<lower=0, upper=1> lapse;   // probability of random responding (repeat=0.5)
}

model {
  // --- Priors ---
  // "My priors were broad until I observed you. Now my posterior is sharply peaked."
  // By passing hyperparameters through data, we can test weaker/stronger priors.
  p_win  ~ beta(a_pwin,  b_pwin);
  p_loss ~ beta(a_ploss, b_ploss);
  lapse  ~ beta(a_lapse, b_lapse);

  // --- Likelihood ---
  // We model repeats at t>=2 depending on win[t-1].
  // Under lapse, repeat probability shrinks toward 0.5.
  if (prior_only == 0) {
    for (t in 2:T) {
      real p_rep = (win[t-1] == 1) ? p_win : p_loss;
      real p_repeat = (1 - lapse) * p_rep + lapse * 0.5;
      repeat_obs[t] ~ bernoulli(p_repeat);
    }
  }
}

generated quantities {
  // log_lik for LOO (one value per transition t>=2; set t=1 to 0)
  vector[T] log_lik;

  // Posterior predictive replicate (repeat + action sequence)
  array[T] int<lower=0, upper=1> repeat_rep;
  array[T] int<lower=0, upper=1> a_rep;

  // Summary statistics for posterior predictive checks
  real rep_rate;
  real rep_win_rate;
  real rep_loss_rate;

  // Seed with observed first action.
  a_rep[1] = a[1];
  repeat_rep[1] = 0;

  {
    int n_all = 0;
    int n_win = 0;
    int n_loss = 0;
    int s_all = 0;
    int s_win = 0;
    int s_loss = 0;

    // log_lik[1] is unused; keep it at 0.
    log_lik[1] = 0;

    for (t in 2:T) {
      real p_rep = (win[t-1] == 1) ? p_win : p_loss;
      real p_repeat = (1 - lapse) * p_rep + lapse * 0.5;

      // log likelihood contribution (if we are fitting)
      if (prior_only == 0) {
        log_lik[t] = bernoulli_lpmf(repeat_obs[t] | p_repeat);
      } else {
        log_lik[t] = 0;
      }

      // simulate replicate repeat decision
      repeat_rep[t] = bernoulli_rng(p_repeat);

      // turn repeat decision into a_rep sequence (flip if not repeating)
      if (repeat_rep[t] == 1) {
        a_rep[t] = a_rep[t-1];
      } else {
        a_rep[t] = 1 - a_rep[t-1];
      }

      // accumulate predictive summary stats
      n_all += 1;
      s_all += repeat_rep[t];

      if (win[t-1] == 1) {
        n_win += 1;
        s_win += repeat_rep[t];
      } else {
        n_loss += 1;
        s_loss += repeat_rep[t];
      }
    }

    rep_rate = s_all * 1.0 / n_all;
    rep_win_rate = (n_win > 0) ? (s_win * 1.0 / n_win) : negative_infinity();
    rep_loss_rate = (n_loss > 0) ? (s_loss * 1.0 / n_loss) : negative_infinity();
  }
}
