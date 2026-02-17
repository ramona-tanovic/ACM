// priors_belief.stan
// -----------------------------------------------------------------------------
// Fixed-parameter Stan model used ONLY to sample heterogeneous belief-learning
// ("k-ToM-inspired") agents.
//
// Parameters map to cognitive constraints:
// - alpha: recency weighting / memory limitation
// - beta : decision sharpness (inverse temperature)
// - lapse: random errors / distraction
// -----------------------------------------------------------------------------

parameters {
  real<lower=0, upper=1> alpha;
  real<lower=0> beta;
  real<lower=0, upper=1> lapse;
}

model {
  alpha ~ beta(2, 2);          // moderate learning rate
  beta  ~ lognormal(0, 0.5);   // positive; around exp(0)=1 with moderate spread
  lapse ~ beta(2, 10);         // usually small
}

generated quantities {
  real alpha_gq = alpha;
  real beta_gq  = beta;
  real lapse_gq = lapse;
}
