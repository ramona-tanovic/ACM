// priors_wsls.stan
// -----------------------------------------------------------------------------
// Fixed-parameter Stan model used ONLY to sample heterogeneous WSLS agents.
//
// Why Stan here?
// - The course emphasises making your assumptions explicit.
// - Declaring priors in Stan gives a clean Bayesian *generative* story.
// - In fixed_param mode, Stan simply draws from these priors each iteration.
// -----------------------------------------------------------------------------

parameters {
  real<lower=0, upper=1> p_repeat_win;
  real<lower=0, upper=1> p_repeat_loss;
  real<lower=0, upper=1> lapse;
}

model {
  // Plausible cognitive priors (not "optimal"):
  // - after a win, repetition is likely
  // - after a loss, repetition is less likely
  // - lapse is usually small
  p_repeat_win  ~ beta(8, 2);
  p_repeat_loss ~ beta(2, 8);
  lapse         ~ beta(2, 10);
}

generated quantities {
  // Copy parameters into generated quantities to make extraction simple.
  real p_repeat_win_gq  = p_repeat_win;
  real p_repeat_loss_gq = p_repeat_loss;
  real lapse_gq         = lapse;
}
