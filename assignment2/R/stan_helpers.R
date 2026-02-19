# stan_helpers.R
# Small helpers around cmdstanr compilation + fitting.

compile_stan <- function(stan_file) {
  # Compiling is expensive. cmdstanr caches compiled models by file path.
  # Keep it explicit so we see which model we are using.
  cmdstanr::cmdstan_model(stan_file)
}

default_priors_single <- function(strength = c("medium", "weak", "strong")) {
  # Priors are passed as data so we can do sensitivity checks without editing Stan.
  strength <- match.arg(strength)

  if (strength == "weak") {
    list(
      a_pwin = 2, b_pwin = 2,
      a_ploss = 2, b_ploss = 2,
      a_lapse = 2, b_lapse = 8
    )
  } else if (strength == "strong") {
    list(
      a_pwin = 10, b_pwin = 2,
      a_ploss = 2,  b_ploss = 10,
      a_lapse = 2, b_lapse = 15
    )
  } else {
    list(
      a_pwin = 8, b_pwin = 2,
      a_ploss = 2, b_ploss = 8,
      a_lapse = 2, b_lapse = 10
    )
  }
}
