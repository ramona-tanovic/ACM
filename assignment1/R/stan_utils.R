# R/stan_utils.R
# -----------------------------------------------------------------------------
# CmdStanR helpers.
#
# Why isolate this in one file?
# - cmdstanr setup is the most "fragile" part (paths, compilation, etc.).
# - keeping it here makes the rest of the project feel like ordinary R code.
# -----------------------------------------------------------------------------

stan_paths <- function() {
  list(
    priors_wsls   = file.path("stan", "priors_wsls.stan"),
    priors_belief = file.path("stan", "priors_belief.stan"),
    fit_wsls      = file.path("stan", "wsls_fit_hier.stan"),
    fit_belief    = file.path("stan", "belief_fit_hier.stan")
  )
}

assert_cmdstan_ready <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Package 'cmdstanr' is required. With renv: renv::install('cmdstanr')")
  }

  # cmdstanr needs CmdStan (C++ toolchain + Stan).
  # We avoid auto-installing here to keep things explicit and predictable.
  ok <- TRUE
  tryCatch({
    cmdstanr::cmdstan_version()
  }, error = function(e) {
    ok <<- FALSE
  })

  if (!ok) {
    stop(
      "CmdStan is not installed or cmdstanr cannot find it.\n\n",
      "In RStudio, run:\n",
      "  cmdstanr::install_cmdstan()\n\n",
      "Then re-run this script.\n"
    )
  }

  invisible(TRUE)
}

compile_models <- function() {
  assert_cmdstan_ready()
  p <- stan_paths()

  list(
    priors_wsls   = cmdstanr::cmdstan_model(p$priors_wsls),
    priors_belief = cmdstanr::cmdstan_model(p$priors_belief),
    fit_wsls      = cmdstanr::cmdstan_model(p$fit_wsls),
    fit_belief    = cmdstanr::cmdstan_model(p$fit_belief)
  )
}

sample_priors <- function(mod, n_draws, seed = 123) {
  # Fixed-parameter sampling: each iteration is an IID draw from the priors.
  fit <- mod$sample(
    data = list(),
    chains = 1,
    parallel_chains = 1,
    iter_warmup = 0,
    iter_sampling = n_draws,
    fixed_param = TRUE,
    refresh = 0,
    seed = seed
  )

  # Convert to a plain data.frame (beginner-friendly)
  draws <- posterior::as_draws_df(fit$draws())
  as.data.frame(draws)
}

fit_model <- function(mod, data_list, seed = 123, chains = 2, iter_warmup = 1000, iter_sampling = 1000) {
  # We keep sampling settings modest. For this assignment, interpretability
  # matters more than squeezing out the last decimal.
  mod$sample(
    data = data_list,
    chains = chains,
    parallel_chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    refresh = 200,
    seed = seed
  )
}

extract_log_lik_matrix <- function(fit) {
  # Returns a matrix [draws x observations] suitable for loo::loo().
  draws_df <- posterior::as_draws_df(fit$draws("log_lik"))
  m <- as.matrix(draws_df)
  # drop metadata columns if present
  m <- m[, !colnames(m) %in% c(".chain", ".iteration", ".draw"), drop = FALSE]
  m
}
