# fit_single.R
# Fit the single-agent Stan model.

fit_wsls_single <- function(mod, a, win,
                            prior_only = 0,
                            priors = default_priors_single("medium"),
                            seed = 1) {

  stopifnot(length(a) == length(win))
  T <- length(a)

  data_list <- c(list(
    T = T,
    a = as.integer(a),
    win = as.integer(win),
    prior_only = as.integer(prior_only)
  ), priors)

  # Keep settings moderate: stable enough for class, not too slow.
  mod$sample(
    data = data_list,
    chains = 2,
    parallel_chains = 2,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = seed,
    refresh = 0
  )
}
