# R/bootstrap.R
# -----------------------------------------------------------------------------
# Bootstrap uncertainty intervals.
#
# Note:
# The bootstrap is a simple way to quantify uncertainty when you have many independent simulation runs. You re-sample simulations with replacement and recompute the statistic (e.g., the mean). The spread of bootstrap replicates gives an interval.
# -----------------------------------------------------------------------------

# Bootstrap confidence interval for the mean.
bootstrap_mean_ci <- function(x, R = 500, level = 0.95, seed = 123) {
  set.seed(seed)

  x <- x[is.finite(x)] # Remove non-finite values (NA, NaN, Inf)
  n <- length(x) # Number of valid observations
  if (n < 2) stop("Need at least 2 values for bootstrap.")

  means <- numeric(R)
  for (r in seq_len(R)) { # for each bootstrap replicate
    idx <- sample.int(n, size = n, replace = TRUE) # re-sample with replacement
    means[r] <- mean(x[idx]) # compute the statistic (mean) for the bootstrap sample
  }

  alpha <- (1 - level) / 2 # Tail probability for two-sided interval
  ci <- stats::quantile(means, probs = c(alpha, 1 - alpha), names = FALSE) # Compute the confidence interval from the bootstrap replicates

  list(
    mean = mean(x), # Original mean
    lo = ci[1], # Lower bound of the confidence interval
    hi = ci[2] # Upper bound of the confidence interval
  )
}
