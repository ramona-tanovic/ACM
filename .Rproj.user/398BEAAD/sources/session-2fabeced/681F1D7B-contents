# R/bootstrap.R
# -----------------------------------------------------------------------------
# Bootstrap uncertainty intervals.
#
# Note:
# The bootstrap is a simple way to quantify uncertainty when you have many
# independent simulation runs. You re-sample simulations with replacement and
# recompute the statistic (e.g., the mean). The spread of bootstrap replicates
# gives an interval.
# -----------------------------------------------------------------------------

bootstrap_mean_ci <- function(x, R = 500, level = 0.95, seed = 123) {
  set.seed(seed)

  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 2) stop("Need at least 2 values for bootstrap.")

  means <- numeric(R)
  for (r in seq_len(R)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    means[r] <- mean(x[idx])
  }

  alpha <- (1 - level) / 2
  ci <- stats::quantile(means, probs = c(alpha, 1 - alpha), names = FALSE)

  list(
    mean = mean(x),
    lo = ci[1],
    hi = ci[2]
  )
}
