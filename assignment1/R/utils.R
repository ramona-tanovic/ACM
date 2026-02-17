# R/utils.R
# Small utilities used across scripts.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# Lapse: with probability lapse, choose random action (0/1)
apply_lapse <- function(action, lapse = 0) {
  if (runif(1) < lapse) sample(c(0L, 1L), size = 1)
  else as.integer(action)
}
