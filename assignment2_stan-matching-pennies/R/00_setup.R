# ============================================================
# Assignment 2 setup
# ============================================================

source(file.path("R", "00_global_setup.R"))

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(patchwork)
})

raw_dir <- file.path("assignment2_stan-matching-pennies", "data", "raw")
sim_dir <- file.path("assignment2_stan-matching-pennies", "data", "simulated")
derived_dir <- file.path("assignment2_stan-matching-pennies", "data", "derived")
stan_dir <- file.path("assignment2_stan-matching-pennies", "stan")
fits_dir <- file.path("assignment2_stan-matching-pennies", "fits")
plots_dir <- file.path("assignment2_stan-matching-pennies", "plots")
model_file <- file.path(stan_dir, "exponential_forgetting_single_agent.stan")

dirs <- c(raw_dir, sim_dir, derived_dir, stan_dir, fits_dir, plots_dir)
for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

sim_trials <- 200
sim_alpha <- 0.20
sim_beta <- 3.00
sim_bias <- 0.00
sim_m0 <- 0.50

sampling_seed <- 1999
chains_main <- 4
iter_warmup_main <- 1000
iter_sampling_main <- 1000
adapt_delta_main <- 0.95

chains_quick <- 2
iter_warmup_quick <- 500
iter_sampling_quick <- 500
adapt_delta_quick <- 0.90

# You must be my posterior predictive check, because you tell me whether my story still makes sense after the model talks.
