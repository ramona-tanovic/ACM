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

a2_dir <- file.path("assignment2_stan-matching-pennies")

r_dir <- file.path(a2_dir, "R")
stan_dir <- file.path(a2_dir, "stan")
data_dir <- file.path(a2_dir, "data")
sim_dir <- file.path(data_dir, "simulated")
derived_dir <- file.path(data_dir, "derived")
plots_dir <- file.path(a2_dir, "plots")
fits_dir <- file.path(a2_dir, "fits")

model_file <- file.path(stan_dir, "exponential_forgetting_single_agent.stan")

simulation_seed <- 21 
sampling_seed <- 21

# Simulation parameters
sim_trials <- 200
sim_alpha <- 0.20 # gradual updating
sim_beta <- 3.00 # memory has a fairly strong effect on choice
sim_bias <- 0.00 # no stable side preference
sim_m0 <- 0.50 # neutral starting memory

# Sampling parameters
chains_main <- 4
iter_warmup_main <- 1000
iter_sampling_main <- 1000
adapt_delta_main <- 0.95

# For quick testing
chains_quick <- 2
iter_warmup_quick <- 500
iter_sampling_quick <- 500
adapt_delta_quick <- 0.90

# You must be my posterior predictive check, because you tell me whether my story still makes sense after the model talks.
