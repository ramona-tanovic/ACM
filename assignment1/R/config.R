# R/config.R
# -----------------------------------------------------------------------------
# Assignment 1 configuration.
# Keeping all important numbers in one place so it's easy to tweak.
# -----------------------------------------------------------------------------

# --- Core simulation protocol ------------------------------------------------
T_TRIALS   <- 100
HALF_TRIAL <- T_TRIALS / 2
SEED       <- 123

# Matching Pennies pay-off (A perspective). Zero-sum.
PAYOFF_WIN  <-  1
PAYOFF_LOSS <- -1

# Strategies (only two, as required by the assignment)
STRATEGIES <- c("WSLS", "kToM")

# Tournament: how many matches per pairing?
N_SIMS <- 600

# Bootstrap uncertainty for tournament plots
BOOT_R   <- 400
CI_LEVEL <- 0.95

# --- Stan fitting settings ---------------------------------------------------
# These are deliberately modest, similar in spirit to course examples.
STAN_CHAINS        <- 2
STAN_WARMUP        <- 1000
STAN_SAMPLES       <- 1000

# Parameter recovery: number of "subjects" per dataset
N_SUBJECTS_RECOVERY <- 25

# --- Output paths ------------------------------------------------------------
OUT_DATA_DIR <- file.path("outputs", "data")
OUT_FIG_DIR  <- file.path("outputs", "figs")

dir.create(OUT_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG_DIR,  recursive = TRUE, showWarnings = FALSE)
