# run_all.R
# End-to-end pipeline for Assignment 2.
#

# --- 0) Set working directory ----
if (interactive()) {
  # Works in RStudio when the file is open
  try({
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }, silent = TRUE)
}

# --- 1) renv (optional, but preferred) ----
if (requireNamespace("renv", quietly = TRUE)) {
  # If this is an renv project, restore ensures same package versions
  if (file.exists("renv.lock")) {
    message("renv.lock found → restoring packages with renv::restore()")
    renv::restore(prompt = FALSE)
  }
}

# --- 2) Load libraries ----
source("R/00_setup.R")

# --- 3) Compile Stan models ----
source("R/stan_helpers.R")
mod_single <- compile_stan("stan/wsls_single_agent.stan")
mod_hier   <- compile_stan("stan/wsls_multilevel.stan")

# --- 4) Simulated data checks (prior predictive, prior→posterior , PPC) ----
source("R/01_simulate.R")
source("R/02_fit_single.R")
source("R/03_checks.R")

# One simulated dataset for illustration in the document
sim <- simulate_wsls_game(
  T = 60,
  true = list(p_win = 0.85, p_loss = 0.15, lapse = 0.08),
  seed = 123
)

fit_single <- fit_wsls_single(
  mod = mod_single,
  a = sim$a,
  win = sim$win,
  prior_only = 0,
  seed = 123
)

make_single_agent_checks(
  mod = mod_single,
  fit = fit_single,
  sim = sim
)

# --- 5) Parameter recovery (single-agent) across trial lengths ----
source("R/04_recovery_single.R")
run_recovery_single(mod = mod_single)

# --- 6) Multilevel extension + recovery ----
source("R/05_recovery_multilevel.R")
run_recovery_multilevel(mod = mod_hier)

# --- 7) Empirical dataset fit (optional) ----
source("R/06_empirical_fit.R")
run_empirical_fit(mod = mod_hier)

message("Done. See outputs/figs and outputs/data.")

# quarto::quarto_render("assignment2.qmd") # render over here if u want the nice diagram; it doesn't work over the render button :P
