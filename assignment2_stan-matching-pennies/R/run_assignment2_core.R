# ============================================================
# Assignment 2 -- run the core workflow in order
# ============================================================

# 1. Simulate data from the cognitive model.
source(file.path("assignment2_stan-matching-pennies", "R", "01_simulate_data.R"))

# 2. Fit the Stan model and make the main checks.
source(file.path("assignment2_stan-matching-pennies", "R", "02_fit_model.R"))

# The optional scripts are not sourced here by default because they take longer.
source(file.path("assignment2_stan-matching-pennies", "R", "03_parameter_recovery.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "04_empirical_example.R"))
