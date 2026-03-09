# ACM

This repository contains my work for Advanced Cognitive Modelling.

## Structure

- `R/00_global_setup.R` sets the seed, loads the basic packages, and sets the plot theme.
- `assignment1_matching-pennies/R/a1_all_in_one.R` contains the full Assignment 1 workflow in one script.
- `assignment1_matching-pennies/report/A1_answer.md` is my short write-up for Assignment 1.
- `assignment2_stan-matching-pennies/R/00_setup.R` stores the simple paths and defaults for Assignment 2.
- `assignment2_stan-matching-pennies/R/01_simulate_data.R` simulates data from the recency-weighted memory model.
- `assignment2_stan-matching-pennies/R/02_fit_model.R` prepares data, fits the Stan model, and makes the main checks.
- `assignment2_stan-matching-pennies/R/03_parameter_recovery.R` runs a small parameter recovery analysis.
- `assignment2_stan-matching-pennies/R/04_empirical_example.R` runs one optional empirical example.
- `assignment2_stan-matching-pennies/R/run_assignment2_core.R` runs the main Assignment 2 workflow in order.
- `assignment2_stan-matching-pennies/stan/exponential_forgetting_single_agent.stan` is the Stan model.
- `assignment2_stan-matching-pennies/report/A2_answer.md` is my short write-up for Assignment 2.

## How I run the code

For Assignment 1 I run:

```r
source(file.path("assignment1_matching-pennies", "R", "a1_all_in_one.R"))
```

For the core Assignment 2 workflow I run:

```r
source(file.path("assignment2_stan-matching-pennies", "R", "run_assignment2_core.R"))
```

For the optional parts I run:

```r
source(file.path("assignment2_stan-matching-pennies", "R", "03_parameter_recovery.R"))
source(file.path("assignment2_stan-matching-pennies", "R", "04_empirical_example.R"))
```

## Assignment 1 approach

For Assignment 1 I keep things very simple. I compare:

1. a noisy WSLS agent
2. a recency-weighted memory agent

Both are tested against the same block-based opponent, because that makes the comparison easy to understand and easy to plot.

## Assignment 2 approach

For Assignment 2 I start with the minimum passing version first: one single-agent Stan model. I simulate data from that model, fit the same model back to the data, check priors and posterior predictive behaviour, and then do a small parameter recovery analysis. After that I also include one optional empirical example.

## Notes

The code comments are intentionally a bit heavier than usual because I am also using the repository as study notes.
