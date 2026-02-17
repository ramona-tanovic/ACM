# Assignment 1 — Matching Pennies (simple)

This repo contains a minimal, readable implementation of **Matching Pennies** under the course protocol:
30 trials, role swap, 30 trials (60 total). Payoffs are +1 for satisfying your role condition (Matcher: same action; Hider: different action), otherwise −1.

Implemented strategies (3):
1) **Random(0.5)**: chooses Left/Right with 50/50 probability each trial.
2) **WSLS** (Win–Stay Lose–Shift): repeats last action after a win, switches after a loss (with a small lapse rate).
3) **ForgetfulBias**: tracks the opponent’s bias toward action 1 using an exponentially weighted moving average, then best-responds by role (Matcher matches the likely action; Hider mismatches).

## Run everything

In RStudio:

```r
#install.packages("renv")
#renv::restore()

source("run_all.R")
```

This will:
- simulate a small tournament across strategy pairs
- write CSV outputs to `data/processed/`
- save figures to `figs/`
- render `assignment1.qmd` (HTML) if Quarto is installed

## Folder structure

- `R/strategies.R` — strategy objects (init/act/learn)
- `R/game.R` — the Matching Pennies game loop + role swap
- `R/tournament.R` — repeated simulations across all strategy pairs
- `R/plotting.R` — figures for the report
- `assignment1.qmd` — the written report
