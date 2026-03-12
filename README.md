# ACM

This repository contains my work for Advanced Cognitive Modelling.

## Structure

- `R/00_global_setup.R` sets the seed, loads the basic packages, and sets the plot theme.
- `assignment1_matching-pennies/R/a1_all_in_one.R` contains the full Assignment 1 workflow in one script.
- `assignment1_matching-pennies/report/A1_answer.md` is my short write-up for Assignment 1.

## Assignment 1 approach

For Assignment 1 I keep things very simple. I compare:

1. a noisy WSLS agent
2. a recency-weighted memory agent

Both are tested against the same block-based opponent, because that makes the comparison easy to understand and easy to plot.

## Assignment 2 approach

For Assignment 2 I start with the minimum passing version first: one single-agent Stan model. I simulate data from that model, fit the same model back to the data, check priors and posterior predictive behaviour, and then do a small parameter recovery analysis. After that I also include one optional empirical example.

## Notes

The code comments are intentionally a bit heavier than usual because I am also using the repository as study notes.
