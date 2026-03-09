# Assignment 2

Repository link:

## Model

In this assignment I built and validated a single-agent cognitive model of Matching Pennies behaviour in Stan.

The model is a **recency-weighted memory model**. The idea is that the player does not store all previous trials separately, but instead keeps a running internal memory state that summarizes recent opponent behaviour.

The model has four parameters:

- **alpha**: learning rate / recency weight  
- **beta**: sensitivity of choice to the internal memory state  
- **bias**: baseline tendency toward one response option  
- **m0**: initial memory state before the first trial  

The update rule is:

`memory_new = memory_old + alpha * (opponent_choice - memory_old)`

The current memory state is then transformed into a choice probability through a logistic function.

So cognitively, the model assumes that the player:
1. maintains an internal state,
2. updates that state from new observations,
3. and uses that state to guide choice.

## Commented Stan model

The Stan file in the repository contains comments explaining the role of each section:
- `data`
- `parameters`
- `transformed parameters`
- `model`
- `generated quantities`

In particular, the transformed parameters block computes the latent memory trajectory across trials, and the generated quantities block produces posterior predictive simulated choices.

## Model quality

### Prior predictive checks

I first checked whether the priors generated reasonable behaviour before fitting the model.

The prior predictive distribution of the mean simulated choice was broad and centered roughly around 0.5. I think this is acceptable because it means the priors are not forcing the model into only one type of behaviour before seeing the data.

So my conclusion is that the prior predictive behaviour is reasonable as a starting point.

### Prior-to-posterior update

I then compared the priors with the posteriors after fitting the model to simulated data.

The results showed clear updating for **alpha**, **beta**, and **bias**, while **m0** updated less strongly. This suggests that the data were informative about some parameters more than others.

I interpret this as meaning that the model can learn from the data, but some parameters are easier to identify than others.

### Posterior predictive checks

The posterior predictive checks showed that the fitted model could reproduce the overall distribution of binary choices fairly well.

This is only a basic posterior predictive check, but it still suggests that the fitted model captures an important part of the observed behaviour.

## Parameter recovery

A major part of the assignment was to test whether the model could recover known parameter values from simulated data.

I simulated datasets with known values of alpha, beta, bias, and m0, fit the model back to those datasets, and compared the true values with the recovered estimates.

The recovery results were mixed.

### Bias

Bias was recovered best. The recovered values tracked the true values closely across the different trial conditions.

### Alpha

Alpha showed partial recovery. The recovered values were related to the true values, but the estimates were often compressed toward the middle and not always close to the identity line.

### Beta

Beta also showed only partial recovery. Some datasets recovered beta reasonably well, but others did not, so the parameter was not as stable as I would want.

### m0

The weakest recovery was for m0. The recovered values stayed in a fairly narrow range even when the true values varied more. This suggests that the initial memory state is difficult to identify from these behavioural sequences.

So overall, the model does **not** recover all parameters equally well.

## How many trials are needed?

I compared recovery across 50, 100, and 200 trials.

My conclusion is:

- **50 trials** is too short for reliable recovery of the full parameter set  
- **100 trials** is better, but still unstable  
- **200 trials** improves recovery, but some parameters, especially **m0**, are still not recovered well  

So increasing the number of trials helps, but in my current setup even 200 trials is not enough for strong recovery of every parameter.

## Role of priors

The priors matter more when the data are not highly informative.

In my results, this was especially clear for **m0**, which was not strongly identified by the data. In that case, the posterior remained relatively constrained and seemed more influenced by the prior.

So I would say that priors are useful for regularizing the model, but they also matter more when behavioural data do not strongly identify a parameter.

## Results on simulated data

When I fit the model to simulated data generated from known values, the posterior means were approximately:

- alpha = 0.27
- beta = 2.98
- bias = -0.13
- m0 = 0.54

The true values were:

- alpha = 0.20
- beta = 3.00
- bias = 0.00
- m0 = 0.50

So beta was recovered quite well, alpha was somewhat overestimated, bias was close to zero but slightly negative, and m0 was reasonably close.

The diagnostics were good, with R-hat values around 1 and high effective sample sizes, which suggests that the chains mixed well.

## Optional empirical analysis

As an optional extension, I also fit the model to one empirical sequence from the students_22 dataset.

The posterior means were approximately:

- alpha = 0.22
- beta = 0.79
- bias = 0.01
- m0 = 0.46

My interpretation is that this participant shows:
- moderate updating from recent evidence,
- relatively weak coupling between memory and actual choice,
- almost no stable choice bias,
- and a near-neutral initial memory state.

So this participant does not appear highly deterministic. The internal memory state seems to influence behaviour, but not extremely strongly.

## Conclusion

The model is useful as a simple cognitive model of Matching Pennies behaviour because it captures:
- trial-by-trial updating,
- a latent memory state,
- and probabilistic choice.

The main limitation is that the parameters are not equally identifiable. Bias is recovered well, but alpha and beta are only partly recovered, and m0 is difficult to recover in this setup.

So my final conclusion is that the model is promising and interpretable, but it should be used with caution. Recovery depends on the amount of data, and some parameters are much easier to estimate than others.
