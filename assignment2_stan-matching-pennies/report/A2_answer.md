# Assignment 2

Repository link:

## Model

In this assignment I built and validated a cognitive model of Matching Pennies behaviour in Stan.

The model is a **single-agent recency-weighted / exponential-forgetting memory model**. Rather than storing all previous trials separately, the player is assumed to maintain a running internal memory state that summarizes recent opponent behaviour. This memory state is updated over time and then used to guide probabilistic choice.

The model has four parameters:

* **alpha**: learning rate / recency weight
* **beta**: sensitivity of choice to the internal memory state
* **bias**: baseline tendency toward one response option
* **m0**: initial memory state before the first trial

The update rule is:

`memory_new = memory_old + alpha * (opponent_choice - memory_old)`

The current memory state is then transformed into a choice probability through a logistic function.

Cognitively, the model assumes that the player:

1. maintains an internal memory state,
2. updates that state from new observations,
3. and uses that state to guide choice.

A useful distinction is that **alpha** governs how memory is updated, whereas **beta** governs how strongly memory affects behaviour.

## Commented Stan model

The Stan file in the repository is commented throughout and explains the role of the main Stan blocks:

* **data**: defines the observed inputs, including the number of trials, the participant’s choices, and the opponent’s choices
* **parameters**: defines the unknown quantities to estimate, namely alpha, beta, bias, and m0
* **transformed parameters**: reconstructs the latent memory trajectory across trials and converts memory into trial-by-trial choice tendencies
* **model**: specifies the priors and the likelihood for the observed choices
* **generated quantities**: produces posterior predictive choices and pointwise log-likelihood values

The transformed parameters block is the core of the model, because this is where the hidden learning process is rebuilt across trials.

## Model quality

### Prior predictive checks

I first examined whether the priors generated reasonable behaviour before fitting the model.

The prior predictive distributions for both mean simulated choice and mean simulated payoff were broad but sensible. The observed dataset fell inside the prior predictive range in both cases. This suggests that the priors were not forcing the model toward only one narrow type of behaviour before seeing the data.

I therefore concluded that the prior predictive behaviour was acceptable as a starting point.

### Prior-to-posterior update

I then compared the prior distributions with the posterior distributions after fitting the model to simulated data.

The results showed clear updating for **alpha**, **beta**, and **bias**. The posterior distributions for these parameters were clearly narrower than the priors, indicating that the data were informative. Updating for **m0** was weaker, which suggests that the initial memory state was less strongly identified by the behavioural sequence.

This means that the model does learn from the data, but not all parameters are equally identifiable.

### Posterior predictive checks

I used two posterior predictive checks.

First, I used a basic posterior predictive check comparing the observed and replicated distribution of binary choices. This showed that the fitted model could reproduce the overall distribution of choices reasonably well.

Second, I added a stronger posterior predictive check based on **cumulative payoff across trials**. This is more relevant for a learning model because it evaluates whether the fitted model can reproduce the broad performance trajectory over time, rather than only the total number of 0 and 1 responses.

The cumulative-payoff posterior predictive check showed that the observed curve stayed inside the 10th to 90th percentile predictive band on about **75% of trials**. This suggests that the model captures the broad learning trajectory reasonably well, although it does not perfectly reproduce some sharper early fluctuations.

Overall, the posterior predictive checks suggest acceptable but not perfect model fit.

## Results on simulated data

I first fit the model to one dataset simulated from known parameter values.

The true values were:

* alpha = 0.20
* beta = 3.00
* bias = 0.00
* m0 = 0.50

The posterior means from the restarted baseline fit were approximately:

* alpha = 0.21
* beta = 2.49
* bias = 0.15
* m0 = 0.39

So alpha was recovered quite well, beta was somewhat underestimated, bias was slightly above zero even though the true value was zero, and m0 was estimated below the true value.

The diagnostics were good, with R-hat values very close to 1, high effective sample sizes, and zero divergences. This suggests that the model fit was technically reliable.

## Parameter recovery

A major part of the assignment was to test whether the model could recover known parameter values from simulated data.

The point of parameter recovery is to test **identifiability**. A model can fit the data overall, but still fail to estimate its parameters reliably. Recovery therefore asks whether the model can learn back the true generating values from datasets simulated under known parameters.

I first ran a small baseline recovery study and then an extended recovery study with more simulated datasets and quantitative recovery summaries.

### Recovery procedure

For the recovery analysis, I:

1. sampled known true parameter values,
2. simulated datasets from those values,
3. fit the same Stan model back to each dataset,
4. compared the true and recovered parameter values.

I varied the number of trials across **50, 100, and 200** to test how recovery changed with more data.

In the extended recovery study, I also summarized recovery using:

* true-estimated correlations,
* mean absolute error (MAE),
* root mean squared error (RMSE),
* and posterior interval coverage.

## Recovery results

The recovery results were mixed, and parameter identifiability was clearly uneven.

### Bias

Bias was the best recovered parameter.

The true-estimated scatterplots were closest to the identity line, recovery correlations were very high, and errors decreased as trial count increased. This suggests that a stable side tendency leaves a relatively strong and consistent signal in the data.

### Alpha

Alpha showed **moderate recovery**.

The model could distinguish broad differences in learning rate, and the recovered values generally moved in the correct direction, but the estimates were still noisy and somewhat compressed.

### Beta

Beta showed only **partial recovery**.

The model could detect broad differences in how strongly memory influenced choice, but the recovered values were still fairly imprecise and compressed relative to the true values.

### m0

The weakest recovery was for **m0**.

Recovered m0 values stayed in a relatively narrow range even when the true values varied more widely. This indicates that the initial memory state was difficult to identify from these behavioural sequences.

Overall, the recovery results showed that the model does **not** recover all parameters equally well.

## How many trials are needed?

I compared recovery across 50, 100, and 200 trials.

My conclusion is:

* **50 trials** is too short for reliable recovery of the full parameter set
* **100 trials** is better, especially for alpha and bias, but still unstable
* **200 trials** improves some recovery measures further, especially for bias and beta, but some parameters, especially **m0**, are still not recovered well

So more trials help, but in my current setup even 200 trials is not enough for strong recovery of every parameter.

## Role of priors

I also tested the role of priors by comparing the baseline model with a tighter-prior version.

The tighter priors produced narrower prior predictive distributions and slightly more regularized posterior estimates, especially for parameters such as beta and m0. However, they did **not** materially change the main conclusions. The posterior summaries remained broadly similar, and the cumulative-payoff posterior predictive checks were also very similar.

My interpretation is that priors matter mainly as a source of **regularization**, especially when the data are only weakly informative. This is most relevant for weaker parameters such as m0, but in this dataset the tighter priors did not substantially improve model fit.

## Additional model checks

### Parameterization comparison

I compared the current constrained Stan model with a legacy unconstrained version.

The posterior summaries and cumulative-payoff posterior predictive checks were almost identical. This suggests that parameterization was not the main issue in this case. The main limitation of the model appears to be parameter identifiability rather than the way the parameters were expressed in Stan.

### Lapse parameter comparison

I also fit a version of the model with an additional **lapse parameter**, which allows some proportion of choices to be random rather than driven by the learning process.

The lapse posterior was concentrated near zero, which is what I would expect because the simulated baseline dataset was generated without a lapse process. Adding the lapse parameter did not materially improve the posterior predictive checks and barely changed the other parameter estimates.

This suggests that the lapse extension was not necessary for the current simulated dataset.

## Conclusion

This model is useful as a simple cognitive model of Matching Pennies behaviour because it captures:

* trial-by-trial updating,
* a latent memory state,
* and probabilistic choice based on that state.

The model is technically stable and gives reasonable prior and posterior predictive behaviour. It can also recover some parameters from simulated data. However, parameter identifiability is uneven.

Bias is recovered best, alpha is moderately recoverable, beta is only partly recoverable, and m0 is difficult to recover in this setup.

My overall conclusion is that the model is interpretable and promising, but it should be used with caution. More data help recovery, and priors can regularize weakly identified parameters, but not all parameters are equally trustworthy.
