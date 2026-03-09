# Assignment 1

Repository link: 

## Two possible strategies for Matching Pennies

In this assignment I compared two simple strategies for the Matching Pennies game: **Win-Stay Lose-Shift (WSLS)** and a **recency-weighted memory agent**.

### 1. WSLS

The WSLS strategy uses the outcome of the previous trial. If the previous choice gave a win, the agent repeats that choice. If the previous choice gave a loss, the agent switches to the other option. In my implementation I also included a small amount of noise, so the agent is not perfectly deterministic on every trial.

This strategy is simple because it only needs information from the immediately previous trial:
- previous choice
- previous feedback

So cognitively, WSLS puts very low demands on memory. It does not try to represent a longer history of the opponent’s behaviour.

### 2. Recency-weighted memory agent

The second strategy is a recency-weighted memory agent. This agent keeps an internal memory value that is updated trial by trial depending on the opponent’s recent choices. The update rule is:

`memory_new = memory_old + alpha * (observation - memory_old)`

where `alpha` is the learning rate.

The memory value is then converted into a choice probability. If the internal memory is above 0.5, the agent becomes more likely to choose 1, and if it is below 0.5, it becomes more likely to choose 0.

Compared to WSLS, this strategy is cognitively richer because it does not only react to the previous trial. Instead, it tries to summarize recent history in one running internal state.

## Cognitive constraints

The two strategies differ in the kind of cognitive assumptions they make.

WSLS is cognitively light. It only needs very short-term memory and a simple rule for repeating or switching. This makes it plausible as a simple heuristic.

The memory agent assumes that the player can maintain and update an internal estimate of recent opponent behaviour. This is still simpler than perfect memory, because it does not store the full sequence of past trials, but it is more demanding than WSLS because it needs:
- an internal state
- trial-by-trial updating
- a mapping from memory to response probability

So the memory agent is more flexible, but also more complex.

## Formalisation

In my code, both strategies were implemented as explicit functions.

For WSLS, the formal rule was:

- if previous feedback = 1, repeat previous choice
- if previous feedback = 0, switch choice
- with some probability of noise, choose randomly instead

For the recency-weighted memory agent, the formal rule was:

1. start with an initial memory value
2. update memory after each observed opponent choice
3. transform the current memory into a choice probability
4. sample a binary response from that probability

Both strategies were tested against the same simple block-structured opponent, which alternated its preferred response pattern across blocks. This made it possible to compare how the two strategies handled structure in the environment.

## Plots and results

I used two plots.

The first plot showed the **cumulative win rate across trials** for many simulated agents. The second plot showed the **final win rate across simulated agents**.

Both plots told the same overall story. WSLS performed better than the recency-weighted memory agent against the block opponent.

The final average performance was approximately:
- **WSLS:** 0.86
- **Memory:** 0.66

So in this particular environment, WSLS was more successful.

## Brief discussion

My interpretation is that WSLS fits the block opponent better because it reacts directly to recent success and failure. When the opponent has a simple structured pattern, this local strategy works well.

The recency-weighted memory agent is smoother and more gradual. That can be useful in other environments, but here it seems less well matched to the abrupt block structure. Instead of switching quickly enough, it averages over recent observations and therefore adapts more slowly.

So the main conclusion from Assignment 1 is that the better strategy depends on the structure of the environment. In this simulation, WSLS was the stronger strategy.

## Files in the repository

The repository contains:
- the code for both strategy implementations
- the simulation code
- the visualization code
- the generated plots used in this document
