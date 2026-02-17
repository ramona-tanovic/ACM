# R/strategies.R
## Three simple strategy "objects" (init/act/learn):
# - init(): returns a mutable state (environment)
# - act(state, role): returns action 0/1
# - learn(state, self_action, opp_action, reward, role): updates state (in-place)

source(here::here("assignment1/R/utils.R"))

make_strategy <- function(name, init, act, learn, params = list()) {
  list(
    name = name,
    params = params,
    init = init,
    act = act,
    learn = learn
  )
}

# 1) Random (baseline): choose Left/Right uniformly each trial.
make_random <- function() {
  make_strategy(
    name = "Random(0.5)",
    init = function() new.env(parent = emptyenv()),
    act = function(state, role) sample(c(0L, 1L), 1),
    learn = function(state, self_action, opp_action, reward, role) invisible(NULL)
  )
}

# 2) Win–Stay Lose–Shift (WSLS): 1-step memory of last action + last outcome.
# If last trial was rewarded -> repeat last action, otherwise switch.
# A small lapse rate means "sometimes I just press randomly".
make_wsls <- function(lapse = 0.02) {
  make_strategy(
    name = "WSLS",
    params = list(lapse = lapse),
    init = function() {
      s <- new.env(parent = emptyenv())
      s$has_prev <- FALSE
      s$prev_action <- NA_integer_
      s$prev_reward <- NA_real_
      s
    },
    act = function(state, role) {
      if (!isTRUE(state$has_prev)) {
        a <- sample(c(0L, 1L), 1)
      } else {
        if (state$prev_reward > 0) a <- state$prev_action else a <- 1L - state$prev_action
      }
      apply_lapse(a, lapse = lapse)
    },
    learn = function(state, self_action, opp_action, reward, role) {
      state$has_prev <- TRUE
      state$prev_action <- as.integer(self_action)
      state$prev_reward <- as.numeric(reward)
      invisible(NULL)
    }
  )
}


# 3) Forgetful bias-tracker (more ambitious, still simple):
# Track opponent's tendency to pick 1 using an exponentially weighted moving average.
# - eta controls memory: higher eta -> "forget faster" (shorter memory)
# - lapse adds occasional random presses
# Best-respond given your role:
#   * Matcher: pick the action opponent is more likely to pick
#   * Hider: pick the opposite of the opponent's most likely action
make_bias_tracker <- function(eta = 0.15, lapse = 0.02) {
  make_strategy(
    name = "ForgetfulBias",
    params = list(eta = eta, lapse = lapse),
    init = function() {
      s <- new.env(parent = emptyenv())
      s$p_hat <- 0.5  # belief that opponent will play action 1
      s
    },
    act = function(state, role) {
      # deterministic best-response to a biased opponent (tie -> random)
      if (abs(state$p_hat - 0.5) < 1e-9) {
        a <- sample(c(0L, 1L), 1)
      } else {
        opp_most_likely <- ifelse(state$p_hat > 0.5, 1L, 0L)
        if (role == "Matcher") {
          a <- opp_most_likely
        } else { # Hider
          a <- 1L - opp_most_likely
        }
      }
      apply_lapse(a, lapse = lapse)
    },
    learn = function(state, self_action, opp_action, reward, role) {
      # Exponentially weighted moving average update:
      # p_hat <- (1-eta)*p_hat + eta*opp_action
      state$p_hat <- (1 - eta) * state$p_hat + eta * as.numeric(opp_action)
      invisible(NULL)
    }
  )
}

# Convenience list used by run_all.R and the report
default_strategies <- function() {
  list(
    Random = make_random(),
    WSLS = make_wsls(lapse = 0.02),
    Bias = make_bias_tracker(eta = 0.15, lapse = 0.02)
  )
}
