# data_prep.R
# Standardise empirical datasets into a common format.

standardise_matching_pennies <- function(df) {
  # Expect common columns (as described in the course readme):
  # ID, Trial, Decision, Payoff, BotStrategy (optional), Block (optional)
  #
  # We keep this mapping explicit because datasets sometimes vary slightly.

  nm <- names(df)

  # Identify participant id
  if ("ID" %in% nm) {
    df$id <- df$ID
  } else if ("id" %in% nm) {
    df$id <- df$id
  } else {
    stop("Could not find participant ID column (expected 'ID').")
  }

  # Trial index
  if ("Trial" %in% nm) {
    df$trial <- df$Trial
  } else if ("trial" %in% nm) {
    df$trial <- df$trial
  } else {
    stop("Could not find Trial column (expected 'Trial').")
  }

  # Player decision (0/1). Some datasets use 0/1 already; others may use strings.
  if ("Decision" %in% nm) {
    dec <- df$Decision
  } else if ("Choice" %in% nm) {
    dec <- df$Choice
  } else {
    stop("Could not find player action column (expected 'Decision' or 'Choice').")
  }

  # Make sure action is 0/1
  if (is.character(dec)) {
    # If needed, user should implement their own mapping here.
    stop("Decision is character; please map to 0/1 explicitly for your dataset.")
  }
  df$a <- as.integer(dec)

  # Win/loss from Payoff: -1 loss, +1 win
  if ("Payoff" %in% nm) {
    df$win <- as.integer(df$Payoff == 1)
  } else {
    stop("Could not find Payoff column (expected 'Payoff').")
  }

  # Keep BotStrategy if present (useful for filtering)
  if ("BotStrategy" %in% nm) {
    df$BotStrategy <- as.character(df$BotStrategy)
  }

  # Optional block structure (schizophrenia and primates)
  if ("Block" %in% nm) {
    df$block <- df$Block
  } else {
    df$block <- 1L
  }

  df %>% select(id, block, trial, a, win, everything())
}

build_transitions <- function(std_df) {
  # Build transition-level dataset for WSLS model:
  # For each (id, block), define y_rep[t] and win_prev[t-1] for t >= 2.
  
  std_df %>%
    arrange(id, block, trial) %>%
    group_by(id, block) %>%
    mutate(
      a_prev   = dplyr::lag(a),
      win_prev = dplyr::lag(win),
      y_rep    = as.integer(a == a_prev)
    ) %>%
    filter(!is.na(a_prev), !is.na(win_prev)) %>%
    ungroup()
}


empirical_stats_from_transitions <- function(trans) {
  # Summaries for PPC comparisons (transition-level data)
  rep <- trans$y_rep
  prev_win <- trans$win_prev == 1
  
  tibble::tibble(
    repeat_rate       = mean(rep),
    repeat_after_win  = mean(rep[prev_win]),
    repeat_after_loss = mean(rep[!prev_win])
  )
}
