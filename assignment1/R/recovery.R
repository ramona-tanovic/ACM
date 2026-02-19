# R/recovery.R
# -----------------------------------------------------------------------------
# Parameter recovery + interpretation figures.
#
# Why parameter recovery? Because we want to show that our parameters are meaningful.
# How to do that:
# - simulate data from a model
# - fit the model back to the data
# - check whether we recover the known parameters
#
# This is the cleanest way to argue "my parameters mean something".
# -----------------------------------------------------------------------------

simulate_dataset <- function(strategy_A, strategy_B, params_A, params_B, T, payoff_win, payoff_loss) {
  # Simulate S independent matches where "subjects" are always player A.
  S <- nrow(params_A)

  a_mat <- matrix(0L, nrow = S, ncol = T)
  b_mat <- matrix(0L, nrow = S, ncol = T)
  role_mat <- matrix(0L, nrow = S, ncol = T)

  # For reporting / sanity checks
  payoff_total <- numeric(S)

  for (s in 1:S) {
    agentA <- make_agent(strategy_A, params_A[s, ])
    agentB <- make_agent(strategy_B, params_B[s, ])

    sim <- simulate_match(agentA, agentB, T = T, payoff_win = payoff_win, payoff_loss = payoff_loss)

    a_mat[s, ] <- sim$a
    b_mat[s, ] <- sim$b
    role_mat[s, ] <- sim$roleA
    payoff_total[s] <- sim$total_payoff
  }

  list(a_mat = a_mat, b_mat = b_mat, role_mat = role_mat, payoff_total = payoff_total)
}

pick_example_subjects <- function(S, n = 3) {
  # Choose a few subjects to show full posterior densities.
  # Why only a few? Because density plots get unreadable with many lines.
  unique(pmin(seq_len(S), n))
}

plot_posterior_densities_with_truth <- function(draws_df, params, true_df, subjects, out_file) {
  # Draw posterior density for selected subjects, with vertical true-value line.
  # params: character vector of parameter base names (e.g. c("p_repeat_win","lapse"))

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Need ggplot2")

  all_long <- list()

  for (p in params) {
    cols <- grep(paste0("^", p, "\\["), colnames(draws_df), value = TRUE)
    if (length(cols) == 0) next
    # Keep only chosen subjects (match by index in the column name).
    # Extract the subject index from column names like "p_repeat_win[12]".
    # parse_number is simple and avoids regex escaping issues.
    col_idx <- readr::parse_number(cols)
    keep_cols <- cols[col_idx %in% subjects]

    df <- tidyr::pivot_longer(
      as.data.frame(draws_df[, keep_cols, drop = FALSE]),
      cols = dplyr::everything(),
      names_to = "param",
      values_to = "value"
    )
    df$subject <- as.integer(gsub("^.*\\[(\\d+)\\]$", "\\1", df$param))
    df$param_base <- p

    all_long[[p]] <- df
  }

  long <- dplyr::bind_rows(all_long)

  # True values in long format
  true_long <- true_df |>
    tidyr::pivot_longer(cols = dplyr::all_of(params), names_to = "param_base", values_to = "true")

  true_long <- true_long |>
    dplyr::filter(subject %in% subjects)

  p <- ggplot2::ggplot(long, ggplot2::aes(x = value)) +
    ggplot2::geom_density(ggplot2::aes(group = subject), alpha = 0.35) +
    ggplot2::geom_vline(
      data = true_long,
      ggplot2::aes(xintercept = true),
      linetype = 2
    ) +
    ggplot2::facet_grid(param_base ~ subject, scales = "free") +
    ggplot2::labs(
      title = "Posterior densities for a few example subjects",
      subtitle = "Dashed line = true generating value (parameter recovery check)",
      x = NULL,
      y = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(out_file, p, width = 10, height = 5, dpi = 200)
  invisible(p)
}

plot_recovery_scatter <- function(recovery_df, param, out_file) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Need ggplot2")

  p <- ggplot2::ggplot(recovery_df, ggplot2::aes(x = true, y = mean)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.02) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::labs(
      title = paste0("Recovery: ", param),
      subtitle = "y = posterior mean (±95% interval), x = true value",
      x = "True value",
      y = "Posterior mean"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(out_file, p, width = 6, height = 5, dpi = 200)
  invisible(p)
}

plot_loo_dot <- function(dot_df, out_file, title = "LOO model comparison") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Need ggplot2")

  p <- ggplot2::ggplot(dot_df, ggplot2::aes(x = elpd_diff, y = dataset)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = elpd_diff - 2 * se, xmax = elpd_diff + 2 * se), height = 0.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2) +
    ggplot2::labs(
      title = title,
      subtitle = "Positive means Belief model fits better; bars are ~95% (±2 SE)",
      x = "elpd difference (Belief − WSLS)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(out_file, p, width = 7, height = 3.5, dpi = 200)
  invisible(p)
}
