# R/plots.R
# -----------------------------------------------------------------------------
# Plotting helpers for the tournament outputs.
# Stan-based interpretation plots are handled in recovery.R.
# -----------------------------------------------------------------------------

summarise_matchups <- function(matches_df, R = 400, level = 0.95, seed = 123) {
  matches_df |>
    dplyr::group_by(pairing, A, B) |>
    dplyr::summarise(
      stats = list(bootstrap_mean_ci(total_payoff_A, R = R, level = level, seed = seed)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mean = vapply(stats, function(z) z$mean, numeric(1)),
      lo   = vapply(stats, function(z) z$lo,   numeric(1)),
      hi   = vapply(stats, function(z) z$hi,   numeric(1))
    ) |>
    dplyr::select(-stats)
}

summarise_role_advantage <- function(players_df, R = 400, level = 0.95, seed = 123) {
  players_df |>
    dplyr::group_by(strategy) |>
    dplyr::summarise(
      stats = list(bootstrap_mean_ci(role_advantage, R = R, level = level, seed = seed)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mean = vapply(stats, function(z) z$mean, numeric(1)),
      lo   = vapply(stats, function(z) z$lo,   numeric(1)),
      hi   = vapply(stats, function(z) z$hi,   numeric(1))
    ) |>
    dplyr::select(-stats)
}

plot_fig1_matchups <- function(matchup_sum, out_file) {
  matchup_sum <- matchup_sum[order(matchup_sum$mean, decreasing = TRUE), ]
  matchup_sum$pairing <- factor(matchup_sum$pairing, levels = matchup_sum$pairing)

  p <- ggplot2::ggplot(matchup_sum, ggplot2::aes(x = pairing, y = mean)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(
      title = "Fig 1 — Tournament performance",
      subtitle = "Mean payoff for player A (± bootstrap 95% interval)",
      x = NULL,
      y = "Total payoff (sum over trials)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  ggplot2::ggsave(out_file, p, width = 8, height = 4.5, dpi = 200)
  invisible(p)
}

plot_fig2_role_advantage <- function(role_sum, out_file) {
  role_sum$strategy <- factor(role_sum$strategy, levels = role_sum$strategy)

  p <- ggplot2::ggplot(role_sum, ggplot2::aes(x = strategy, y = mean)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(
      title = "Fig 2 — Role sensitivity (Matcher advantage)",
      subtitle = "Payoff as Matcher minus payoff as Mismatcher (± bootstrap 95% CI)",
      x = NULL,
      y = "Matcher advantage"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(out_file, p, width = 6, height = 4, dpi = 200)
  invisible(p)
}

plot_fig4_payoff_distribution <- function(matches_df, out_file) {
  p <- ggplot2::ggplot(matches_df, ggplot2::aes(x = pairing, y = total_payoff_A)) +
    ggplot2::geom_violin(scale = "width", trim = TRUE) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(
      title = "Fig 4 — Payoff distributions",
      subtitle = "Variance across simulations (not just the mean)",
      x = NULL,
      y = "Total payoff for A"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  ggplot2::ggsave(out_file, p, width = 8, height = 4.5, dpi = 200)
  invisible(p)
}
