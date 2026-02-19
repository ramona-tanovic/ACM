# R/plots.R
# -----------------------------------------------------------------------------
# Plotting helpers.
# -----------------------------------------------------------------------------

# Small helper used by stat_summary(fun.data = ...):
mean_se <- function(x) {
  x <- x[is.finite(x)]
  m <- mean(x)
  se <- stats::sd(x) / sqrt(length(x))
  data.frame(y = m, ymin = m - se, ymax = m + se)
}

# summarise matchups: for each pairing, compute mean payoff for A and bootstrap CI
summarise_matchups <- function(matches_df, R = 400, level = 0.95, seed = 123) { # note: R = 400 is a bit low for final results, but speeds up testing
  matches_df |>
    dplyr::group_by(pairing, A, B) |>
    dplyr::summarise(
      stats = list(bootstrap_mean_ci(total_payoff_A, R = R, level = level, seed = seed)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mean = vapply(stats, function(z) z$mean, numeric(1)),
      lo   = vapply(stats, function(z) z$lo, numeric(1)),
      hi   = vapply(stats, function(z) z$hi, numeric(1))
    ) |>
    dplyr::select(-stats)
}

# summarise role advantage: for each strategy, compute mean payoff as Matcher minus payoff as Mismatcher, and bootstrap CI
summarise_role_advantage <- function(players_df, R = 400, level = 0.95, seed = 123) {
  players_df |>
    dplyr::group_by(strategy) |>
    dplyr::summarise(
      stats = list(bootstrap_mean_ci(role_advantage, R = R, level = level, seed = seed)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mean = vapply(stats, function(z) z$mean, numeric(1)),
      lo   = vapply(stats, function(z) z$lo, numeric(1)),
      hi   = vapply(stats, function(z) z$hi, numeric(1))
    ) |>
    dplyr::select(-stats)
}

# fig0: plot trial dynamics, with mean + SE across sims, and vertical line at swap trial
plot_fig0_dynamics <- function(trials_df, out_file, swap_trial = 30) {
  # What does it do:
  # 1) compute cum payoff per sim
  # 2) summarise mean + SE per pairing x trial
  # 3) plot line + ribbon (no per-sim grouping)
  
  df <- trials_df |>
    dplyr::group_by(pairing, sim_id) |>
    dplyr::arrange(trial, .by_group = TRUE) |>
    dplyr::mutate(cum_payoffA = cumsum(payoffA)) |>
    dplyr::ungroup()
  
  sum_df <- df |>
    dplyr::group_by(pairing, trial) |>
    dplyr::summarise(
      mean = mean(cum_payoffA),
      se   = stats::sd(cum_payoffA) / sqrt(dplyr::n()),
      .groups = "drop"
    ) |>
    dplyr::mutate(ymin = mean - se, ymax = mean + se)
  
  p <- ggplot2::ggplot(sum_df, ggplot2::aes(x = trial, y = mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin, ymax = ymax, fill = pairing), alpha = 0.15) +
    ggplot2::geom_line(ggplot2::aes(color = pairing), linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_vline(xintercept = swap_trial + 0.5, linetype = 2) +
    ggplot2::facet_wrap(~ pairing, ncol = 2, scales = "free_y") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Fig 0 — Trial dynamics and the role swap",
      subtitle = "Mean cumulative payoff (±1 SE). Dashed vertical line = swap at trial 30.",
      x = "Trial",
      y = "Cumulative payoff for player A"
    )
  
  ggplot2::ggsave(out_file, p, width = 10, height = 6, dpi = 200)
  invisible(p)
}

# fig1: plot mean payoff for A in each pairing, with bootstrap CI, sorted by mean payoff
plot_fig1_matchups <- function(matchup_sum, out_file) {
  matchup_sum <- matchup_sum[order(matchup_sum$mean, decreasing = TRUE), ]
  matchup_sum$pairing <- factor(matchup_sum$pairing, levels = matchup_sum$pairing)
  
  p <- ggplot2::ggplot(matchup_sum, ggplot2::aes(x = pairing, y = mean)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = "Fig 1 — Tournament performance",
      subtitle = "Mean payoff for player A (± bootstrap 95% interval)",
      x = NULL,
      y = "Total payoff (sum over trials)"
    )
  
  ggplot2::ggsave(out_file, p, width = 9, height = 5, dpi = 200)
  invisible(p)
}

# fig2: plot role advantage (Matcher payoff minus Mismatcher payoff) for each strategy, with bootstrap CI, sorted by mean advantage
plot_fig2_role_advantage <- function(role_sum, out_file) {
  role_sum$strategy <- factor(role_sum$strategy, levels = role_sum$strategy)
  
  p <- ggplot2::ggplot(role_sum, ggplot2::aes(x = strategy, y = mean)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Fig 2 — Role sensitivity (Matcher advantage)",
      subtitle = "Payoff as Matcher minus payoff as Mismatcher (± bootstrap 95% CI)",
      x = NULL,
      y = "Matcher advantage"
    )
  
  ggplot2::ggsave(out_file, p, width = 7, height = 4.5, dpi = 200)
  invisible(p)
}

# fig4: plot payoff distributions for each pairing, as violins (not just mean + CI)
plot_fig4_payoff_distribution <- function(matches_df, out_file) {
  p <- ggplot2::ggplot(matches_df, ggplot2::aes(x = pairing, y = total_payoff_A)) +
    ggplot2::geom_violin(scale = "width", trim = TRUE) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = "Fig 4 — Payoff distributions",
      subtitle = "Variance across simulations (not just the mean)",
      x = NULL,
      y = "Total payoff for A"
    )
  
  ggplot2::ggsave(out_file, p, width = 9, height = 5, dpi = 200)
  invisible(p)
}
