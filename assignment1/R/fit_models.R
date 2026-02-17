# R/fit_models.R
# -----------------------------------------------------------------------------
# Fit WSLS and belief-learning models to simulated sequences.
# Also compute LOO model comparison.
#
# Why fit both models to both datasets?
# - It makes the comparison explicit: which model explains which behaviour?
# - It turns the assignment into "competing cognitive hypotheses" rather than
#   a single-score tournament.
# -----------------------------------------------------------------------------

prepare_stan_data <- function(sim_list) {
  # sim_list must contain: a_mat, b_mat, role_mat
  list(
    S = nrow(sim_list$a_mat),
    T = ncol(sim_list$a_mat),
    a = sim_list$a_mat,
    b = sim_list$b_mat,
    roleA = sim_list$role_mat
  )
}

summarise_subject_params <- function(draws_df, param_regex, true_values = NULL) {
  # draws_df is a draws_df (or data.frame) with columns like p_repeat_win[1]
  if (!requireNamespace("posterior", quietly = TRUE)) stop("Need posterior.")

  cols <- grep(param_regex, colnames(draws_df), value = TRUE)
  if (length(cols) == 0) stop("No columns matched: ", param_regex)

  # Long format: draw x subject
  long <- tidyr::pivot_longer(
    as.data.frame(draws_df[, cols, drop = FALSE]),
    cols = dplyr::everything(),
    names_to = "param",
    values_to = "value"
  )

  # Extract subject index from param name, e.g. p_repeat_win[12]
  long$subject <- as.integer(gsub("^.*\\[(\\d+)\\]$", "\\1", long$param))

  summ <- long |>
    dplyr::group_by(subject) |>
    dplyr::summarise(
      mean = mean(value),
      lo = stats::quantile(value, 0.025),
      hi = stats::quantile(value, 0.975),
      .groups = "drop"
    )

  if (!is.null(true_values)) {
    summ$true <- true_values[summ$subject]
  }

  summ
}

compute_loo <- function(fit) {
  if (!requireNamespace("loo", quietly = TRUE)) stop("Need loo.")
  ll <- extract_log_lik_matrix(fit)
  loo::loo(ll)
}

loo_table_and_dot <- function(loo_a, loo_b, label_a = "WSLS", label_b = "Belief") {
  # Returns a list(table_df, dot_df) used for reporting.
  comp <- loo::loo_compare(loo_a, loo_b)
  comp_df <- as.data.frame(comp)
  comp_df$model <- rownames(comp_df)
  rownames(comp_df) <- NULL

  # elpd_diff is relative to best model in loo_compare output.
  # For a dot plot, we compute difference directly (b - a).
  diff <- loo_b$estimates["elpd_loo", "Estimate"] - loo_a$estimates["elpd_loo", "Estimate"]
  se   <- sqrt(loo_a$estimates["elpd_loo", "SE"]^2 + loo_b$estimates["elpd_loo", "SE"]^2)

  dot_df <- data.frame(
    comparison = paste0(label_b, " - ", label_a),
    elpd_diff = diff,
    se = se
  )

  list(table = comp_df, dot = dot_df)
}
