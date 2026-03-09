# ============================================================
# Assignment 2 -- optional empirical example
# ============================================================

source(file.path("assignment2_stan-matching-pennies", "R", "00_setup.R"))

# I use the students_22 dataset because each sequence has 40 trials, which is simple to work with for a first empirical example.

df_raw <- read_csv(file.path(raw_dir, "mp_students_22.csv"), show_col_types = FALSE)

# Pick one complete sequence.
# Here I just take the first available ID x Role x BotStrategy combination.
sequence_info <- df_raw %>%
  group_by(ID, Role, BotStrategy) %>%
  summarise(n_trials = n(), .groups = "drop") %>%
  arrange(desc(n_trials), ID)

chosen_id <- sequence_info$ID[1]
chosen_role <- sequence_info$Role[1]
chosen_bot <- sequence_info$BotStrategy[1]

empirical_df <- df_raw %>%
  filter(ID == chosen_id, Role == chosen_role, BotStrategy == chosen_bot) %>%
  arrange(Trial) %>%
  transmute(
    trial = Trial,
    choice = Choice,
    opponent = BotChoice,
    payoff = ifelse(Payoff == 1, 1, 0)
  )

write_csv(empirical_df, file.path(derived_dir, "empirical_sequence_students22.csv"))

stan_data <- list(
  T = nrow(empirical_df),
  y = as.integer(empirical_df$choice),
  opponent = as.integer(empirical_df$opponent)
)

mod <- cmdstan_model(model_file)

fit_empirical <- mod$sample(
  data = stan_data,
  seed = sampling_seed,
  chains = chains_main,
  parallel_chains = chains_main,
  iter_warmup = iter_warmup_main,
  iter_sampling = iter_sampling_main,
  adapt_delta = adapt_delta_main,
  refresh = 200
)

fit_empirical$save_object(file.path(fits_dir, "fit_empirical_students22.rds"))

empirical_summary <- fit_empirical$summary(variables = c("alpha", "beta", "bias", "m0"))
write_csv(empirical_summary, file.path(derived_dir, "fit_summary_empirical_students22.csv"))
print(empirical_summary)

yrep_emp <- fit_empirical$draws("y_rep", format = "matrix")

plot_emp_1 <- ggplot(empirical_df, aes(trial, cumsum(payoff) / trial)) +
  geom_line() +
  labs(
    title = paste("Empirical sequence:", chosen_id, "bot", chosen_bot),
    x = "Trial",
    y = "Cumulative mean payoff"
  )

plot_emp_2 <- ppc_bars(empirical_df$choice, yrep_emp[1:100, ]) +
  ggtitle("Posterior predictive check on the empirical sequence")

save_plot(plot_emp_1, file.path(plots_dir, "empirical_cumulative_payoff.png"), width = 8, height = 5)
save_plot(plot_emp_2, file.path(plots_dir, "empirical_posterior_predictive_check.png"), width = 7, height = 5)

print(plot_emp_1)
print(plot_emp_2)

# You must be my empirical sequence, because you are messy enough to be real and still interesting enough to model.
