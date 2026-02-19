# 00_setup.R
# Package setup + global options.

pkgs <- c(
  "cmdstanr",
  "posterior",
  "bayesplot",
  "loo",
  "dplyr",
  "tidyr",
  "ggplot2",
  "readr",
  "purrr",
  "stringr"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall them (preferably via renv) and rerun.")
}

# Load what we use a lot (explicit for readability)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(stringr)

# bayesplot theme
bayesplot::bayesplot_theme_set(bayesplot::theme_default())

# cmdstanr: make sure CmdStan exists
if (is.null(cmdstanr::cmdstan_path()) || cmdstanr::cmdstan_path() == "") {
  message("CmdStan not set. If needed run: cmdstanr::install_cmdstan()")
}
