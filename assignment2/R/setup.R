# setup.R
# Package setup + global options.

pkgs <- c(
  "cmdstanr",
  "posterior",
  "bayesplot",
  "loo",
  "dplyr",
  "tidyr",
  "tibble",
  "ggplot2",
  "readr",
  "purrr",
  "stringr"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    "Missing packages: ", paste(missing, collapse = ", "), "\n",
    "Install them (preferably via renv) and rerun."
  )
}

# Load what we use a lot (explicit for readability)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(purrr)
  library(stringr)
})

# bayesplot theme
bayesplot::bayesplot_theme_set(bayesplot::theme_default())

# cmdstanr: warn early if CmdStan is not configured
if (is.null(cmdstanr::cmdstan_path()) || cmdstanr::cmdstan_path() == "") {
  message("CmdStan not set. If needed run: cmdstanr::install_cmdstan()")
}
