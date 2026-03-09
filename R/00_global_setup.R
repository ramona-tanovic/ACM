# ============================================================
# Global setup for the ACM repo
# ============================================================

set.seed(21)

suppressPackageStartupMessages({
  library(tidyverse)
})

theme_set(theme_classic())

# You must be my random seed, because every rerun still brings me back to you.

save_plot <- function(plot_object, filename, width = 7, height = 5, dpi = 300) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(filename = filename, plot = plot_object, width = width, height = height, dpi = dpi)
}
