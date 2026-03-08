# renv_maintenance.R
# Helper functions for checking, updating, maintaining, and troubleshooting renv

ensure_renv <- function() {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }
  invisible(TRUE)
}

renv_bootstrap <- function(init_if_needed = FALSE, bare = TRUE) {
  ensure_renv()
  
  if (file.exists("renv.lock") || dir.exists("renv")) {
    renv::load()
    message("renv project loaded.")
  } else if (isTRUE(init_if_needed)) {
    renv::init(bare = bare)
    message("renv initialized for this project.")
  } else {
    message("No renv project found. Set init_if_needed = TRUE to initialize.")
  }
  
  invisible(TRUE)
}

renv_check <- function(show_dependencies = TRUE) {
  ensure_renv()
  renv::load()
  
  cat("\n== renv status ==\n")
  renv::status()
  
  if (isTRUE(show_dependencies)) {
    cat("\n== detected project dependencies ==\n")
    print(renv::dependencies())
  }
  
  invisible(TRUE)
}

renv_update_check <- function(packages = NULL) {
  ensure_renv()
  renv::load()
  
  # Check what can be updated without installing anything
  renv::update(packages = packages, check = TRUE, prompt = FALSE)
}

renv_update_install <- function(packages = NULL, exclude = NULL, rebuild = FALSE) {
  ensure_renv()
  renv::load()
  
  # Install available updates, but do NOT lock them yet
  renv::update(
    packages = packages,
    exclude = exclude,
    rebuild = rebuild,
    prompt = interactive(),
    lock = FALSE
  )
  
  message("Updates installed. Run your project/tests, then call renv_snapshot_now() if everything works.")
  invisible(TRUE)
}

renv_snapshot_now <- function(type = NULL, force = FALSE) {
  ensure_renv()
  renv::load()
  
  if (is.null(type)) {
    renv::snapshot(prompt = FALSE, force = force)
  } else {
    renv::snapshot(type = type, prompt = FALSE, force = force)
  }
  
  message("Lockfile updated.")
  invisible(TRUE)
}

renv_restore_known_good <- function(clean = FALSE, rebuild = FALSE) {
  ensure_renv()
  renv::load()
  
  renv::restore(
    clean = clean,
    rebuild = rebuild,
    prompt = FALSE
  )
  
  message("Project library restored from renv.lock.")
  invisible(TRUE)
}

renv_clean_project <- function(remove_unused = FALSE) {
  ensure_renv()
  renv::load()
  
  actions <- c("package.locks", "library.tempdirs")
  if (isTRUE(remove_unused)) {
    actions <- c(actions, "unused.packages")
  }
  
  renv::clean(actions = actions, prompt = FALSE)
  invisible(TRUE)
}

renv_fix_common <- function(rebuild_packages = NULL, recursive = TRUE) {
  ensure_renv()
  renv::load()
  
  cat("\n== diagnostics ==\n")
  renv::diagnostics()
  
  cat("\n== repair common problems ==\n")
  renv::repair()
  
  if (!is.null(rebuild_packages)) {
    cat("\n== rebuild selected packages ==\n")
    renv::rebuild(
      packages = rebuild_packages,
      recursive = recursive,
      prompt = FALSE
    )
  }
  
  invisible(TRUE)
}

renv_rollback_info <- function() {
  ensure_renv()
  renv::load()
  
  # Requires git history for renv.lock
  renv::history()
}

renv_rollback_to <- function(commit = "HEAD") {
  ensure_renv()
  renv::load()
  
  # Revert lockfile to an earlier git commit
  renv::revert(commit = commit)
  message("Lockfile reverted. Run renv_restore_known_good() to reinstall that state.")
  
  invisible(TRUE)
}

renv_stale_lock_cleanup <- function() {
  # Use only if you hit stale renv lock issues
  if (dir.exists("renv/lock")) {
    unlink("renv/lock", recursive = TRUE, force = TRUE)
    message("Removed stale renv/lock directory.")
  } else {
    message("No renv/lock directory found.")
  }
  
  invisible(TRUE)
}

# Optional session settings:
# options(renv.config.auto.snapshot = FALSE)
# options(renv.config.install.verbose = TRUE)
# options(renv.config.locking.enabled = FALSE)  # only if you have locking issues

# Examples:
# First-time setup in a new project:
# renv_bootstrap(init_if_needed = TRUE)

# Daily checks:
# renv_bootstrap()
# renv_check()

# See what can be updated:
# renv_update_check()

# Update one or more packages:
# renv_update_install(packages = c("dplyr", "ggplot2"))

# Update everything except a few:
# renv_update_install(exclude = c("sf", "terra"))

# After testing your project successfully:
# renv_snapshot_now()

# If an update broke the project:
# renv_restore_known_good()

# Force exact match to the lockfile:
# renv_restore_known_good(clean = TRUE)

# Clean leftover install locks / temp dirs:
# renv_clean_project()

# Repair common problems and rebuild problematic packages:
# renv_fix_common(rebuild_packages = c("xml2", "rlang"))

# Inspect lockfile history, then roll back:
# renv_rollback_info()
# renv_rollback_to("HEAD~1")
# renv_restore_known_good()
