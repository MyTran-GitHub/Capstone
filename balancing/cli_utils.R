#' Shared CLI parsing helpers for balancing scripts.
#'
#' Provides functions for parsing command-line flags, boolean and integer arguments, and resolving experiment directories.
## Shared CLI parsing helpers for balancing scripts.

parse_flag_value <- function(args, flag, default = NULL) {
  # Supports both: --flag=value and --flag value
  flag_eq <- paste0(flag, "=")
  hit_eq <- args[startsWith(args, flag_eq)]
  if (length(hit_eq) > 0) return(sub(flag_eq, "", hit_eq[1], fixed = TRUE))
  idx <- which(args == flag)
  if (length(idx) > 0 && idx[1] < length(args)) return(args[idx[1] + 1])
  default
}

parse_bool_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  lx <- tolower(as.character(x)[1])
  if (lx %in% c("1", "true", "t", "yes", "y")) return(TRUE)
  if (lx %in% c("0", "false", "f", "no", "n")) return(FALSE)
  default
}

parse_years_list <- function(x, flag_name) {
  if (is.null(x) || !nzchar(x)) return(NULL)
  vals <- as.integer(strsplit(x, ",", fixed = TRUE)[[1]])
  vals <- vals[is.finite(vals)]
  vals <- sort(unique(vals))
  if (length(vals) == 0) {
    stop(paste0("No valid years supplied for ", flag_name))
  }
  vals
}

parse_positive_int <- function(x, flag_name, default = NULL, min_value = 1L) {
  if (is.null(x) || !nzchar(x)) return(default)
  val <- suppressWarnings(as.integer(x))
  min_value <- as.integer(min_value)
  if (!is.finite(val) || is.na(val) || val < min_value) {
    stop(paste0("Invalid value for ", flag_name, ": expected integer >= ", min_value))
  }
  val
}

resolve_experiment_dir <- function(base_dir, experiment_name) {
  base_norm <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  if (basename(base_norm) == experiment_name) return(base_dir)
  file.path(base_dir, experiment_name)
}
