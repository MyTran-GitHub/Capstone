##
#' Run CBPS with selected controls
#'
#' This script runs the CBPS pipeline using a filtered control pool, saving outputs and metrics for a given year and configuration.
#' Usage: Rscript 04_run_cbps_with_selected_controls.R <year> <selected_units_csv> <output_prefix> <train_start> <train_end> <test_start> <test_end> [flags]
#' Flags: --experiment-name, --output-experiment-name, --analysis-base-dir, --output-base-dir, --save-full-weights, --use-cache, --cache-max-items, --embedding-k, --rolling-windows-json

#!/usr/bin/env Rscript

source("balancing/cli_utils.R")
source("balancing/run_cbps_filtered.R")
parse_flag_value <- get("parse_flag_value", mode = "function")
parse_bool_flag <- get("parse_bool_flag", mode = "function")
parse_positive_int <- get("parse_positive_int", mode = "function")
run_cbps_filtered <- get("run_cbps_filtered", mode = "function")
save_cbps_filtered_outputs <- get("save_cbps_filtered_outputs", mode = "function")

parse_args <- function() {
  #' Parse command line arguments for the CBPS run.
  raw_args <- commandArgs(trailingOnly = TRUE)

  # Allow flags to be interleaved with positional args.
  pos_args <- character(0)
  opt <- character(0)
  i <- 1
  while (i <= length(raw_args)) {
    a <- raw_args[i]
    if (startsWith(a, "--")) {
      # flag token; if next token exists and isn't another flag, treat as its value
      if (i < length(raw_args) && !startsWith(raw_args[i + 1], "--")) {
        opt <- c(opt, a, raw_args[i + 1])
        i <- i + 2
      } else {
        # boolean flag provided without explicit value -> treat as true
        opt <- c(opt, a, "true")
        i <- i + 1
      }
    } else {
      pos_args <- c(pos_args, a)
      i <- i + 1
    }
  }

  if (length(pos_args) < 7) {
    stop("Usage: Rscript 04_run_cbps_with_selected_controls.R <year> <selected_units_csv> <output_prefix> <train_start> <train_end> <test_start> <test_end>\nPositional arguments must be supplied; optional flags (e.g. --experiment-name) may appear anywhere.)")
  }

  pos <- pos_args[1:7]

  list(
    treated_year = as.integer(pos[1]),
    selected_units_path = pos[2],
    output_prefix = pos[3],
    train_start = as.integer(pos[4]),
    train_end = as.integer(pos[5]),
    test_start = as.integer(pos[6]),
    test_end = as.integer(pos[7]),
    experiment_name = parse_flag_value(opt, "--experiment-name", "full_pool"),
    output_experiment_name = parse_flag_value(opt, "--output-experiment-name", ""),
    analysis_base_dir = parse_flag_value(opt, "--analysis-base-dir", "data/processed_data/rev_analysis_low"),
    output_base_dir = parse_flag_value(opt, "--output-base-dir", "Embeddings/data/cbps_integration"),
    save_full_weights = parse_bool_flag(parse_flag_value(opt, "--save-full-weights", "false"), FALSE),
    use_cache = parse_bool_flag(parse_flag_value(opt, "--use-cache", "true"), TRUE),
    cache_max_items = parse_positive_int(parse_flag_value(opt, "--cache-max-items", "8"), "--cache-max-items", default = 8L, min_value = 1L),
    embedding_k = suppressWarnings(as.integer(parse_flag_value(opt, "--embedding-k", NA_character_))),
    rolling_windows_json = parse_flag_value(opt, "--rolling-windows-json", NULL)
  )
}

validate_args <- function(args) {
  #' Validate parsed arguments for logical consistency.
  int_fields <- c("treated_year", "train_start", "train_end", "test_start", "test_end")
  for (nm in int_fields) {
    val <- args[[nm]]
    if (is.null(val) || is.na(val)) {
      stop(paste0("Invalid integer argument: ", nm))
    }
  }
  if (args$treated_year <= 0) stop("treated_year must be positive")
  if (args$train_start > args$train_end) stop("train_start must be <= train_end")
  if (args$test_start > args$test_end) stop("test_start must be <= test_end")
  if (args$train_end >= args$test_start) {
    stop("Expected non-overlapping windows: train_end must be < test_start")
  }
  if (args$test_end >= args$treated_year) {
    stop("Expected pre-treatment evaluation only: test_end must be < treated_year")
  }
  if (args$train_end >= args$treated_year) {
    stop("Expected pre-treatment evaluation only: train_end must be < treated_year")
  }
}

read_rolling_windows <- function(path) {
  #' Read rolling windows configuration from a JSON file, if provided.
  if (is.null(path) || !nzchar(path)) return(NULL)
  if (!file.exists(path)) stop(paste("Rolling windows file not found:", path))
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required when --rolling-windows-json is used")
  }
  obj <- jsonlite::fromJSON(path)
  if (is.data.frame(obj)) return(obj)
  if (is.list(obj)) return(obj)
  stop("Rolling windows JSON must decode to a list or data.frame")
}

read_selected_units <- function(path) {
  #' Read selected units from a CSV file and validate required columns.
  if (!file.exists(path)) stop(paste("Selected units file not found:", path))
  selected_units <- read.csv(path, stringsAsFactors = FALSE)
  if (nrow(selected_units) == 0) {
    stop("Selected units file is empty")
  }
  allowed_cols <- c("unit", "Unit", "units", "unit_id", "unitID", "pixel", "pixel_id", "id")
  if (length(intersect(allowed_cols, names(selected_units))) == 0) {
    stop(paste("Selected units CSV must contain one of:", paste(allowed_cols, collapse = ", ")))
  }
  selected_units
}

main <- function() {
  #' Main entry point for running CBPS with selected controls.
  args <- parse_args()
  validate_args(args)

  message("Running CBPS with filtered control pool")
  message("Treatment year: ", args$treated_year)
  message("Output prefix: ", args$output_prefix)
  message("Experiment: ", args$experiment_name)
  if (!is.null(args$output_experiment_name) && nzchar(args$output_experiment_name)) {
    message("Output namespace: ", args$output_experiment_name)
  } else {
    message("Output namespace: <none> (flat year directory under Embeddings/data/cbps_integration)")
  }
  if (!is.null(args$rolling_windows_json) && nzchar(args$rolling_windows_json)) {
    message("Rolling windows: ", args$rolling_windows_json)
  }
  message("Use cache: ", args$use_cache, " (max items: ", args$cache_max_items, ")")

  selected_units <- tryCatch(
    read_selected_units(args$selected_units_path),
    error = function(e) {
      msg <- tryCatch(conditionMessage(e), error = function(...) "")
      if (is.null(msg) || !nzchar(msg)) msg <- "<empty error message>"
      stop(paste0("[read_selected_units] ", msg))
    }
  )

  rolling_windows <- tryCatch(
    read_rolling_windows(args$rolling_windows_json),
    error = function(e) {
      msg <- tryCatch(conditionMessage(e), error = function(...) "")
      if (is.null(msg) || !nzchar(msg)) msg <- "<empty error message>"
      stop(paste0("[read_rolling_windows] ", msg))
    }
  )

  result <- tryCatch(
    run_cbps_filtered(
      selected_units = selected_units,
      treated_year = args$treated_year,
      train_start = args$train_start,
      train_end = args$train_end,
      test_start = args$test_start,
      test_end = args$test_end,
      rolling_windows = rolling_windows,
      output_prefix = args$output_prefix,
      experiment_name = args$experiment_name,
      analysis_base_dir = args$analysis_base_dir,
      output_base_dir = args$output_base_dir,
      output_experiment_name = args$output_experiment_name,
      save_full_weights = args$save_full_weights,
      embedding_k = args$embedding_k,
      firms_data = NULL,
      use_cache = args$use_cache,
      cache_max_items = args$cache_max_items
    ),
    error = function(e) {
      msg <- tryCatch(conditionMessage(e), error = function(...) "")
      if (is.null(msg) || !nzchar(msg)) msg <- "<empty error message>"
      stop(paste0("[run_cbps_filtered] ", msg))
    }
  )

  saved <- tryCatch(
    save_cbps_filtered_outputs(result, save_full_weights = args$save_full_weights),
    error = function(e) {
      msg <- tryCatch(conditionMessage(e), error = function(...) "")
      if (is.null(msg) || !nzchar(msg)) msg <- "<empty error message>"
      stop(paste0("[save_cbps_filtered_outputs] ", msg))
    }
  )

  # --- Additional: Save RDS weights to experiment-specific subfolder ---
  weights_csv <- saved$weights_path
  year <- args$treated_year
  exp_name <- args$experiment_name
  rds_dir <- file.path("data/processed_data/rev_analysis_low", exp_name)
  dir.create(rds_dir, recursive = TRUE, showWarnings = FALSE)
  rds_path <- file.path(rds_dir, sprintf("cbps_weights_%s_conifer.RDS", year))
  w <- tryCatch(read.csv(weights_csv, stringsAsFactors=FALSE), error=function(e) NULL)
  if (!is.null(w)) {
    saveRDS(w, rds_path)
    message("Saved experiment weights RDS: ", rds_path)
  } else {
    message("WARNING: Could not read weights CSV for RDS export")
  }

  # Optionally, only save to top-level if experiment_name is 'full_pool' and user sets an explicit flag
  if (!is.null(exp_name) && exp_name == "full_pool" && !is.null(Sys.getenv("CBPS_OVERWRITE_TOPLEVEL", unset=NA))) {
    top_rds <- file.path("data/processed_data/rev_analysis_low", sprintf("cbps_weights_%s_conifer.RDS", year))
    saveRDS(w, top_rds)
    message("(Explicit) Overwrote top-level weights RDS: ", top_rds)
  }

  message("Saved metrics: ", saved$metrics_path)
  if (!is.null(saved$window_metrics_path) && !is.na(saved$window_metrics_path)) {
    message("Saved window metrics: ", saved$window_metrics_path)
  }
  message("Saved weights: ", saved$weights_path)
  if (!is.na(saved$weights_full_path)) {
    message("Saved full weights: ", saved$weights_full_path)
  }
  message("CBPS filtered run completed successfully")
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      msg <- tryCatch(conditionMessage(e), error = function(...) "")
      if (is.null(msg) || !nzchar(msg)) {
        detail <- paste(capture.output(str(e)), collapse = " | ")
        if (!is.null(detail) && nzchar(detail)) {
          msg <- paste0("<empty error message>; condition str: ", detail)
        } else {
          msg <- "<empty error message>"
        }
      }
      message("FATAL: ", msg)

      calls <- sys.calls()
      if (length(calls) > 0) {
        message("Call stack (most recent last):")
        start_idx <- max(1, length(calls) - 8)
        for (i in seq.int(start_idx, length(calls))) {
          message("  [", i, "] ", deparse(calls[[i]], nlines = 1))
        }
      }

      tb <- capture.output(traceback(2))
      if (length(tb) > 0) {
        message("Traceback:")
        message(paste(tb, collapse = "\n"))
      }

      message("Condition class: ", paste(class(e), collapse = ","))
      quit(status = 2)
    }
  )
}
