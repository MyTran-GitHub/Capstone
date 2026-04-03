#!/usr/bin/env Rscript

source("balancing/cli_utils.R")
source("balancing/run_cbps_filtered.R")
parse_flag_value <- get("parse_flag_value", mode = "function")
parse_bool_flag <- get("parse_bool_flag", mode = "function")
parse_positive_int <- get("parse_positive_int", mode = "function")
run_cbps_filtered <- get("run_cbps_filtered", mode = "function")
save_cbps_filtered_outputs <- get("save_cbps_filtered_outputs", mode = "function")

parse_args <- function() {
  raw_args <- commandArgs(trailingOnly = TRUE)
  if (length(raw_args) < 7) {
    stop("Usage: Rscript 04_run_cbps_with_selected_controls.R <year> <selected_units_csv> <output_prefix> <train_start> <train_end> <test_start> <test_end>")
  }

  pos <- raw_args[1:7]
  opt <- if (length(raw_args) > 7) raw_args[8:length(raw_args)] else character(0)

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
}

read_rolling_windows <- function(path) {
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
  args <- parse_args()
  validate_args(args)

  cat("Running CBPS with filtered control pool\n")
  cat("Treatment year:", args$treated_year, "\n")
  cat("Output prefix:", args$output_prefix, "\n")
  cat("Experiment:", args$experiment_name, "\n")
  if (!is.null(args$output_experiment_name) && nzchar(args$output_experiment_name)) {
    cat("Output namespace:", args$output_experiment_name, "\n")
  } else {
    cat("Output namespace: <none> (flat year directory under Embeddings/data/cbps_integration)\n")
  }
  if (!is.null(args$rolling_windows_json) && nzchar(args$rolling_windows_json)) {
    cat("Rolling windows:", args$rolling_windows_json, "\n")
  }
  cat("Use cache:", args$use_cache, "(max items:", args$cache_max_items, ")\n")

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

  cat("Saved metrics:", saved$metrics_path, "\n")
  if (!is.null(saved$window_metrics_path) && !is.na(saved$window_metrics_path)) {
    cat("Saved window metrics:", saved$window_metrics_path, "\n")
  }
  cat("Saved weights:", saved$weights_path, "\n")
  if (!is.na(saved$weights_full_path)) {
    cat("Saved full weights:", saved$weights_full_path, "\n")
  }
  cat("CBPS filtered run completed successfully\n")
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
      cat("FATAL:", msg, "\n")

      calls <- sys.calls()
      if (length(calls) > 0) {
        cat("Call stack (most recent last):\n")
        start_idx <- max(1, length(calls) - 8)
        for (i in seq.int(start_idx, length(calls))) {
          cat("  [", i, "] ", deparse(calls[[i]], nlines = 1), "\n", sep = "")
        }
      }

      tb <- capture.output(traceback(2))
      if (length(tb) > 0) {
        cat("Traceback:\n")
        cat(paste(tb, collapse = "\n"), "\n")
      }

      cat("Condition class:", paste(class(e), collapse = ","), "\n")
      quit(status = 2)
    }
  )
}
