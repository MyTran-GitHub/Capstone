source("balancing/cbps_ATT.R")
source("balancing/cbps_lambda_utils.R")
source("balancing/calculate_fire_outcomes.R")
source("balancing/balancing_config.R")
source("balancing/prepare_cbps_design.R")
source("balancing/cli_utils.R")

# -----------------------------------------------------------------------------
# Imported symbols
# -----------------------------------------------------------------------------
cbps_att <- get("cbps_att", mode = "function")
make_lambda_grid <- get("make_lambda_grid", mode = "function")
make_full_lambda_grid <- get("make_full_lambda_grid", mode = "function")
make_very_coarse_lambda_grid <- get("make_very_coarse_lambda_grid", mode = "function")
make_refined_lambda_grid <- get("make_refined_lambda_grid", mode = "function")
compute_weights_metrics <- get("compute_weights_metrics", mode = "function")
run_lambda_selection <- get("run_lambda_selection", mode = "function")
annotate_lambda_gate_diagnostics <- get("annotate_lambda_gate_diagnostics", mode = "function")
calculate_pretreatment_rmse <- get("calculate_pretreatment_rmse", mode = "function")
calculate_fire_frequency <- get("calculate_fire_frequency", mode = "function")
get_diagnostics_config <- get("get_diagnostics_config", mode = "function")
prepare_cbps_design <- get("prepare_cbps_design", mode = "function")
resolve_experiment_dir <- get("resolve_experiment_dir", mode = "function")

# -----------------------------------------------------------------------------
# In-memory cache utilities
# -----------------------------------------------------------------------------
.cbps_filtered_cache <- new.env(parent = emptyenv())
.cache_keys_name <- ".__keys__"
.cache_max_items_default <- 8L

clear_cbps_filtered_cache <- function() {
  rm(list = ls(envir = .cbps_filtered_cache, all.names = TRUE), envir = .cbps_filtered_cache)
  invisible(NULL)
}

normalize_cache_path <- function(path_like) {
  normalizePath(path_like, winslash = "/", mustWork = FALSE)
}

get_cache_keys <- function() {
  if (!exists(.cache_keys_name, envir = .cbps_filtered_cache, inherits = FALSE)) {
    assign(.cache_keys_name, character(0), envir = .cbps_filtered_cache)
  }
  get(.cache_keys_name, envir = .cbps_filtered_cache, inherits = FALSE)
}

set_cache_keys <- function(keys) {
  assign(.cache_keys_name, as.character(keys), envir = .cbps_filtered_cache)
}

cache_get <- function(cache_key) {
  if (!exists(cache_key, envir = .cbps_filtered_cache, inherits = FALSE)) return(NULL)
  get(cache_key, envir = .cbps_filtered_cache, inherits = FALSE)
}

cache_put <- function(cache_key, value, max_items = .cache_max_items_default) {
  max_items <- as.integer(max_items)
  if (!is.finite(max_items) || max_items <= 0) max_items <- .cache_max_items_default

  assign(cache_key, value, envir = .cbps_filtered_cache)
  keys <- get_cache_keys()
  keys <- c(keys[keys != cache_key], cache_key)

  while (length(keys) > max_items) {
    evict_key <- keys[1]
    keys <- keys[-1]
    if (exists(evict_key, envir = .cbps_filtered_cache, inherits = FALSE)) {
      rm(list = evict_key, envir = .cbps_filtered_cache)
    }
  }
  set_cache_keys(keys)
  invisible(NULL)
}

resolve_output_dir <- function(output_base_dir, treated_year, output_experiment_name = NULL) {
  if (is.null(output_experiment_name) || !nzchar(trimws(output_experiment_name))) {
    return(file.path(output_base_dir, as.character(treated_year)))
  }
  output_scope <- resolve_experiment_dir(output_base_dir, output_experiment_name)
  file.path(output_scope, as.character(treated_year))
}

build_unit_key <- function(lat, lon, digits = 6L) {
  sprintf(paste0("%.", as.integer(digits), "f|%.", as.integer(digits), "f"), as.numeric(lat), as.numeric(lon))
}

# -----------------------------------------------------------------------------
# Analysis input loading and normalization
# -----------------------------------------------------------------------------
read_analysis_data <- function(treated_year, analysis_base_dir, experiment_name) {
  analysis_dir <- resolve_experiment_dir(analysis_base_dir, experiment_name)
  candidate_dirs <- unique(c(analysis_dir, analysis_base_dir))

  input_csv <- NA_character_
  input_rds <- NA_character_
  for (dir_path in candidate_dirs) {
    csv_path <- file.path(dir_path, paste0("analysis_treated", treated_year, "_conifer.csv"))
    rds_path <- file.path(dir_path, paste0("analysis_treated", treated_year, "_conifer.RDS"))
    if (file.exists(csv_path) || file.exists(rds_path)) {
      input_csv <- csv_path
      input_rds <- rds_path
      break
    }
  }

  if (is.na(input_csv) || is.na(input_rds)) {
    searched <- vapply(
      candidate_dirs,
      function(dir_path) {
        paste0(
          file.path(dir_path, paste0("analysis_treated", treated_year, "_conifer.csv")),
          " and ",
          file.path(dir_path, paste0("analysis_treated", treated_year, "_conifer.RDS"))
        )
      },
      character(1)
    )
    stop(paste("Input covariate data not found. Looked for", paste(searched, collapse = "; ")))
  }

  if (file.exists(input_csv)) {
    df <- tryCatch({
      if (requireNamespace("data.table", quietly = TRUE)) {
        data.table::fread(input_csv, showProgress = FALSE, data.table = FALSE)
      } else {
        read.csv(input_csv, stringsAsFactors = FALSE)
      }
    }, error = function(e) {
      stop(paste("Failed to read CSV:", input_csv, "-", e$message))
    })
  } else if (file.exists(input_rds)) {
    df <- tryCatch(readRDS(input_rds), error = function(e) stop(paste("Failed to read RDS:", input_rds, "-", e$message)))
    if (inherits(df, "sf")) {
      df <- as.data.frame(sf::st_drop_geometry(df))
    } else if (inherits(df, "Spatial")) {
      df <- tryCatch(as.data.frame(df), error = function(e) stop("Unsupported spatial object in analysis data"))
    }
  } else {
    stop(paste("Input covariate data not found. Looked for", input_csv, "and", input_rds))
  }

  if (!"treated" %in% names(df)) {
    alt_names <- c("treat", "TREATED", "is_treated", "treated_flag", "treated1")
    found <- intersect(alt_names, names(df))
    if (length(found) >= 1) {
      df$treated <- df[[found[1]]]
      warning(sprintf("Mapped alternative treatment column '%s' to 'treated'", found[1]))
    }
  }

  if (!"unit" %in% names(df)) stop("Input data missing 'unit' column")
  if (!"treated" %in% names(df)) stop("Input data missing 'treated' column")

  treated_vals <- unique(as.integer(df$treated))
  treated_vals <- treated_vals[!is.na(treated_vals)]
  if (length(setdiff(treated_vals, c(0L, 1L))) > 0) {
    stop("Input data has invalid treated values; expected only 0/1")
  }

  if (any(is.na(df$unit) | !nzchar(as.character(df$unit)))) {
    stop("Input data contains missing/empty unit identifiers")
  }
  if (anyDuplicated(as.character(df$unit)) > 0) {
    stop("Input data contains duplicate unit identifiers")
  }

  df
}

# -----------------------------------------------------------------------------
# Cohort subset preparation
# -----------------------------------------------------------------------------
normalize_selected_units <- function(selected_units, df_full) {
  if (is.data.frame(selected_units)) {
    alt_unit_names <- c("unit", "Unit", "units", "unit_id", "unitID", "pixel", "pixel_id", "id")
    found <- intersect(alt_unit_names, names(selected_units))
    if (length(found) == 0) {
      stop(sprintf("selected_units data.frame must contain one of: %s", paste(alt_unit_names, collapse = ", ")))
    }
    units <- selected_units[[found[1]]]
  } else {
    units <- selected_units
  }

  units <- unique(as.character(units))
  units <- units[!is.na(units) & nzchar(units)]
  if (length(units) == 0) stop("selected_units is empty after normalization")

  missing_units <- setdiff(units, unique(as.character(df_full$unit)))
  if (length(missing_units) > 0) {
    stop(sprintf("selected_units includes %d units not found in analysis data", length(missing_units)))
  }

  treated_units <- unique(as.character(df_full$unit[df_full$treated == 1]))
  overlap <- intersect(treated_units, units)
  if (length(overlap) > 0) {
    stop(sprintf("Selected controls contain treated units (n=%d). Example: %s", length(overlap), paste(head(overlap, 5), collapse = ", ")))
  }

  units
}

get_analysis_data_cached <- function(treated_year,
                                     analysis_base_dir,
                                     experiment_name,
                                     use_cache = TRUE,
                                     max_cache_items = .cache_max_items_default) {
  cache_key <- paste("analysis", treated_year, normalize_cache_path(analysis_base_dir), as.character(experiment_name), sep = "::")
  if (isTRUE(use_cache)) {
    cached <- cache_get(cache_key)
    if (!is.null(cached)) return(cached)
  }

  df_full <- read_analysis_data(treated_year, analysis_base_dir, experiment_name)
  if (isTRUE(use_cache)) cache_put(cache_key, df_full, max_items = max_cache_items)
  df_full
}

subset_design <- function(df_full, selected_units, preprocess_opts = list()) {
  unit_idx <- as.character(df_full$unit)
  W_full <- as.numeric(df_full$treated)

  rows <- which(unit_idx %in% selected_units | W_full == 1)
  if (length(rows) == 0) stop("No rows remain after filtering by selected units and treated units")

  df_sub <- df_full[rows, , drop = FALSE]
  if (anyDuplicated(as.character(df_sub$unit)) > 0) {
    stop("Filtered design has duplicate unit identifiers")
  }
  prep <- prepare_cbps_design(df_sub, opts = preprocess_opts)
  X_sub <- prep$X.scl
  W_sub <- as.numeric(prep$W)

  n_control <- sum(W_sub == 0, na.rm = TRUE)
  if (n_control <= 0) stop("No control units remain after filtering")

  list(rows = rows, X_sub = X_sub, W_sub = W_sub, df_sub = df_sub, n_covariates = ncol(X_sub))
}

# -----------------------------------------------------------------------------
# FIRMS loading and normalization
# -----------------------------------------------------------------------------
get_firms_data_cached <- function(firms_data = NULL,
                                  firms_rds_path = "data/processed_data/FIRMS.RDS",
                                  use_cache = TRUE,
                                  max_cache_items = .cache_max_items_default) {
  if (!is.null(firms_data)) return(firms_data)
  cache_key <- paste("firms", normalize_cache_path(firms_rds_path), sep = "::")
  if (isTRUE(use_cache)) {
    cached <- cache_get(cache_key)
    if (!is.null(cached)) return(cached)
  }
  if (!file.exists(firms_rds_path)) return(NULL)
  loaded <- tryCatch(readRDS(firms_rds_path), error = function(e) stop(paste("Failed to read FIRMS RDS:", firms_rds_path, "-", e$message)))
  if (isTRUE(use_cache)) cache_put(cache_key, loaded, max_items = max_cache_items)
  loaded
}

# -----------------------------------------------------------------------------
# Lambda search and fit selection
# -----------------------------------------------------------------------------
run_cbps_grid_search <- function(X_sub, W_sub) {
  cfg <- get_diagnostics_config()
  n_ctrl <- sum(W_sub == 0)
  n_treated <- sum(W_sub == 1)

  lambda_grid <- make_very_coarse_lambda_grid()
  candidates <- list()
  fit_by_lambda <- list()
  theta_start <- rep(0, ncol(X_sub) + 1)

  lambda_key <- function(x) sprintf("%.12g", as.numeric(x))
  try_lambda <- function(lam, stage_label) {
    key <- lambda_key(lam)
    if (key %in% names(fit_by_lambda)) return(invisible(NULL))

    lam_vec <- rep(lam, ncol(X_sub))
    res_try <- tryCatch(
      cbps_att(as.matrix(X_sub),
               W_sub,
               theta.init = theta_start,
               control = list(trace = 0, maxit = 6000),
               lambda = lam_vec),
      error = function(e) NULL
    )
    if (is.null(res_try) || is.null(res_try$convergence) || res_try$convergence != 0) return(invisible(NULL))

    metrics <- compute_weights_metrics(res_try, W_sub)
    if (is.null(metrics)) return(invisible(NULL))

    candidates[[length(candidates) + 1]] <<- data.frame(
      lambda = as.numeric(lam),
      ess = metrics$ess,
      top10_share = metrics$top10_share,
      max_weight = metrics$max_weight,
      max_smd = metrics$max_smd,
      median_smd = if (!is.null(metrics$median_smd)) metrics$median_smd else NA_real_,
      converged = TRUE,
      stage = stage_label,
      stringsAsFactors = FALSE
    )
    fit_by_lambda[[key]] <<- res_try

    if (!is.null(res_try$theta.hat) && length(res_try$theta.hat) == length(theta_start)) {
      theta_start <<- res_try$theta.hat
    }

    invisible(NULL)
  }

  for (lam in lambda_grid) try_lambda(lam, "stage1")
  cand_stage1 <- if (length(candidates) > 0) do.call(rbind, candidates) else data.frame()
  if (nrow(cand_stage1) == 0) {
    stop("No converged candidates in stage-1 lambda grid")
  }

  selection_stage1 <- tryCatch(
    run_lambda_selection(cand_stage1, n_ctrl, n_treated = n_treated),
    error = function(e) e
  )
  if (!inherits(selection_stage1, "error")) {
    rho_stage1 <- as.numeric(selection_stage1$selected_row$lambda)
    upper_candidates <- sort(unique(cand_stage1$lambda[cand_stage1$lambda > rho_stage1]))
    lower_candidates <- sort(unique(cand_stage1$lambda[cand_stage1$lambda < rho_stage1]), decreasing = TRUE)
    upper_neighbor <- if (length(upper_candidates) > 0) upper_candidates[1] else NA_real_
    lower_neighbor <- if (length(lower_candidates) > 0) lower_candidates[1] else NA_real_

    refine_lower <- rho_stage1
    refine_upper <- upper_neighbor
    if (!is.finite(refine_upper)) {
      refine_lower <- lower_neighbor
      refine_upper <- rho_stage1
    }

    refine_grid <- make_refined_lambda_grid(
      lower_lambda = refine_lower,
      upper_lambda = refine_upper,
      n_inner = 6
    )
    if (length(refine_grid) > 0) {
      for (lam in refine_grid) try_lambda(lam, "stage2")
    }
  }

  cand_df <- if (length(candidates) > 0) do.call(rbind, candidates) else data.frame()
  cand_df <- cand_df[order(cand_df$lambda, decreasing = TRUE), , drop = FALSE]

  selection_result <- tryCatch(
    run_lambda_selection(cand_df, n_ctrl, n_treated = n_treated),
    error = function(e) e
  )
  if (inherits(selection_result, "error")) {
    min_lambda <- min(cand_df$lambda, na.rm = TRUE)
    max_lambda <- max(cand_df$lambda, na.rm = TRUE)
    fallback_grid <- sort(unique(c(make_full_lambda_grid(), 1e-5, 1e-6, 0.3, 1, 3, 10)), decreasing = TRUE)
    fallback_grid <- fallback_grid[fallback_grid < min_lambda | fallback_grid > max_lambda]

    if (length(fallback_grid) > 0) {
      for (lam in fallback_grid) try_lambda(lam, "fallback")
      cand_df <- if (length(candidates) > 0) do.call(rbind, candidates) else data.frame()
      cand_df <- cand_df[order(cand_df$lambda, decreasing = TRUE), , drop = FALSE]
    }

    selection_result <- tryCatch(
      run_lambda_selection(cand_df, n_ctrl, n_treated = n_treated),
      error = function(e) e
    )
  }

  if (inherits(selection_result, "error")) {
    stop(paste("Lambda selection failed:", selection_result$message))
  }

  chosen_row <- selection_result$selected_row
  selection_log <- selection_result$selection_log
  rho <- chosen_row$lambda
  res <- fit_by_lambda[[lambda_key(rho)]]
  if (is.null(res)) {
    stop("Selected lambda was not found among fitted candidates")
  }

  chosen_tol <- max(.Machine$double.eps * 10, abs(rho) * 1e-8)
  cand_df <- annotate_lambda_gate_diagnostics(cand_df, n_ctrl = n_ctrl, n_treated = n_treated, cfg = cfg)
  cand_df$chosen <- abs(cand_df$lambda - rho) <= chosen_tol
  cand_df$ess_ratio <- cand_df$ess / n_ctrl
  cand_df$tier_used <- selection_log$tier_used
  cand_df$gate_used <- selection_log$gate_used
  cand_df$required_ess_floor <- as.numeric(selection_log$required_ess_floor)
  cand_df$selected_is_emergency <- identical(selection_log$gate_used, "emergency")

  attr(cand_df, "selection_policy") <- cfg$lambda_selection
  attr(cand_df, "selection_context") <- list(
    n_control = n_ctrl,
    n_treated = n_treated,
    selected_lambda = as.numeric(rho),
    selected_gate = selection_log$gate_used,
    selected_tier = selection_log$tier_used,
    required_ess_floor = as.numeric(selection_log$required_ess_floor)
  )

  list(
    best_fit = list(
      lambda = as.numeric(rho),
      res = res,
      max_balance_std = if (!is.null(res$balance.std)) max(abs(res$balance.std), na.rm = TRUE) else NA_real_,
      mean_balance_std = if (!is.null(res$balance.std)) mean(abs(res$balance.std), na.rm = TRUE) else NA_real_
    ),
    candidate_table = cand_df
  )
}

# -----------------------------------------------------------------------------
# Weight construction and diagnostics
# -----------------------------------------------------------------------------
build_weights_df <- function(df_sub, W_sub, res) {
  weights <- rep(NA_real_, nrow(df_sub))

  if (length(res$weights.0) == sum(W_sub == 0)) {
    weights[W_sub == 0] <- res$weights.0
  } else {
    weights[W_sub == 0] <- res$weights.0[W_sub == 0]
  }

  if (length(res$weights.1) == sum(W_sub == 1)) {
    weights[W_sub == 1] <- res$weights.1
  } else {
    weights[W_sub == 1] <- res$weights.1[W_sub == 1]
  }

  data.frame(
    unit = df_sub$unit,
    LATITUDE = df_sub$LATITUDE,
    LONGITUDE = df_sub$LONGITUDE,
    treated = W_sub,
    weight = weights,
    stringsAsFactors = FALSE
  )
}

compute_weight_diagnostics <- function(weights_df, W_sub, hard_gates) {
  ctrl_w <- weights_df$weight[weights_df$treated == 0]
  n_controls <- length(ctrl_w)
  n_treated <- sum(W_sub == 1)

  if (any(is.na(ctrl_w))) stop("Weights contain NA values in control units")
  if (any(!is.finite(ctrl_w))) stop("Weights contain non-finite values in control units")

  control_weight_sum <- sum(ctrl_w, na.rm = TRUE)
  ess <- if (control_weight_sum > 0) (control_weight_sum^2) / sum(ctrl_w^2, na.rm = TRUE) else 0
  topN <- max(1, ceiling(0.10 * n_controls))
  top10_share <- if (control_weight_sum > 0) sum(sort(ctrl_w, decreasing = TRUE)[1:topN], na.rm = TRUE) / control_weight_sum else NA_real_
  max_weight_share <- if (control_weight_sum > 0) max(ctrl_w, na.rm = TRUE) / control_weight_sum else NA_real_

  ess_floor <- max(
    as.numeric(hard_gates$ess_frac) * n_controls,
    as.numeric(hard_gates$ess_mult_treated) * n_treated
  )
  if (!is.finite(ess) || ess < ess_floor) {
    stop(sprintf("ESS below threshold: ESS=%.3f < required=%.3f", ess, ess_floor))
  }

  if (n_treated > 0) {
    rel_gap <- abs(control_weight_sum - n_treated) / n_treated
    if (is.finite(rel_gap) && rel_gap > 0.10) {
      warning(sprintf("Control weight sum differs from treated count by %.1f%%", 100 * rel_gap))
    }
  }

  list(
    ess = ess,
    ess_ratio = if (n_treated > 0) ess / n_treated else NA_real_,
    top10_share = top10_share,
    max_weight_share = max_weight_share,
    control_weight_sum = control_weight_sum,
    treated_count = n_treated
  )
}

# -----------------------------------------------------------------------------
# Error and debug artifact helpers
# -----------------------------------------------------------------------------
normalize_firms_frame <- function(firms_data) {
  fire_base <- firms_data
  if (inherits(fire_base, "sf")) {
    if (requireNamespace("sf", quietly = TRUE)) {
      coords_try <- try(sf::st_coordinates(fire_base), silent = TRUE)
      if (!inherits(coords_try, "try-error") && is.matrix(coords_try) && ncol(coords_try) >= 2) {
        if (!"LONGITUDE" %in% colnames(fire_base)) fire_base$LONGITUDE <- coords_try[, 1]
        if (!"LATITUDE" %in% colnames(fire_base)) fire_base$LATITUDE <- coords_try[, 2]
      }
      fire_base <- sf::st_drop_geometry(fire_base)
    } else {
      fire_base <- as.data.frame(fire_base)
    }
  }

  is_sfc_like <- sapply(
    fire_base,
    function(col) inherits(col, "sfc") || any(grepl("sfc", class(col), fixed = TRUE)) || is.list(col)
  )
  if (any(is_sfc_like)) {
    fire_base <- fire_base[, !is_sfc_like, drop = FALSE]
  }

  fire_base
}

safe_condition_message <- function(e) {
  msg <- tryCatch(conditionMessage(e), error = function(...) "")
  if (is.null(msg) || !nzchar(msg)) "<empty error message>" else msg
}

persist_rmse_failure_context <- function(output_base_dir,
                                         treated_year,
                                         output_experiment_name,
                                         output_prefix,
                                         experiment_name,
                                         analysis_base_dir,
                                         firms_rds_path,
                                         weights_df,
                                         windows_df,
                                         error_message) {
  debug_dir <- file.path(resolve_output_dir(output_base_dir, treated_year, output_experiment_name), "debug_failures")
  dir.create(debug_dir, recursive = TRUE, showWarnings = FALSE)

  stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
  dbg_prefix <- paste0(output_prefix, "_", stamp)

  weights_rds <- file.path(debug_dir, paste0("weights_df_", dbg_prefix, ".rds"))
  windows_rds <- file.path(debug_dir, paste0("windows_df_", dbg_prefix, ".rds"))
  context_txt <- file.path(debug_dir, paste0("rmse_failure_context_", dbg_prefix, ".txt"))

  try(saveRDS(weights_df, weights_rds), silent = TRUE)
  try(saveRDS(windows_df, windows_rds), silent = TRUE)

  context_lines <- c(
    paste0("treated_year=", treated_year),
    paste0("output_prefix=", output_prefix),
    paste0("experiment_name=", experiment_name),
    paste0("analysis_base_dir=", analysis_base_dir),
    paste0("firms_rds_path=", firms_rds_path),
    paste0("error_message=", error_message),
    paste0("weights_df_columns=", paste(names(weights_df), collapse = ",")),
    paste0("weights_df_nrow=", nrow(weights_df)),
    paste0("windows_df_columns=", paste(names(windows_df), collapse = ",")),
    paste0("windows_df_nrow=", nrow(windows_df)),
    "weights_df_head:",
    paste(capture.output(utils::head(weights_df, 5)), collapse = "\n"),
    "windows_df:",
    paste(capture.output(print(windows_df)), collapse = "\n")
  )
  try(writeLines(context_lines, con = context_txt), silent = TRUE)

  list(debug_dir = debug_dir, context_file = context_txt)
}

# -----------------------------------------------------------------------------
# RMSE cache construction and windowed metrics
# -----------------------------------------------------------------------------
collect_window_years <- function(windows_df) {
  years <- integer(0)
  for (i in seq_len(nrow(windows_df))) {
    w <- windows_df[i, , drop = FALSE]
    years <- c(
      years,
      seq.int(as.integer(w$train_start), as.integer(w$train_end)),
      seq.int(as.integer(w$test_start), as.integer(w$test_end))
    )
  }
  sort(unique(as.integer(years)))
}

build_fire_outcome_cache <- function(weights_df,
                                     windows_df,
                                     firms_data = NULL,
                                     firms_rds_path = "data/processed_data/FIRMS.RDS") {
  make_match_key <- function(df, unit_col = "unit") {
    out <- rep(NA_character_, nrow(df))
    has_latlon <- ('LATITUDE' %in% names(df)) && ('LONGITUDE' %in% names(df))
    if (has_latlon) {
      lat_ok <- !is.na(df$LATITUDE)
      lon_ok <- !is.na(df$LONGITUDE)
      idx <- which(lat_ok & lon_ok)
      if (length(idx) > 0) {
        out[idx] <- paste0(as.character(df$LATITUDE[idx]), as.character(df$LONGITUDE[idx]))
      }
    }
    if (unit_col %in% names(df)) {
      unit_raw <- as.character(df[[unit_col]])
      fill_idx <- which(is.na(out) | !nzchar(out))
      if (length(fill_idx) > 0) {
        out[fill_idx] <- unit_raw[fill_idx]
      }
    }
    out
  }

  all_years <- collect_window_years(windows_df)
  if (length(all_years) == 0) {
    stop("No years available in rolling windows for RMSE cache")
  }

  fire_freq <- tryCatch(
    calculate_fire_frequency(
      weights_df = weights_df,
      firms_rds_path = firms_rds_path,
      years_to_include = all_years,
      firms_data = firms_data
    ),
    error = function(e) {
      msg <- tryCatch(conditionMessage(e), error = function(...) "")
      if (is.null(msg) || !nzchar(msg)) msg <- "<empty error message>"
      stop(
        paste0(
          "calculate_fire_frequency failed: ", msg,
          " | weights_df columns=", paste(names(weights_df), collapse = ","),
          " | n_weights=", nrow(weights_df),
          " | years=", paste(range(all_years), collapse = "-")
        )
      )
    }
  )

  required_fire_cols <- c("year", "treated", "hifire95.frac")
  missing_fire_cols <- setdiff(required_fire_cols, names(fire_freq))
  if (length(missing_fire_cols) > 0) {
    stop(
      paste0(
        "calculate_fire_frequency returned missing columns: ",
        paste(missing_fire_cols, collapse = ", "),
        " | returned columns=", paste(names(fire_freq), collapse = ",")
      )
    )
  }

  treated_fire <- fire_freq[fire_freq$treated == 1, c("year", "hifire95.frac"), drop = FALSE]
  names(treated_fire)[2] <- "treated_1"
  control_fire <- fire_freq[fire_freq$treated == 0, c("year", "hifire95.frac"), drop = FALSE]
  names(control_fire)[2] <- "treated_0"
  yearly_gap <- merge(treated_fire, control_fire, by = "year", all = TRUE, sort = FALSE)
  yearly_gap$gap <- yearly_gap$treated_1 - yearly_gap$treated_0

  if (!is.null(firms_data)) {
    fire_base <- firms_data
  } else {
    if (!file.exists(firms_rds_path)) stop(paste("FIRMS data not found:", firms_rds_path))
    fire_base <- readRDS(firms_rds_path)
  }
  fire_base <- normalize_firms_frame(fire_base)

  if (!('unit' %in% names(weights_df))) {
    if ('LATITUDE' %in% names(weights_df) && 'LONGITUDE' %in% names(weights_df)) {
      weights_df$unit <- paste0(as.character(weights_df$LATITUDE), as.character(weights_df$LONGITUDE))
    } else {
      stop("weights_df must contain unit or LATITUDE/LONGITUDE columns")
    }
  }
  weights_df$unit_match <- make_match_key(weights_df, unit_col = "unit")

  if (!('unit' %in% names(fire_base))) {
    if (!('LATITUDE' %in% names(fire_base) && 'LONGITUDE' %in% names(fire_base))) {
      stop("FIRMS data must contain either unit or LATITUDE/LONGITUDE columns")
    }
    fire_base$unit <- paste0(as.character(fire_base$LATITUDE), as.character(fire_base$LONGITUDE))
  } else {
    fire_base$unit <- as.character(fire_base$unit)
    missing_unit <- is.na(fire_base$unit) | !nzchar(fire_base$unit)
    if (any(missing_unit)) {
      if (!('LATITUDE' %in% names(fire_base) && 'LONGITUDE' %in% names(fire_base))) {
        stop("FIRMS data has missing unit values and lacks LATITUDE/LONGITUDE fallback")
      }
      fire_base$unit[missing_unit] <- paste0(
        as.character(fire_base$LATITUDE[missing_unit]),
        as.character(fire_base$LONGITUDE[missing_unit])
      )
    }
  }
  fire_base$unit_match <- make_match_key(fire_base, unit_col = "unit")
  fire_base$has.hifire95 <- 0L
  fire_base$has.hifire95[!is.na(fire_base$max_FRP) & fire_base$max_FRP >= 1000] <- 1L

  treated_units <- unique(weights_df$unit_match[weights_df$treated == 1])
  control_units <- unique(weights_df$unit_match[weights_df$treated == 0])
  treated_units <- treated_units[!is.na(treated_units) & nzchar(treated_units)]
  control_units <- control_units[!is.na(control_units) & nzchar(control_units)]

  overlap_units <- intersect(treated_units, control_units)
  if (length(overlap_units) > 0) {
    stop(sprintf("Treated/control unit overlap detected in weighted data (n=%d)", length(overlap_units)))
  }

  treated_panel <- data.frame()
  if (length(treated_units) > 0) {
    treated_panel <- expand.grid(year = all_years, unit = treated_units, stringsAsFactors = FALSE)

    treated_obs <- fire_base[
      fire_base$unit_match %in% treated_units & fire_base$year %in% all_years,
      c("year", "unit_match", "has.hifire95"),
      drop = FALSE
    ]
    names(treated_obs)[names(treated_obs) == "unit_match"] <- "unit"
    treated_panel <- merge(treated_panel, treated_obs, by = c("year", "unit"), all.x = TRUE, sort = FALSE)
    treated_panel$has.hifire95[is.na(treated_panel$has.hifire95)] <- 0

    control_obs <- fire_base[
      fire_base$unit_match %in% control_units & fire_base$year %in% all_years,
      c("year", "unit_match", "has.hifire95"),
      drop = FALSE
    ]
    names(control_obs)[names(control_obs) == "unit_match"] <- "unit"
    control_obs <- control_obs[!is.na(control_obs$year) & !is.na(control_obs$unit), , drop = FALSE]
    if (nrow(control_obs) == 0) {
      synth <- data.frame(year = all_years, synth_hifire95 = NA_real_, stringsAsFactors = FALSE)
    } else {
      control_w <- weights_df[weights_df$unit_match %in% control_units, c("unit_match", "weight"), drop = FALSE]
      names(control_w)[names(control_w) == "unit_match"] <- "unit"
      control_obs <- merge(control_obs, control_w, by = "unit", all.x = TRUE, sort = FALSE)
      control_obs$weight_hifire95 <- control_obs$weight * control_obs$has.hifire95

      synth <- tryCatch(
        aggregate(cbind(weight_hifire95, weight) ~ year, data = control_obs, FUN = sum, na.rm = TRUE),
        error = function(e) data.frame(year = integer(0), weight_hifire95 = numeric(0), weight = numeric(0))
      )
      if (nrow(synth) == 0) {
        synth <- data.frame(year = all_years, synth_hifire95 = NA_real_, stringsAsFactors = FALSE)
      } else {
        synth$synth_hifire95 <- ifelse(synth$weight > 0, synth$weight_hifire95 / synth$weight, NA_real_)
        synth <- synth[, c("year", "synth_hifire95"), drop = FALSE]
      }
    }

    treated_panel <- merge(treated_panel, synth, by = "year", all.x = TRUE, sort = FALSE)
    treated_panel$sq_err <- (treated_panel$has.hifire95 - treated_panel$synth_hifire95)^2
  }

  list(
    yearly_gap = yearly_gap,
    treated_panel = treated_panel
  )
}

compute_window_rmse_from_cache <- function(rmse_cache, train_start, train_end, test_start, test_end) {
  compute_group_metrics <- function(years_vec) {
    sub <- rmse_cache$yearly_gap[rmse_cache$yearly_gap$year %in% years_vec, , drop = FALSE]
    valid <- !is.na(sub$treated_1) & !is.na(sub$treated_0) & !is.na(sub$gap)
    n_used <- sum(valid)
    if (n_used == 0) {
      return(list(rmse = NA_real_, rmse_norm = NA_real_, n_used = 0L))
    }

    diffs <- sub$gap[valid]
    rmse <- sqrt(mean(diffs^2, na.rm = TRUE))
    mean_treated <- mean(sub$treated_1[valid], na.rm = TRUE)
    rmse_norm <- if (is.na(mean_treated) || mean_treated == 0) NA_real_ else rmse / abs(mean_treated)

    list(rmse = rmse, rmse_norm = rmse_norm, n_used = as.integer(n_used))
  }

  compute_unit_metrics <- function(years_vec) {
    if (is.null(rmse_cache$treated_panel) || nrow(rmse_cache$treated_panel) == 0) {
      return(list(median = NA_real_, p90 = NA_real_, maxv = NA_real_))
    }

    sub <- rmse_cache$treated_panel[rmse_cache$treated_panel$year %in% years_vec, c("unit", "sq_err"), drop = FALSE]
    sub <- sub[!is.na(sub$unit), , drop = FALSE]
    if (nrow(sub) == 0) {
      return(list(median = NA_real_, p90 = NA_real_, maxv = NA_real_))
    }

    rmse_i <- tryCatch(
      aggregate(
        sq_err ~ unit,
        data = sub,
        FUN = function(x) if (all(is.na(x))) NA_real_ else sqrt(mean(x, na.rm = TRUE))
      ),
      error = function(e) data.frame(unit = character(0), sq_err = numeric(0), stringsAsFactors = FALSE)
    )
    if (nrow(rmse_i) == 0) {
      return(list(median = NA_real_, p90 = NA_real_, maxv = NA_real_))
    }
    vals <- rmse_i$sq_err[is.finite(rmse_i$sq_err)]
    if (length(vals) == 0) {
      return(list(median = NA_real_, p90 = NA_real_, maxv = NA_real_))
    }

    list(
      median = as.numeric(stats::quantile(vals, probs = 0.50, na.rm = TRUE, names = FALSE)),
      p90 = as.numeric(stats::quantile(vals, probs = 0.90, na.rm = TRUE, names = FALSE)),
      maxv = as.numeric(max(vals, na.rm = TRUE))
    )
  }

  train_years <- seq.int(as.integer(train_start), as.integer(train_end))
  test_years <- seq.int(as.integer(test_start), as.integer(test_end))

  g_train <- compute_group_metrics(train_years)
  g_test <- compute_group_metrics(test_years)
  u_train <- compute_unit_metrics(train_years)
  u_test <- compute_unit_metrics(test_years)

  list(
    rmse_train = g_train$rmse,
    rmse_test = g_test$rmse,
    rmse_train_norm = g_train$rmse_norm,
    rmse_test_norm = g_test$rmse_norm,
    n_years_used_train = g_train$n_used,
    n_years_used_test = g_test$n_used,
    median_rmse_train = u_train$median,
    p90_rmse_train = u_train$p90,
    max_rmse_train = u_train$maxv,
    median_rmse_test = u_test$median,
    p90_rmse_test = u_test$p90,
    max_rmse_test = u_test$maxv
  )
}

normalize_rolling_windows <- function(rolling_windows,
                                      train_start,
                                      train_end,
                                      test_start,
                                      test_end,
                                      treated_year = NULL) {
  if (is.null(rolling_windows) || length(rolling_windows) == 0) {
    return(data.frame(
      window_id = "w1",
      train_start = as.integer(train_start),
      train_end = as.integer(train_end),
      test_start = as.integer(test_start),
      test_end = as.integer(test_end),
      stringsAsFactors = FALSE
    ))
  }

  if (is.data.frame(rolling_windows)) {
    win_df <- rolling_windows
  } else if (is.matrix(rolling_windows)) {
    win_df <- as.data.frame(rolling_windows, stringsAsFactors = FALSE)
  } else if (is.list(rolling_windows)) {
    rows <- lapply(seq_along(rolling_windows), function(i) {
      w <- rolling_windows[[i]]
      data.frame(
        window_id = if (!is.null(w$window_id)) as.character(w$window_id) else paste0("w", i),
        train_start = as.integer(w$train_start),
        train_end = as.integer(w$train_end),
        test_start = as.integer(w$test_start),
        test_end = as.integer(w$test_end),
        stringsAsFactors = FALSE
      )
    })
    win_df <- do.call(rbind, rows)
  } else {
    stop("rolling_windows must be NULL, data.frame, matrix, or list")
  }

  required_cols <- c("train_start", "train_end", "test_start", "test_end")
  missing_cols <- setdiff(required_cols, names(win_df))
  if (length(missing_cols) > 0) {
    stop(paste("rolling_windows missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  if (!"window_id" %in% names(win_df)) {
    win_df$window_id <- paste0("w", seq_len(nrow(win_df)))
  }

  win_df <- win_df[, c("window_id", "train_start", "train_end", "test_start", "test_end"), drop = FALSE]
  win_df$window_id <- as.character(win_df$window_id)
  for (col in required_cols) win_df[[col]] <- as.integer(win_df[[col]])

  if (any(win_df$train_end >= win_df$test_start, na.rm = TRUE)) {
    stop("Invalid rolling windows: train_end must be strictly before test_start")
  }
  if (!is.null(treated_year)) {
    if (any(win_df$test_end >= as.integer(treated_year), na.rm = TRUE)) {
      stop("Invalid rolling windows: test_end must be strictly before treated_year")
    }
    if (any(win_df$train_end >= as.integer(treated_year), na.rm = TRUE)) {
      stop("Invalid rolling windows: train_end must be strictly before treated_year")
    }
  }

  win_df
}

compute_rmse_windows <- function(weights_df,
                                 windows_df,
                                 firms_data = NULL,
                                 firms_rds_path = "data/processed_data/FIRMS.RDS") {
  rmse_cache <- build_fire_outcome_cache(
    weights_df = weights_df,
    windows_df = windows_df,
    firms_data = firms_data,
    firms_rds_path = firms_rds_path
  )

  rows <- lapply(seq_len(nrow(windows_df)), function(i) {
    w <- windows_df[i, , drop = FALSE]
    rmse_i <- compute_window_rmse_from_cache(
      rmse_cache = rmse_cache,
      train_start = w$train_start,
      train_end = w$train_end,
      test_start = w$test_start,
      test_end = w$test_end
    )

    data.frame(
      window_id = as.character(w$window_id),
      train_start = as.integer(w$train_start),
      train_end = as.integer(w$train_end),
      test_start = as.integer(w$test_start),
      test_end = as.integer(w$test_end),
      rmse_train = as.numeric(rmse_i$rmse_train),
      rmse_test = as.numeric(rmse_i$rmse_test),
      median_rmse_train = as.numeric(rmse_i$median_rmse_train),
      p90_rmse_train = as.numeric(rmse_i$p90_rmse_train),
      max_rmse_train = as.numeric(rmse_i$max_rmse_train),
      median_rmse_test = as.numeric(rmse_i$median_rmse_test),
      p90_rmse_test = as.numeric(rmse_i$p90_rmse_test),
      max_rmse_test = as.numeric(rmse_i$max_rmse_test),
      rmse_train_norm = as.numeric(rmse_i$rmse_train_norm),
      rmse_test_norm = as.numeric(rmse_i$rmse_test_norm),
      n_years_used_train = as.integer(rmse_i$n_years_used_train),
      n_years_used_test = as.integer(rmse_i$n_years_used_test),
      stringsAsFactors = FALSE
    )
  })

  window_metrics <- do.call(rbind, rows)

  safe_median <- function(x) if (all(is.na(x))) NA_real_ else as.numeric(stats::median(x, na.rm = TRUE))
  safe_max <- function(x) {
    x_ok <- x[is.finite(x)]
    if (length(x_ok) == 0) NA_real_ else as.numeric(max(x_ok, na.rm = TRUE))
  }
  safe_mean_int <- function(x) {
    x_ok <- x[is.finite(x)]
    if (length(x_ok) == 0) NA_integer_ else as.integer(round(mean(x_ok, na.rm = TRUE)))
  }

  agg <- list(
    rmse_train = safe_median(window_metrics$rmse_train),
    rmse_test = safe_median(window_metrics$rmse_test),
    rmse_train_norm = safe_median(window_metrics$rmse_train_norm),
    rmse_test_norm = safe_median(window_metrics$rmse_test_norm),
    median_rmse_train = safe_median(window_metrics$median_rmse_train),
    p90_rmse_train = safe_median(window_metrics$p90_rmse_train),
    max_rmse_train = safe_max(window_metrics$max_rmse_train),
    median_rmse_test = safe_median(window_metrics$median_rmse_test),
    p90_rmse_test = safe_median(window_metrics$p90_rmse_test),
    max_rmse_test = safe_max(window_metrics$max_rmse_test),
    n_years_used_train = safe_mean_int(window_metrics$n_years_used_train),
    n_years_used_test = safe_mean_int(window_metrics$n_years_used_test),
    n_windows = as.integer(nrow(window_metrics))
  )

  list(window_metrics = window_metrics, aggregate = agg)
}

# -----------------------------------------------------------------------------
# Main orchestration entrypoint
# -----------------------------------------------------------------------------
run_cbps_filtered <- function(selected_units,
                              treated_year,
                              train_start,
                              train_end,
                              test_start,
                              test_end,
                              rolling_windows = NULL,
                              output_prefix = "cbps",
                              experiment_name = "full_pool",
                              analysis_base_dir = "data/processed_data/rev_analysis_low",
                              output_base_dir = "Embeddings/data/cbps_integration",
                              output_experiment_name = experiment_name,
                              save_full_weights = FALSE,
                              embedding_k = NA_integer_,
                              firms_data = NULL,
                              firms_rds_path = "data/processed_data/FIRMS.RDS",
                              use_cache = TRUE,
                              cache_max_items = .cache_max_items_default) {
  cfg <- get_diagnostics_config()
  hard_gates <- cfg$lambda_selection$hard_gates

  t0 <- Sys.time()

  df_full <- get_analysis_data_cached(
    treated_year = treated_year,
    analysis_base_dir = analysis_base_dir,
    experiment_name = experiment_name,
    use_cache = use_cache,
    max_cache_items = cache_max_items
  )

  selected_units_vec <- normalize_selected_units(selected_units, df_full)
  windows_df <- normalize_rolling_windows(
    rolling_windows = rolling_windows,
    train_start = train_start,
    train_end = train_end,
    test_start = test_start,
    test_end = test_end,
    treated_year = treated_year
  )
  subset <- subset_design(
    df_full,
    selected_units_vec,
    preprocess_opts = list(default_winsor_p = cfg$preprocessing$default_winsor_p)
  )

  n_selected_controls <- length(unique(selected_units_vec))
  if (sum(subset$W_sub == 0) <= 0) stop("No control units remain after filtering")

  grid <- run_cbps_grid_search(subset$X_sub, subset$W_sub)
  best <- grid$best_fit

  weights_df <- build_weights_df(subset$df_sub, subset$W_sub, best$res)
  if (any(is.na(weights_df$weight)) || any(!is.finite(weights_df$weight))) {
    stop("Weights contain NA or non-finite values")
  }

  diag <- compute_weight_diagnostics(weights_df, subset$W_sub, hard_gates)

  if (is.na(embedding_k)) {
    k_match <- regmatches(output_prefix, regexec("^k([0-9]+)", output_prefix))
    if (length(k_match[[1]]) >= 2) {
      embedding_k <- suppressWarnings(as.integer(k_match[[1]][2]))
    }
  }

  firms_data_loaded <- get_firms_data_cached(
    firms_data = firms_data,
    firms_rds_path = firms_rds_path,
    use_cache = use_cache,
    max_cache_items = cache_max_items
  )

  rmse_result <- tryCatch(
    compute_rmse_windows(
      weights_df = weights_df,
      windows_df = windows_df,
      firms_rds_path = firms_rds_path,
      firms_data = firms_data_loaded
    ),
    error = function(e) {
      msg <- safe_condition_message(e)
      dbg <- persist_rmse_failure_context(
        output_base_dir = output_base_dir,
        treated_year = treated_year,
        output_experiment_name = output_experiment_name,
        output_prefix = output_prefix,
        experiment_name = experiment_name,
        analysis_base_dir = analysis_base_dir,
        firms_rds_path = firms_rds_path,
        weights_df = weights_df,
        windows_df = windows_df,
        error_message = msg
      )

      stop(
        paste0(
          "compute_rmse_windows failed: ", msg,
          " | weights_df columns=", paste(names(weights_df), collapse = ","),
          " | n_weights=", nrow(weights_df),
          " | debug_dir=", dbg$debug_dir,
          " | context_file=", dbg$context_file
        )
      )
    }
  )
  rmse <- rmse_result$aggregate

  runtime_seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  output_dir <- resolve_output_dir(output_base_dir, treated_year, output_experiment_name)

  metrics_df <- data.frame(
    year = as.integer(treated_year),
    output_prefix = as.character(output_prefix),
    experiment_name = as.character(experiment_name),
    embedding_k = as.integer(embedding_k),
    n_selected_controls = as.integer(n_selected_controls),
    n_treated = as.integer(sum(subset$W_sub == 1)),
    n_control = as.integer(sum(subset$W_sub == 0)),
    n_covariates = as.integer(subset$n_covariates),
    n_eval_windows = as.integer(rmse$n_windows),
    rho = as.numeric(best$lambda),
    converged = TRUE,
    max_balance_std = as.numeric(best$max_balance_std),
    mean_balance_std = as.numeric(best$mean_balance_std),
    ess_control = as.numeric(diag$ess),
    ess_ratio = as.numeric(diag$ess_ratio),
    top10_share = as.numeric(diag$top10_share),
    max_weight_share = as.numeric(diag$max_weight_share),
    control_weight_sum = as.numeric(diag$control_weight_sum),
    treated_count = as.integer(diag$treated_count),
    runtime_seconds = as.numeric(runtime_seconds),
    rmse_train = as.numeric(rmse$rmse_train),
    rmse_test = as.numeric(rmse$rmse_test),
    median_rmse_train = as.numeric(rmse$median_rmse_train),
    p90_rmse_train = as.numeric(rmse$p90_rmse_train),
    max_rmse_train = as.numeric(rmse$max_rmse_train),
    median_rmse_test = as.numeric(rmse$median_rmse_test),
    p90_rmse_test = as.numeric(rmse$p90_rmse_test),
    max_rmse_test = as.numeric(rmse$max_rmse_test),
    rmse_train_norm = as.numeric(rmse$rmse_train_norm),
    rmse_test_norm = as.numeric(rmse$rmse_test_norm),
    n_years_used_train = as.integer(rmse$n_years_used_train),
    n_years_used_test = as.integer(rmse$n_years_used_test),
    gate_max_smd = if (!is.null(hard_gates$max_smd)) as.numeric(hard_gates$max_smd) else NA_real_,
    gate_top10_share = if (!is.null(hard_gates$top10_share)) as.numeric(hard_gates$top10_share) else NA_real_,
    gate_max_weight = if (!is.null(hard_gates$max_weight)) as.numeric(hard_gates$max_weight) else NA_real_,
    gate_ess_frac = if (!is.null(hard_gates$ess_frac)) as.numeric(hard_gates$ess_frac) else NA_real_,
    gate_ess_mult_treated = if (!is.null(hard_gates$ess_mult_treated)) as.numeric(hard_gates$ess_mult_treated) else NA_real_,
    stringsAsFactors = FALSE
  )

  list(
    metrics_df = metrics_df,
    window_metrics_df = rmse_result$window_metrics,
    weights_df = weights_df,
    candidate_table = grid$candidate_table,
    output_dir = output_dir,
    output_prefix = output_prefix,
    treated_year = treated_year
  )
}

# -----------------------------------------------------------------------------
# Output persistence
# -----------------------------------------------------------------------------
save_cbps_filtered_outputs <- function(result, save_full_weights = FALSE) {
  write_csv_atomic <- function(df, out_path) {
    tmp_path <- tempfile(pattern = paste0(basename(out_path), ".tmp_"), tmpdir = dirname(out_path))
    on.exit(if (file.exists(tmp_path)) unlink(tmp_path), add = TRUE)
    write.csv(df, tmp_path, row.names = FALSE)
    renamed <- file.rename(tmp_path, out_path)
    if (!isTRUE(renamed)) {
      copied <- file.copy(tmp_path, out_path, overwrite = TRUE)
      if (!isTRUE(copied)) {
        stop(paste("Failed to atomically write CSV:", out_path))
      }
      unlink(tmp_path)
    }
  }

  dir.create(result$output_dir, recursive = TRUE, showWarnings = FALSE)

  metrics_path <- file.path(result$output_dir, paste0("cbps_metrics_", result$output_prefix, "_", result$treated_year, ".csv"))
  window_metrics_path <- file.path(result$output_dir, paste0("cbps_rmse_windows_", result$output_prefix, "_", result$treated_year, ".csv"))
  weights_path <- file.path(result$output_dir, paste0("cbps_weights_", result$output_prefix, "_", result$treated_year, ".csv"))
  weights_full_path <- NA_character_

  write_csv_atomic(result$metrics_df, metrics_path)
  if (!is.null(result$window_metrics_df)) {
    write_csv_atomic(result$window_metrics_df, window_metrics_path)
  }
  write_csv_atomic(result$weights_df[, c("unit", "treated", "weight")], weights_path)

  if (isTRUE(save_full_weights)) {
    weights_full_path <- file.path(result$output_dir, paste0("cbps_weights_full_", result$output_prefix, "_", result$treated_year, ".csv"))
    write_csv_atomic(result$weights_df, weights_full_path)
  }

  list(
    metrics_path = metrics_path,
    window_metrics_path = window_metrics_path,
    weights_path = weights_path,
    weights_full_path = weights_full_path
  )
}
