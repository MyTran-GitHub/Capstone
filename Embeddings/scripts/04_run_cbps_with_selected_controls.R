#!/usr/bin/env Rscript
## Python → R Integration Script
## Run CBPS on a filtered control pool (embedding-based selection)
## 
## ===== DESIGN RATIONALE =====
## Why not call implement_cbps.R directly?
## - implement_cbps.R: Runs CBPS on FULL control pool (~50k pixels) [BASELINE]
## - This script: Runs CBPS on FILTERED pool (K-nearest, ~1-5k) [EMBEDDING METHOD]
##
## Both scripts MUST:
## 1. Apply IDENTICAL covariate transformations (two-part SWE, log+winsorize, etc.)
## 2. Use IDENTICAL CBPS algorithm (cbps_ATT.R with regularization grid)
## 3. Compute IDENTICAL pre-treatment RMSE (calculate_fire_outcomes.R)
##
## ONLY difference: Control pool composition (full vs embedding-filtered)
##
## This ensures apples-to-apples comparison: any performance difference is
## attributable to control pool quality, not methodology artifacts.
##
## ===== INTEGRATION WITH PYTHON =====
## Called by: select_optimal_k.py → run_cbps_crossval()
## Input: CSV with selected control unit IDs (embedding-filtered)
## Output: Metrics CSV with RMSE, balance, convergence diagnostics
## 
## Usage:
##   Rscript run_cbps_with_selected_controls.R <year> <selected_units_csv> <output_prefix> <train_start> <train_end> <test_start> <test_end>
##
## Arguments:
##   year: Treatment year (e.g., 2019)
##   selected_units_csv: Path to CSV with selected control units (must have 'unit' column)
##   output_prefix: Prefix for output files (e.g., "k10" for K=10 nearest)
## --- Modularized functions -------------------------------------------------


source ("balancing/cbps_ATT.R") # for cbps_att() function
source ("balancing/cbps_lambda_utils.R") # for make_lambda_grid() function
source ("balancing/calculate_fire_outcomes.R") # for compute_rmse_from_weights() function
source ("balancing/balancing_config.R")

parse_flag_value <- function(args, flag, default = NULL) {
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

resolve_experiment_dir <- function(base_dir, experiment_name) {
  base_norm <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  if (basename(base_norm) == experiment_name) return(base_dir)
  file.path(base_dir, experiment_name)
}

parse_args <- function() {
  raw_args <- commandArgs(trailingOnly = TRUE)
  pos <- raw_args
  if (length(pos) < 7) stop("Usage: Rscript 04_run_cbps_with_selected_controls.R <year> <selected_units_csv> <output_prefix> <train_start> <train_end> <test_start> <test_end>")
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
    analysis_base_dir = parse_flag_value(opt, "--analysis-base-dir", "data/processed_data/rev_analysis_low"),
    output_base_dir = parse_flag_value(opt, "--output-base-dir", "Embeddings/data/cbps_integration"),
    save_full_weights = parse_bool_flag(parse_flag_value(opt, "--save-full-weights", "false"), FALSE)
  )
}

load_and_filter <- function(treated_year, selected_units_path, analysis_base_dir, experiment_name) {
  analysis_dir <- resolve_experiment_dir(analysis_base_dir, experiment_name)
  input_csv <- file.path(analysis_dir, paste0("analysis_treated", treated_year, "_conifer.csv"))
  input_rds <- file.path(analysis_dir, paste0("analysis_treated", treated_year, "_conifer.RDS"))

  if (file.exists(input_csv)) {
    df <- tryCatch({
      if (requireNamespace("data.table", quietly = TRUE)) {
        data.table::fread(input_csv, showProgress = FALSE, data.table = FALSE)
      } else {
        read.csv(input_csv, stringsAsFactors = FALSE)
      }
    }, error = function(e) {
      stop(paste("Failed to read CSV:", input_csv, " — ", e$message,
                 "\nSuggestion: convert the original RDS to a smaller CSV/RDS or run this on a machine with more RAM."))
    })
  } else if (file.exists(input_rds)) {
    df <- tryCatch({
      readRDS(input_rds)
    }, error = function(e) stop(paste("Failed to read RDS:", input_rds, "-", e$message)))
    # If it's an sf object, coerce to data.frame (drop geometry)
    if (inherits(df, 'sf')) {
      df <- as.data.frame(sf::st_drop_geometry(df))
    } else if (inherits(df, 'Spatial')) {
      # sp objects: try to coerce
      df <- tryCatch({ as.data.frame(df) }, error = function(e) stop("Unsupported spatial RDS object; please convert to data.frame with 'unit' and 'treated' columns."))
    }
  } else {
    stop(paste("Input covariate CSV or RDS not found. Looked for:", input_csv, "and", input_rds))
  }

  # Normalize common alternate names for the treatment indicator
  if (!"treated" %in% names(df)) {
    alt_names <- c("treat", "TREATED", "is_treated", "treated_flag", "treated1")
    found <- intersect(alt_names, names(df))
    if (length(found) >= 1) {
      df$treated <- df[[found[1]]]
      warning(sprintf("Mapped alternative treatment column '%s' -> 'treated'", found[1]))
    }
  }
  if (!file.exists(selected_units_path)) stop(paste("Selected units file not found:", selected_units_path))
  selected_units <- tryCatch({ read.csv(selected_units_path, stringsAsFactors = FALSE) }, error = function(e) stop(paste("Failed to read selected-units CSV:", selected_units_path, "-", e$message)))

  # Map common alternate names for the unit identifier in the selected-units CSV
  alt_unit_names <- c('unit', 'Unit', 'units', 'unit_id', 'unitID', 'pixel', 'pixel_id', 'id')
  found_unit <- intersect(alt_unit_names, names(selected_units))
  if (length(found_unit) == 0) stop(sprintf("Selected units CSV must contain a unit identifier column. Tried: %s", paste(alt_unit_names, collapse = ", ")))
  if (found_unit[1] != 'unit') selected_units$unit <- selected_units[[found_unit[1]]]

  # If the selected-units CSV includes a treatment indicator under alternate names, normalize it.
  alt_treat_names <- c('treated', 'treat', 'TREATED', 'is_treated', 'treated_flag', 'treated1')
  found_treat <- intersect(alt_treat_names, names(selected_units))
  if (length(found_treat) >= 1) {
    if (found_treat[1] != 'treated') selected_units$treated <- selected_units[[found_treat[1]]]
  } else {
    # If selected-units CSV does not include 'treated', attempt to pull treatment status from the covariate df
    if (!'unit' %in% names(df)) stop("Input covariate file must contain 'unit' column to merge treatment status for selected units")
    # Merge by unit to bring treated flag into selected_units (NA if not found)
    matched_treated <- df$treated[match(selected_units$unit, df$unit)]
    selected_units$treated <- matched_treated
    missing_idx <- which(is.na(selected_units$treated))
    if (length(missing_idx) > 0) warning(sprintf("%d selected units not found in covariate file; their 'treated' set to NA", length(missing_idx)))
  }

  # Sanity checks
  if (!"unit" %in% names(df)) stop(paste("Input covariate file is missing 'unit' column. Available columns:", paste(names(df), collapse = ", ")))
  if (!"treated" %in% names(df)) stop(paste("Input covariate file is missing 'treated' column. Available columns:", paste(names(df), collapse = ", ")))

  # Build filtered dataframe: keep all treated units and all selected units
  df_filtered <- df[df$treated == 1 | df$unit %in% selected_units$unit, , drop = FALSE]
  if (nrow(df_filtered) == 0) stop("ERROR: No rows after filtering by selected units and treated units")
  if (!"treated" %in% names(df_filtered)) stop("ERROR: 'treated' column missing from filtered dataframe")
  if (sum(df_filtered$treated == 0, na.rm = TRUE) == 0) stop("ERROR: No control pixels after filtering!")
  list(df = df, df_filtered = df_filtered)
}

apply_preprocessing <- function(df_filtered, strict_canonical = TRUE) {
  # Attempt to reuse canonical prepare_cbps_design() when available
  impl_path <- "balancing/implement_cbps.R"
  if (file.exists(impl_path)) {
    try({
      SKIP_IMPLEMENT_CBPS_MAIN <- TRUE
      source(impl_path)
      rm(SKIP_IMPLEMENT_CBPS_MAIN)
      if (exists("prepare_cbps_design")) {
        cat("Using prepare_cbps_design() from balancing/implement_cbps.R\n")
          res <- prepare_cbps_design(df_filtered)
          # Expect res to contain at least: X, X.scl (or X_scl), W, use_strong_regularization
          # Normalize field names if necessary and tolerate either dot or underscore naming
          X <- if (!is.null(res$X)) res$X else if (!is.null(res$X_scl)) res$X_scl else res$X.scl
          X_scl <- if (!is.null(res$X.scl)) res$X.scl else if (!is.null(res$X_scl)) res$X_scl else NULL
          W <- if (!is.null(res$W)) res$W else df_filtered$treated
          n_covariates_used <- if (!is.null(res$n_covariates_used)) res$n_covariates_used else if (!is.null(ncol(X_scl))) ncol(X_scl) else ncol(X)
          use_strong_regularization <- if (!is.null(res$use_strong_regularization)) res$use_strong_regularization else FALSE
          return(list(X = X, X_scl = X_scl, W = W, n_covariates_used = n_covariates_used, use_strong_regularization = use_strong_regularization))
      }
    }, silent = TRUE)
  }

  if (isTRUE(strict_canonical)) {
    stop("Canonical preprocessing unavailable. Expected prepare_cbps_design() from balancing/prepare_cbps_design.R via balancing/implement_cbps.R. Set strict_canonical=FALSE only for debugging.")
  }

  cat("Falling back to local preprocessing (matches previous inline logic)\n")
  W <- df_filtered$treated
  X <- df_filtered
  X$unit <- NULL; X$LATITUDE <- NULL; X$LONGITUDE <- NULL; X$treated <- NULL; X$num.fire <- NULL
  X <- X[, sapply(X, is.numeric), drop = FALSE]
  X <- X[, apply(X, 2, sd, na.rm = TRUE) > 0, drop = FALSE]

  # Define seasonal aggregation helper (match canonical implementation)
  month_to_season <- function(m) {
    m <- as.integer(m)
    if (m %in% c(12,1,2)) return('winter')
    if (m %in% c(3,4,5)) return('spring')
    if (m %in% c(6,7,8)) return('summer')
    if (m %in% c(9,10,11)) return('autumn')
    return(NA)
  }

  make_seasonal <- function(X, prefix) {
    cols <- grep(paste0('^', prefix), colnames(X), value = TRUE)
    if (length(cols) == 0) return(X)
    info <- dplyr::bind_rows(lapply(cols, function(nm) {
      parts <- strsplit(sub(paste0('^', prefix), '', nm), '_')[[1]]
      year <- parts[1]
      month <- parts[length(parts)]
      season <- month_to_season(month)
      data.frame(name = nm, year = year, month = as.integer(month), season = season, stringsAsFactors = FALSE)
    }))
    seasons <- c('winter','spring','summer','autumn')
    for (yr in unique(info$year)) {
      for (s in seasons) {
        cols_here <- info$name[info$year == yr & info$season == s]
        if (length(cols_here) == 0) next
        newname <- paste0(prefix, yr, '_', s)
        X[[newname]] <- rowSums(X[, cols_here, drop = FALSE], na.rm = TRUE)
      }
    }
    X <- X[, !colnames(X) %in% cols, drop = FALSE]
    X
  }

  # Apply seasonal aggregation for swe_ and prcp_ as in canonical preprocessing
  X <- make_seasonal(X, 'swe_')
  X <- make_seasonal(X, 'prcp_')

  ## Two-part SWE
  swe_cols <- grep("^swe_", colnames(X), value = TRUE)
  if (length(swe_cols) > 0) {
    cols_to_remove <- c(); cols_converted <- 0
    for (col in swe_cols) {
      x <- X[[col]]; pct_zero <- sum(x == 0 | is.na(x), na.rm = TRUE) / length(x)
      if (pct_zero > 0.95) { cols_to_remove <- c(cols_to_remove, col) } else {
        X[[paste0(col, "_present")]] <- as.numeric(x > 0)
        x_pos <- ifelse(x > 0, log1p(x), 0)
        pos_vals <- x_pos[x_pos > 0]
        if (length(pos_vals) > 0) { p995 <- quantile(pos_vals, 0.995, na.rm = TRUE); if (!is.na(p995)) x_pos[x_pos > p995] <- p995 }
        X[[col]] <- x_pos; cols_converted <- cols_converted + 1
      }
    }
    if (length(cols_to_remove) > 0) X <- X[, !colnames(X) %in% cols_to_remove]
  }

  ## winsorize heavy tails
  frp_cols <- grep("^max_FRP_", colnames(X), value = TRUE)
  for (col in frp_cols) { x <- X[[col]]; if (!all(is.na(x))) { x <- log1p(x); p995 <- quantile(x, 0.995, na.rm = TRUE); if (!is.na(p995)) x[x > p995] <- p995; X[[col]] <- x } }
  prcp_cols <- grep("^prcp_", colnames(X), value = TRUE)
  bright_cols <- grep("^avg_BRIGHTNESS_", colnames(X), value = TRUE)
  for (col in c(prcp_cols, bright_cols)) {
    x <- X[[col]]
    if (!all(is.na(x))) {
      x <- log1p(x)
      p995 <- quantile(x, 0.995, na.rm = TRUE)
      if (!is.na(p995)) x[x > p995] <- p995
      X[[col]] <- x
      # add explicit prcp variants when applicable
      if (grepl('^prcp_', col)) {
        X[[paste0(col, '_tr')]] <- x
        non_na_idx <- which(!is.na(x))
        if (length(non_na_idx) > 0) {
          ranks <- rep(NA_real_, length(x))
          ranks[non_na_idx] <- rank(x[non_na_idx], ties.method = 'average') / length(non_na_idx)
          X[[paste0(col, '_rnk')]] <- ranks
          breaks <- unique(quantile(x[non_na_idx], probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
          if (length(breaks) >= 2) {
            X[[paste0(col, '_q4')]] <- as.numeric(cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE))
          } else {
            X[[paste0(col, '_q4')]] <- rep(NA_real_, length(x))
          }
        } else {
          X[[paste0(col, '_rnk')]] <- rep(NA_real_, length(x))
          X[[paste0(col, '_q4')]] <- rep(NA_real_, length(x))
        }
      }
    }
  }

  ## drop sparse fire_*
  fire_cols <- grep("^fire_", colnames(X), value = TRUE)
  if (length(fire_cols) > 0) {
    sparse_fire <- c()
    # compute any-year aggregate
    any_present_vec <- as.numeric(rowSums(X[, fire_cols, drop = FALSE] > 0, na.rm = TRUE) > 0)
    n_years_present_vec <- as.numeric(rowSums(X[, fire_cols, drop = FALSE] > 0, na.rm = TRUE))
    for (col in fire_cols) {
      p_one <- mean(X[[col]] > 0, na.rm = TRUE)
      if (!is.na(p_one) && p_one < 0.005) sparse_fire <- c(sparse_fire, col)
    }
    # compute simple SMD heuristic
    treated_idx <- which(W == 1)
    ctrl_idx <- which(W == 0)
    smd_raws <- c()
    for (col in fire_cols) {
      pres_t <- mean((X[[col]] > 0)[treated_idx], na.rm = TRUE)
      pres_c <- mean((X[[col]] > 0)[ctrl_idx], na.rm = TRUE)
      sd_c <- sd((X[[col]] > 0)[ctrl_idx], na.rm = TRUE)
      if (is.na(sd_c) || sd_c == 0) sd_c <- 1
      smd_raws <- c(smd_raws, (pres_t - pres_c) / sd_c)
    }
    any_pres_t <- mean(any_present_vec[treated_idx], na.rm = TRUE)
    any_pres_c <- mean(any_present_vec[ctrl_idx], na.rm = TRUE)
    sd_c_any <- sd(any_present_vec[ctrl_idx], na.rm = TRUE)
    if (is.na(sd_c_any) || sd_c_any == 0) sd_c_any <- 1
    smd_any <- (any_pres_t - any_pres_c) / sd_c_any
    prefer_any <- FALSE
    if (!is.na(any_pres_c) && any_pres_c >= 0.002 && length(smd_raws) > 0) {
      if (abs(smd_any) <= max(abs(smd_raws), na.rm = TRUE)) prefer_any <- TRUE
    }
    if (prefer_any) {
      to_drop <- intersect(colnames(X), fire_cols)
      if (length(to_drop) > 0) X <- X[, !colnames(X) %in% to_drop]
      X[['fire_any_present']] <- as.numeric(any_present_vec > 0)
      X[['fire_n_years_present']] <- as.numeric(n_years_present_vec)
    } else {
      if (length(sparse_fire) > 0) X <- X[, !colnames(X) %in% sparse_fire]
    }
  }

  if (ncol(X) == 0) stop("ERROR: No covariates remaining after transformations!")

  n_treated <- sum(W); n_control <- sum(1 - W)
  if (n_control < 2 * n_treated) stop(paste("ERROR: Insufficient controls! Have", n_control, "controls for", n_treated, "treated"))
  if (n_control < 10 * n_treated) cat("⚠ WARNING: Small control pool - recommend ≥10× treated\n")
  if (n_control / ncol(X) < 5) cat("⚠ WARNING: Low obs:cov ratio\n")

  X_mean <- colMeans(X, na.rm = TRUE); X_sd <- apply(X, 2, sd, na.rm = TRUE); X_sd[is.na(X_sd) | X_sd == 0] <- 1
  X_scl <- scale(X, center = X_mean, scale = X_sd)
  X_var_original <- apply(X, 2, var, na.rm = TRUE)
  near_constant_threshold <- 1e-10; keep_variance <- X_var_original >= near_constant_threshold
  n_removed <- sum(!keep_variance); if (n_removed > 0) X_scl <- X_scl[, keep_variance, drop = FALSE]

  obs_per_cov <- n_control / ncol(X_scl)
  use_strong_regularization <- obs_per_cov < 10
  n_covariates_used <- ncol(X_scl)
  list(X = X, X_scl = X_scl, W = W, n_covariates_used = n_covariates_used, use_strong_regularization = use_strong_regularization)
}

run_cbps_grid <- function(X_scl, W, use_strong_regularization) {
  # Initial lambda grid depending on obs:cov ratio
  if (use_strong_regularization) {
    lambda_levels <- 10; lambda_range <- -4:5
  } else {
    lambda_levels <- 8; lambda_range <- -6:1
  }

  attempt_cbps_with_grid <- function(lambda_range_vals) {
    res_list <- lapply(seq_along(lambda_range_vals), function(i) {
      lambda_val <- 10^(lambda_range_vals[i])
      tryCatch({
        cbps_att(as.matrix(X_scl), W, theta.init = rep(0, ncol(X_scl) + 1), control = list(trace = 0, maxit = 6000), lambda = rep(lambda_val, ncol(X_scl)))
      }, error = function(e) { NULL })
    })
    # Filter out NULL results
    res_list <- Filter(Negate(is.null), res_list)
    if (length(res_list) == 0) return(NULL)
    converge_set <- sapply(res_list, function(res) {
      converged <- (!is.null(res) && !is.null(res$convergence) && res$convergence == 0)
      valid_weights <- FALSE
      if (!is.null(res) && !is.null(res$weights.0)) {
        valid_weights <- !any(is.na(res$weights.0)) && all(is.finite(res$weights.0)) && !any(is.na(res$weights.1)) && all(is.finite(res$weights.1))
      }
      converged && valid_weights
    })
    if (!any(converge_set)) return(NULL)
    idx <- which(converge_set)[1]
    res <- res_list[[idx]]
    # rho corresponds to the exponent value used; map back via lambda_range_vals
    rho <- 10^(lambda_range_vals[idx])
    list(res = res, rho = rho)
  }

  # 1) Try initial grid
  logger_msg <- function(msg) cat(msg, "\n")
  attempt <- attempt_cbps_with_grid(lambda_range)
  if (!is.null(attempt)) {
    logger_msg("CBPS: succeeded on initial lambda grid")
    return(attempt)
  }
  # 2) Try targeted per-covariate inflation for known problematic groups (conservative)
  logger_msg("CBPS: initial grid failed — trying targeted per-covariate inflation")
  # Identify problematic covariate groups by prefix (match canonical preprocessing)
  coln <- colnames(X_scl)
  problem_prefixes <- c('^swe_', '^prcp_', '^fire_')
  problem_idx <- unique(unlist(lapply(problem_prefixes, function(p) grep(p, coln, perl = TRUE))))
  if (length(problem_idx) > 0) {
    logger_msg(sprintf("Attempting per-covariate inflation on %d covariates", length(problem_idx)))
    multipliers_exp <- c(0, 1, 2) # multiplier exponents: *1, *10, *100
    base_range <- lambda_range
    attempt_per_covariate <- function(base_range_vals, mult_exps) {
      for (be in base_range_vals) {
        base_lambda <- 10^be
        for (me in mult_exps) {
          lambda_vec <- rep(base_lambda, ncol(X_scl))
          lambda_vec[problem_idx] <- lambda_vec[problem_idx] * (10^me)
          res_try <- tryCatch({
            cbps_att(as.matrix(X_scl), W, theta.init = rep(0, ncol(X_scl) + 1), control = list(trace = 0, maxit = 6000), lambda = lambda_vec)
          }, error = function(e) NULL)
          if (!is.null(res_try) && !is.null(res_try$convergence) && res_try$convergence == 0) {
            # verify weights are finite and non-NA
            if (!is.null(res_try$weights.0) && !any(is.na(res_try$weights.0)) && all(is.finite(res_try$weights.0))) {
              return(list(res = res_try, rho = base_lambda))
            }
          }
        }
      }
      return(NULL)
    }

    attempt_cov <- attempt_per_covariate(base_range, multipliers_exp)
    if (!is.null(attempt_cov)) {
      logger_msg("CBPS: succeeded with targeted per-covariate inflation")
      return(attempt_cov)
    } else {
      logger_msg("CBPS: targeted inflation did not yield valid fit; falling back to extended scalar grid")
    }
  } else {
    logger_msg("No problematic covariates found for targeted inflation; skipping this step")
  }

  # 3) Try extended, stronger regularization grid (broader scalar search)
  logger_msg("CBPS: trying extended regularization grid")
  ext_range <- seq(-8, 8)
  attempt_ext <- attempt_cbps_with_grid(ext_range)
  if (!is.null(attempt_ext)) {
    logger_msg("CBPS: succeeded on extended regularization grid")
    return(attempt_ext)
  }

  # 4) Try dimensionality reduction (PCA) then CBPS on PC scores
  logger_msg("CBPS: extended grid failed — trying PCA-based dimensionality reduction")
  # Determine number of components to keep: min(ncol, floor(n_control/5),  max 50)
  n_control <- sum(1 - W)
  max_comps <- min(ncol(X_scl), max(2, floor(n_control / 5)), 50)
  # perform PCA on scaled X_scl
  pca_res <- tryCatch({ prcomp(X_scl, center = FALSE, scale. = FALSE) }, error = function(e) NULL)
  if (!is.null(pca_res)) {
    pcs <- pca_res$x[, seq_len(max_comps), drop = FALSE]
    # Try CBPS on PCs with extended grid
    attempt_pca <- tryCatch({
      res_list <- lapply(ext_range, function(expv) {
        lambda_val <- 10^expv
        tryCatch({
          cbps_att(as.matrix(pcs), W, theta.init = rep(0, ncol(pcs) + 1), control = list(trace = 0, maxit = 6000), lambda = rep(lambda_val, ncol(pcs)))
        }, error = function(e) NULL)
      })
      res_list <- Filter(Negate(is.null), res_list)
      if (length(res_list) == 0) return(NULL)
      converge_set <- sapply(res_list, function(res) (!is.null(res) && !is.null(res$convergence) && res$convergence == 0 && !any(is.na(res$weights.0)) && all(is.finite(res$weights.0))))
      if (!any(converge_set)) return(NULL)
      idx <- which(converge_set)[1]
      res <- res_list[[idx]]
      list(res = res, rho = 10^(ext_range[idx]))
    }, error = function(e) NULL)
    if (!is.null(attempt_pca)) {
      logger_msg(sprintf("CBPS: succeeded on PCA (%d components)", ncol(pcs)))
      return(attempt_pca)
    }
  }

  # If all attempts failed, stop with informative error so caller can handle
  stop("No solution with valid convergence AND valid weights found after extended attempts (regularization+PCA)")
}

compute_weights_and_metrics <- function(df_filtered, res) {
  max_balance_std <- max(abs(res$balance.std), na.rm = TRUE)
  mean_balance_std <- mean(abs(res$balance.std), na.rm = TRUE)
  weights_df <- data.frame(unit = df_filtered$unit, LATITUDE = df_filtered$LATITUDE, LONGITUDE = df_filtered$LONGITUDE, treated = df_filtered$treated, weight = ifelse(df_filtered$treated == 1, res$weights.1, res$weights.0), stringsAsFactors = FALSE)
  # Diagnostics: control-side stats
  ctrl_w <- weights_df$weight[weights_df$treated == 0]
  n_controls <- length(ctrl_w)
  n_na <- sum(is.na(ctrl_w))
  n_inf <- sum(!is.finite(ctrl_w))
  ESS <- if (n_controls == 0) 0 else (sum(ctrl_w, na.rm = TRUE)^2) / sum((ctrl_w^2), na.rm = TRUE)
  ctrl_total <- sum(ctrl_w, na.rm = TRUE)
  topN <- max(1, ceiling(0.10 * n_controls))
  top10_share <- if (ctrl_total == 0) 0 else sum(sort(ctrl_w, decreasing = TRUE)[1:min(topN, length(ctrl_w))], na.rm = TRUE) / ctrl_total
  max_weight_share <- if (ctrl_total == 0) 0 else max(ctrl_w, na.rm = TRUE) / ctrl_total
  list(weights_df = weights_df, max_balance_std = max_balance_std, mean_balance_std = mean_balance_std, diagnostics = list(n_controls = n_controls, n_na = n_na, n_inf = n_inf, ESS = ESS, top10_share = top10_share, max_weight_share = max_weight_share))
}

compute_rmse <- function(weights_df, train_start, train_end, test_start, test_end) {
  # Do not swallow errors: allow RMSE computation failures to propagate
  calculate_pretreatment_rmse(weights_df = weights_df, train_start = train_start, train_end = train_end, test_start = test_start, test_end = test_end, firms_rds_path = "data/processed_data/FIRMS.RDS")
}

save_outputs <- function(output_dir, treated_year, output_prefix, metrics_df, weights_df, save_full_weights = FALSE) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  metrics_path <- file.path(output_dir, paste0("cbps_metrics_", output_prefix, "_", treated_year, ".csv"))
  weights_path <- file.path(output_dir, paste0("cbps_weights_", output_prefix, "_", treated_year, ".csv"))
  write.csv(metrics_df, metrics_path, row.names = FALSE)
  write.csv(weights_df[, c("unit", "treated", "weight")], weights_path, row.names = FALSE)
  weights_full_path <- NA_character_
  if (isTRUE(save_full_weights)) {
    weights_full_path <- file.path(output_dir, paste0("cbps_weights_full_", output_prefix, "_", treated_year, ".csv"))
    write.csv(weights_df, weights_full_path, row.names = FALSE)
  }
  list(metrics_path = metrics_path, weights_path = weights_path, weights_full_path = weights_full_path)
}

main <- function() {
  args <- parse_args()
  cfg <- get_diagnostics_config()
  hard_gates <- cfg$lambda_selection$hard_gates
  cat("========== Covariate balancing with filtered control pool\n")
  cat("Treatment year:", args$treated_year, "\n")
  cat("Output prefix:", args$output_prefix, "\n")
  cat("Train period:", args$train_start, "-", args$train_end, "\n")
  cat("Test period:", args$test_start, "-", args$test_end, "\n")

  dat <- load_and_filter(
    args$treated_year,
    args$selected_units_path,
    analysis_base_dir = args$analysis_base_dir,
    experiment_name = args$experiment_name
  )
  df <- dat$df; df_filtered <- dat$df_filtered

  prep <- apply_preprocessing(df_filtered, strict_canonical = TRUE)
  X <- prep$X; X_scl <- prep$X_scl; W <- prep$W; n_covariates_used <- prep$n_covariates_used; use_strong_regularization <- prep$use_strong_regularization

  cbps_start_time <- Sys.time()
  cbps_res <- run_cbps_grid(X_scl, W, use_strong_regularization)
  runtime_seconds <- as.numeric(difftime(Sys.time(), cbps_start_time, units = "secs"))
  res <- cbps_res$res; rho <- cbps_res$rho

  cat("Converged with valid weights at rho =", rho, "\n")
  metrics_w <- compute_weights_and_metrics(df_filtered, res)
  weights_df <- metrics_w$weights_df
  diag <- metrics_w$diagnostics

  # Always enforce diagnostics: fail fast if degenerate weights (no proxy allowed)
  cat("Weight diagnostics:\n")
  print(diag)
  min_ess_frac <- 0.1
  if (diag$n_na > 0 || diag$n_inf > 0) stop("Degenerate weights: NA or infinite values present")
  if (diag$ESS < (min_ess_frac * max(1, diag$n_controls))) stop(paste0("Degenerate weights: ESS (", round(diag$ESS,2),") below threshold (", min_ess_frac,")"))

  cat("\nComputing pre-treatment fire frequency RMSE...\n")
  rmse_result <- compute_rmse(weights_df, args$train_start, args$train_end, args$test_start, args$test_end)

  rmse_train <- rmse_result$rmse_train; rmse_test <- rmse_result$rmse_test
  rmse_train_norm <- if (!is.null(rmse_result$rmse_train_norm)) rmse_result$rmse_train_norm else NA_real_
  rmse_test_norm <- if (!is.null(rmse_result$rmse_test_norm)) rmse_result$rmse_test_norm else NA_real_
  median_rmse_train <- if (!is.null(rmse_result$median_rmse_train)) rmse_result$median_rmse_train else NA_real_
  p90_rmse_train <- if (!is.null(rmse_result$p90_rmse_train)) rmse_result$p90_rmse_train else NA_real_
  max_rmse_train <- if (!is.null(rmse_result$max_rmse_train)) rmse_result$max_rmse_train else NA_real_
  median_rmse_test <- if (!is.null(rmse_result$median_rmse_test)) rmse_result$median_rmse_test else NA_real_
  p90_rmse_test <- if (!is.null(rmse_result$p90_rmse_test)) rmse_result$p90_rmse_test else NA_real_
  max_rmse_test <- if (!is.null(rmse_result$max_rmse_test)) rmse_result$max_rmse_test else NA_real_
  n_years_used_train <- if (!is.null(rmse_result$n_years_used_train)) rmse_result$n_years_used_train else 0
  n_years_used_test <- if (!is.null(rmse_result$n_years_used_test)) rmse_result$n_years_used_test else 0

  cat("Train RMSE:", ifelse(is.na(rmse_train), "NA", round(rmse_train, 4)), "\n")
  cat("Test RMSE:", ifelse(is.na(rmse_test), "NA", round(rmse_test, 4)), "\n")

  output_dir <- file.path(args$output_base_dir, args$experiment_name, as.character(args$treated_year))

  n_treated <- sum(W)
  metrics_df <- data.frame(
    year = args$treated_year,
    output_prefix = args$output_prefix,
    n_treated = n_treated,
    n_control = sum(1 - W),
    n_covariates = n_covariates_used,
    rho = rho,
    converged = res$convergence == 0,
    max_balance_std = metrics_w$max_balance_std,
    mean_balance_std = metrics_w$mean_balance_std,
    ess_control = diag$ESS,
    ess_ratio = ifelse(n_treated > 0, diag$ESS / n_treated, NA_real_),
    top10_share = diag$top10_share,
    max_weight_share = diag$max_weight_share,
    runtime_seconds = runtime_seconds,
    rmse_train = rmse_train,
    rmse_test = rmse_test,
    median_rmse_train = median_rmse_train,
    p90_rmse_train = p90_rmse_train,
    max_rmse_train = max_rmse_train,
    median_rmse_test = median_rmse_test,
    p90_rmse_test = p90_rmse_test,
    max_rmse_test = max_rmse_test,
    rmse_train_norm = rmse_train_norm,
    rmse_test_norm = rmse_test_norm,
    n_years_used_train = n_years_used_train,
    n_years_used_test = n_years_used_test,
    gate_max_smd = if (!is.null(hard_gates$max_smd)) as.numeric(hard_gates$max_smd) else NA_real_,
    gate_top10_share = if (!is.null(hard_gates$top10_share)) as.numeric(hard_gates$top10_share) else NA_real_,
    gate_max_weight = if (!is.null(hard_gates$max_weight)) as.numeric(hard_gates$max_weight) else NA_real_,
    gate_ess_frac = if (!is.null(hard_gates$ess_frac)) as.numeric(hard_gates$ess_frac) else NA_real_,
    gate_ess_mult_treated = if (!is.null(hard_gates$ess_mult_treated)) as.numeric(hard_gates$ess_mult_treated) else NA_real_,
    stringsAsFactors = FALSE
  )

  saved <- save_outputs(
    output_dir,
    args$treated_year,
    args$output_prefix,
    metrics_df,
    weights_df,
    save_full_weights = args$save_full_weights
  )
  cat("\nSaved:\n  ", saved$metrics_path, "\n  ", saved$weights_path, "\n", sep = "")
  if (!is.na(saved$weights_full_path)) {
    cat("  ", saved$weights_full_path, "\n", sep = "")
  }
  cat("\n✓ CBPS completed successfully\n")
}

## Run main when invoked as script
if (identical(environment(), globalenv())) {
  tryCatch({ main() }, error = function(e) { cat("FATAL:", e$message, "\n"); quit(status = 2) })
}
