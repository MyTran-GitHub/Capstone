## Covariate balancing synthetic control for conifer-only pipeline
## Apply synthetic control approach via covariate balancing (Imai 2014, Zhao 2019, Tan 2020)
## Calculate ATT weights for each unit (conifer area only, focal years 2005:2020)
##
## Note: SWE uses a two-part representation (presence + log-intensity).
## max_FRP and prcp are log1p-winsorized; avg_BRIGHTNESS columns are dropped when `fire_*` exist.
library("sf")

source("balancing/cbps_ATT.R")
source("balancing/cbps_lambda_utils.R")
source("balancing/balancing_config.R")
get_diagnostics_config <- get("get_diagnostics_config", mode = "function")
source("balancing/prepare_cbps_design.R")
source("diagnostics/diagnostics_scripts/covariates/run_covariate_exploration.R")


if (!exists('SKIP_IMPLEMENT_CBPS_MAIN') || !SKIP_IMPLEMENT_CBPS_MAIN) {

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

cli_args <- commandArgs(trailingOnly = TRUE)
area <- parse_flag_value(cli_args, "--area", "conifer")
years_arg <- parse_flag_value(cli_args, "--years", NULL)
year_arg <- parse_flag_value(cli_args, "--year", NULL)
diag_out_dir <- parse_flag_value(
  cli_args,
  "--covariate-diagnostics-dir",
  "diagnostics/diagnostics_results/covariates"
)

record_covariate_diagnostics <- parse_bool_flag(
  parse_flag_value(cli_args, "--record-covariate-diagnostics", NULL),
  FALSE
)
record_lambda_path <- parse_bool_flag(
  parse_flag_value(cli_args, "--record-lambda-path", NULL),
  FALSE
)
if (exists("RECORD_COVARIATE_DIAGNOSTICS")) {
  record_covariate_diagnostics <- isTRUE(RECORD_COVARIATE_DIAGNOSTICS)
}
if (exists("RECORD_LAMBDA_PATH")) {
  record_lambda_path <- isTRUE(RECORD_LAMBDA_PATH)
}

outDir <- "data/processed_data/rev_analysis_low/"
# Focal treatment years with sufficient pre-treatment data
years <- 2005:2020
if (!is.null(year_arg)) {
  years <- as.integer(strsplit(year_arg, ",", fixed = TRUE)[[1]])
}
if (!is.null(years_arg)) {
  years <- as.integer(strsplit(years_arg, ",", fixed = TRUE)[[1]])
}

for (treated.year in years) {
  input_file <- paste0(outDir, "analysis_treated", treated.year, "_", area, ".RDS")

  # Reset per-year state to avoid leaking prior loop values.
  res <- NULL
  rho <- NA_real_
  cand_df <- NULL
  
  cat("Processing year:", treated.year, "\n")
  
  if (!file.exists(input_file)) {
    cat("  File not found, skipping.\n")
    next
  }
  
  df <- readRDS(input_file)
  cfg <- get_diagnostics_config()
  # Use canonical preprocessing function to prepare design matrix
  prep <- prepare_cbps_design(df, opts = list(default_winsor_p = cfg$preprocessing$default_winsor_p))
  W <- prep$W
  X <- prep$X
  X.scl <- prep$X.scl

  # ------------------------------------------------------------------
  # Lambda grid search + selection
  n_ctrl <- sum(W == 0)

  # First-pass grid (user-requested): very coarse but includes small values
  lambda_grid <- make_very_coarse_lambda_grid()
  cat('  Using initial lambda grid:', paste(lambda_grid, collapse=','), '\n')
  candidates <- list()
  fit_by_lambda <- list()
  theta_start <- rep(0, ncol(X.scl) + 1)

  lambda_key <- function(x) sprintf('%.12g', as.numeric(x))
  try_lambda <- function(lam, stage_label) {
    key <- lambda_key(lam)
    if (key %in% names(fit_by_lambda)) return(invisible(NULL))

    cat('  Trying lambda =', lam, '(stage =', stage_label, ')\n')
    lam_vec <- rep(lam, ncol(X.scl))
    res_try <- tryCatch(cbps_att(as.matrix(X.scl),
                                W,
                                theta.init = theta_start,
                                control = list(trace = 0, maxit = 6000),
                                lambda = lam_vec), error = function(e) NULL)
    if (is.null(res_try) || is.null(res_try$convergence)) return(invisible(NULL))
    if (res_try$convergence != 0) {
      cat('    optimizer did not converge (code=', res_try$convergence, '), skipping\n', sep = '')
      return(invisible(NULL))
    }

    metrics <- compute_weights_metrics(res_try, W)
    if (is.null(metrics)) return(invisible(NULL))

    candidates[[length(candidates) + 1]] <<- data.frame(
      lambda = as.numeric(lam),
      ess = metrics$ess,
      top10_share = metrics$top10_share,
      max_weight = metrics$max_weight,
      max_smd = metrics$max_smd,
      converged = res_try$convergence == 0,
      stage = stage_label,
      stringsAsFactors = FALSE
    )
    fit_by_lambda[[key]] <<- res_try

    if (!is.null(res_try$theta.hat) && length(res_try$theta.hat) == length(theta_start)) {
      theta_start <<- res_try$theta.hat
    }

    invisible(NULL)
  }

  # Stage 1: coarse grid search.
  for (lam in lambda_grid) try_lambda(lam, 'stage1')

  cand_stage1 <- if (length(candidates) > 0) do.call(rbind, candidates) else data.frame()
  if (nrow(cand_stage1) == 0) {
    stop('No feasible lambda found under any threshold tier. Check overlap or relax constraints. Diagnostic summary: no converged lambda candidates available.')
  }

  # Stage 2: refine only in the interval just above the stage-1 winner.
  selection_stage1 <- run_lambda_selection(cand_stage1, n_ctrl)
  rho_stage1 <- as.numeric(selection_stage1$selected_row$lambda)
  upper_candidates <- sort(unique(cand_stage1$lambda[cand_stage1$lambda > rho_stage1]))
  upper_neighbor <- if (length(upper_candidates) > 0) upper_candidates[1] else NA_real_

  refine_grid <- make_refined_lambda_grid(
    lower_lambda = rho_stage1,
    upper_lambda = upper_neighbor,
    n_inner = 6
  )
  if (length(refine_grid) > 0) {
    cat('  Refining lambda between', rho_stage1, 'and', upper_neighbor, 'with', length(refine_grid), 'candidates\n')
    for (lam in refine_grid) try_lambda(lam, 'stage2')
  }

  cand_df <- if (length(candidates) > 0) do.call(rbind, candidates) else data.frame()
  cand_df <- cand_df[order(cand_df$lambda, decreasing = TRUE), , drop = FALSE]

  selection_result <- tryCatch(
    run_lambda_selection(cand_df, n_ctrl),
    error = function(e) e
  )
  if (inherits(selection_result, 'error')) {
    min_lambda <- min(cand_df$lambda, na.rm = TRUE)
    fallback_grid <- sort(unique(c(make_full_lambda_grid(), 1e-5, 1e-6)), decreasing = TRUE)
    fallback_grid <- fallback_grid[fallback_grid < min_lambda]

    if (length(fallback_grid) > 0) {
      cat('  No feasible lambda yet; expanding lower-tail fallback grid:', paste(fallback_grid, collapse = ','), '\n')
      for (lam in fallback_grid) try_lambda(lam, 'fallback')
      cand_df <- if (length(candidates) > 0) do.call(rbind, candidates) else data.frame()
      cand_df <- cand_df[order(cand_df$lambda, decreasing = TRUE), , drop = FALSE]
    }

    selection_result <- run_lambda_selection(cand_df, n_ctrl)
  }

  chosen_row <- selection_result$selected_row
  selection_log <- selection_result$selection_log

  rho <- chosen_row$lambda
  res <- fit_by_lambda[[lambda_key(rho)]]

  chosen_tol <- max(.Machine$double.eps * 10, abs(rho) * 1e-8)
  cand_df$chosen <- abs(cand_df$lambda - rho) <= chosen_tol
  cand_df$ess_ratio <- cand_df$ess / n_ctrl
  cand_df$tier_used <- selection_log$tier_used

  ess_ratio <- selection_log$ess_ratio
  ess_quality <- if (is.na(ess_ratio)) {
    'unknown'
  } else if (ess_ratio >= 0.3) {
    'strong'
  } else if (ess_ratio >= 0.2) {
    'acceptable'
  } else if (ess_ratio >= 0.1) {
    'borderline'
  } else {
    'problematic'
  }

  cat('  Selected lambda =', rho, 'using tier =', selection_log$tier_used, '\n')
  cat('  Selection metrics: max_smd =', round(chosen_row$max_smd, 4),
      ', top10_share =', round(chosen_row$top10_share, 4),
      ', max_weight =', round(chosen_row$max_weight, 4),
      ', ess =', round(chosen_row$ess, 2), '\n')
  cat('  ESS/N(control) =', round(ess_ratio, 4), '(', ess_quality, ')\n')
  if (length(selection_log$warnings) > 0) {
    for (w in selection_log$warnings) cat('  WARNING:', w, '\n')
  }

  lambda_run_file <- paste0(outDir, 'lambda_run_', treated.year, '_', area, '.rds')
  saveRDS(cand_df, lambda_run_file)
  cat('  Saved: ', lambda_run_file, '\n', sep = '')

  # Post-fit validity checks
  if (is.null(res) || is.null(res$convergence)) {
    cat('  No valid cbps fit returned, skipping.\n')
    next
  }
  if (res$convergence != 0) {
    cat('  ⚠ cbps_att did not converge (code=', res$convergence, '), continuing with result but review logs.\n', sep='')
  }
  
  # Check post-balance covariate balance
  max_balance_std <- max(abs(res$balance.std), na.rm = TRUE)
  median_balance_std <- median(abs(res$balance.std), na.rm = TRUE)

  cat("  Covariate balance: median |SMD| =", round(median_balance_std, 3),
      ", max |SMD| =", round(max_balance_std, 3), "\n")

  if (max_balance_std > 0.1) {
    cat("  ⚠ WARNING: Max |balance.std| = ", round(max_balance_std, 3), " (recommend |SMD| < 0.1)\n", sep = "")
  }
  
  # Create weights table (treated get weights.1, control get weights.0)
  weights_df <- data.frame(
    unit = df$unit,
    treated = df$treated,
    weight = ifelse(df$treated == 1, res$weights.1, res$weights.0)
  )

  # Save fit results and weights before running diagnostics to prioritize outputs
  saveRDS(res, paste0(outDir, "cbps_fit_", treated.year, "_", area, "_rho", rho, ".RDS"))
  saveRDS(weights_df, paste0(outDir, "cbps_weights_", treated.year, "_", area, ".RDS"))
  cat("  Saved: cbps_fit_", treated.year, "_", area, "_rho", rho, ".RDS\n", sep = "")
  cat("  Saved: cbps_weights_", treated.year, "_", area, ".RDS\n", sep = "")

  # Optional lightweight lambda-path export without full covariate diagnostics.
  if (record_lambda_path && !record_covariate_diagnostics) {
    tryCatch({
      run_covariate_exploration(
        treated_year = treated.year,
        area = area,
        X = X,
        W = W,
        res = res,
        cand_df = cand_df,
        selected_lambda = rho,
        out_dir = diag_out_dir,
        run_prefit_overlap = FALSE,
        write_prepost_metrics = FALSE,
        write_distribution = FALSE,
        write_block_summary = FALSE,
        write_summary = FALSE
      )
    }, error = function(e) {
      cat("  Lambda-path plotting failed: ", e$message, "\n", sep = "")
    })
  }

  if (record_covariate_diagnostics) {
    tryCatch({
      run_covariate_exploration(
        treated_year = treated.year,
        area = area,
        X = X,
        W = W,
        res = res,
        cand_df = cand_df,
        selected_lambda = rho,
        out_dir = diag_out_dir,
        run_prefit_overlap = FALSE,
        write_prepost_metrics = FALSE,
        write_distribution = FALSE,
        write_block_summary = FALSE,
        write_summary = TRUE
      )
    }, error = function(e) {
      cat("  Covariate diagnostics failed: ", e$message, "\n", sep = "")
    })
  }
  
  gc()
}

} # end guard for SKIP_IMPLEMENT_CBPS_MAIN