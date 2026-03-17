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
diag_out_dir <- parse_flag_value(
  cli_args,
  "--covariate-diagnostics-dir",
  "diagnostics/diagnostics_results/covariates"
)

record_covariate_diagnostics <- parse_bool_flag(
  parse_flag_value(cli_args, "--record-covariate-diagnostics", NULL),
  FALSE
)
if (exists("RECORD_COVARIATE_DIAGNOSTICS")) {
  record_covariate_diagnostics <- isTRUE(RECORD_COVARIATE_DIAGNOSTICS)
}

outDir <- "data/processed_data/rev_analysis_low/"
#years <- 2005:2020  # Focal treatment years with sufficient pre-treatment data
years <- 2019  # Temporarily run dry-run for 2019 only
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

  for (lam in lambda_grid) {
    cat('  Trying lambda =', lam, '\n')
    lam_vec <- rep(lam, ncol(X.scl))
    res_try <- tryCatch(cbps_att(as.matrix(X.scl),
                                W,
                                theta.init = theta_start,
                                control = list(trace = 0, maxit = 6000),
                                lambda = lam_vec), error = function(e) NULL)
    if (is.null(res_try) || is.null(res_try$convergence)) next
    if (res_try$convergence != 0) {
      cat('    optimizer did not converge (code=', res_try$convergence, '), skipping\n', sep = '')
      next
    }
    # compute metrics
    metrics <- compute_weights_metrics(res_try, W)
    if (is.null(metrics)) next

    candidates[[length(candidates) + 1]] <- data.frame(lambda = lam,
                                                       ess = metrics$ess,
                                                       top10_share = metrics$top10_share,
                                                       max_weight = metrics$max_weight,
                                                       max_smd = metrics$max_smd,
                                                       converged = res_try$convergence == 0,
                                                       stringsAsFactors = FALSE)
    fit_by_lambda[[as.character(lam)]] <- res_try
    if (!is.null(res_try$theta.hat) && length(res_try$theta.hat) == length(theta_start)) {
      theta_start <- res_try$theta.hat
    }
  }

  cand_df <- if (length(candidates) > 0) do.call(rbind, candidates) else data.frame()
  if (nrow(cand_df) == 0) {
    stop('No feasible lambda found under any threshold tier. Check overlap or relax constraints. Diagnostic summary: no converged lambda candidates available.')
  }

  selection_result <- run_lambda_selection(cand_df, n_ctrl)
  chosen_row <- selection_result$selected_row
  selection_log <- selection_result$selection_log

  rho <- chosen_row$lambda
  res <- fit_by_lambda[[as.character(rho)]]

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

  bundle_file <- paste0(outDir, 'lambda_diagnostics_bundle_', treated.year, '_', area, '.RDS')
  saveRDS(
    list(
      year = treated.year,
      area = area,
      selected_lambda = rho,
      cand_df = cand_df,
      selection_log = selection_log
    ),
    bundle_file
  )
  cat('  Saved: ', bundle_file, '\n', sep = '')

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

  if (record_covariate_diagnostics) {
    tryCatch({
      run_covariate_exploration(
        treated_year = treated.year,
        area = area,
        X = X,
        W = W,
        res = res,
        cand_df = cand_df,
        selection_log = selection_log,
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