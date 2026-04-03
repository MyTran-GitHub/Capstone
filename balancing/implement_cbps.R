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
source("balancing/cli_utils.R")
source("diagnostics/diagnostics_scripts/covariates/run_covariate_exploration.R")


if (!exists('SKIP_IMPLEMENT_CBPS_MAIN') || !SKIP_IMPLEMENT_CBPS_MAIN) {

cli_args <- commandArgs(trailingOnly = TRUE)
area <- parse_flag_value(cli_args, "--area", "conifer")
years_arg <- parse_flag_value(cli_args, "--years", NULL)
year_arg <- parse_flag_value(cli_args, "--year", NULL)
experiment_name <- parse_flag_value(cli_args, "--experiment-name", "full_pool")
diag_out_dir <- parse_flag_value(
  cli_args,
  "--covariate-diagnostics-dir",
  "diagnostics/diagnostics_results/covariates"
)
lambda_run_out_dir <- parse_flag_value(
  cli_args,
  "--lambda-run-dir",
  "diagnostics/diagnostics_results/lambda_run"
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
result_out_dir <- resolve_experiment_dir(outDir, experiment_name)
lambda_run_out_dir <- resolve_experiment_dir(lambda_run_out_dir, experiment_name)
diag_out_dir <- resolve_experiment_dir(diag_out_dir, experiment_name)

dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
dir.create(result_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(lambda_run_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diag_out_dir, recursive = TRUE, showWarnings = FALSE)
# Focal treatment years with sufficient pre-treatment data
years <- 2005:2020
if (!is.null(year_arg)) {
  years <- parse_years_list(year_arg, "--year")
}
if (!is.null(years_arg)) {
  years <- parse_years_list(years_arg, "--years")
}

cfg <- get_diagnostics_config()

for (treated.year in years) {
  tryCatch({
    input_file <- paste0(outDir, "analysis_treated", treated.year, "_", area, ".RDS")

    # Reset per-year state to avoid leaking prior loop values.
    res <- NULL
    rho <- NA_real_
    cand_df <- NULL
  
    cat("Processing year:", treated.year, "\n")
    cat("  Experiment:", experiment_name, "\n")
  
    if (!file.exists(input_file)) {
      cat("  File not found, skipping.\n")
      return(invisible(NULL))
    }
  
    df <- readRDS(input_file)
    # Use canonical preprocessing function to prepare design matrix
    prep <- prepare_cbps_design(df, opts = list(default_winsor_p = cfg$preprocessing$default_winsor_p))
    W <- prep$W
    X <- prep$X
    X.scl <- prep$X.scl

    # ------------------------------------------------------------------
    # Lambda grid search + selection
    n_ctrl <- sum(W == 0)
    n_treated <- sum(W == 1)

    if (n_ctrl == 0 || n_treated == 0) {
      cat('  Invalid design matrix: n_ctrl =', n_ctrl, ', n_treated =', n_treated, '; skipping year.\n')
      return(invisible(NULL))
    }

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
      median_smd = if (!is.null(metrics$median_smd)) metrics$median_smd else NA_real_,
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
    cat('  No converged candidates in stage-1 grid; skipping year.', '\n')
    next
  }

  # Stage 2: refine around the stage-1 winner (prefer upper interval, else lower).
  selection_stage1 <- tryCatch(
    run_lambda_selection(cand_stage1, n_ctrl, n_treated = n_treated),
    error = function(e) e
  )
  if (!inherits(selection_stage1, 'error')) {
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
      cat('  Refining lambda between', refine_lower, 'and', refine_upper, 'with', length(refine_grid), 'candidates\n')
      for (lam in refine_grid) try_lambda(lam, 'stage2')
    }
  } else {
    cat('  Stage-1 selection produced no feasible lambda; skipping local refinement and proceeding to fallback expansion if needed.\n')
  }

  cand_df <- if (length(candidates) > 0) do.call(rbind, candidates) else data.frame()
  cand_df <- cand_df[order(cand_df$lambda, decreasing = TRUE), , drop = FALSE]

  selection_result <- tryCatch(
    run_lambda_selection(cand_df, n_ctrl, n_treated = n_treated),
    error = function(e) e
  )
  if (inherits(selection_result, 'error')) {
    min_lambda <- min(cand_df$lambda, na.rm = TRUE)
    max_lambda <- max(cand_df$lambda, na.rm = TRUE)
    fallback_grid <- sort(unique(c(make_full_lambda_grid(), 1e-5, 1e-6, 0.3, 1, 3, 10)), decreasing = TRUE)
    fallback_grid <- fallback_grid[fallback_grid < min_lambda | fallback_grid > max_lambda]

    if (length(fallback_grid) > 0) {
      cat('  No feasible lambda yet; expanding bidirectional fallback grid:', paste(fallback_grid, collapse = ','), '\n')
      for (lam in fallback_grid) try_lambda(lam, 'fallback')
      cand_df <- if (length(candidates) > 0) do.call(rbind, candidates) else data.frame()
      cand_df <- cand_df[order(cand_df$lambda, decreasing = TRUE), , drop = FALSE]
    }

    selection_result <- tryCatch(
      run_lambda_selection(cand_df, n_ctrl, n_treated = n_treated),
      error = function(e) e
    )
  }

  if (inherits(selection_result, 'error')) {
    cat('  Lambda selection failed after fallback expansion: ', selection_result$message, '\n', sep = '')
    next
  }

  chosen_row <- selection_result$selected_row
  selection_log <- selection_result$selection_log

  rho <- chosen_row$lambda
  res <- fit_by_lambda[[lambda_key(rho)]]

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
  if (!is.null(selection_log$required_ess_floor) && is.finite(selection_log$required_ess_floor)) {
    cat('  ESS floor check: required >=', round(selection_log$required_ess_floor, 2),
        '; achieved =', round(chosen_row$ess, 2), '\n')
  }
  cat('  ESS/N(control) =', round(ess_ratio, 4), '(', ess_quality, ')\n')
  if (length(selection_log$warnings) > 0) {
    for (w in selection_log$warnings) cat('  WARNING:', w, '\n')
  }

  lambda_run_file <- file.path(lambda_run_out_dir, paste0('lambda_run_', treated.year, '_', area, '.rds'))
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

  smd_warn_threshold <- if (!is.null(cfg$lambda_selection) &&
    !is.null(cfg$lambda_selection$hard_gates) &&
    !is.null(cfg$lambda_selection$hard_gates$max_smd)) {
    as.numeric(cfg$lambda_selection$hard_gates$max_smd)
  } else {
    0.10
  }
  if (max_balance_std > smd_warn_threshold) {
    cat("  ⚠ WARNING: Max |balance.std| = ", round(max_balance_std, 3),
      " (recommend |SMD| < ", formatC(smd_warn_threshold, format = "f", digits = 2), ")\n", sep = "")
  }
  
  # Create weights table with explicit group-index mapping (avoids vector recycling).
  treated_idx_df <- which(df$treated == 1)
  control_idx_df <- which(df$treated == 0)
  weight <- rep(NA_real_, nrow(df))

  if (length(res$weights.1) == length(treated_idx_df)) {
    weight[treated_idx_df] <- as.numeric(res$weights.1)
  } else if (length(res$weights.1) == nrow(df)) {
    weight[treated_idx_df] <- as.numeric(res$weights.1[treated_idx_df])
  } else if (length(res$weights.1) == 1) {
    weight[treated_idx_df] <- as.numeric(res$weights.1)
  } else {
    stop('Unexpected treated-weight length in res$weights.1.')
  }

  if (length(res$weights.0) == length(control_idx_df)) {
    weight[control_idx_df] <- as.numeric(res$weights.0)
  } else if (length(res$weights.0) == nrow(df)) {
    weight[control_idx_df] <- as.numeric(res$weights.0[control_idx_df])
  } else if (length(res$weights.0) == 1) {
    weight[control_idx_df] <- as.numeric(res$weights.0)
  } else {
    stop('Unexpected control-weight length in res$weights.0.')
  }

  weights_df <- data.frame(
    unit = df$unit,
    treated = df$treated,
    weight = weight
  )

  # Persist design inputs with the fit so diagnostics can be rerun from fit files alone.
  res$X <- X
  res$W <- W

  # Save fit results and weights before running diagnostics to prioritize outputs
  rho_token <- lambda_key(rho)
  saveRDS(res, paste0(result_out_dir, "/cbps_fit_", treated.year, "_", area, "_rho", rho_token, ".RDS"))
  saveRDS(weights_df, paste0(result_out_dir, "/cbps_weights_", treated.year, "_", area, ".RDS"))
  cat("  Saved: cbps_fit_", treated.year, "_", area, "_rho", rho_token, ".RDS\n", sep = "")
  cat("  Saved: cbps_weights_", treated.year, "_", area, ".RDS\n", sep = "")

  run_covariate_diag <- function(write_lambda_plot, write_summary, fail_label) {
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
        write_lambda_plot = write_lambda_plot,
        run_prefit_overlap = FALSE,
        write_prepost_metrics = FALSE,
        write_distribution = FALSE,
        write_block_summary = FALSE,
        write_summary = write_summary
      )
    }, error = function(e) {
      cat("  ", fail_label, ": ", e$message, "\n", sep = "")
    })
  }

  # Optional lightweight lambda-path export without full covariate diagnostics.
  if (record_lambda_path && !record_covariate_diagnostics) {
    run_covariate_diag(
      write_lambda_plot = TRUE,
      write_summary = FALSE,
      fail_label = "Lambda-path plotting failed"
    )
  }

  if (record_covariate_diagnostics) {
    run_covariate_diag(
      write_lambda_plot = FALSE,
      write_summary = TRUE,
      fail_label = "Covariate diagnostics failed"
    )
  }
  
  gc()
  }, error = function(e) {
    cat("  ERROR processing year ", treated.year, ": ", e$message, "\n", sep = "")
  })
}

} # end guard for SKIP_IMPLEMENT_CBPS_MAIN