#' Comprehensive CBPS Weight Diagnostics
#'
#' Purpose: Identify why embedding-selected controls produce NaN/Inf weights when baseline (full pool) produces valid weights.
#' Usage: Rscript diagnose_cbps_weights.R <year> [selected_units_csv]
#' Outputs: Covariate balance comparison, separation/extreme value detection, propensity diagnostics, and sample size checks.
#!/usr/bin/env Rscript
## Comprehensive CBPS Weight Diagnostics
## 
## Purpose: Identify why embedding-selected controls produce NaN/Inf weights
##          when baseline (full pool) produces valid weights
##
## Usage:
##   Rscript diagnose_cbps_weights.R <year> [selected_units_csv]
##
## Arguments:
##   year: Treatment year (e.g., 2019)
##   selected_units_csv (optional): Path to embedding-selected control units
##                                  If omitted, compares transformations only
##
## Outputs:
##   - Covariate balance comparison (full vs embedding pool)
##   - Perfect separation detection
##   - Extreme value detection
##   - Propensity score diagnostics
##   - Sample size adequacy checks

suppressPackageStartupMessages({
  library("dplyr")
  library("tidyr")
})

source("balancing/cli_utils.R")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

compute_smd <- function(treated_vals, control_vals) {
  # Compute standardized mean difference
  mean_t <- mean(treated_vals, na.rm = TRUE)
  mean_c <- mean(control_vals, na.rm = TRUE)
  var_t <- var(treated_vals, na.rm = TRUE)
  var_c <- var(control_vals, na.rm = TRUE)
  
  # Handle edge cases
  if (is.na(var_t)) var_t <- 0
  if (is.na(var_c)) var_c <- 0
  
  sd_pooled <- sqrt((var_t + var_c) / 2)
  
  if (sd_pooled == 0 || is.na(sd_pooled)) {
    return(NA_real_)
  }
  
  return((mean_t - mean_c) / sd_pooled)
}

check_separation <- function(treated_vals, control_vals, covariate_name) {
  # Check for perfect separation (range-based or binary)
  
  # Remove NAs
  treated_vals <- treated_vals[!is.na(treated_vals)]
  control_vals <- control_vals[!is.na(control_vals)]
  
  if (length(treated_vals) == 0 || length(control_vals) == 0) {
    return(list(
      type = "insufficient_data",
      separated = TRUE,
      details = "Empty group after NA removal"
    ))
  }
  
  # Get ranges
  min_t <- min(treated_vals)
  max_t <- max(treated_vals)
  min_c <- min(control_vals)
  max_c <- max(control_vals)
  
  # Check for non-overlapping ranges
  no_overlap <- (max_t < min_c) || (max_c < min_t)
  
  if (no_overlap) {
    return(list(
      type = "range_separation",
      separated = TRUE,
      details = sprintf("Treated [%.3f, %.3f], Control [%.3f, %.3f] - NO OVERLAP",
                       min_t, max_t, min_c, max_c)
    ))
  }
  
  # Check for binary with perfect separation
  unique_t <- unique(treated_vals)
  unique_c <- unique(control_vals)
  
  if (length(unique_t) <= 2 && length(unique_c) <= 2) {
    common_vals <- intersect(unique_t, unique_c)
    
    if (length(common_vals) == 0) {
      return(list(
        type = "binary_separation",
        separated = TRUE,
        details = sprintf("Binary: Treated=%s, Control=%s - NO OVERLAP",
                         paste(unique_t, collapse=","),
                         paste(unique_c, collapse=","))
      ))
    }
  }
  
  # Calculate overlap percentage (used in multiple checks below)
  overlap_range <- c(max(min_t, min_c), min(max_t, max_c))
  overlap_width <- overlap_range[2] - overlap_range[1]
  full_range <- max(max_t, max_c) - min(min_t, min_c)
  
  overlap_pct <- 0.0  # Default if full_range is 0
  if (full_range > 0) {
    overlap_pct <- overlap_width / full_range
    
    if (overlap_pct < 0.05) {
      return(list(
        type = "quasi_separation",
        separated = TRUE,
        details = sprintf("Only %.1f%% overlap in ranges", overlap_pct * 100)
      ))
    }
  }
  
  # Check if all treated are clustered at one extreme
  treated_in_top_10pct <- mean(treated_vals > quantile(control_vals, 0.9, na.rm = TRUE))
  treated_in_bottom_10pct <- mean(treated_vals < quantile(control_vals, 0.1, na.rm = TRUE))
  
  if (treated_in_top_10pct > 0.9) {
    return(list(
      type = "extreme_clustering",
      separated = TRUE,
      details = sprintf("%.0f%% of treated in top 10%% of control distribution",
                       treated_in_top_10pct * 100)
    ))
  }
  
  if (treated_in_bottom_10pct > 0.9) {
    return(list(
      type = "extreme_clustering",
      separated = TRUE,
      details = sprintf("%.0f%% of treated in bottom 10%% of control distribution",
                       treated_in_bottom_10pct * 100)
    ))
  }
  
  # No separation detected
  return(list(
    type = "no_separation",
    separated = FALSE,
    details = sprintf("Overlap OK: %.1f%% common range", overlap_pct * 100)
  ))
}

check_extreme_values <- function(values, covariate_name) {
  # Check for extreme values that might cause numerical issues
  
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(list(has_issues = FALSE, details = "No data"))
  }
  
  issues <- list()
  
  # Check for infinities
  if (any(is.infinite(values))) {
    n_inf <- sum(is.infinite(values))
    issues <- c(issues, sprintf("%d infinite values", n_inf))
  }
  
  # Check for very large values (> 1e10)
  if (any(abs(values) > 1e10, na.rm = TRUE)) {
    n_large <- sum(abs(values) > 1e10, na.rm = TRUE)
    max_val <- max(abs(values), na.rm = TRUE)
    issues <- c(issues, sprintf("%d values > 1e10 (max: %.2e)", n_large, max_val))
  }
  
  # Check for very small non-zero values (< 1e-10)
  non_zero <- values[values != 0]
  if (length(non_zero) > 0 && any(abs(non_zero) < 1e-10)) {
    n_small <- sum(abs(non_zero) < 1e-10)
    min_val <- min(abs(non_zero), na.rm = TRUE)
    issues <- c(issues, sprintf("%d tiny non-zero values < 1e-10 (min: %.2e)", n_small, min_val))
  }
  
  # Check for extreme range (span > 1e12)
  value_range <- max(values, na.rm = TRUE) - min(values, na.rm = TRUE)
  if (value_range > 1e12) {
    issues <- c(issues, sprintf("Extreme range: %.2e", value_range))
  }
  
  return(list(
    has_issues = length(issues) > 0,
    details = if (length(issues) > 0) paste(issues, collapse = "; ") else "OK"
  ))
}

apply_transformations <- function(X) {
  # Apply same transformations as implement_cbps.R and run_cbps_with_selected_controls.R
  
  cat("\nApplying transformations...\n")
  
  # Track transformations
  transform_log <- list()
  
  # Two-part SWE: presence indicator + log-intensity (winsorized)
  swe_cols <- grep("^swe_", colnames(X), value = TRUE)
  if (length(swe_cols) > 0) {
    cols_to_remove <- c()
    cols_converted <- 0
    
    for (col in swe_cols) {
      x <- X[[col]]
      pct_zero <- sum(x == 0 | is.na(x), na.rm = TRUE) / length(x)
      
      if (pct_zero > 0.95) {
        cols_to_remove <- c(cols_to_remove, col)
      } else {
        X[[paste0(col, "_present")]] <- as.numeric(x > 0)
        
        x_pos <- ifelse(x > 0, log1p(x), 0)
        pos_vals <- x_pos[x_pos > 0]
        if (length(pos_vals) > 0) {
          p995 <- quantile(pos_vals, 0.995, na.rm = TRUE)
          if (!is.na(p995)) {
            x_pos[x_pos > p995] <- p995
          }
        }
        
        X[[col]] <- x_pos
        cols_converted <- cols_converted + 1
      }
    }
    
    if (length(cols_to_remove) > 0) {
      X <- X[, !colnames(X) %in% cols_to_remove]
      cat("  SWE: Removed", length(cols_to_remove), "sparse columns (>95% zero)\n")
      transform_log$swe_removed <- length(cols_to_remove)
    }
    
    if (cols_converted > 0) {
      cat("  SWE: Two-part transformation on", cols_converted, "columns\n")
      transform_log$swe_converted <- cols_converted
    }
  }
  
  # Log1p + winsorize max_FRP_*
  frp_cols <- grep("^max_FRP_", colnames(X), value = TRUE)
  if (length(frp_cols) > 0) {
    for (col in frp_cols) {
      x <- X[[col]]
      if (all(is.na(x))) next
      
      x <- log1p(x)
      p995 <- quantile(x, 0.995, na.rm = TRUE)
      if (!is.na(p995)) {
        x[x > p995] <- p995
      }
      X[[col]] <- x
    }
    cat("  FRP: Log+winsorized", length(frp_cols), "columns (99.5%)\n")
    transform_log$frp_transformed <- length(frp_cols)
  }
  
  # Log1p + winsorize prcp_* and avg_BRIGHTNESS_*
  prcp_cols <- grep("^prcp_", colnames(X), value = TRUE)
  bright_cols <- grep("^avg_BRIGHTNESS_", colnames(X), value = TRUE)
  for (col in c(prcp_cols, bright_cols)) {
    x <- X[[col]]
    if (all(is.na(x))) next
    
    x <- log1p(x)
    p995 <- quantile(x, 0.995, na.rm = TRUE)
    if (!is.na(p995)) {
      x[x > p995] <- p995
    }
    X[[col]] <- x
  }
  if (length(prcp_cols) + length(bright_cols) > 0) {
    cat("  Other: Log+winsorized", length(prcp_cols), "prcp and",
        length(bright_cols), "BRIGHTNESS columns\n")
    transform_log$other_transformed <- length(prcp_cols) + length(bright_cols)
  }
  
  # Drop extremely sparse fire_* columns
  fire_cols <- grep("^fire_", colnames(X), value = TRUE)
  if (length(fire_cols) > 0) {
    sparse_fire <- c()
    for (col in fire_cols) {
      p_one <- mean(X[[col]] > 0, na.rm = TRUE)
      if (!is.na(p_one) && p_one < 0.005) {
        sparse_fire <- c(sparse_fire, col)
      }
    }
    if (length(sparse_fire) > 0) {
      X <- X[, !colnames(X) %in% sparse_fire]
      cat("  Fire: Dropped", length(sparse_fire), "sparse columns (<0.5% ones)\n")
      transform_log$fire_dropped <- length(sparse_fire)
    }
  }
  
  return(list(X = X, log = transform_log))
}

# ============================================================================
# MAIN DIAGNOSTIC FUNCTION
# ============================================================================

diagnose_cbps_data <- function(year, selected_units_path = NULL) {
  
  cat("============================================================\n")
  cat("CBPS WEIGHT DIAGNOSTIC FOR YEAR", year, "\n")
  cat("============================================================\n\n")
  
  # Load full dataset
  outDir <- "data/processed_data/rev_analysis_low/"
  input_file <- paste0(outDir, "analysis_treated", year, "_conifer.RDS")
  
  if (!file.exists(input_file)) {
    stop(paste("Input file not found:", input_file))
  }
  
  df_full <- readRDS(input_file)
  
  cat("Dataset loaded:\n")
  cat("  Total pixels:", nrow(df_full), "\n")
  cat("  Treated:", sum(df_full$treated), "\n")
  cat("  Control:", sum(df_full$treated == 0), "\n\n")
  
  # Create comparison datasets
  datasets <- list()
  
  # Baseline: full pool
  datasets$full <- list(
    name = "Full Pool (Baseline)",
    data = df_full,
    n_control = sum(df_full$treated == 0)
  )
  
  # Embedding: filtered pool (if selected_units provided)
  if (!is.null(selected_units_path)) {
    if (file.exists(selected_units_path)) {
      selected_units <- read.csv(selected_units_path, stringsAsFactors = FALSE)
      
      if (!"unit" %in% names(selected_units)) {
        stop("Selected units CSV must have 'unit' column")
      }
      
      df_embedding <- df_full %>%
        filter(.data$treated == 1 | .data$unit %in% selected_units$unit)
      
      datasets$embedding <- list(
        name = "Embedding-Selected Pool",
        data = df_embedding,
        n_control = sum(df_embedding$treated == 0)
      )
      
      cat("Loaded embedding-selected controls:", nrow(selected_units), "\n")
      cat("  After filtering:", sum(df_embedding$treated == 0), "controls\n\n")
    } else {
      cat("WARNING: Selected units file not found, analyzing baseline only\n\n")
    }
  }
  
  # ============================================================================
  # ANALYSIS 1: Pre-transformation covariate balance
  # ============================================================================
  
  cat("\n")
  cat("============================================================\n")
  cat("ANALYSIS 1: PRE-TRANSFORMATION COVARIATE BALANCE\n")
  cat("============================================================\n\n")
  
  # Get covariate columns (before transformations)
  covariate_cols <- setdiff(names(df_full), 
                            c("unit", "LATITUDE", "LONGITUDE", "treated", "num.fire"))
  covariate_cols <- covariate_cols[sapply(df_full[covariate_cols], is.numeric)]
  
  cat("Analyzing", length(covariate_cols), "numeric covariates\n\n")
  
  # Compute balance for each dataset
  balance_results <- list()
  
  for (ds_name in names(datasets)) {
    ds <- datasets[[ds_name]]
    cat("--- ", ds$name, " ---\n")
    
    treated_vals_list <- list()
    control_vals_list <- list()
    smd_vals <- numeric(length(covariate_cols))
    separation_flags <- logical(length(covariate_cols))
    separation_types <- character(length(covariate_cols))
    extreme_flags <- logical(length(covariate_cols))
    
    for (i in seq_along(covariate_cols)) {
      cov <- covariate_cols[i]
      
      treated_vals <- ds$data[[cov]][ds$data$treated == 1]
      control_vals <- ds$data[[cov]][ds$data$treated == 0]
      
      treated_vals_list[[cov]] <- treated_vals
      control_vals_list[[cov]] <- control_vals
      
      # Compute SMD
      smd_vals[i] <- compute_smd(treated_vals, control_vals)
      
      # Check separation
      sep_result <- check_separation(treated_vals, control_vals, cov)
      separation_flags[i] <- sep_result$separated
      separation_types[i] <- sep_result$type
      
      # Check extreme values
      extreme_result <- check_extreme_values(c(treated_vals, control_vals), cov)
      extreme_flags[i] <- extreme_result$has_issues
    }
    
    balance_results[[ds_name]] <- data.frame(
      covariate = covariate_cols,
      smd = smd_vals,
      separation = separation_flags,
      separation_type = separation_types,
      extreme_values = extreme_flags,
      stringsAsFactors = FALSE
    )
    
    # Summary statistics
    cat("  SMD statistics:\n")
    cat("    Mean |SMD|:", round(mean(abs(smd_vals), na.rm = TRUE), 3), "\n")
    cat("    Median |SMD|:", round(median(abs(smd_vals), na.rm = TRUE), 3), "\n")
    cat("    Max |SMD|:", round(max(abs(smd_vals), na.rm = TRUE), 3), "\n")
    cat("    # with |SMD| > 0.25:", sum(abs(smd_vals) > 0.25, na.rm = TRUE), "\n")
    cat("    # with |SMD| > 0.50:", sum(abs(smd_vals) > 0.5, na.rm = TRUE), "\n")
    cat("    # with |SMD| > 1.00:", sum(abs(smd_vals) > 1.0, na.rm = TRUE), "\n")
    
    cat("\n  Separation issues:\n")
    cat("    # with separation:", sum(separation_flags), "\n")
    if (sum(separation_flags) > 0) {
      sep_summary <- table(separation_types[separation_flags])
      for (st in names(sep_summary)) {
        cat("      -", st, ":", sep_summary[st], "\n")
      }
    }
    
    cat("\n  Extreme value issues:\n")
    cat("    # with extreme values:", sum(extreme_flags), "\n\n")
  }
  
  # ============================================================================
  # ANALYSIS 2: Compare full vs embedding balance
  # ============================================================================
  
  if (length(datasets) > 1) {
    cat("\n")
    cat("============================================================\n")
    cat("ANALYSIS 2: FULL vs EMBEDDING POOL COMPARISON\n")
    cat("============================================================\n\n")
    
    comparison <- balance_results$full %>%
      left_join(balance_results$embedding, by = "covariate", suffix = c("_full", "_emb"))
    
    # Find new separation issues
    new_separation <- comparison %>%
      filter(!.data$separation_full & .data$separation_emb) %>%
      arrange(desc(abs(.data$smd_emb)))
    
    if (nrow(new_separation) > 0) {
      cat("❌ CRITICAL: Found", nrow(new_separation), "covariates with NEW separation in embedding pool!\n\n")
      
      cat("Top 10 problematic covariates:\n")
      for (i in seq_len(min(10, nrow(new_separation)))) {
        row <- new_separation[i, ]
        cat(sprintf("  %d. %s\n", i, row$covariate))
        cat(sprintf("     SMD: %.3f (full) → %.3f (embedding)\n", 
                   row$smd_full, row$smd_emb))
        cat(sprintf("     Type: %s\n", row$separation_type_emb))
        cat("\n")
      }
      
      cat("\n💡 ROOT CAUSE: Embedding selection creates separation on these covariates!\n")
      cat("   The K-NN filter optimizes for pre-treatment similarity (embedding space)\n")
      cat("   but inadvertently removes controls that bridge covariate gaps.\n\n")
    } else {
      cat("✓ No NEW separation in embedding pool vs full pool\n\n")
    }
    
    # Find worsening imbalances
    worsening <- comparison %>%
      filter(!is.na(.data$smd_full) & !is.na(.data$smd_emb)) %>%
      mutate(smd_change = abs(.data$smd_emb) - abs(.data$smd_full)) %>%
      filter(.data$smd_change > 0.1) %>%
      arrange(desc(.data$smd_change))
    
    if (nrow(worsening) > 0) {
      cat("⚠ ", nrow(worsening), "covariates with WORSENING imbalance (Δ|SMD| > 0.1):\n\n")
      
      for (i in seq_len(min(10, nrow(worsening)))) {
        row <- worsening[i, ]
        cat(sprintf("  %d. %s: |SMD| %.3f → %.3f (Δ = +%.3f)\n",
                   i, row$covariate, abs(row$smd_full), abs(row$smd_emb), row$smd_change))
      }
      cat("\n")
    } else {
      cat("✓ No major worsening of covariate balance\n\n")
    }
  }
  
  # ============================================================================
  # ANALYSIS 3: Post-transformation balance
  # ============================================================================
  
  cat("\n")
  cat("============================================================\n")
  cat("ANALYSIS 3: POST-TRANSFORMATION COVARIATE BALANCE\n")
  cat("============================================================\n\n")
  
  post_transform_results <- list()
  
  for (ds_name in names(datasets)) {
    ds <- datasets[[ds_name]]
    cat("--- ", ds$name, " ---\n")
    
    # Extract covariates
    X <- ds$data
    X$unit <- NULL
    X$LATITUDE <- NULL
    X$LONGITUDE <- NULL
    X$treated <- NULL
    X$num.fire <- NULL
    X <- X[, sapply(X, is.numeric), drop = FALSE]
    X <- X[, apply(X, 2, sd, na.rm = TRUE) > 0, drop = FALSE]
    
    cat("  Before transformation:", ncol(X), "covariates\n")
    
    # Apply transformations
    result <- apply_transformations(X)
    X_transformed <- result$X
    W <- ds$data$treated
    
    cat("  After transformation:", ncol(X_transformed), "covariates\n")
    
    # Check sample size adequacy
    n_treated <- sum(W)
    n_control <- sum(1 - W)
    n_covariates <- ncol(X_transformed)
    
    cat("\n  Sample size diagnostics:\n")
    cat("    Control:Treated ratio:", round(n_control / n_treated, 1), "× (recommend ≥ 10×)\n")
    cat("    Control:Covariates ratio:", round(n_control / n_covariates, 1), 
        "obs/cov (recommend ≥ 5)\n")
    
    if (n_control < 10 * n_treated) {
      cat("    ⚠ WARNING: Low control:treated ratio may cause weight instability\n")
    }
    
    if (n_control / n_covariates < 5) {
      cat("    ⚠ WARNING: Low obs:covariate ratio may cause overfitting\n")
    }
    
    # Compute post-transformation SMD
    post_smd <- numeric(ncol(X_transformed))
    post_sep_flags <- logical(ncol(X_transformed))
    post_extreme_flags <- logical(ncol(X_transformed))
    
    for (i in seq_len(ncol(X_transformed))) {
      cov <- colnames(X_transformed)[i]
      
      treated_vals <- X_transformed[[cov]][W == 1]
      control_vals <- X_transformed[[cov]][W == 0]
      
      post_smd[i] <- compute_smd(treated_vals, control_vals)
      
      sep_result <- check_separation(treated_vals, control_vals, cov)
      post_sep_flags[i] <- sep_result$separated
      
      extreme_result <- check_extreme_values(c(treated_vals, control_vals), cov)
      post_extreme_flags[i] <- extreme_result$has_issues
    }
    
    cat("\n  Post-transformation SMD:\n")
    cat("    Mean |SMD|:", round(mean(abs(post_smd), na.rm = TRUE), 3), "\n")
    cat("    Median |SMD|:", round(median(abs(post_smd), na.rm = TRUE), 3), "\n")
    cat("    Max |SMD|:", round(max(abs(post_smd), na.rm = TRUE), 3), "\n")
    cat("    # still with |SMD| > 0.25:", sum(abs(post_smd) > 0.25, na.rm = TRUE), "\n")
    cat("    # still with |SMD| > 1.00:", sum(abs(post_smd) > 1.0, na.rm = TRUE), "\n")
    cat("    # still with separation:", sum(post_sep_flags), "\n")
    cat("    # still with extreme values:", sum(post_extreme_flags), "\n\n")
    
    post_transform_results[[ds_name]] <- data.frame(
      covariate = colnames(X_transformed),
      smd = post_smd,
      separation = post_sep_flags,
      extreme_values = post_extreme_flags,
      stringsAsFactors = FALSE
    )
    
    # Identify covariates that STILL have separation after transformations
    if (sum(post_sep_flags) > 0) {
      cat("  ⚠ Covariates STILL separated after transformations:\n")
      still_separated <- colnames(X_transformed)[post_sep_flags]
      for (cov in head(still_separated, 10)) {
        cat("    -", cov, "(SMD =", round(post_smd[which(colnames(X_transformed) == cov)], 3), ")\n")
      }
      cat("\n")
    }
  }
  
  # ============================================================================
  # ANALYSIS 4: Identify root cause
  # ============================================================================
  
  cat("\n")
  cat("============================================================\n")
  cat("ANALYSIS 4: ROOT CAUSE ANALYSIS\n")
  cat("============================================================\n\n")
  
  if (length(datasets) > 1) {
    # Compare post-transformation results
    full_still_sep <- post_transform_results$full %>% filter(.data$separation)
    emb_still_sep <- post_transform_results$embedding %>% filter(.data$separation)
    
    new_sep_post_transform <- setdiff(emb_still_sep$covariate, full_still_sep$covariate)
    
    if (length(new_sep_post_transform) > 0) {
      cat("❌ SMOKING GUN: ", length(new_sep_post_transform), 
          " covariates have separation in embedding pool EVEN AFTER transformations:\n\n")
      
      for (cov in new_sep_post_transform) {
        smd_val <- emb_still_sep$smd[emb_still_sep$covariate == cov]
        cat("  •", cov, "(post-transform |SMD| =", round(abs(smd_val), 3), ")\n")
      }
      
      cat("\n💡 DIAGNOSIS:\n")
      cat("   These covariates have perfect/quasi-perfect separation in the embedding-selected\n")
      cat("   pool DESPITE the transformations (two-part SWE, log+winsorize, sparse removal).\n")
      cat("   This causes CBPS to produce extreme weights (attempting to bridge unbridgeable gap),\n")
      cat("   which overflow to NaN/Inf during weight computation.\n\n")
      
      cat("💊 RECOMMENDED SOLUTIONS (in order of preference):\n")
      cat("   1. INCREASE K: Larger K → more controls → better overlap → valid weights\n")
      cat("      Try K=100 or K=150 to see if adding more controls fills the covariate gaps\n\n")
      
      cat("   2. LOWER min_ratio: Accept smaller control pools (e.g., min_ratio=5 instead of 10)\n")
      cat("      This allows K=10 (currently rejected) which might have better overlap\n\n")
      
      cat("   3. ADD COVARIATE PRUNING: Remove covariates with separation BEFORE CBPS\n")
      cat("      In run_cbps_with_selected_controls.R, add:\n")
      cat("      ```\n")
      cat("      # Remove separated covariates\n")
      cat("      for (col in colnames(X_scl)) {\n")
      cat("        t_vals <- X_scl[W == 1, col]\n")
      cat("        c_vals <- X_scl[W == 0, col]\n")
      cat("        if (max(t_vals) < min(c_vals) || max(c_vals) < min(t_vals)) {\n")
      cat("          X_scl <- X_scl[, colnames(X_scl) != col]\n")
      cat("        }\n")
      cat("      }\n")
      cat("      ```\n\n")
      
      cat("   4. WEIGHT TRUNCATION: Cap extreme weights BEFORE they overflow\n")
      cat("      Add to run_cbps_with_selected_controls.R after CBPS:\n")
      cat("      ```\n")
      cat("      # Cap extreme weights\n")
      cat("      weights_0 <- res$weights.0\n")
      cat("      weights_0[weights_0 > median(weights_0) * 100] <- median(weights_0) * 100\n")
      cat("      weights_0[is.na(weights_0) | is.infinite(weights_0)] <- 0\n")
      cat("      res$weights.0 <- weights_0\n")
      cat("      ```\n\n")
      
    } else if (nrow(emb_still_sep) > nrow(full_still_sep)) {
      cat("⚠ FINDING: Embedding pool has more separated covariates than full pool\n")
      cat("   (", nrow(emb_still_sep), "vs", nrow(full_still_sep), "separated covariates)\n\n")
      
      cat("   This suggests embedding selection worsens separation, but transformations\n")
      cat("   aren't sufficient to fix it. Try solutions 1-4 above.\n\n")
      
    } else {
      cat("✓ Separation patterns similar between full and embedding pools\n")
      cat("  This suggests a different issue causing weight blow-up.\n\n")
      
      cat("  Possible causes:\n")
      cat("  - Numerical instability in CBPS optimization (try different rho values)\n")
      cat("  - Very high-dimensional covariate space (", ncol(post_transform_results$embedding), 
          "covariates) relative to sample size\n")
      cat("  - Check if baseline CBPS succeeded with similar sample size\n\n")
    }
  } else {
    cat("Only baseline analyzed. Provide selected_units_csv to compare.\n\n")
  }
  
  # ============================================================================
  # Save results
  # ============================================================================
  
  cat("\n")
  cat("============================================================\n")
  cat("SAVING DIAGNOSTIC RESULTS\n")
  cat("============================================================\n\n")
  
  output_dir <- "Embeddings/logs"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save pre-transformation balance
  for (ds_name in names(balance_results)) {
    output_file <- file.path(output_dir, sprintf("diagnosis_%s_pretransform_%d.csv", ds_name, year))
    write.csv(balance_results[[ds_name]], output_file, row.names = FALSE)
    cat("  Saved:", output_file, "\n")
  }
  
  # Save post-transformation balance
  for (ds_name in names(post_transform_results)) {
    output_file <- file.path(output_dir, sprintf("diagnosis_%s_posttransform_%d.csv", ds_name, year))
    write.csv(post_transform_results[[ds_name]], output_file, row.names = FALSE)
    cat("  Saved:", output_file, "\n")
  }
  
  # Save comparison if available
  if (length(datasets) > 1) {
    write.csv(comparison, 
              file.path(output_dir, sprintf("diagnosis_comparison_%d.csv", year)),
              row.names = FALSE)
    cat("  Saved: diagnosis_comparison_", year, ".csv\n", sep = "")
  }
  
  cat("\n✓ Diagnostic complete!\n\n")
}

# ============================================================================
# COMMAND-LINE INTERFACE
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("Usage: Rscript diagnose_cbps_weights.R <year> [selected_units_csv]\n\n")
  cat("Arguments:\n")
  cat("  year               : Treatment year (e.g., 2019)\n")
  cat("  selected_units_csv : Path to embedding-selected controls (optional)\n\n")
  cat("Examples:\n")
  cat("  Rscript diagnose_cbps_weights.R 2019\n")
  cat("  Rscript diagnose_cbps_weights.R 2019 Embeddings/data/cbps_integration/2019/selected_controls_k20_2019.csv\n\n")
  quit(status = 1)
}

parsed_years <- parse_years_list(args[1], "positional <year>")
if (length(parsed_years) != 1) {
  stop("Please provide exactly one treatment year as the first positional argument")
}
year <- parsed_years[1]
selected_units_path <- if (length(args) >= 2 && nzchar(args[2])) args[2] else NULL

diagnose_cbps_data(year, selected_units_path)
