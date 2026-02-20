#!/usr/bin/env Rscript
## Phase 2: Statistical Efficiency Analysis
##
## Computes ATT estimates with confidence intervals for:
## 1. Baseline method (full control pool CBPS)
## 2. Embedding method (filtered control pool CBPS)
##
## Features:
## - Comprehensive error checking at each step
## - Detailed logging of operations
## - Validates inputs before processing
## - Creates checkpoints for intermediate outputs
##
## Outputs:
## - ATT estimates with CIs for each method
## - Variance comparison metrics
## - Precision gain (CI width reduction)
## - Efficiency gains (equivalent sample size increase)
##
## Usage:
##   Rscript scripts/06_compute_phase2_efficiency.R <year> <K> <post_treatment_years>
##
## Arguments:
##   year: Treatment year (e.g., 2019)
##   K: Optimal K value from Phase 1 (e.g., 50)
##   post_treatment_years: Comma-separated list of years (e.g., "2020,2021,2022")
##
## Example:
##   Rscript scripts/06_compute_phase2_efficiency.R 2019 50 "2020,2021,2022"

suppressPackageStartupMessages({
  library("dplyr")
  library("tidyr")
})

# Check for required packages
if (!requireNamespace("sandwich", quietly = TRUE)) {
  stop("Package 'sandwich' required. Install with: install.packages('sandwich')")
}

if (!requireNamespace("lmtest", quietly = TRUE)) {
  stop("Package 'lmtest' required. Install with: install.packages('lmtest')")
}

source("balancing/calculate_fire_outcomes.R")

# Error handling function
stop_with_error <- function(msg, ...) {
  formatted_msg <- sprintf(msg, ...)
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("✗ ERROR:", formatted_msg, "\n")
  cat(strrep("=", 80), "\n")
  cat("\n")
  stop(formatted_msg, call. = FALSE)
}

# Warning function
warn_with_message <- function(msg, ...) {
  formatted_msg <- sprintf(msg, ...)
  cat("⚠ WARNING:", formatted_msg, "\n")
  warning(formatted_msg, call. = FALSE, immediate. = TRUE)
}

# Success message function
success_message <- function(msg, ...) {
  formatted_msg <- sprintf(msg, ...)
  cat("✓", formatted_msg, "\n")
}

# Validate file exists and has data
validate_file <- function(filepath, description) {
  if (!file.exists(filepath)) {
    stop_with_error("Required file not found: %s\n  Description: %s\n  Make sure previous steps completed successfully", 
                   filepath, description)
  }
  
  file_size <- file.info(filepath)$size
  if (file_size == 0) {
    stop_with_error("File exists but is empty: %s\n  Description: %s", filepath, description)
  }
  
  success_message("Validated: %s (%s, %.1f KB)", description, basename(filepath), file_size / 1024)
  return(TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop_with_error("Insufficient arguments\nUsage: Rscript 06_compute_phase2_efficiency.R <year> <K> <post_treatment_years>")
}

treated_year <- as.integer(args[1])
optimal_K <- as.integer(args[2])
post_years_str <- args[3]

# Validate arguments
if (is.na(treated_year) || treated_year < 2000 || treated_year > 2025) {
  stop_with_error("Invalid year: %d (must be between 2000-2025)", treated_year)
}

if (is.na(optimal_K) || optimal_K < 1 || optimal_K > 1000) {
  stop_with_error("Invalid K value: %d (must be between 1-1000)", optimal_K)
}

# Parse post-treatment years
post_treatment_years <- tryCatch({
  as.integer(strsplit(post_years_str, ",")[[1]])
}, error = function(e) {
  stop_with_error("Failed to parse post-treatment years: %s\n  Expected format: '2020,2021,2022'", post_years_str)
})

if (any(is.na(post_treatment_years))) {
  stop_with_error("Invalid post-treatment years detected: %s", post_years_str)
}

cat(strrep("=", 80), "\n")
cat("PHASE 2: STATISTICAL EFFICIENCY ANALYSIS\n")
cat(strrep("=", 80), "\n")
cat("Treatment year:", treated_year, "\n")
cat("Optimal K:", optimal_K, "\n")
cat("Post-treatment years:", paste(post_treatment_years, collapse = ", "), "\n")
cat("R version:", R.version.string, "\n")
cat("Working directory:", getwd(), "\n")
cat("\n")

# Setup output directory
output_dir <- paste0("data/phase2_efficiency/", treated_year, "/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# STEP 1: Load CBPS weights for both methods
# ============================================================================

cat(strrep("=", 80), "\n")
cat("STEP 1: Loading CBPS weights\n")
cat(strrep("=", 80), "\n")

# Baseline weights (full pool)
baseline_weights_file <- paste0("data/outputs/balance/cbps_weights_", treated_year, "_conifer.RDS")

cat("Loading baseline weights...\n")
validate_file(baseline_weights_file, "Baseline CBPS weights")

weights_baseline_list <- tryCatch({
  readRDS(baseline_weights_file)
}, error = function(e) {
  stop_with_error("Failed to load baseline weights from %s\n  Error: %s", 
                 baseline_weights_file, e$message)
})

if (!("weights" %in% names(weights_baseline_list))) {
  stop_with_error("Baseline weights file does not contain 'weights' component\n  File: %s", 
                 baseline_weights_file)
}

weights_baseline <- weights_baseline_list$weights

# Validate baseline weights structure
required_cols <- c("unit", "treated", "weight")
missing_cols <- setdiff(required_cols, names(weights_baseline))
if (length(missing_cols) > 0) {
  stop_with_error("Baseline weights missing required columns: %s", 
                 paste(missing_cols, collapse = ", "))
}

cat("✓ Baseline weights loaded:", nrow(weights_baseline), "pixels\n")
cat("  Treated:", sum(weights_baseline$treated), "\n")
cat("  Control:", sum(weights_baseline$treated == 0), "\n")

# Embedding weights (filtered pool)
embedding_weights_file <- paste0("data/cbps_integration/", treated_year, 
                                "/cbps_weights_full_k", optimal_K, "_", treated_year, ".csv")

cat("\nLoading embedding weights...\n")
validate_file(embedding_weights_file, "Embedding CBPS weights")

weights_embedding <- tryCatch({
  read.csv(embedding_weights_file, stringsAsFactors = FALSE)
}, error = function(e) {
  stop_with_error("Failed to load embedding weights from %s\n  Error: %s", 
                 embedding_weights_file, e$message)
})

# Validate embedding weights structure
missing_cols <- setdiff(required_cols, names(weights_embedding))
if (length(missing_cols) > 0) {
  stop_with_error("Embedding weights missing required columns: %s", 
                 paste(missing_cols, collapse = ", "))
}

if (nrow(weights_embedding) == 0) {
  stop_with_error("Embedding weights file is empty: %s", embedding_weights_file)
}

cat("✓ Embedding weights loaded:", nrow(weights_embedding), "pixels\n")
cat("  Treated:", sum(weights_embedding$treated), "\n")
cat("  Control:", sum(weights_embedding$treated == 0), "\n")
cat("\n")

# ============================================================================
# STEP 2: Estimate ATT with confidence intervals - BASELINE
# ============================================================================

cat(strrep("=", 80), "\n")
cat("STEP 2: Estimating ATT with CI - BASELINE\n")
cat(strrep("=", 80), "\n")

att_baseline <- tryCatch({
  estimate_att_with_ci(
    weights_df = weights_baseline,
    outcome_years = post_treatment_years,
    treatment_year = treated_year,
    firms_rds_path = "data/processed_data/FIRMS.RDS",
    cluster_by_unit = TRUE
  )
}, error = function(e) {
  stop_with_error("Failed to estimate baseline ATT\n  Error: %s\n  Check FIRMS data and weights", 
                 e$message)
})

if (nrow(att_baseline) == 0) {
  warn_with_message("No baseline ATT estimates computed - check post-treatment data availability")
  att_baseline <- data.frame(
    year = integer(),
    treatment_year = integer(),
    att = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    ci_width = numeric(),
    n_treated = integer(),
    n_control = integer()
  )
}

att_baseline$method <- "baseline_full_pool"

cat("\nBaseline ATT Summary:\n")
print(att_baseline)
cat("\n")

# Save baseline ATT
baseline_att_file <- paste0(output_dir, "att_estimates_baseline_", treated_year, ".csv")
tryCatch({
  write.csv(att_baseline, baseline_att_file, row.names = FALSE)
}, error = function(e) {
  stop_with_error("Failed to save baseline ATT to %s\n  Error: %s", baseline_att_file, e$message)
})
validate_file(baseline_att_file, "Baseline ATT estimates")
success_message("Saved baseline ATT to: %s", baseline_att_file)
cat("\n")

# ============================================================================
# STEP 3: Estimate ATT with confidence intervals - EMBEDDING
# ============================================================================

cat(strrep("=", 80), "\n")
cat("STEP 3: Estimating ATT with CI - EMBEDDING (K=", optimal_K, ")\n")
cat(strrep("=", 80), "\n")

att_embedding <- tryCatch({
  estimate_att_with_ci(
    weights_df = weights_embedding,
    outcome_years = post_treatment_years,
    treatment_year = treated_year,
    firms_rds_path = "data/processed_data/FIRMS.RDS",
    cluster_by_unit = TRUE
  )
}, error = function(e) {
  stop_with_error("Failed to estimate embedding ATT\n  Error: %s\n  Check FIRMS data and weights", 
                 e$message)
})

if (nrow(att_embedding) == 0) {
  warn_with_message("No embedding ATT estimates computed - check post-treatment data availability")
  att_embedding <- data.frame(
    year = integer(),
    treatment_year = integer(),
    att = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    ci_width = numeric(),
    n_treated = integer(),
    n_control = integer()
  )
}

att_embedding$method <- paste0("embedding_k", optimal_K)

cat("\nEmbedding ATT Summary:\n")
print(att_embedding)
cat("\n")

# Save embedding ATT
embedding_att_file <- paste0(output_dir, "att_estimates_embedding_k", optimal_K, "_", treated_year, ".csv")
tryCatch({
  write.csv(att_embedding, embedding_att_file, row.names = FALSE)
}, error = function(e) {
  stop_with_error("Failed to save embedding ATT to %s\n  Error: %s", embedding_att_file, e$message)
})
validate_file(embedding_att_file, "Embedding ATT estimates")
success_message("Saved embedding ATT to: %s", embedding_att_file)
cat("\n")

# ============================================================================
# STEP 4: Compare efficiency metrics
# ============================================================================

cat(strrep("=", 80), "\n")
cat("STEP 4: Computing efficiency metrics\n")
cat(strrep("=", 80), "\n")

# Validate that we have data from both methods
if (nrow(att_baseline) == 0 && nrow(att_embedding) == 0) {
  stop_with_error("No ATT estimates available from either method - cannot compute efficiency metrics")
}

if (nrow(att_baseline) == 0) {
  warn_with_message("No baseline ATT estimates - efficiency metrics will be incomplete")
}

if (nrow(att_embedding) == 0) {
  warn_with_message("No embedding ATT estimates - efficiency metrics will be incomplete")
}

# Combine results
att_combined <- tryCatch({
  rbind(att_baseline, att_embedding)
}, error = function(e) {
  stop_with_error("Failed to combine ATT estimates\n  Error: %s", e$message)
})

  # Save att_combined as CSV for downstream visualization
  att_combined_file <- paste0(output_dir, "att_combined_", treated_year, ".csv")
  tryCatch({
    write.csv(att_combined, att_combined_file, row.names = FALSE)
  }, error = function(e) {
    stop_with_error("Failed to save att_combined to %s\n  Error: %s", att_combined_file, e$message)
  })
  validate_file(att_combined_file, "Combined ATT estimates (att_combined)")
  success_message("Saved att_combined to: %s", att_combined_file)
  cat("\n")

# Compute variance comparison for each year
variance_comparison_list <- list()

for (yr in post_treatment_years) {
  baseline_yr <- att_baseline[att_baseline$year == yr, ]
  embedding_yr <- att_embedding[att_embedding$year == yr, ]
  
  if (nrow(baseline_yr) == 0 || nrow(embedding_yr) == 0) {
    warn_with_message("Missing data for year %d - skipping efficiency comparison", yr)
    next
  }
  
  tryCatch({
    # Variance reduction
    var_baseline <- baseline_yr$se^2
    var_embedding <- embedding_yr$se^2
    var_reduction_pct <- 100 * (var_baseline - var_embedding) / var_baseline
    
    # CI width comparison
    ci_width_baseline <- baseline_yr$ci_width
    ci_width_embedding <- embedding_yr$ci_width
    ci_width_reduction_pct <- 100 * (ci_width_baseline - ci_width_embedding) / ci_width_baseline
    
    # Effective sample size increase
    # Variance reduction of X% is equivalent to increasing sample size by 1/(1-X%)
    effective_n_increase_pct <- if (var_reduction_pct < 100) {
      100 * (1 / (1 - var_reduction_pct/100) - 1)
    } else {
      NA  # Undefined if variance increased
    }
    
    # ATT agreement (sign and magnitude)
    att_sign_agreement <- sign(baseline_yr$att) == sign(embedding_yr$att)
    
    variance_comparison_list[[length(variance_comparison_list) + 1]] <- data.frame(
      year = yr,
      treatment_year = treated_year,
      att_baseline = baseline_yr$att,
      att_embedding = embedding_yr$att,
      att_diff = abs(baseline_yr$att - embedding_yr$att),
      att_sign_agreement = att_sign_agreement,
      se_baseline = baseline_yr$se,
      se_embedding = embedding_yr$se,
      se_reduction_pct = 100 * (baseline_yr$se - embedding_yr$se) / baseline_yr$se,
      var_baseline = var_baseline,
      var_embedding = var_embedding,
      var_reduction_pct = var_reduction_pct,
      ci_width_baseline = ci_width_baseline,
      ci_width_embedding = ci_width_embedding,
      ci_width_reduction_pct = ci_width_reduction_pct,
      effective_n_increase_pct = effective_n_increase_pct,
      n_control_baseline = baseline_yr$n_control,
      n_control_embedding = embedding_yr$n_control,
      control_pool_reduction_pct = 100 * (baseline_yr$n_control - embedding_yr$n_control) / baseline_yr$n_control,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    warn_with_message("Failed to compute efficiency metrics for year %d: %s", yr, e$message)
  })
}

# Combine variance comparison results
if (length(variance_comparison_list) == 0) {
  stop_with_error("No variance comparisons computed - no overlapping years with data from both methods")
}

variance_comparison <- tryCatch({
  do.call(rbind, variance_comparison_list)
}, error = function(e) {
  stop_with_error("Failed to combine variance comparisons\n  Error: %s", e$message)
})

# Print summary
cat("\nVariance Comparison Summary:\n")
print(variance_comparison[, c("year", "att_baseline", "att_embedding", 
                              "ci_width_reduction_pct", "effective_n_increase_pct")])
cat("\n")

# Save variance comparison
variance_file <- paste0(output_dir, "variance_comparison_", treated_year, ".csv")
tryCatch({
  write.csv(variance_comparison, variance_file, row.names = FALSE)
}, error = function(e) {
  stop_with_error("Failed to save variance comparison to %s\n  Error: %s", variance_file, e$message)
})
validate_file(variance_file, "Variance comparison")
success_message("Saved variance comparison to: %s", variance_file)
cat("\n")

# ============================================================================
# STEP 5: Compute precision gain summary
# ============================================================================

cat(strrep("=", 80), "\n")
cat("STEP 5: Precision gain summary\n")
cat(strrep("=", 80), "\n")

precision_summary <- tryCatch({
  data.frame(
    treatment_year = treated_year,
    optimal_K = optimal_K,
    n_years = length(post_treatment_years),
    mean_se_reduction_pct = mean(variance_comparison$se_reduction_pct, na.rm = TRUE),
    mean_var_reduction_pct = mean(variance_comparison$var_reduction_pct, na.rm = TRUE),
    mean_ci_width_reduction_pct = mean(variance_comparison$ci_width_reduction_pct, na.rm = TRUE),
    mean_effective_n_increase_pct = mean(variance_comparison$effective_n_increase_pct, na.rm = TRUE),
    att_sign_consistency_rate = mean(variance_comparison$att_sign_agreement, na.rm = TRUE),
    mean_control_pool_reduction_pct = mean(variance_comparison$control_pool_reduction_pct, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}, error = function(e) {
  stop_with_error("Failed to compute precision summary\n  Error: %s", e$message)
})

cat("\nPrecision Gain Summary:\n")
print(precision_summary)
cat("\n")

# Save precision summary
precision_file <- paste0(output_dir, "precision_gain_summary_", treated_year, ".csv")
tryCatch({
  write.csv(precision_summary, precision_file, row.names = FALSE)
}, error = function(e) {
  stop_with_error("Failed to save precision summary to %s\n  Error: %s", precision_file, e$message)
})
validate_file(precision_file, "Precision gain summary")
success_message("Saved precision summary to: %s", precision_file)
cat("\n")

# ============================================================================
# STEP 6: Summary report
# ============================================================================

cat(strrep("=", 80), "\n")
cat("PHASE 2 SUMMARY REPORT\n")
cat(strrep("=", 80), "\n")
cat("\n")

# Validate precision summary values
if (is.na(precision_summary$mean_ci_width_reduction_pct)) {
  warn_with_message("Mean CI width reduction is NA - using 0 for display")
  precision_summary$mean_ci_width_reduction_pct <- 0
}

if (is.na(precision_summary$mean_effective_n_increase_pct)) {
  warn_with_message("Mean effective sample size increase is NA - using 0 for display")
  precision_summary$mean_effective_n_increase_pct <- 0
}

if (is.infinite(precision_summary$mean_effective_n_increase_pct)) {
  warn_with_message("Mean effective sample size increase is infinite - capping at 999")
  precision_summary$mean_effective_n_increase_pct <- 999
}

cat("KEY FINDINGS:\n")
cat("-------------\n")
cat(sprintf("1. Mean CI width reduction: %.1f%%\n", precision_summary$mean_ci_width_reduction_pct))
cat(sprintf("2. Equivalent sample size increase: %.1f%%\n", precision_summary$mean_effective_n_increase_pct))
cat(sprintf("3. Control pool reduction: %.1f%%\n", precision_summary$mean_control_pool_reduction_pct))
cat(sprintf("4. ATT sign consistency: %.0f%%\n", precision_summary$att_sign_consistency_rate * 100))
cat("\n")

if (precision_summary$mean_ci_width_reduction_pct > 0) {
  success_message("SUCCESS: Embedding method improves statistical efficiency!")
  cat("  → Narrower confidence intervals indicate more precise estimates\n")
  cat("  → With", sprintf("%.1f%%", precision_summary$mean_control_pool_reduction_pct), 
      "fewer controls, we achieve", sprintf("%.1f%%", precision_summary$mean_ci_width_reduction_pct), 
      "narrower CIs\n")
} else {
  warn_with_message("CAUTION: Embedding method does not improve CI width")
  cat("  → May indicate insufficient control pool reduction benefit\n")
  cat("  → Consider investigating covariate balance vs sample size tradeoff\n")
}

cat("\n")
cat(strrep("=", 80), "\n")
success_message("PHASE 2 COMPLETE - Outputs saved to: %s", output_dir)
cat(strrep("=", 80), "\n")
cat("\n")
cat("Next steps:\n")
cat("1. Visualize trajectory plots: See scripts/figures/plot_trajectory.R\n")
cat("2. Create ATT forest plot: See scripts/figures/plot_att_forest.R\n")
cat("3. Run Phase 3 robustness checks: See scripts/07_run_phase3_robustness.R\n")
cat("\n")
