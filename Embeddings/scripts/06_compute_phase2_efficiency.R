#!/usr/bin/env Rscript
## Phase 2: Statistical Efficiency Analysis
##
## Computes ATT estimates with confidence intervals for:
## 1. Baseline method (full control pool CBPS)
## 2. Embedding method (filtered control pool CBPS)
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

source("balancing/calculate_fire_outcomes.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript 06_compute_phase2_efficiency.R <year> <K> <post_treatment_years>")
}

treated_year <- as.integer(args[1])
optimal_K <- as.integer(args[2])
post_years_str <- args[3]

# Parse post-treatment years
post_treatment_years <- as.integer(strsplit(post_years_str, ",")[[1]])

cat("="*80, "\n")
cat("PHASE 2: STATISTICAL EFFICIENCY ANALYSIS\n")
cat("="*80, "\n")
cat("Treatment year:", treated_year, "\n")
cat("Optimal K:", optimal_K, "\n")
cat("Post-treatment years:", paste(post_treatment_years, collapse = ", "), "\n")
cat("\n")

# Setup output directory
output_dir <- paste0("Embeddings/data/phase2_efficiency/", treated_year, "/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# STEP 1: Load CBPS weights for both methods
# ============================================================================

cat("="*80, "\n")
cat("STEP 1: Loading CBPS weights\n")
cat("="*80, "\n")

# Baseline weights (full pool)
baseline_weights_file <- paste0("data/outputs/balance/cbps_weights_", treated_year, "_conifer.RDS")

if (!file.exists(baseline_weights_file)) {
  stop(paste("Baseline weights not found:", baseline_weights_file, 
             "\nRun implement_cbps.R first!"))
}

weights_baseline_list <- readRDS(baseline_weights_file)
weights_baseline <- weights_baseline_list$weights

cat("✓ Baseline weights loaded:", nrow(weights_baseline), "pixels\n")
cat("  Treated:", sum(weights_baseline$treated), "\n")
cat("  Control:", sum(weights_baseline$treated == 0), "\n")

# Embedding weights (filtered pool)
embedding_weights_file <- paste0("Embeddings/data/cbps_integration/", treated_year, 
                                "/cbps_weights_full_k", optimal_K, "_", treated_year, ".csv")

if (!file.exists(embedding_weights_file)) {
  stop(paste("Embedding weights not found:", embedding_weights_file,
             "\nRun select_optimal_k.py and run_cbps_with_selected_controls.R first!"))
}

weights_embedding <- read.csv(embedding_weights_file, stringsAsFactors = FALSE)

cat("✓ Embedding weights loaded:", nrow(weights_embedding), "pixels\n")
cat("  Treated:", sum(weights_embedding$treated), "\n")
cat("  Control:", sum(weights_embedding$treated == 0), "\n")
cat("\n")

# ============================================================================
# STEP 2: Estimate ATT with confidence intervals - BASELINE
# ============================================================================

cat("="*80, "\n")
cat("STEP 2: Estimating ATT with CI - BASELINE\n")
cat("="*80, "\n")

att_baseline <- estimate_att_with_ci(
  weights_df = weights_baseline,
  outcome_years = post_treatment_years,
  treatment_year = treated_year,
  firms_rds_path = "data/processed_data/FIRMS.RDS",
  cluster_by_unit = TRUE
)

att_baseline$method <- "baseline_full_pool"

cat("\nBaseline ATT Summary:\n")
print(att_baseline)
cat("\n")

# Save baseline ATT
baseline_att_file <- paste0(output_dir, "att_estimates_baseline_", treated_year, ".csv")
write.csv(att_baseline, baseline_att_file, row.names = FALSE)
cat("✓ Saved baseline ATT to:", baseline_att_file, "\n\n")

# ============================================================================
# STEP 3: Estimate ATT with confidence intervals - EMBEDDING
# ============================================================================

cat("="*80, "\n")
cat("STEP 3: Estimating ATT with CI - EMBEDDING (K=", optimal_K, ")\n")
cat("="*80, "\n")

att_embedding <- estimate_att_with_ci(
  weights_df = weights_embedding,
  outcome_years = post_treatment_years,
  treatment_year = treated_year,
  firms_rds_path = "data/processed_data/FIRMS.RDS",
  cluster_by_unit = TRUE
)

att_embedding$method <- paste0("embedding_k", optimal_K)

cat("\nEmbedding ATT Summary:\n")
print(att_embedding)
cat("\n")

# Save embedding ATT
embedding_att_file <- paste0(output_dir, "att_estimates_embedding_k", optimal_K, "_", treated_year, ".csv")
write.csv(att_embedding, embedding_att_file, row.names = FALSE)
cat("✓ Saved embedding ATT to:", embedding_att_file, "\n\n")

# ============================================================================
# STEP 4: Compare efficiency metrics
# ============================================================================

cat("="*80, "\n")
cat("STEP 4: Computing efficiency metrics\n")
cat("="*80, "\n")

# Combine results
att_combined <- rbind(att_baseline, att_embedding)

# Compute variance comparison for each year
variance_comparison_list <- list()

for (yr in post_treatment_years) {
  baseline_yr <- att_baseline[att_baseline$year == yr, ]
  embedding_yr <- att_embedding[att_embedding$year == yr, ]
  
  if (nrow(baseline_yr) == 0 || nrow(embedding_yr) == 0) {
    warning(paste("Missing data for year", yr))
    next
  }
  
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
  effective_n_increase_pct <- 100 * (1 / (1 - var_reduction_pct/100) - 1)
  
  # ATT agreement (sign and magnitude)
  att_sign_agreement <- sign(baseline_yr$att) == sign(embedding_yr$att)
  att_correlation <- NA  # Will compute across years
  
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
}

variance_comparison <- do.call(rbind, variance_comparison_list)

# Print summary
cat("\nVariance Comparison Summary:\n")
print(variance_comparison[, c("year", "att_baseline", "att_embedding", 
                              "ci_width_reduction_pct", "effective_n_increase_pct")])
cat("\n")

# Save variance comparison
variance_file <- paste0(output_dir, "variance_comparison_", treated_year, ".csv")
write.csv(variance_comparison, variance_file, row.names = FALSE)
cat("✓ Saved variance comparison to:", variance_file, "\n\n")

# ============================================================================
# STEP 5: Compute precision gain summary
# ============================================================================

cat("="*80, "\n")
cat("STEP 5: Precision gain summary\n")
cat("="*80, "\n")

precision_summary <- data.frame(
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

cat("\nPrecision Gain Summary:\n")
print(precision_summary)
cat("\n")

# Save precision summary
precision_file <- paste0(output_dir, "precision_gain_summary_", treated_year, ".csv")
write.csv(precision_summary, precision_file, row.names = FALSE)
cat("✓ Saved precision summary to:", precision_file, "\n\n")

# ============================================================================
# STEP 6: Summary report
# ============================================================================

cat("="*80, "\n")
cat("PHASE 2 SUMMARY REPORT\n")
cat("="*80, "\n")
cat("\n")
cat("KEY FINDINGS:\n")
cat("-------------\n")
cat(sprintf("1. Mean CI width reduction: %.1f%%\n", precision_summary$mean_ci_width_reduction_pct))
cat(sprintf("2. Equivalent sample size increase: %.1f%%\n", precision_summary$mean_effective_n_increase_pct))
cat(sprintf("3. Control pool reduction: %.1f%%\n", precision_summary$mean_control_pool_reduction_pct))
cat(sprintf("4. ATT sign consistency: %.0f%%\n", precision_summary$att_sign_consistency_rate * 100))
cat("\n")

if (precision_summary$mean_ci_width_reduction_pct > 0) {
  cat("✓ SUCCESS: Embedding method improves statistical efficiency!\n")
  cat("  → Narrower confidence intervals indicate more precise estimates\n")
  cat("  → With", sprintf("%.1f%%", precision_summary$mean_control_pool_reduction_pct), 
      "fewer controls, we achieve", sprintf("%.1f%%", precision_summary$mean_ci_width_reduction_pct), 
      "narrower CIs\n")
} else {
  cat("⚠ CAUTION: Embedding method does not improve CI width\n")
  cat("  → May indicate insufficient control pool reduction benefit\n")
  cat("  → Consider investigating covariate balance vs sample size tradeoff\n")
}

cat("\n")
cat("="*80, "\n")
cat("PHASE 2 COMPLETE - Outputs saved to:", output_dir, "\n")
cat("="*80, "\n")
cat("\n")
cat("Next steps:\n")
cat("1. Visualize trajectory plots: See scripts/figures/plot_trajectory.R\n")
cat("2. Create ATT forest plot: See scripts/figures/plot_att_forest.R\n")
cat("3. Run Phase 3 robustness checks: See scripts/07_run_phase3_robustness.R\n")
cat("\n")
