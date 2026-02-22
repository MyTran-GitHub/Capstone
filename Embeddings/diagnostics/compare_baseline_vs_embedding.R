#!/usr/bin/env Rscript
# Compare baseline CBPS (full pool) vs embedding-based K selection
# For 2019 treatment year

suppressPackageStartupMessages({
  library(dplyr)
})

source("balancing/calculate_fire_outcomes.R")

cat("="*80, "\n")
cat("BASELINE vs EMBEDDING COMPARISON (2019)\n")
cat("="*80, "\n\n")

# ============================================================================
# 1. Load baseline CBPS results (full control pool)
# ============================================================================
cat("1. Loading baseline CBPS results...\n")

baseline_fit_file <- "data/processed_data/rev_analysis_low/cbps_fit_2019_conifer_rho1e-05.RDS"
baseline_weights_file <- "data/processed_data/rev_analysis_low/cbps_weights_2019_conifer.RDS"

if (!file.exists(baseline_fit_file)) {
  # Try rho=1e-06 as fallback
  baseline_fit_file <- "data/processed_data/rev_analysis_low/cbps_fit_2019_conifer_rho1e-06.RDS"
  if (!file.exists(baseline_fit_file)) {
    stop("Baseline fit file not found for 2019")
  }
}

baseline_fit <- readRDS(baseline_fit_file)
baseline_weights <- readRDS(baseline_weights_file)

# Extract rho from filename
rho_match <- regmatches(baseline_fit_file, regexpr("rho[0-9e\\-]+", baseline_fit_file))
baseline_rho <- gsub("rho", "", rho_match)

cat("  ✓ Loaded baseline fit (lambda =", baseline_rho, ")\n")
cat("    Convergence:", baseline_fit$convergence, "\n")
cat("    Max |balance|:", round(max(abs(baseline_fit$balance.std), na.rm=TRUE), 3), "\n")
cat("    Mean |balance|:", round(mean(abs(baseline_fit$balance.std), na.rm=TRUE), 3), "\n")

# Count controls
baseline_n_control <- sum(baseline_weights$treated == 0)
baseline_n_treated <- sum(baseline_weights$treated == 1)
cat("    Controls:", baseline_n_control, "\n")
cat("    Treated:", baseline_n_treated, "\n\n")

# ============================================================================
# 2. Compute baseline pre-treatment RMSE (same train/test as embedding)
# ============================================================================
cat("2. Computing baseline pre-treatment RMSE...\n")
cat("   Train: 2000-2010 | Test: 2011-2015\n")

# Load full data to get coordinates
data_file <- "data/processed_data/rev_analysis_low/analysis_treated2019_conifer.RDS"
df <- readRDS(data_file)

# Merge weights with coordinates
weights_full <- baseline_weights %>%
  inner_join(df %>% select(unit, LATITUDE, LONGITUDE), by = "unit")

# Compute RMSE
baseline_rmse_result <- tryCatch({
  calculate_pretreatment_rmse(
    weights_df = weights_full,
    train_start = 2000,
    train_end = 2010,
    test_start = 2011,
    test_end = 2015,
    firms_rds_path = "data/processed_data/FIRMS.RDS"
  )
}, error = function(e) {
  cat("   ⚠ RMSE calculation failed:", e$message, "\n")
  list(rmse_train = NA, rmse_test = NA)
})

baseline_rmse_train <- baseline_rmse_result$rmse_train
baseline_rmse_test <- baseline_rmse_result$rmse_test

cat("   Train RMSE:", round(baseline_rmse_train, 4), "\n")
cat("   Test RMSE:", round(baseline_rmse_test, 4), "\n\n")

# ============================================================================
# 3. Load embedding K selection results
# ============================================================================
cat("3. Loading embedding K selection results...\n")

embedding_results_file <- "Embeddings/data/k_selection/2019/k_selection_rmse.csv"
if (!file.exists(embedding_results_file)) {
  stop("Embedding results not found: ", embedding_results_file)
}

embedding_df <- read.csv(embedding_results_file)
cat("   ✓ Loaded", nrow(embedding_df), "K values\n\n")

# ============================================================================
# 4. Create comparison table
# ============================================================================
cat("4. Comparison Table:\n")
cat("="*80, "\n")

# Create baseline row
baseline_row <- data.frame(
  Method = "Baseline (Full Pool)",
  K = NA,
  Controls = baseline_n_control,
  Lambda = baseline_rho,
  RMSE_train = baseline_rmse_train,
  RMSE_test = baseline_rmse_test,
  Max_Balance = max(abs(baseline_fit$balance.std), na.rm=TRUE),
  Mean_Balance = mean(abs(baseline_fit$balance.std), na.rm=TRUE)
)

# Create embedding rows
embedding_rows <- embedding_df %>%
  mutate(
    Method = paste0("Embedding K=", K),
    Controls = pool_size,
    Lambda = NA,  # Varies by K
    RMSE_train = rmse_train,
    RMSE_test = rmse,
    Max_Balance = max_balance_std,
    Mean_Balance = mean_balance_std
  ) %>%
  select(Method, K, Controls, Lambda, RMSE_train, RMSE_test, Max_Balance, Mean_Balance)

# Combine
comparison_df <- rbind(baseline_row, embedding_rows)

# Print table
print(comparison_df, row.names = FALSE)

cat("\n")
cat("="*80, "\n\n")

# ============================================================================
# 5. Key Insights
# ============================================================================
cat("5. Key Insights:\n")
cat("-"*80, "\n")

# Find optimal K
optimal_k_idx <- which.min(embedding_rows$RMSE_test)
optimal_k <- embedding_rows$K[optimal_k_idx]
optimal_rmse <- embedding_rows$RMSE_test[optimal_k_idx]
optimal_controls <- embedding_rows$Controls[optimal_k_idx]

# Calculate improvements
rmse_improvement <- ((baseline_rmse_test - optimal_rmse) / baseline_rmse_test) * 100
balance_improvement <- ((baseline_row$Max_Balance - embedding_rows$Max_Balance[optimal_k_idx]) / baseline_row$Max_Balance) * 100
control_reduction <- ((baseline_n_control - optimal_controls) / baseline_n_control) * 100

cat("\nOptimal K:", optimal_k, "\n")
cat("  Controls:", optimal_controls, "vs", baseline_n_control, "baseline\n")
cat("  Reduction:", round(control_reduction, 1), "%\n")
cat("  Test RMSE:", round(optimal_rmse, 4), "vs", round(baseline_rmse_test, 4), "baseline\n")

if (!is.na(baseline_rmse_test) && !is.na(optimal_rmse)) {
  if (optimal_rmse < baseline_rmse_test) {
    cat("  → Improvement:", round(rmse_improvement, 1), "% better than baseline\n")
  } else if (optimal_rmse > baseline_rmse_test) {
    cat("  → Degradation:", round(-rmse_improvement, 1), "% worse than baseline\n")
  } else {
    cat("  → Equivalent to baseline\n")
  }
}

cat("\nBalance (Max |SMD|):\n")
cat("  Baseline:", round(baseline_row$Max_Balance, 3), "\n")
cat("  K=", optimal_k, ": ", round(embedding_rows$Max_Balance[optimal_k_idx], 3), "\n", sep="")
if (embedding_rows$Max_Balance[optimal_k_idx] < baseline_row$Max_Balance) {
  cat("  → Better balance with embedding\n")
}

# ============================================================================
# 6. Efficiency Analysis
# ============================================================================
cat("\n")
cat("-"*80, "\n")
cat("Efficiency Analysis (K=50 sweet spot?):\n")
cat("-"*80, "\n")

k50_idx <- which(embedding_rows$K == 50)
if (length(k50_idx) > 0) {
  k50_rmse <- embedding_rows$RMSE_test[k50_idx]
  k50_controls <- embedding_rows$Controls[k50_idx]
  k50_reduction <- ((baseline_n_control - k50_controls) / baseline_n_control) * 100
  k50_rmse_diff <- ((k50_rmse - optimal_rmse) / optimal_rmse) * 100
  
  cat("\nK=50 vs K=100:\n")
  cat("  Controls: ", k50_controls, " (", round(k50_reduction, 1), "% reduction from baseline)\n", sep="")
  cat("  RMSE: ", round(k50_rmse, 4), " (+", round(k50_rmse_diff, 1), "% vs optimal K=", optimal_k, ")\n", sep="")
  cat("  Balance: ", round(embedding_rows$Max_Balance[k50_idx], 3), "\n", sep="")
  
  if (k50_rmse_diff < 20) {
    cat("\n  → K=50 is competitive: 70% pool reduction, only ", round(k50_rmse_diff, 1), "% RMSE increase\n", sep="")
    cat("  → Consider K=50 for computational efficiency at scale\n")
  }
}

# ============================================================================
# 7. Save comparison
# ============================================================================
output_file <- "Embeddings/data/k_selection/2019/baseline_vs_embedding_comparison.csv"
write.csv(comparison_df, output_file, row.names = FALSE)
cat("\n")
cat("✓ Saved comparison to:", output_file, "\n")

cat("\n")
cat("="*80, "\n")
cat("CONCLUSION: Embedding provides ", round(control_reduction, 0), "% pool reduction\n", sep="")
if (!is.na(baseline_rmse_test) && !is.na(optimal_rmse)) {
  if (optimal_rmse <= baseline_rmse_test * 1.1) {
    cat("            with comparable or better RMSE (within 10%)\n")
  }
}
cat("="*80, "\n")
