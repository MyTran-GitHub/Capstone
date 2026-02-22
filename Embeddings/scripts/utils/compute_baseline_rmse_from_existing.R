#!/usr/bin/env Rscript
## Extract baseline pre-treatment RMSE from existing weighted_outcome_analysis.R outputs
## Avoids re-running CBPS - just reads df.freq.year files that already exist
##
## Usage:
##   Rscript compute_baseline_rmse_from_existing.R <year> <train_start> <train_end> <test_start> <test_end>
##
## Example:
##   source /work/11105/mee_tran/miniconda3/bin/activateconda activate r-spatial

suppressPackageStartupMessages({
  library("dplyr")
  library("tidyr")
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) {
  stop("Usage: Rscript compute_baseline_rmse_from_existing.R <year> <train_start> <train_end> <test_start> <test_end>")
}

year <- as.integer(args[1])
train_start <- as.integer(args[2])
train_end <- as.integer(args[3])
test_start <- as.integer(args[4])
test_end <- as.integer(args[5])

cat("Computing baseline RMSE from existing outputs\n")
cat("Year:", year, "\n")
cat("Train period:", train_start, "-", train_end, "\n")
cat("Test period:", test_start, "-", test_end, "\n")

# Find the df.freq.year file from weighted_outcome_analysis.R
# It's saved in rev_result_low/{start_year}/ directories
result_dirs <- c("data/processed_data/rev_result_low/2006",
                 "data/processed_data/rev_result_low/2008",
                 "data/processed_data/rev_result_low/2010")

freq_file <- NULL
for (dir in result_dirs) {
  candidate <- file.path(dir, paste0("df.freq.year", year, "_conifer.RDS"))
  if (file.exists(candidate)) {
    freq_file <- candidate
    break
  }
}

if (is.null(freq_file)) {
  cat("ERROR: Could not find df.freq.year file for year", year, "\n")
  cat("Searched in:\n")
  for (dir in result_dirs) {
    cat("  ", file.path(dir, paste0("df.freq.year", year, "_conifer.RDS")), "\n")
  }
  cat("\n")
  cat("This file should be created by weighted_outcome_analysis.R\n")
  cat("If not present, run: Rscript analysis/weighted_outcome_analysis.R\n")
  stop("Missing frequency data")
}

cat("Found frequency data:", freq_file, "\n")

# Load fire frequency data
df_freq <- readRDS(freq_file)

# Filter to pre-treatment years
train_years <- seq(train_start, train_end)
test_years <- seq(test_start, test_end)

# Compute RMSE for training period
fire_train <- df_freq %>%
  filter(year %in% train_years)

if (nrow(fire_train) > 0) {
  fire_train_wide <- fire_train %>%
    select(year, treated, hifire95.frac) %>%
    pivot_wider(names_from = treated, 
                values_from = hifire95.frac, 
                names_prefix = "treated_")
  
  # Fill NAs with 0
  fire_train_wide$treated_0[is.na(fire_train_wide$treated_0)] <- 0
  fire_train_wide$treated_1[is.na(fire_train_wide$treated_1)] <- 0
  
  rmse_train <- sqrt(mean((fire_train_wide$treated_1 - fire_train_wide$treated_0)^2, na.rm = TRUE))
} else {
  rmse_train <- NA_real_
  cat("⚠ WARNING: No training period data\n")
}

# Compute RMSE for test period
fire_test <- df_freq %>%
  filter(year %in% test_years)

if (nrow(fire_test) > 0) {
  fire_test_wide <- fire_test %>%
    select(year, treated, hifire95.frac) %>%
    pivot_wider(names_from = treated, 
                values_from = hifire95.frac, 
                names_prefix = "treated_")
  
  # Fill NAs with 0
  fire_test_wide$treated_0[is.na(fire_test_wide$treated_0)] <- 0
  fire_test_wide$treated_1[is.na(fire_test_wide$treated_1)] <- 0
  
  rmse_test <- sqrt(mean((fire_test_wide$treated_1 - fire_test_wide$treated_0)^2, na.rm = TRUE))
} else {
  rmse_test <- NA_real_
  cat("⚠ WARNING: No test period data\n")
}

cat("\nRMSE Results:\n")
cat("  Train RMSE:", round(rmse_train, 4), "\n")
cat("  Test RMSE:", round(rmse_test, 4), "\n")

# Load baseline weights to get sample sizes
weights_file <- paste0("data/processed_data/rev_analysis_low/cbps_weights_", year, "_conifer.RDS")
if (file.exists(weights_file)) {
  weights_df <- readRDS(weights_file)
  n_treated <- sum(weights_df$treated == 1)
  n_control <- sum(weights_df$treated == 0)
  cat("  Treated pixels:", n_treated, "\n")
  cat("  Control pixels:", n_control, "\n")
} else {
  cat("⚠ WARNING: Weights file not found:", weights_file, "\n")
  n_treated <- NA_integer_
  n_control <- NA_integer_
}

# Save metrics
metrics_df <- data.frame(
  year = year,
  method = "baseline_full_pool",
  n_treated = n_treated,
  n_control = n_control,
  rmse_train = rmse_train,
  rmse_test = rmse_test,
  stringsAsFactors = FALSE
)

# Save to Embeddings results directory for easy comparison
output_dir <- paste0("Embeddings/data/cbps_integration/", year, "/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- paste0(output_dir, "baseline_metrics_", year, ".csv")

write.csv(metrics_df, output_file, row.names = FALSE)
cat("\n✓ Saved baseline metrics to:", output_file, "\n")

# Also print for easy copying
cat("\nBaseline Metrics Summary:\n")
print(metrics_df, row.names = FALSE)
