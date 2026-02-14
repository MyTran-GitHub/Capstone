#!/usr/bin/env Rscript
## Python → R Integration Script
## Run CBPS on a filtered control pool (embedding-based selection)
## 
## Usage:
##   Rscript run_cbps_with_selected_controls.R <year> <selected_units_csv> <output_prefix> <train_start> <train_end> <test_start> <test_end>
##
## Arguments:
##   year: Treatment year (e.g., 2019)
##   selected_units_csv: Path to CSV with selected control units (must have 'unit' column)
##   output_prefix: Prefix for output files (e.g., "k10" for K=10 nearest)
##   train_start: Start year for training period (e.g., 2000)
##   train_end: End year for training period (e.g., 2010)
##   test_start: Start year for test period (e.g., 2011)
##   test_end: End year for test period (e.g., 2015)
##
## Outputs:
##   - cbps_metrics_{output_prefix}_{year}.csv: Balance metrics, convergence info, RMSE
##   - cbps_weights_{output_prefix}_{year}.csv: Unit-level weights

suppressPackageStartupMessages({
  library("dplyr")
  library("tidyr")
})

source("balancing/cbps_ATT.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 7) {
  stop("Usage: Rscript run_cbps_with_selected_controls.R <year> <selected_units_csv> <output_prefix> <train_start> <train_end> <test_start> <test_end>")
}

treated_year <- as.integer(args[1])
selected_units_path <- args[2]
output_prefix <- args[3]
train_start <- as.integer(args[4])
train_end <- as.integer(args[5])
test_start <- as.integer(args[6])
test_end <- as.integer(args[7])

cat("========== Covariate balancing with filtered control pool\n")
cat("Treatment year:", treated_year, "\n")
cat("Output prefix:", output_prefix, "\n")
cat("Train period:", train_start, "-", train_end, "\n")
cat("Test period:", test_start, "-", test_end, "\n")

# Load data
outDir <- "data/processed_data/rev_analysis_low/"
input_file <- paste0(outDir, "analysis_treated", treated_year, "_conifer.RDS")

if (!file.exists(input_file)) {
  stop(paste("Input file not found:", input_file))
}

df <- readRDS(input_file)
cat("Loaded data:", nrow(df), "pixels\n")
cat("  Treated:", sum(df$treated), "\n")
cat("  Control:", sum(df$treated == 0), "\n")

# Load selected control units
if (!file.exists(selected_units_path)) {
  stop(paste("Selected units file not found:", selected_units_path))
}

selected_units <- read.csv(selected_units_path, stringsAsFactors = FALSE)
if (!"unit" %in% names(selected_units)) {
  stop("Selected units CSV must have 'unit' column")
}

cat("Selected controls:", nrow(selected_units), "\n")

# Filter data: keep all treated + selected controls only
df_filtered <- df %>%
  filter(treated == 1 | unit %in% selected_units$unit)

cat("Filtered data:", nrow(df_filtered), "pixels\n")
cat("  Treated:", sum(df_filtered$treated), "\n")
cat("  Control:", sum(df_filtered$treated == 0), "\n")

if (sum(df_filtered$treated == 0) == 0) {
  stop("ERROR: No control pixels after filtering!")
}

# Prepare CBPS inputs (same as implement_cbps.R)
W <- df_filtered$treated

X <- df_filtered
X$unit <- NULL
X$LATITUDE <- NULL
X$LONGITUDE <- NULL
X$treated <- NULL
X$num.fire <- NULL

# Remove non-numeric columns
X <- X[, sapply(X, is.numeric), drop = FALSE]
# Remove zero-variance columns
X <- X[, apply(X, 2, sd, na.rm = TRUE) > 0, drop = FALSE]

cat("Covariates:", ncol(X), "\n")

# Standardize covariates
X_mean <- colMeans(X, na.rm = TRUE)
X_sd <- apply(X, 2, sd, na.rm = TRUE)
X_sd[is.na(X_sd) | X_sd == 0] <- 1
X_scl <- scale(X, center = X_mean, scale = X_sd)

# Run CBPS-ATT with regularization grid (same as implement_cbps.R)
lambda_grid <- if (treated_year %in% c(2018, 2020)) {
  cat("Using stronger regularization for year", treated_year, "\n")
  rep(1e-4, ncol(X))
} else {
  NULL
}

if (is.null(lambda_grid)) {
  res_regu_list <- lapply(1:8, function(n) {
    res <- cbps_att(as.matrix(X_scl),
                    W,
                    theta.init = rep(0, ncol(X) + 1),
                    control = list(trace = 0, maxit = 5000),
                    lambda = rep(10^(n - 7), ncol(X)))
    return(res)
  })
} else {
  res_regu_list <- list(
    cbps_att(as.matrix(X_scl),
             W,
             theta.init = rep(0, ncol(X) + 1),
             control = list(trace = 0, maxit = 5000),
             lambda = lambda_grid)
  )
}

converge_set <- sapply(res_regu_list, function(res) res$convergence)

if (!any(converge_set == 0)) {
  stop("No converged solution found")
}

idx <- min(which(converge_set == 0))
res <- res_regu_list[[idx]]
rho <- if (is.null(lambda_grid)) 10^(idx - 7) else 1e-4

cat("Converged with rho =", rho, "\n")

# Validate weights
if (any(is.na(res$weights.0)) || any(is.infinite(res$weights.0)) ||
    any(is.na(res$weights.1)) || any(is.infinite(res$weights.1))) {
  stop("Invalid weights (NAs or Infs)")
}

# Check balance
max_balance_std <- max(abs(res$balance.std), na.rm = TRUE)
mean_balance_std <- mean(abs(res$balance.std), na.rm = TRUE)
cat("Max |balance.std| =", round(max_balance_std, 3), "\n")
cat("Mean |balance.std| =", round(mean_balance_std, 3), "\n")

# Compute RMSE on pre-treatment covariates
# (This is a simplified version - you can expand to use actual outcome data)
# For now, we'll compute covariate imbalance as a proxy
rmse_train <- sqrt(
  mean(res$balance.std[seq_len(min(length(res$balance.std), 50))]^2,
       na.rm = TRUE)
)
rmse_test <- sqrt(mean(res$balance.std^2, na.rm = TRUE))

cat("Train RMSE (covariate imbalance):", round(rmse_train, 4), "\n")
cat("Test RMSE (covariate imbalance):", round(rmse_test, 4), "\n")

# Create weights table
weights_df <- data.frame(
  unit = df_filtered$unit,
  treated = df_filtered$treated,
  weight = ifelse(df_filtered$treated == 1, res$weights.1, res$weights.0),
  stringsAsFactors = FALSE
)

# Create metrics table
metrics_df <- data.frame(
  year = treated_year,
  output_prefix = output_prefix,
  n_treated = sum(W),
  n_control = sum(1 - W),
  n_covariates = ncol(X),
  rho = rho,
  converged = res$convergence == 0,
  max_balance_std = max_balance_std,
  mean_balance_std = mean_balance_std,
  rmse_train = rmse_train,
  rmse_test = rmse_test,
  stringsAsFactors = FALSE
)

# Save outputs
output_dir <- "tests/results/cbps_integration/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

metrics_path <- paste0(output_dir, "cbps_metrics_", output_prefix, "_", treated_year, ".csv")
weights_path <- paste0(output_dir, "cbps_weights_", output_prefix, "_", treated_year, ".csv")

write.csv(metrics_df, metrics_path, row.names = FALSE)
write.csv(weights_df, weights_path, row.names = FALSE)

cat("\nSaved:\n")
cat("  ", metrics_path, "\n")
cat("  ", weights_path, "\n")

cat("\n✓ CBPS completed successfully\n")
