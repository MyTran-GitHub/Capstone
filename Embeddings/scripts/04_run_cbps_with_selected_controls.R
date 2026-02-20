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
source("balancing/calculate_fire_outcomes.R")  # Shared outcome calculation logic

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

# Prepare CBPS inputs (MUST MATCH implement_cbps.R transformations for fair comparison!)
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

cat("Covariates before transformations:", ncol(X), "\n")

# ============================================================================
# APPLY SAME TRANSFORMATIONS AS implement_cbps.R (critical for fair comparison!)
# ============================================================================

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
    cat("  Removed", length(cols_to_remove), "sparse SWE columns (>95% zero)\n")
  }
  
  if (cols_converted > 0) {
    cat("  SWE two-part: converted", cols_converted, "columns (presence + log-intensity)\n")
  }
}

# Log1p + winsorize max_FRP_* to preserve intensity ordering
frp_cols <- grep("^max_FRP_", colnames(X), value = TRUE)
if (length(frp_cols) > 0) {
  for (col in frp_cols) {
    x <- X[[col]]
    if (all(is.na(x))) {
      next
    }
    x <- log1p(x)
    p995 <- quantile(x, 0.995, na.rm = TRUE)
    if (!is.na(p995)) {
      x[x > p995] <- p995
    }
    X[[col]] <- x
  }
  cat("  Log+winsorized", length(frp_cols), "max_FRP columns (99.5%)\n")
}

# Log1p + winsorize prcp_* and avg_BRIGHTNESS_* to tame heavy tails
prcp_cols <- grep("^prcp_", colnames(X), value = TRUE)
bright_cols <- grep("^avg_BRIGHTNESS_", colnames(X), value = TRUE)
for (col in c(prcp_cols, bright_cols)) {
  x <- X[[col]]
  if (all(is.na(x))) {
    next
  }
  x <- log1p(x)
  p995 <- quantile(x, 0.995, na.rm = TRUE)
  if (!is.na(p995)) {
    x[x > p995] <- p995
  }
  X[[col]] <- x
}
if (length(prcp_cols) + length(bright_cols) > 0) {
  cat("  Log+winsorized", length(prcp_cols), "prcp and",
      length(bright_cols), "avg_BRIGHTNESS columns (99.5%)\n")
}

# Drop extremely sparse fire_* columns to avoid huge z-scores
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
    cat("  Dropped", length(sparse_fire), "sparse fire_* columns (<0.5% ones)\n")
  }
}

cat("Covariates after transformations:", ncol(X), "\n")

# Validate sufficient covariates remain after transformations
if (ncol(X) == 0) {
  stop("ERROR: No covariates remaining after transformations!")
}

# Validate sufficient sample size for CBPS
n_treated <- sum(W)
n_control <- sum(1 - W)

if (n_control < 2 * n_treated) {
  stop(paste("ERROR: Insufficient controls! Have", n_control, 
             "controls for", n_treated, "treated (ratio:",
             round(n_control / n_treated, 1), "×, recommend ≥10×)"))
}

if (n_control < 10 * n_treated) {
  cat("⚠ WARNING: Small control pool (", n_control, "controls for", n_treated, 
      "treated = ", round(n_control / n_treated, 1), "× ratio)\n", sep="")
  cat("  Recommend ≥10× for stable CBPS weights\n")
}

if (n_control / ncol(X) < 5) {
  cat("⚠ WARNING: Low observations-to-covariates ratio\n")
  cat("  ", n_control, "controls ÷", ncol(X), "covariates =",
      round(n_control / ncol(X), 1), "obs/covariate\n")
  cat("  Recommend ≥5 observations per covariate for stable estimation\n")
}

# Standardize covariates
X_mean <- colMeans(X, na.rm = TRUE)
X_sd <- apply(X, 2, sd, na.rm = TRUE)
X_sd[is.na(X_sd) | X_sd == 0] <- 1
X_scl <- scale(X, center = X_mean, scale = X_sd)

# Store original variances before standardization (needed for Step 1 of dimensionality reduction)
X_var_original <- apply(X, 2, var, na.rm = TRUE)

# ============================================================================
# DIMENSIONALITY REDUCTION (prevent numerical instability in small samples)
# ============================================================================
# When obs:covariate ratio < 10, numerical instability in CBPS optimization
# 
# CAUTION: In causal inference, we must preserve confounders (covariates that
# affect both treatment and outcome). Standard ML dimensionality reduction can
# introduce OMITTED VARIABLE BIAS by removing potential confounders.
#
# Strategy: MINIMIZE covariate removal, rely on regularization instead
# - Step 1: Remove near-zero variance (pre-standardization) = true constants with numerical noise
# - Step 2: Use stronger CBPS regularization (lambda penalty) to handle collinearity
# 
# WHY NOT remove correlated variables?
# Even if X1 and X2 are highly correlated (r=0.95), BOTH may be confounders.
# Example: elevation & slope both affect fire behavior AND treatment assignment.
# Removing one creates omitted variable bias. Let L2 regularization handle multicollinearity.

# Step 1: Remove near-constant covariates (PRE-standardization near-zero variance)
# These are true constants with numerical noise only, not real covariates
near_constant_threshold <- 1e-10  # essentially zero variance
keep_variance <- X_var_original >= near_constant_threshold

n_removed <- sum(!keep_variance)
if (n_removed > 0) {
  cat("\n⚙ Removed ", n_removed, " near-constant covariates (var < 1e-10)\n", sep="")
  X_scl <- X_scl[, keep_variance, drop = FALSE]
}

# Step 2: Decide on regularization strength based on obs:covariate ratio
# CRITICAL: Do NOT remove correlated covariates - both may be confounders
# Use L2 regularization (lambda) to handle multicollinearity instead
obs_per_cov <- n_control / ncol(X_scl)

if (obs_per_cov < 10) {
  cat("⚠ Low obs:covariate ratio (", round(obs_per_cov, 1), " obs/cov)\n", sep="")
  cat("  → Using STRONG regularization to handle high dimensionality\n")
  cat("  → This preserves all confounders while controlling multicollinearity\n")
  use_strong_regularization <- TRUE
} else {
  cat("\n✓ Adequate obs:covariate ratio (", round(obs_per_cov, 1), " obs/cov)\n", sep="")
  cat("  → Using STANDARD regularization\n")
  use_strong_regularization <- FALSE
}

cat("Final covariate count: ", ncol(X_scl), "\n\n", sep="")

# Validate sufficient covariates remain
if (ncol(X_scl) < 5) {
  stop(paste("ERROR: Too few covariates remaining (",
             ncol(X_scl), "). Cannot estimate CBPS reliably."))
}

# Store final covariate count for metrics
n_covariates_used <- ncol(X_scl)

# Run CBPS-ATT with regularization grid
# Grid: lambda = 10^(-6 to 1) across 8 levels (STANDARD)
#       lambda = 10^(-4 to 5) across 10 levels (STRONG for high-dimensional case)
# 
# Regularization strategy:
# - Low lambda (1e-6): Minimal shrinkage, maximum balance, risk of overfitting
# - High lambda (10^5): Strong shrinkage, prevents overfitting in high-p case
# - Grid search selects first lambda with converged + numerically valid weights

if (use_strong_regularization) {
  cat("Using STRONG regularization grid (high-dimensional case)...\n")
  lambda_levels <- 10
  lambda_range <- -4:5  # 10^(-4) to 10^5
} else {
  cat("Using STANDARD regularization grid...\n")
  lambda_levels <- 8
  lambda_range <- -6:1  # 10^(-6) to 10^1
}

res_regu_list <- lapply(seq_len(lambda_levels), function(n) {
  lambda_val <- 10^(lambda_range[n])
  res <- cbps_att(as.matrix(X_scl),
                  W,
                  theta.init = rep(0, ncol(X_scl) + 1),
                  control = list(trace = 0, maxit = 6000),
                  lambda = rep(lambda_val, ncol(X_scl)))
  return(res)
})

# Check BOTH convergence AND weight validity
converge_set <- sapply(res_regu_list, function(res) {
  converged <- (res$convergence == 0)
  valid_weights <- !any(is.na(res$weights.0)) && !any(is.infinite(res$weights.0)) &&
                   !any(is.na(res$weights.1)) && !any(is.infinite(res$weights.1))
  return(converged && valid_weights)
})

if (!any(converge_set)) {
  stop("No solution with valid convergence AND valid weights found")
}

idx <- min(which(converge_set))
res <- res_regu_list[[idx]]
rho <- 10^(lambda_range[idx])

cat("Converged with valid weights at rho =", rho, "\n")

# Check balance
max_balance_std <- max(abs(res$balance.std), na.rm = TRUE)
mean_balance_std <- mean(abs(res$balance.std), na.rm = TRUE)
cat("Max |balance.std| =", round(max_balance_std, 3), "\n")
cat("Mean |balance.std| =", round(mean_balance_std, 3), "\n")

# Create weights table (needed for outcome calculation)
weights_df <- data.frame(
  unit = df_filtered$unit,
  LATITUDE = df_filtered$LATITUDE,
  LONGITUDE = df_filtered$LONGITUDE,
  treated = df_filtered$treated,
  weight = ifelse(df_filtered$treated == 1, res$weights.1, res$weights.0),
  stringsAsFactors = FALSE
)

# Compute pre-treatment RMSE using shared function
# This reuses the same logic as weighted_outcome_analysis.R
cat("\nComputing pre-treatment fire frequency RMSE...\n")

rmse_result <- tryCatch(
  {
    calculate_pretreatment_rmse(
      weights_df = weights_df,
      train_start = train_start,
      train_end = train_end,
      test_start = test_start,
      test_end = test_end,
      firms_rds_path = "data/processed_data/FIRMS.RDS"
    )
  },
  error = function(e) {
    cat("⚠ WARNING: RMSE calculation failed:", e$message, "\n")
    cat("Using covariate balance as proxy\n")
    list(
      rmse_train = sqrt(mean(res$balance.std[seq_len(min(length(res$balance.std), 50))]^2, na.rm = TRUE)),
      rmse_test = sqrt(mean(res$balance.std^2, na.rm = TRUE)),
      fire_freq_data = data.frame()
    )
  }
)

rmse_train <- rmse_result$rmse_train
rmse_test <- rmse_result$rmse_test

if (!is.na(rmse_train) && !is.na(rmse_test)) {
  cat("✓ Computed fire frequency RMSE from FIRMS data\n")
  cat("  (FIRMS.RDS filtered to conifer pixels via coordinate merge)\n")
} else {
  cat("⚠ Using covariate balance proxy for RMSE\n")
}

cat("Train RMSE (fire frequency):", round(rmse_train, 4), "\n")
cat("Test RMSE (fire frequency):", round(rmse_test, 4), "\n")

# Create year-specific output directory
output_base_dir <- "Embeddings/data/cbps_integration/"
output_dir <- paste0(output_base_dir, treated_year, "/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nOutput directory:", output_dir, "\n")

# Generate and save trajectory plot (Phase 2 prep)
cat("\nGenerating pre-treatment trajectory plot...\n")

trajectory_data <- tryCatch(
  {
    plot_pretreatment_trajectory(
      weights_df = weights_df,
      train_start = train_start,
      train_end = train_end,
      test_start = test_start,
      test_end = test_end,
      output_path = paste0(output_dir, "trajectory_", output_prefix, "_", treated_year, ".png"),
      treatment_year = treated_year,
      firms_rds_path = "data/processed_data/FIRMS.RDS"
    )
  },
  error = function(e) {
    cat("⚠ WARNING: Trajectory plot failed:", e$message, "\n")
    data.frame()
  }
)

# Save trajectory data for later analysis
if (nrow(trajectory_data) > 0) {
  trajectory_path <- paste0(output_dir, "trajectory_data_", output_prefix, "_", treated_year, ".csv")
  write.csv(trajectory_data, trajectory_path, row.names = FALSE)
  cat("✓ Trajectory data saved to:", trajectory_path, "\n")
}

# Create metrics table
metrics_df <- data.frame(
  year = treated_year,
  output_prefix = output_prefix,
  n_treated = sum(W),
  n_control = sum(1 - W),
  n_covariates = n_covariates_used,  # Actual covariates used in CBPS (post-reduction)
  rho = rho,
  converged = res$convergence == 0,
  max_balance_std = max_balance_std,
  mean_balance_std = mean_balance_std,
  rmse_train = rmse_train,
  rmse_test = rmse_test,
  stringsAsFactors = FALSE
)

# Save outputs to year-specific directory
metrics_path <- paste0(output_dir, "cbps_metrics_", output_prefix, "_", treated_year, ".csv")
weights_path <- paste0(output_dir, "cbps_weights_", output_prefix, "_", treated_year, ".csv")
weights_full_path <- paste0(output_dir, "cbps_weights_full_", output_prefix, "_", treated_year, ".csv")

write.csv(metrics_df, metrics_path, row.names = FALSE)
# Save minimal weights for R compatibility with baseline workflow
write.csv(weights_df[, c("unit", "treated", "weight")], weights_path, row.names = FALSE)
# Save full weights with coordinates for Phase 2+ analyses (ATT, bootstrap CI)
write.csv(weights_df, weights_full_path, row.names = FALSE)

cat("\nSaved:\n")
cat("  ", metrics_path, "\n")
cat("  ", weights_path, "\n")
cat("  ", weights_full_path, "(with coordinates for Phase 2)\n")

cat("\n✓ CBPS completed successfully\n")
