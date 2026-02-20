#!/usr/bin/env Rscript
# ============================================================================
# COMPARISON: Theta Contributors in Random vs Embedding Controls
# ============================================================================
# Test if embedding changes which variables drive theta explosion

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  cat("Usage: Rscript compare_theta_random_vs_embedding.R <year>\n")
  quit(status = 1)
}

year <- as.integer(args[1])

suppressMessages({
  library(dplyr)
})

source("balancing/cbps_ATT.R")

prepare_data <- function(df_subset, name) {
  cat("\n", paste(rep("=", 80), collapse=""), "\n", sep="")
  cat(name, "\n")
  cat(paste(rep("=", 80), collapse=""), "\n\n")
  
  W <- df_subset$treated
  
  # Prepare covariates
  exclude_cols <- c("treated", "unit", "year", "fire", "avg_BRIGHTNESS", "avg_FRP",
                    grep("post_", names(df_subset), value=TRUE))
  X_raw <- as.matrix(df_subset[, setdiff(names(df_subset), exclude_cols)])
  
  # Standard transformations
  log_vars <- grep("prcp|tmmn|tmmx|fire|BRIGHTNESS|FRP", colnames(X_raw), value = TRUE)
  X <- X_raw
  for (var in log_vars) {
    if (var %in% colnames(X)) {
      X[, var] <- log(X[, var] + 1)
    }
  }
  
  for (j in 1:ncol(X)) {
    q <- quantile(X[, j], probs = c(0.01, 0.99), na.rm = TRUE)
    X[, j] <- pmax(pmin(X[, j], q[2]), q[1])
  }
  
  X_scl <- scale(X)
  sds <- apply(X_scl, 2, sd, na.rm = TRUE)
  keep <- which(sds >= 0.01)
  X_scl <- X_scl[, keep, drop = FALSE]
  
  cat(sprintf("Sample: %d treated + %d controls\n", sum(W), sum(W == 0)))
  cat(sprintf("Covariates: %d\n", ncol(X_scl)))
  cat(sprintf("Obs:Cov: %.1f:1\n\n", nrow(X_scl) / ncol(X_scl)))
  
  # Identify variable types
  fire_idx <- grep("fire_|BRIGHTNESS|FRP", colnames(X_scl))
  prcp_idx <- grep("prcp_", colnames(X_scl))
  swe_idx <- grep("swe_", colnames(X_scl))
  
  # Run CBPS
  cat("Running CBPS...\n")
  fit <- tryCatch({
    cbps_att(X_scl, W, intercept = TRUE,
             theta.init = rep(0, ncol(X_scl) + 1),
             lambda = rep(0.001, ncol(X_scl)),
             control = list(trace = 0, maxit = 1000))
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(fit)) {
    return(NULL)
  }
  
  theta <- fit$theta.hat[-1]
  weights <- fit$weights.0
  
  cat(sprintf("Convergence: %d\n", fit$convergence))
  
  if (any(is.na(weights)) || any(is.infinite(weights))) {
    cat(sprintf("Weights: NaN=%d, Inf=%d\n", sum(is.na(weights)), sum(is.infinite(weights))))
  } else {
    cat(sprintf("Weights: Valid, range [%.3f, %.3f]\n", min(weights), max(weights)))
  }
  
  X_theta <- X_scl %*% theta
  max_X_theta <- max(abs(X_theta))
  cat(sprintf("max|X*theta|: %.2f\n\n", max_X_theta))
  
  # Calculate contributions
  contributions <- sapply(1:ncol(X_scl), function(j) {
    max(abs(X_scl[, j] * theta[j]))
  })
  
  var_analysis <- data.frame(
    Variable = colnames(X_scl),
    Theta = theta,
    Max_Contribution = contributions,
    Type = "Other",
    stringsAsFactors = FALSE
  )
  
  var_analysis$Type[fire_idx] <- "Fire"
  var_analysis$Type[prcp_idx] <- "Precipitation"
  var_analysis$Type[swe_idx] <- "SWE"
  
  var_analysis <- var_analysis[order(var_analysis$Max_Contribution, decreasing = TRUE), ]
  
  # Show top 10
  cat("TOP 10 CONTRIBUTORS:\n")
  print(var_analysis[1:10, c("Variable", "Type", "Max_Contribution")], row.names = FALSE)
  
  cat("\n")
  
  # Type summary
  type_summary <- aggregate(
    Max_Contribution ~ Type, 
    data = var_analysis, 
    FUN = function(x) c(
      Count = length(x),
      Total = sum(x),
      Pct = 100 * sum(x) / sum(var_analysis$Max_Contribution)
    )
  )
  
  type_stats <- data.frame(
    Type = type_summary$Type,
    Count = type_summary$Max_Contribution[, "Count"],
    Pct = type_summary$Max_Contribution[, "Pct"]
  )
  
  type_stats <- type_stats[order(type_stats$Pct, decreasing = TRUE), ]
  
  cat("BY TYPE:\n")
  print(type_stats, row.names = FALSE)
  
  return(list(
    success = !any(is.na(weights)) && !any(is.infinite(weights)),
    max_X_theta = max_X_theta,
    var_analysis = var_analysis,
    type_stats = type_stats,
    fire_pct = type_stats$Pct[type_stats$Type == "Fire"]
  ))
}

# ============================================================================
# Load Data
# ============================================================================

cat("================================================================================\n")
cat("RANDOM vs EMBEDDING THETA COMPARISON - Year:", year, "\n")
cat("================================================================================\n")

outDir <- "data/processed_data/rev_analysis_low/"
data_path <- paste0(outDir, "analysis_treated", year, "_conifer.RDS")
df <- readRDS(data_path)

treated_idx <- which(df$treated == 1)

# ============================================================================
# Test 1: Random 7k Controls (match embedding size)
# ============================================================================

set.seed(12345)
random_7k <- sample(which(df$treated == 0), 7000)
df_random <- df[c(treated_idx, random_7k), ]

result_random <- prepare_data(df_random, "TEST 1: RANDOM 7K CONTROLS")

# ============================================================================
# Test 2: Embedding K=20 Controls (if available)
# ============================================================================

# Try to load from diagnose_separation output or reconstruction
# For now, simulate by selecting worst overlap controls
cat("\n", paste(rep("=", 80), collapse=""), "\n", sep="")
cat("TEST 2: SIMULATED POOR-OVERLAP CONTROLS\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")
cat("(Selecting controls with worst fire overlap to simulate embedding behavior)\n\n")

# Get fire variables from treated
fire_cols <- grep("fire_|avg_BRIGHTNESS|avg_FRP", names(df), value = TRUE)
fire_cols <- intersect(fire_cols, names(df)[sapply(df, is.numeric)])

if (length(fire_cols) > 0) {
  # Calculate fire distance for each control
  treated_fire <- df[treated_idx, fire_cols, drop = FALSE]
  control_idx <- which(df$treated == 0)
  
  # For each control, calculate sum of squared differences from treated mean
  treated_mean <- colMeans(as.matrix(treated_fire), na.rm = TRUE)
  
  fire_distances <- sapply(control_idx, function(i) {
    control_vals <- as.numeric(df[i, fire_cols])
    sum((control_vals - treated_mean)^2, na.rm = TRUE)
  })
  
  # Select 7k controls with WORST fire overlap (largest distance)
  worst_fire_idx <- control_idx[order(fire_distances, decreasing = TRUE)[1:7000]]
  df_poor_fire <- df[c(treated_idx, worst_fire_idx), ]
  
  result_poor <- prepare_data(df_poor_fire, "POOR FIRE OVERLAP CONTROLS")
} else {
  cat("No fire variables found\n")
  result_poor <- NULL
}

# ============================================================================
# Comparison Summary
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("COMPARISON SUMMARY\n")
cat("================================================================================\n\n")

if (!is.null(result_random) && !is.null(result_poor)) {
  cat(sprintf("Random 7k controls:\n"))
  cat(sprintf("  Success: %s\n", ifelse(result_random$success, "YES", "NO")))
  cat(sprintf("  max|X*theta|: %.2f\n", result_random$max_X_theta))
  cat(sprintf("  Fire contribution: %.1f%%\n\n", result_random$fire_pct))
  
  cat(sprintf("Poor fire overlap controls (simulated embedding):\n"))
  cat(sprintf("  Success: %s\n", ifelse(result_poor$success, "YES", "NO")))
  cat(sprintf("  max|X*theta|: %.2f\n", result_poor$max_X_theta))
  cat(sprintf("  Fire contribution: %.1f%%\n\n", result_poor$fire_pct))
  
  cat("KEY FINDING:\n")
  if (result_poor$fire_pct > result_random$fire_pct * 2) {
    cat("  ✗ FIRE CONTRIBUTION DOUBLED with poor overlap\n")
    cat("  → Poor fire overlap AMPLIFIES fire's role in theta explosion\n")
  } else if (result_poor$fire_pct > result_random$fire_pct * 1.5) {
    cat("  ~ Fire contribution increased 50% with poor overlap\n")
    cat("  → Fire overlap matters but is not sole cause\n")
  } else {
    cat("  ✓ Fire contribution similar in both cases\n")
    cat("  → Fire overlap is NOT the key differentiator\n")
  }
}
