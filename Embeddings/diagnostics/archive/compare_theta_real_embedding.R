#!/usr/bin/env Rscript
# ============================================================================
# REAL TEST: Theta Contributors in Random vs ACTUAL Embedding Controls
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  cat("Usage: Rscript compare_theta_real_embedding.R <year> <K>\n")
  quit(status = 1)
}

year <- as.integer(args[1])
K <- as.integer(args[2])

suppressMessages({
  library(dplyr)
})

source("balancing/cbps_ATT.R")

prepare_and_analyze <- function(df_subset, name) {
  cat("\n", paste(rep("=", 80), collapse=""), "\n", sep="")
  cat(name, "\n")
  cat(paste(rep("=", 80), collapse=""), "\n\n")
  
  W <- df_subset$treated
  
  # Prepare covariates (standard pipeline)
  exclude_cols <- c("treated", "unit", "year", "fire", "avg_BRIGHTNESS", "avg_FRP",
                    grep("post_", names(df_subset), value=TRUE))
  X_raw <- as.matrix(df_subset[, setdiff(names(df_subset), exclude_cols)])
  
  # Log transformation
  log_vars <- grep("prcp|tmmn|tmmx|fire|BRIGHTNESS|FRP", colnames(X_raw), value = TRUE)
  X <- X_raw
  for (var in log_vars) {
    if (var %in% colnames(X)) {
      X[, var] <- log(X[, var] + 1)
    }
  }
  
  # Winsorize
  for (j in 1:ncol(X)) {
    q <- quantile(X[, j], probs = c(0.01, 0.99), na.rm = TRUE)
    X[, j] <- pmax(pmin(X[, j], q[2]), q[1])
  }
  
  # Standardize
  X_scl <- scale(X)
  sds <- apply(X_scl, 2, sd, na.rm = TRUE)
  keep <- which(sds >= 0.01)
  X_scl <- X_scl[, keep, drop = FALSE]
  
  cat(sprintf("Sample: %d treated + %d controls = %d total\n", 
              sum(W), sum(W == 0), length(W)))
  cat(sprintf("Covariates: %d\n", ncol(X_scl)))
  cat(sprintf("Obs:Cov: %.1f:1\n\n", nrow(X_scl) / ncol(X_scl)))
  
  # Identify variable types
  fire_idx <- grep("fire_|BRIGHTNESS|FRP", colnames(X_scl))
  prcp_idx <- grep("prcp_", colnames(X_scl))
  swe_idx <- grep("swe_", colnames(X_scl))
  
  cat(sprintf("Variable types: Fire=%d, Precipitation=%d, SWE=%d, Other=%d\n\n",
              length(fire_idx), length(prcp_idx), length(swe_idx),
              ncol(X_scl) - length(fire_idx) - length(prcp_idx) - length(swe_idx)))
  
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
  
  cat(sprintf("Convergence: %d (0=success)\n", fit$convergence))
  
  has_invalid <- any(is.na(weights)) || any(is.infinite(weights))
  if (has_invalid) {
    cat(sprintf("Weights: ✗ NaN=%d, Inf=%d\n", sum(is.na(weights)), sum(is.infinite(weights))))
  } else {
    cat(sprintf("Weights: ✓ Valid, range [%.3f, %.3f]\n", min(weights), max(weights)))
  }
  
  X_theta <- X_scl %*% theta
  max_X_theta <- max(abs(X_theta))
  cat(sprintf("max|X*theta|: %.2f ", max_X_theta))
  if (max_X_theta > 700) {
    cat("(OVERFLOW - will produce Inf)\n\n")
  } else if (max_X_theta > 100) {
    cat("(HIGH RISK)\n\n")
  } else {
    cat("(SAFE)\n\n")
  }
  
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
  
  # Show top 15
  cat("TOP 15 CONTRIBUTORS:\n")
  cat(paste(rep("-", 70), collapse=""), "\n")
  print(var_analysis[1:15, c("Variable", "Type", "Theta", "Max_Contribution")], row.names = FALSE)
  
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
  
  cat("CONTRIBUTION BY TYPE:\n")
  cat(paste(rep("-", 70), collapse=""), "\n")
  print(type_stats, row.names = FALSE)
  
  # Count fire vars in top ranks
  fire_in_top10 <- sum(var_analysis$Type[1:10] == "Fire")
  fire_in_top20 <- sum(var_analysis$Type[1:20] == "Fire")
  
  cat(sprintf("\nFire variables in top 10: %d\n", fire_in_top10))
  cat(sprintf("Fire variables in top 20: %d\n", fire_in_top20))
  
  return(list(
    success = !has_invalid,
    max_X_theta = max_X_theta,
    var_analysis = var_analysis,
    type_stats = type_stats,
    fire_pct = type_stats$Pct[type_stats$Type == "Fire"],
    fire_in_top10 = fire_in_top10,
    fire_in_top20 = fire_in_top20
  ))
}

# ============================================================================
# Load Data
# ============================================================================

cat("================================================================================\n")
cat("REAL EMBEDDING TEST - Year:", year, "K:", K, "\n")
cat("================================================================================\n")

outDir <- "data/processed_data/rev_analysis_low/"
data_path <- paste0(outDir, "analysis_treated", year, "_conifer.RDS")
df <- readRDS(data_path)

treated_idx <- which(df$treated == 1)
n_treated <- length(treated_idx)

cat(sprintf("\nDataset: %d treated, %d control pool\n", n_treated, sum(df$treated == 0)))

# ============================================================================
# Test 1: Random Controls (matching embedding size)
# ============================================================================

# Load embedding to get size
emb_path <- sprintf("Embeddings/results/cbps_integration/%d/selected_controls_k%d_%d.csv", year, K, year)
if (!file.exists(emb_path)) {
  stop("Embedding file not found: ", emb_path)
}

emb_units <- read.csv(emb_path)$unit
n_emb <- length(emb_units)

cat(sprintf("Embedding file has %d controls\n", n_emb))

# Random sample of same size
set.seed(12345)
random_controls <- sample(which(df$treated == 0), n_emb)
df_random <- df[c(treated_idx, random_controls), ]

result_random <- prepare_and_analyze(df_random, 
                                     sprintf("TEST 1: RANDOM %d CONTROLS", n_emb))

# ============================================================================
# Test 2: ACTUAL Embedding-Selected Controls
# ============================================================================

# Filter to embedding-selected units
df_embedding <- df[df$treated == 1 | df$unit %in% emb_units, ]

result_embedding <- prepare_and_analyze(df_embedding, 
                                        sprintf("TEST 2: ACTUAL EMBEDDING K=%d CONTROLS", K))

# ============================================================================
# Comparison Summary
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("COMPARISON SUMMARY\n")
cat("================================================================================\n\n")

if (!is.null(result_random) && !is.null(result_embedding)) {
  cat(sprintf("%-30s | Random %.1fk | Embedding K=%d | Ratio\n", "Metric", n_emb/1000, K))
  cat(paste(rep("-", 75), collapse=""), "\n")
  cat(sprintf("%-30s | %-12s | %-16s | %.1fx\n", 
              "Success", 
              ifelse(result_random$success, "YES", "NO"),
              ifelse(result_embedding$success, "YES", "NO"),
              NA))
  cat(sprintf("%-30s | %12.2f | %16.2f | %.1fx\n", 
              "max|X*theta|",
              result_random$max_X_theta,
              result_embedding$max_X_theta,
              result_embedding$max_X_theta / result_random$max_X_theta))
  cat(sprintf("%-30s | %11.1f%% | %15.1f%% | %.1fx\n", 
              "Fire contribution %",
              result_random$fire_pct,
              result_embedding$fire_pct,
              result_embedding$fire_pct / result_random$fire_pct))
  cat(sprintf("%-30s | %12d | %16d | \n", 
              "Fire vars in top 10",
              result_random$fire_in_top10,
              result_embedding$fire_in_top10))
  cat(sprintf("%-30s | %12d | %16d | \n", 
              "Fire vars in top 20",
              result_random$fire_in_top20,
              result_embedding$fire_in_top20))
  
  cat("\n")
  cat("KEY FINDING:\n")
  cat(paste(rep("-", 75), collapse=""), "\n")
  
  ratio <- result_embedding$fire_pct / result_random$fire_pct
  
  if (!result_embedding$success && result_random$success) {
    cat("✗ EMBEDDING CAUSES CBPS FAILURE\n")
    cat(sprintf("  Fire contribution increased %.1fx (%.1f%% → %.1f%%)\n",
                ratio, result_random$fire_pct, result_embedding$fire_pct))
    cat("  → Poor fire overlap in embedding is THE problem\n")
  } else if (ratio > 3) {
    cat("✗ FIRE CONTRIBUTION TRIPLED with embedding\n")
    cat(sprintf("  %.1f%% (random) → %.1f%% (embedding)\n",
                result_random$fire_pct, result_embedding$fire_pct))
    cat("  → Embedding's fire overlap is critical bottleneck\n")
  } else if (ratio > 2) {
    cat("✗ FIRE CONTRIBUTION DOUBLED with embedding\n")
    cat("  → Poor fire overlap amplifies theta explosion risk\n")
  } else if (ratio > 1.5) {
    cat("~ Fire contribution increased 50% with embedding\n")
    cat("  → Fire overlap matters but may not be sole cause\n")
  } else {
    cat("✓ Fire contribution similar in both cases\n")
    cat("  → Fire overlap NOT the key differentiator\n")
  }
}
