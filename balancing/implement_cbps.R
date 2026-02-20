## Covariate balancing synthetic control for conifer-only pipeline
## Apply synthetic control approach via covariate balancing (Imai 2014, Zhao 2019, Tan 2020)
## Calculate ATT weights for each unit (conifer area only, focal years 2005:2020)
##
## Note: SWE uses a two-part representation (presence + log-intensity). max_FRP, prcp, and
## avg_BRIGHTNESS are log1p winsorized to reduce extreme tails while preserving ordering.
library("sf")

source("balancing/cbps_ATT.R")

outDir <- "data/processed_data/rev_analysis_low/"

years <- 2005:2020  # Focal treatment years with sufficient pre-treatment data

for (treated.year in years) {
  input_file <- paste0(outDir, "analysis_treated", treated.year, "_conifer.RDS")
  
  cat("Processing year:", treated.year, "\n")
  
  if (!file.exists(input_file)) {
    cat("  File not found, skipping.\n")
    next
  }
  
  df <- readRDS(input_file)
  W <- df$treated

  # Only keep covariates that aim to balance
  X <- df
  X$unit <- NULL
  X$LATITUDE <- NULL
  X$LONGITUDE <- NULL
  X$treated <- NULL
  X$num.fire <- NULL

  # Remove any non-numeric columns
  X <- X[, sapply(X, is.numeric), drop = FALSE]
  # Remove zero-variance columns (e.g., conifer)
  X <- X[, apply(X, 2, sd, na.rm=TRUE) > 0, drop = FALSE]

 
  # Two-part SWE: presence indicator + log-intensity (winsorized)
  swe_cols <- grep("^swe_", colnames(X), value = TRUE)
  if (length(swe_cols) > 0) {
    cols_to_remove <- c()
    cols_converted <- 0

    for (col in swe_cols) {
      x <- X[[col]]
      pct_zero <- sum(x == 0 | is.na(x), na.rm = TRUE) / length(x)

      # If >95% zero, remove entirely (no discriminatory power for covariate balance)
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

  # Log1p + winsorize max_FRP_* to preserve intensity ordering and reduce tail risk
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

  # Drop extremely sparse fire_* columns to avoid huge z-scores from rare events
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
  
  # Standardize covariates
  X.mean <- colMeans(X, na.rm=TRUE)
  X.sd <- apply(X, 2, sd, na.rm=TRUE)
  X.sd[is.na(X.sd) | X.sd == 0] <- 1  # in case Xj is constant
  X.scl <- scale(X, center = X.mean, scale = X.sd)

  # Run CBPS-ATT with regularization grid search
  # Grid: lambda = 10^(-6 to 1) across 8 levels
  res_regu.list <- lapply(1:8, function(n) {
    res <- cbps_att(as.matrix(X.scl),
                    W,
                    theta.init = rep(0, ncol(X) + 1),
                    control = list(trace = 10, maxit = 6000),
                    lambda = rep(10^(n - 7), ncol(X)))
    return(res)
  })

  # Check BOTH convergence AND weight validity during lambda selection
  # (prevents selecting numerically unstable solutions)
  converge_set <- sapply(res_regu.list, function(res) {
    converged <- (res$convergence == 0)
    valid_weights <- !any(is.na(res$weights.0)) && !any(is.infinite(res$weights.0)) &&
                     !any(is.na(res$weights.1)) && !any(is.infinite(res$weights.1))
    return(converged && valid_weights)
  })
  
  if (!any(converge_set)) {
    cat("  No solution with valid convergence AND valid weights found, skipping.\n")
    next
  }
  
  idx <- min(which(converge_set))
  res <- res_regu.list[[idx]]
  rho <- 10^(idx - 7)
  
  # Check post-balance covariate balance
  max_balance_std <- max(abs(res$balance.std), na.rm=TRUE)
  median_balance_std <- median(abs(res$balance.std), na.rm=TRUE)
  
  cat("  Covariate balance: median |SMD| =", round(median_balance_std, 3),
      ", max |SMD| =", round(max_balance_std, 3), "\n")
  
  if (max_balance_std > 0.5) {
    cat("  ⚠ WARNING: Max |balance.std| = ", round(max_balance_std, 3), 
        " (recommend |SMD| < 0.1)\n", sep="")
  }
  
  # Create weights table (treated get weights.1, control get weights.0)
  weights_df <- data.frame(
    unit = df$unit,
    treated = df$treated,
    weight = ifelse(df$treated == 1, res$weights.1, res$weights.0)
  )

  # Save fit results and weights
  saveRDS(res, paste0(outDir, "cbps_fit_", treated.year, "_conifer_rho", rho, ".RDS"))
  saveRDS(weights_df, paste0(outDir, "cbps_weights_", treated.year, "_conifer.RDS"))
  cat("  Saved: cbps_fit_", treated.year, "_conifer_rho", rho, ".RDS\n", sep = "")
  cat("  Saved: cbps_weights_", treated.year, "_conifer.RDS\n", sep = "")
  
  gc()
}