## Covariate balancing synthetic control for conifer-only pipeline
## Apply synthetic control approach via covariate balancing (Imai 2014, Zhao 2019, Tan 2020)
## Calculate ATT weights for each unit (conifer area only, focal years 2000:2020)
library("sf")

source("balancing/cbps_ATT.R")

outDir <- "data/processed_data/rev_analysis_low/"

years <- 2000:2020

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

  # Standardize covariates
  X.mean <- colMeans(X)
  X.sd <- apply(X, 2, sd)
  X.sd[X.sd == 0] <- 1  # in case Xj is constant
  X.scl <- scale(X, center = X.mean, scale = X.sd)

  # Run CBPS-ATT with regularization grid
  res_regu.list <- lapply(1:8, function(n) {
    res <- cbps_att(as.matrix(X.scl),
                    W,
                    theta.init = rep(0, ncol(X) + 1),
                    control = list(trace = 10, maxit = 5000),
                    lambda = rep(10^(n - 7), ncol(X)))
    return(res)
  })

  converge_set <- sapply(res_regu.list, function(res) res$convergence)
  
  if (!any(converge_set == 0)) {
    cat("  No converged solution found, skipping.\n")
    next
  }
  
  idx <- min(which(converge_set == 0))
  res <- res_regu.list[[idx]]
  rho <- 10^(idx - 7)

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