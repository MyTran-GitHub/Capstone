## Covariate balancing synthetic control for conifer-only pipeline
library("sf")
source("balancing/cbps_ATT.R")

outDir <- "data/processed_data/rev_analysis_low/"
years <- 2000:2020



for (treated.year in years) {
  input_file <- paste0(outDir, "analysis_treated", treated.year, "_conifer.RDS")
  cat("\n---\nYear:", treated.year, "\nInput file:", input_file, "\n")
  if (!file.exists(input_file)) {
    cat("File does not exist, skipping.\n")
    next
  }
  df <- readRDS(input_file)
  W <- df$treated

  # Only keep covariates to balance (remove identifiers and treatment info)
  X <- df
  X$unit <- NULL
  X$num.fire <- NULL
  X$treated <- NULL

  # Remove any non-numeric columns (e.g., factors, characters)
  X <- X[, sapply(X, is.numeric), drop = FALSE]

  # Exclude all minat (min temperature) variables
  minat_cols <- grep("^minat", names(X), value = TRUE)
  if (length(minat_cols) > 0) {
    cat("Excluding minat columns:", paste(minat_cols, collapse=", "), "\n")
    X <- X[, !(names(X) %in% minat_cols), drop = FALSE]
  }

  # Investigate columns with high NA proportion
  na_prop <- colMeans(is.na(X))
  high_na_cols <- names(na_prop[na_prop > 0.5])
  cat("Columns with >50% NA (", length(high_na_cols), "):\n")
  print(high_na_cols)
  cat("Top 10 columns by NA proportion:\n")
  print(sort(na_prop, decreasing = TRUE)[1:min(10, length(na_prop))])

  # Summarize which layers/sources are affected
  if (length(high_na_cols) > 0) {
    # Extract prefix before first underscore as layer/source
    layer_prefix <- sub("_.*", "", high_na_cols)
    layer_table <- table(layer_prefix)
    cat("\nSummary of columns with >50% NA by layer/source:\n")
    print(layer_table)
  }

  # Remove rows with any NA or infinite values
  na_rows <- apply(X, 1, function(row) any(is.na(row) | is.infinite(row)))
  cat("Rows with NA/Inf:", sum(na_rows), "\n")
  if (any(na_rows)) {
    X <- X[!na_rows, , drop = FALSE]
    W <- W[!na_rows]
  }

  # If X has zero columns after filtering, skip this iteration
  if (ncol(X) == 0) {
    cat("No numeric covariates for year", treated.year, "- skipping.\n")
    next
  }

  # Standardize covariates
  X.mean <- colMeans(X, na.rm = TRUE)
  X.sd <- apply(X, 2, sd, na.rm = TRUE)
  X.sd[X.sd == 0] <- 1
  X.scl <- scale(X, center = X.mean, scale = X.sd)
  cat("After scaling, X.scl dims:", dim(X.scl), "\n")

  # Check for NA or infinite values in X.scl
  if (any(!is.finite(X.scl))) {
    cat("Non-finite values found in X.scl for year", treated.year, "- skipping.\n")
    next
  }

  # Check that W is binary and has both classes
  if (!all(W %in% c(0, 1)) || length(unique(W)) < 2) {
    cat("W is not binary or has only one class for year", treated.year, "- skipping.\n")
    next
  }

  # Check that X.scl has at least one row and one column
  if (nrow(X.scl) == 0 || ncol(X.scl) == 0) {
    cat("X.scl has zero rows or columns for year", treated.year, "- skipping.\n")
    next
  }

  # Check that theta.init is finite
  theta.init <- rep(0, ncol(X.scl) + 1)
  if (any(!is.finite(theta.init))) {
    cat("Non-finite theta.init for year", treated.year, "- skipping.\n")
    next
  }

  # Check dimensions before cbps_att
  if (!is.matrix(X.scl) || nrow(X.scl) != length(W)) {
    cat("nrow(X.scl):", nrow(X.scl), "length(W):", length(W), "\n")
    stop("X.scl must be a numeric matrix with nrow equal to length(W)")
  }

  # Run CBPS-ATT with regularization grid
  res_regu.list <- list()
  for (n in 1:8) {
    lambda_vec <- rep(10^(n - 7), ncol(X.scl))
    tryCatch({
      system.time(res <- cbps_att(as.matrix(X.scl), W,
                                  theta.init = theta.init,
                                  control = list(trace = 10, maxit = 5000),
                                  lambda = lambda_vec))
      res_regu.list[[n]] <- res
    }, error = function(e) {
      cat("Error in cbps_att for year", treated.year, "lambda", lambda_vec[1], ":", e$message, "\n")
      res_regu.list[[n]] <- NULL
    })
  }
  converge_set <- sapply(res_regu.list, function(res) !is.null(res) && !is.null(res$convergence) && res$convergence == 0)
  if (!any(converge_set)) {
    cat("No converged solution for year", treated.year, "- skipping.\n")
    next
  }
  idx <- min(which(converge_set))
  res <- res_regu.list[[idx]]
  rho <- 10^(idx - 7)

  # Save weights and results for downstream pipeline
  df$cbps_weight <- res$weights.0
  saveRDS(df, file = paste0(outDir, "cbps_weighted_", treated.year, "_conifer.RDS"))
  saveRDS(res, file = paste0(outDir, "cbps_fit_", treated.year, "_conifer_rho", rho, ".RDS"))
  gc()
}