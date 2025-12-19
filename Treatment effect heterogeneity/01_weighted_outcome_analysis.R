# 01_weighted_outcome_analysis.R
# Unified outcome analysis for CBPS-weighted data, compatible with heterogeneity pipeline

library(tidyverse)
library(survey)

input_dir <- 'data/processed_data/rev_analysis_low/'

output_dir <- 'data/processed_data/rev_analysis_low/heterogeneity_results/'

# Use double backslashes for regex in R strings
cbps_files <- list.files(input_dir, pattern = '^cbps_weighted_\\d{4}_conifer.RDS$', full.names = TRUE)
years <- as.integer(str_extract(cbps_files, '\\d{4}'))
# Restrict to years 2000-2009
keep_idx <- which(years >= 2000 & years <= 2009)
cbps_files <- cbps_files[keep_idx]
years <- years[keep_idx]
cbps_files <- cbps_files[order(years)]
years <- sort(years)

cat('Found CBPS-weighted files for years:', paste(years, collapse=", "), "\n")

results <- list()
for (i in seq_along(cbps_files)) {
  year <- years[i]
  file <- cbps_files[i]
  cat("\n---\nYear:", year, "\nFile:", file, "\n")
  df <- readRDS(file)

  # Basic checks
  cat("Rows:", nrow(df), "Cols:", ncol(df), "\n")
  cat("Treated units:", sum(df$treated == 1), ", Control units:", sum(df$treated == 0), "\n")
  cat("Weight summary:\n"); print(summary(df$cbps_weight))

  # Check for NA or Inf in weights
  if (any(is.na(df$cbps_weight)) || any(is.infinite(df$cbps_weight))) {
    cat('Warning: NA or Inf detected in cbps_weight for year', year, '- skipping this year.\n')
    next
  }

  # Specify your outcome variable here
  outcome_var <- paste0('max_FRP_', year)
  if (!outcome_var %in% names(df)) {
    cat('Outcome variable', outcome_var, 'not found for year', year, '- skipping.\n')
    next
  }
  design <- svydesign(ids = ~1, weights = ~cbps_weight, data = df)
  fit <- svyglm(as.formula(paste(outcome_var, "~ treated")), design = design)
  print(summary(fit))
  results[[as.character(year)]] <- coef(summary(fit))
}

# Save results
saveRDS(results, file = file.path(output_dir, 'cbps_weighted_outcome_results.RDS'))
cat("\nWeighted outcome analysis completed for years 2000-2009 only.\n")
