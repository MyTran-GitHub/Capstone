# Diagnostics and summary for baseline SCM results
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(dplyr)
library(ggplot2)
library(sf)
library(here)

setwd(here::here())

# Load subset and SCM result
subset_dat <- readRDS("data/processed_data/subset_conifer2012_region.rds")
scm_files <- list.files("data/processed_data", pattern = "^scm_result_.*\\.rds$", full.names = TRUE)

if (length(scm_files) == 0) {
  stop("No SCM results found. Run R/03_baseline_scm.R first.")
}

# Drop geometry if present (sf object issue)
if ("geometry" %in% names(subset_dat)) {
  subset_dat <- st_drop_geometry(subset_dat)
}

# Process first result (can loop for multiple later)
scm_result <- readRDS(scm_files[1])
treated_unit <- gsub(".*scm_result_(.*)\\.rds", "\\1", basename(scm_files[1]))

message("=== Baseline SCM Diagnostics ===")
message("Treated unit: ", treated_unit)
message("Number of donor units: ", length(scm_result$donor_units))
message("Donors with weight > 1%: ", sum(scm_result$weights > 0.01))
message("Max donor weight: ", round(max(scm_result$weights), 3))

# Pre-treatment fit metrics
pre_rmspe <- sqrt(mean((scm_result$pre_df$treated - scm_result$pre_df$synthetic)^2))
pre_mae <- mean(abs(scm_result$pre_df$treated - scm_result$pre_df$synthetic))
message("\nPre-treatment fit:")
message("  RMSPE: ", round(pre_rmspe, 2))
message("  MAE: ", round(pre_mae, 2))

# Post-treatment effects
post_avg_gap <- mean(scm_result$post_df$gap, na.rm = TRUE)
post_cumulative <- sum(scm_result$post_df$gap, na.rm = TRUE)
message("\nPost-treatment effects:")
message("  Average gap: ", round(post_avg_gap, 2))
message("  Cumulative gap: ", round(post_cumulative, 2))

# Covariate balance check (simple pre-treatment fire history)
focal_year <- 2012

treated_covars <- subset_dat %>% 
  filter(unit == treated_unit, year == focal_year) %>% 
  select(any_fire_lag1, any_fire_lag2, any_fire_lag3, max_FRP_lag1, max_FRP_lag2, max_FRP_lag3)

donor_covars <- subset_dat %>% 
  filter(unit %in% scm_result$donor_units, year == focal_year) %>%
  select(any_fire_lag1, any_fire_lag2, any_fire_lag3, max_FRP_lag1, max_FRP_lag2, max_FRP_lag3)

# Weighted donor mean
weighted_donor_covars <- sapply(names(treated_covars), function(v) {
  vals <- donor_covars[[v]]
  if (length(vals) == length(scm_result$weights)) {
    sum(vals * scm_result$weights, na.rm = TRUE)
  } else {
    NA_real_
  }
})

balance_df <- data.frame(
  covariate = names(treated_covars),
  treated = as.numeric(treated_covars[1,]),
  synthetic = weighted_donor_covars,
  diff = as.numeric(treated_covars[1,]) - weighted_donor_covars
)

message("\nCovariate balance (treated vs synthetic):")
print(balance_df, row.names = FALSE)

# Save diagnostics
saveRDS(list(
  treated_unit = treated_unit,
  pre_rmspe = pre_rmspe,
  pre_mae = pre_mae,
  post_avg_gap = post_avg_gap,
  post_cumulative = post_cumulative,
  balance = balance_df,
  num_donors = length(scm_result$donor_units),
  weights = scm_result$weights
), file = "data/processed_data/baseline_scm_diagnostics.rds")

message("\n✓ Diagnostics saved to data/processed_data/baseline_scm_diagnostics.rds")
