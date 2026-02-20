#!/usr/bin/env Rscript
## ATT Forest Plot: Visualize Treatment Effects with Confidence Intervals
##
## Compares ATT estimates between baseline and embedding methods
## Shows precision gains through narrower confidence intervals
##
## Usage:
##   Rscript scripts/figures/plot_att_forest.R <year> <K>
##
## Arguments:
##   year: Treatment year (e.g., 2019)
##   K: Optimal K value (e.g., 50)
##
## Requires:
##   - Phase 2 efficiency analysis outputs
##   - ATT estimates with confidence intervals
##
## Example:
##   Rscript scripts/figures/plot_att_forest.R 2019 50

suppressPackageStartupMessages({
  required_pkgs <- c("ggplot2", "dplyr", "tidyr")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(paste("Missing required packages:", paste(missing_pkgs, collapse = ", "), "\nPlease install them with install.packages()."))
  }
  lapply(required_pkgs, library, character.only = TRUE)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript plot_att_forest.R <year> <K>")
}

treated_year <- as.integer(args[1])
optimal_K <- as.integer(args[2])

cat(strrep("=", 80), "\n")
cat("ATT FOREST PLOT\n")
cat(strrep("=", 80), "\n")
cat("Treatment year:", treated_year, "\n")
cat("Optimal K:", optimal_K, "\n\n")

# Setup output directory
output_dir <- paste0("data/figures/robustness_plots/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# STEP 1: Load ATT estimates
# ============================================================================

cat("Loading ATT estimates...\n")

# Define input directory
input_dir <- paste0("data/phase2_efficiency/", treated_year, "/")

# Load baseline ATT
baseline_file <- paste0(input_dir, "att_estimates_baseline_", treated_year, ".csv")
if (!file.exists(baseline_file)) {
  stop(paste("Baseline ATT not found:", baseline_file, "\nRun 06_compute_phase2_efficiency.R first!"))
}


# Load and check baseline ATT
att_baseline <- read.csv(baseline_file, stringsAsFactors = FALSE)
cat("✓ Baseline ATT loaded:", nrow(att_baseline), "years\n")
required_cols <- c("year", "att", "se", "ci_lower", "ci_upper", "ci_width", "n_control", "method")
missing_cols <- setdiff(required_cols, names(att_baseline))
if (length(missing_cols) > 0) {
  stop(paste("Baseline ATT file missing columns:", paste(missing_cols, collapse = ", ")))
}

# Load and check embedding ATT
embedding_file <- paste0(input_dir, "att_estimates_embedding_k", optimal_K, "_", treated_year, ".csv")
if (!file.exists(embedding_file)) {
  stop(paste("Embedding ATT not found:", embedding_file, "\nRun 06_compute_phase2_efficiency.R first!"))
}
att_embedding <- read.csv(embedding_file, stringsAsFactors = FALSE)
cat("✓ Embedding ATT loaded:", nrow(att_embedding), "years\n\n")
missing_cols2 <- setdiff(required_cols, names(att_embedding))
if (length(missing_cols2) > 0) {
  stop(paste("Embedding ATT file missing columns:", paste(missing_cols2, collapse = ", ")))
}

# ============================================================================
# STEP 2: Combine and prepare data
# ============================================================================


# Combine datasets
att_combined <- rbind(att_baseline, att_embedding)

# Save att_combined for reproducibility
att_combined_file <- paste0(input_dir, "att_combined_k", optimal_K, ".csv")
write.csv(att_combined, att_combined_file, row.names = FALSE)
cat("✓ Saved att_combined to:", att_combined_file, "\n")

# Clean method names for plotting
att_combined$method_label <- ifelse(
  grepl("baseline", att_combined$method),
  "Baseline (Full Pool)",
  paste0("Embedding (K=", optimal_K, ")")
)

# Order years and methods for plotting
att_combined$year <- as.factor(att_combined$year)
att_combined$method_label <- factor(att_combined$method_label, 
                                   levels = c("Baseline (Full Pool)", 
                                            paste0("Embedding (K=", optimal_K, ")")))

# Use forestplot package for robust forest plot
if (!requireNamespace("forestplot", quietly = TRUE)) {
  cat("Installing forestplot package...\n")
  install.packages("forestplot")
}
library(forestplot)

# Prepare data for forestplot
fp_data <- att_combined %>%
  arrange(method_label, year) %>%
  mutate(
    label = paste(method_label, year, sep = ": "),
    mean = att,
    lower = ci_lower,
    upper = ci_upper
  )

tabletext <- cbind(
  c("Method-Year", fp_data$label),
  c("ATT", sprintf("%.3f", fp_data$mean)),
  c("Lower CI", sprintf("%.3f", fp_data$lower)),
  c("Upper CI", sprintf("%.3f", fp_data$upper))
)

# Forestplot


png_file <- paste0(output_dir, "att_forest_", treated_year, "_k", optimal_K, "_forestplot.png")

# Save PNG only
tryCatch({
  png(png_file, width = 1000, height = 600)
  forestplot(
    tabletext,
    mean = fp_data$mean_att,
    lower = fp_data$ci_lower,
    upper = fp_data$ci_upper,
    zero = 0,
    boxsize = 0.3,
    col = fp_colors,
    xlab = "ATT Estimate",
    title = "Forest Plot of ATT Estimates"
  )
  dev.off()
  cat("✓ Saved PNG to:", png_file, "\n")
}, error = function(e) {
  cat("Forest plot failed: ", e$message, "\n")
})

