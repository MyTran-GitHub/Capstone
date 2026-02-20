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
  library("ggplot2")
  library("dplyr")
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript plot_att_forest.R <year> <K>")
}

treated_year <- as.integer(args[1])
optimal_K <- as.integer(args[2])

cat("="*80, "\n")
cat("ATT FOREST PLOT\n")
cat("="*80, "\n")
cat("Treatment year:", treated_year, "\n")
cat("Optimal K:", optimal_K, "\n\n")

# Setup output directory
output_dir <- paste0("Embeddings/data/figures/robustness_plots/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# STEP 1: Load ATT estimates
# ============================================================================

cat("Loading ATT estimates...\n")

# Define input directory
input_dir <- paste0("Embeddings/data/phase2_efficiency/", treated_year, "/")

# Load baseline ATT
baseline_file <- paste0(input_dir, "att_estimates_baseline_", treated_year, ".csv")
if (!file.exists(baseline_file)) {
  stop(paste("Baseline ATT not found:", baseline_file, "\nRun 06_compute_phase2_efficiency.R first!"))
}

att_baseline <- read.csv(baseline_file, stringsAsFactors = FALSE)
cat("✓ Baseline ATT loaded:", nrow(att_baseline), "years\n")

# Load embedding ATT
embedding_file <- paste0(input_dir, "att_estimates_embedding_k", optimal_K, "_", treated_year, ".csv")
if (!file.exists(embedding_file)) {
  stop(paste("Embedding ATT not found:", embedding_file, "\nRun 06_compute_phase2_efficiency.R first!"))
}

att_embedding <- read.csv(embedding_file, stringsAsFactors = FALSE)
cat("✓ Embedding ATT loaded:", nrow(att_embedding), "years\n\n")

# ============================================================================
# STEP 2: Combine and prepare data
# ============================================================================

# Combine datasets
att_combined <- rbind(att_baseline, att_embedding)

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

# ============================================================================
# STEP 3: Create forest plot
# ============================================================================

cat("Creating ATT forest plot...\n")

p_forest <- ggplot(att_combined, aes(x = year, y = att, color = method_label)) +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  # Confidence intervals
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.3, position = position_dodge(width = 0.5),
                linewidth = 0.8) +
  # Point estimates
  geom_point(aes(shape = method_label), 
             position = position_dodge(width = 0.5), 
             size = 3) +
  # Styling
  scale_color_manual(
    name = "Method",
    values = setNames(c("#377EB8", "#FF7F00"),
                     c("Baseline (Full Pool)", paste0("Embedding (K=", optimal_K, ")")))
  ) +
  scale_shape_manual(
    name = "Method",
    values = setNames(c(16, 17),
                     c("Baseline (Full Pool)", paste0("Embedding (K=", optimal_K, ")")))
  ) +
  labs(
    title = paste("Average Treatment Effect on the Treated (ATT): Year", treated_year),
    subtitle = "Error bars show 95% confidence intervals | Narrower CI = higher precision",
    x = "Post-Treatment Year",
    y = "ATT (Fire Frequency Difference)",
    caption = paste("Treatment year:", treated_year, "| Optimal K:", optimal_K)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Save plot
plot_file <- paste0(output_dir, "att_forest_", treated_year, "_k", optimal_K, ".pdf")
ggsave(plot_file, plot = p_forest, width = 10, height = 6, dpi = 600)
cat("✓ Saved ATT forest plot to:", plot_file, "\n")

plot_file_png <- paste0(output_dir, "att_forest_", treated_year, "_k", optimal_K, ".png")
ggsave(plot_file_png, plot = p_forest, width = 10, height = 6, dpi = 300)
cat("✓ Saved PNG version to:", plot_file_png, "\n\n")

# ============================================================================
# STEP 4: Create CI width comparison plot
# ============================================================================

cat("Creating CI width comparison plot...\n")

# Compute CI width comparison
ci_comparison <- att_combined %>%
  group_by(year) %>%
  summarise(
    baseline_ci_width = ci_width[method_label == "Baseline (Full Pool)"],
    embedding_ci_width = ci_width[method_label == paste0("Embedding (K=", optimal_K, ")")],
    .groups = "drop"
  ) %>%
  mutate(
    ci_width_reduction = baseline_ci_width - embedding_ci_width,
    ci_width_reduction_pct = 100 * ci_width_reduction / baseline_ci_width
  )

p_ci_width <- ggplot(ci_comparison, aes(x = year)) +
  geom_col(aes(y = baseline_ci_width, fill = "Baseline"), 
           position = "dodge", alpha = 0.7, width = 0.4) +
  geom_col(aes(y = embedding_ci_width, fill = "Embedding"),
           position = "dodge", alpha = 0.7, width = 0.4) +
  geom_text(aes(y = baseline_ci_width, 
                label = sprintf("%.4f", baseline_ci_width)),
            vjust = -0.5, size = 3, color = "#377EB8") +
  geom_text(aes(y = embedding_ci_width,
                label = sprintf("%.4f", embedding_ci_width)),
            vjust = -0.5, size = 3, color = "#FF7F00") +
  scale_fill_manual(
    name = "Method",
    values = c("Baseline" = "#377EB8", "Embedding" = "#FF7F00")
  ) +
  labs(
    title = "Confidence Interval Width Comparison",
    subtitle = paste("Narrower CI with embedding demonstrates precision gain | K =", optimal_K),
    x = "Post-Treatment Year",
    y = "95% CI Width",
    caption = paste("Mean CI width reduction:", 
                    sprintf("%.1f%%", mean(ci_comparison$ci_width_reduction_pct)))
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    panel.grid.minor = element_blank()
  )

# Save CI width plot
ci_width_file <- paste0(output_dir, "ci_width_comparison_", treated_year, "_k", optimal_K, ".pdf")
ggsave(ci_width_file, plot = p_ci_width, width = 10, height = 6, dpi = 600)
cat("✓ Saved CI width plot to:", ci_width_file, "\n")

ci_width_file_png <- paste0(output_dir, "ci_width_comparison_", treated_year, "_k", optimal_K, ".png")
ggsave(ci_width_file_png, plot = p_ci_width, width = 10, height = 6, dpi = 300)
cat("✓ Saved PNG version to:", ci_width_file_png, "\n\n")

# ============================================================================
# STEP 5: Create summary table plot
# ============================================================================

cat("Creating summary table...\n")

# Create summary data
summary_data <- att_combined %>%
  group_by(method_label) %>%
  summarise(
    n_years = n(),
    mean_att = mean(att, na.rm = TRUE),
    mean_se = mean(se, na.rm = TRUE),
    mean_ci_width = mean(ci_width, na.rm = TRUE),
    n_control = mean(n_control, na.rm = TRUE),
    .groups = "drop"
  )

# Add comparison metrics
baseline_mean_ci <- summary_data$mean_ci_width[summary_data$method_label == "Baseline (Full Pool)"]
embedding_mean_ci <- summary_data$mean_ci_width[summary_data$method_label != "Baseline (Full Pool)"]

ci_reduction_pct <- 100 * (baseline_mean_ci - embedding_mean_ci) / baseline_mean_ci

summary_table <- data.frame(
  Metric = c("Mean ATT", "Mean SE", "Mean CI Width", "Mean N Control", "CI Width Reduction (%)"),
  Baseline = c(
    sprintf("%.4f", summary_data$mean_att[1]),
    sprintf("%.4f", summary_data$mean_se[1]),
    sprintf("%.4f", summary_data$mean_ci_width[1]),
    sprintf("%.0f", summary_data$n_control[1]),
    "—"
  ),
  Embedding = c(
    sprintf("%.4f", summary_data$mean_att[2]),
    sprintf("%.4f", summary_data$mean_se[2]),
    sprintf("%.4f", summary_data$mean_ci_width[2]),
    sprintf("%.0f", summary_data$n_control[2]),
    sprintf("%.1f%%", ci_reduction_pct)
  )
)

# Save summary table
summary_file <- paste0("Embeddings/data/phase2_efficiency/", treated_year, 
                      "/att_summary_table_", treated_year, ".csv")
write.csv(summary_table, summary_file, row.names = FALSE)
cat("✓ Saved summary table to:", summary_file, "\n")

# Print summary
cat("\n")
cat("="*80, "\n")
cat("SUMMARY TABLE\n")
cat("="*80, "\n")
print(summary_table)
cat("\n")

cat("="*80, "\n")
cat("ATT FOREST PLOTS COMPLETE\n")
cat("="*80, "\n")
cat("\nOutputs:\n")
cat("1. ATT forest plot:", plot_file, "\n")
cat("2. CI width comparison:", ci_width_file, "\n")
cat("3. Summary table:", summary_file, "\n")
cat("\n")
