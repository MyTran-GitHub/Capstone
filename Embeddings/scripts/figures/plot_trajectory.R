#!/usr/bin/env Rscript
## Trajectory Plot: Pre- and Post-Treatment Fire Frequency
##
## Visualizes parallel trends assumption and treatment effect over time
## Compares baseline (full pool) vs embedding (filtered pool) matching quality
##
## Usage:
##   Rscript scripts/figures/plot_trajectory.R <year> <K> <train_start> <train_end> <test_start> <test_end> [post_years]
##
## Arguments:
##   year: Treatment year (e.g., 2019)
##   K: Optimal K value (e.g., 50)
##   train_start: Start of training period (e.g., 2000)
##   train_end: End of training period (e.g., 2010)
##   test_start: Start of test period (e.g., 2011)
##   test_end: End of test period (e.g., 2015)
##   post_years: Optional comma-separated post-treatment years (e.g., "2020,2021")
##
## Example:
##   Rscript scripts/figures/plot_trajectory.R 2019 50 2000 2010 2011 2015 "2020,2021"

suppressPackageStartupMessages({
  library("ggplot2")
  library("dplyr")
  library("tidyr")
})

source("balancing/calculate_fire_outcomes.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 6) {
  stop("Usage: Rscript plot_trajectory.R <year> <K> <train_start> <train_end> <test_start> <test_end> [post_years]")
}

treated_year <- as.integer(args[1])
optimal_K <- as.integer(args[2])
train_start <- as.integer(args[3])
train_end <- as.integer(args[4])
test_start <- as.integer(args[5])
test_end <- as.integer(args[6])

# Parse optional post-treatment years
if (length(args) >= 7) {
  post_years <- as.integer(strsplit(args[7], ",")[[1]])
} else {
  post_years <- NULL
}

cat(strrep("=", 80), "\n")
cat("TRAJECTORY PLOT\n")
cat(strrep("=", 80), "\n")
cat("Treatment year:", treated_year, "\n")
cat("Optimal K:", optimal_K, "\n")
cat("Train period:", train_start, "-", train_end, "\n")
cat("Test period:", test_start, "-", test_end, "\n")
if (!is.null(post_years)) {
  cat("Post-treatment years:", paste(post_years, collapse = ", "), "\n")
}
cat("\n")

# Setup output directory
output_dir <- paste0("data/figures/trajectory_plots/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# STEP 1: Load CBPS weights
# ============================================================================

cat("Loading CBPS weights...\n")

# Baseline weights
baseline_weights_file <- paste0("data/outputs/balance/cbps_weights_", treated_year, "_conifer.RDS")

if (!file.exists(baseline_weights_file)) {
  stop(paste("Baseline weights not found:", baseline_weights_file))
}

weights_baseline_list <- readRDS(baseline_weights_file)
weights_baseline <- weights_baseline_list$weights

cat("✓ Baseline weights loaded:", nrow(weights_baseline), "pixels\n")

# Embedding weights
embedding_weights_file <- paste0("data/cbps_integration/", treated_year,
                                "/cbps_weights_full_k", optimal_K, "_", treated_year, ".csv")

if (!file.exists(embedding_weights_file)) {
  stop(paste("Embedding weights not found:", embedding_weights_file))
}

weights_embedding <- read.csv(embedding_weights_file, stringsAsFactors = FALSE)

cat("✓ Embedding weights loaded:", nrow(weights_embedding), "pixels\n\n")

# ============================================================================
# STEP 2: Calculate fire frequency trajectories
# ============================================================================

# Pre-treatment years
pre_years <- seq(train_start, test_end)

# All years to include
if (!is.null(post_years)) {
  all_years <- c(pre_years, post_years)
} else {
  all_years <- pre_years
}

cat("Calculating fire frequency trajectories...\n")

# Baseline trajectory
fire_baseline <- calculate_fire_frequency(
  weights_df = weights_baseline,
  firms_rds_path = "data/processed_data/FIRMS.RDS",
  years_to_include = all_years
)

fire_baseline$method <- "Baseline"

# Embedding trajectory
fire_embedding <- calculate_fire_frequency(
  weights_df = weights_embedding,
  firms_rds_path = "data/processed_data/FIRMS.RDS",
  years_to_include = all_years
)

fire_embedding$method <- "Embedding"

cat("✓ Trajectories calculated\n\n")

# ============================================================================
# STEP 3: Create trajectory plot
# ============================================================================

cat("Creating trajectory plot...\n")

# Combine data
fire_combined <- rbind(fire_baseline, fire_embedding)

# Add period labels
fire_combined$period <- "Pre-treatment"
fire_combined$period[fire_combined$year > treated_year] <- "Post-treatment"

# Add group labels
fire_combined$group <- ifelse(fire_combined$treated == 1, "Treated", "Control")

# Calculate gap (treated - control) for each method
fire_wide <- fire_combined %>%
  select(year, method, treated, fire.frac) %>%
  pivot_wider(names_from = treated, values_from = fire.frac, names_prefix = "treated_") %>%
  mutate(gap = treated_1 - treated_0)

# Create comprehensive plot
p <- ggplot(fire_combined, aes(x = year, y = fire.frac, 
                               color = interaction(method, group),
                               linetype = group)) +
  # Pre-treatment region (train)
  annotate("rect", xmin = train_start, xmax = train_end,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "blue") +
  annotate("text", x = mean(c(train_start, train_end)),
           y = Inf, label = "Train", vjust = 1.5, color = "blue", size = 3.5) +
  # Pre-treatment region (test)
  annotate("rect", xmin = test_start, xmax = test_end,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "green") +
  annotate("text", x = mean(c(test_start, test_end)),
           y = Inf, label = "Test", vjust = 1.5, color = "darkgreen", size = 3.5) +
  # Treatment year line
  geom_vline(xintercept = treated_year, linetype = "dashed", 
             color = "gray40", linewidth = 0.7) +
  annotate("text", x = treated_year, y = Inf,
           label = paste("Treatment:", treated_year),
           vjust = 1.5, hjust = -0.1, color = "gray30", size = 3.5) +
  # Fire frequency trajectories
  geom_line(linewidth = 0.8) +
  geom_point(aes(shape = group), size = 2) +
  # Styling
  scale_color_manual(
    name = "Method & Group",
    values = c(
      "Baseline.Treated" = "#E41A1C",
      "Baseline.Control" = "#377EB8",
      "Embedding.Treated" = "#FF7F00",
      "Embedding.Control" = "#4DAF4A"
    ),
    labels = c(
      "Baseline.Treated" = "Baseline - Treated",
      "Baseline.Control" = "Baseline - Control",
      "Embedding.Treated" = "Embedding - Treated",
      "Embedding.Control" = "Embedding - Control"
    )
  ) +
  scale_linetype_manual(
    name = "Group",
    values = c("Treated" = "solid", "Control" = "dashed")
  ) +
  scale_shape_manual(
    name = "Group",
    values = c("Treated" = 16, "Control" = 1)
  ) +
  labs(
    title = paste("Fire Frequency Trajectory: Baseline vs Embedding (K =", optimal_K, ")"),
    subtitle = paste("Treatment year:", treated_year, "| Parallel trends = successful matching"),
    x = "Year",
    y = "Weighted Fire Frequency",
    caption = paste("Pre-treatment RMSE - Baseline:",
                    round(sqrt(mean((fire_baseline$fire.frac[fire_baseline$year %in% pre_years & fire_baseline$treated == 1] -
                                       fire_baseline$fire.frac[fire_baseline$year %in% pre_years & fire_baseline$treated == 0])^2,
                                   na.rm = TRUE)), 4),
                    "| Embedding:",
                    round(sqrt(mean((fire_embedding$fire.frac[fire_embedding$year %in% pre_years & fire_embedding$treated == 1] -
                                       fire_embedding$fire.frac[fire_embedding$year %in% pre_years & fire_embedding$treated == 0])^2,
                                   na.rm = TRUE)), 4))
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1)
  )

# Save plot
plot_file <- paste0(output_dir, "trajectory_", treated_year, "_k", optimal_K, ".pdf")
ggsave(plot_file, plot = p, width = 12, height = 7, dpi = 600)
cat("✓ Saved trajectory plot to:", plot_file, "\n")

# Also save PNG version
plot_file_png <- paste0(output_dir, "trajectory_", treated_year, "_k", optimal_K, ".png")
ggsave(plot_file_png, plot = p, width = 12, height = 7, dpi = 300)
cat("✓ Saved PNG version to:", plot_file_png, "\n\n")

# ============================================================================
# STEP 4: Create gap plot (treated - control difference)
# ============================================================================

p_gap <- ggplot(fire_wide, aes(x = year, y = gap, color = method)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_vline(xintercept = treated_year, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(
    name = "Method",
    values = c("Baseline" = "#377EB8", "Embedding" = "#FF7F00")
  ) +
  labs(
    title = paste("Treatment Effect Gap: Treated - Control (K =", optimal_K, ")"),
    subtitle = "Pre-treatment gap should be near zero (parallel trends)",
    x = "Year",
    y = "Fire Frequency Gap (Treated - Control)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank()
  )

# Save gap plot
gap_file_png <- paste0(output_dir, "gap_", treated_year, "_k", optimal_K, ".png")

gap_file_png <- paste0(output_dir, "gap_", treated_year, "_k", optimal_K, ".png")
ggsave(gap_file_png, plot = p_gap, width = 10, height = 6, dpi = 300)
cat("✓ Saved PNG version to:", gap_file_png, "\n\n")

# --- Quick re-plot instructions ---
# To re-plot the gap plot without rerunning analysis:
# 1. Open R and set working directory to project root.
# 2. Load gap data:
#    gap_file_data <- paste0("data/cbps_integration/", treated_year,
#                           "/gap_comparison_k", optimal_K, "_", treated_year, ".csv")
#    fire_wide <- read.csv(gap_file_data)
# 3. Recreate gap plot with:
#    p_gap <- ggplot(fire_wide, aes(x = year, y = gap, color = method, group = method)) +
#      geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
#      geom_vline(xintercept = treated_year, linetype = "dashed", color = "gray40") +
#      geom_line(linewidth = 1) +
#      geom_point(size = 2.5) +
#      scale_color_manual(name = "Method", values = c("Baseline" = "#377EB8", "Embedding" = "#FF7F00")) +
#      labs(title = paste("Treatment Effect Gap: Treated - Control (K =", optimal_K, ")"),
#           subtitle = "Pre-treatment gap should be near zero (parallel trends)",
#           x = "Year", y = "Fire Frequency Gap (Treated - Control)") +
#      theme_minimal() +
#      theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14),
#            plot.subtitle = element_text(size = 10, color = "gray30"), panel.grid.minor = element_blank())
#    ggsave("gap_quick_replot.png", plot = p_gap, width = 10, height = 6, dpi = 300)


# ============================================================================
# STEP 5: Save trajectory data
# ============================================================================

# Save combined trajectory data for further analysis
trajectory_file <- paste0("data/cbps_integration/", treated_year,
                         "/trajectory_comparison_k", optimal_K, "_", treated_year, ".csv")
write.csv(fire_combined, trajectory_file, row.names = FALSE)
cat("✓ Saved trajectory data to:", trajectory_file, "\n")

# Save gap data
gap_file_data <- paste0("data/cbps_integration/", treated_year,
                       "/gap_comparison_k", optimal_K, "_", treated_year, ".csv")
write.csv(fire_wide, gap_file_data, row.names = FALSE)
cat("✓ Saved gap data to:", gap_file_data, "\n\n")

cat(strrep("=", 80), "\n")
cat("TRAJECTORY PLOTS COMPLETE\n")
cat(strrep("=", 80), "\n")
cat("\nOutputs:\n")
cat("1.", plot_file, "\n")
cat("2.", gap_file, "\n")
cat("3.", trajectory_file, "\n")
cat("\n")
