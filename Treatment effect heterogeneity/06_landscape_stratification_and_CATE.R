# 06_landscape_stratification_and_CATE.R
# This script demonstrates how to calculate landscape metrics (e.g., edge density, diversity) using R packages,
# stratify pixels by these metrics, and estimate Conditional Average Treatment Effects (CATE) for each group.
# Requires: landscapemetrics, raster, dplyr, sf

library(raster)
library(landscapemetrics)
library(dplyr)
library(sf)

years <- 2000:2009
area <- 'conifer'
processed_dir <- '../data/processed_data/'
veg_raster <- raster(file.path(processed_dir, 'veg_class.tif')) # Replace with your raster path

for (year in years) {
  input_file <- file.path(processed_dir, 'rev_analysis_low', paste0('analysis_treated', year, '_', area, '.RDS'))
  if (!file.exists(input_file)) {
    cat('Missing analysis file for year', year, '- skipping.\n')
    next
  }
  df <- readRDS(input_file)
  # Calculate edge density in a 5km moving window
  edge_density <- window_lsm(veg_raster, window = 5000, what = "lsm_l_ed")
  coords <- df[, c("LONGITUDE", "LATITUDE")]
  df$edge_density_5km <- extract(edge_density, coords)
  # Calculate Shannon diversity index in a 5km window
  shannon_div <- window_lsm(veg_raster, window = 5000, what = "lsm_l_shdi")
  df$shannon_5km <- extract(shannon_div, coords)
  # Stratify by edge density and diversity (quartiles)
  df$edge_group <- ntile(df$edge_density_5km, 4)
  df$diversity_group <- ntile(df$shannon_5km, 4)
  # Estimate CATE for each group (example for edge density)
  CATE_edge <- df %>%
    group_by(edge_group) %>%
    summarise(
      n = n(),
      treated_mean = mean(tree_cover_2020[treated == 1], na.rm = TRUE),
      control_mean = mean(tree_cover_2020[treated == 0], na.rm = TRUE),
      effect = treated_mean - control_mean
    )
  print(CATE_edge)
  # Estimate CATE for diversity groups
  CATE_diversity <- df %>%
    group_by(diversity_group) %>%
    summarise(
      n = n(),
      treated_mean = mean(tree_cover_2020[treated == 1], na.rm = TRUE),
      control_mean = mean(tree_cover_2020[treated == 0], na.rm = TRUE),
      effect = treated_mean - control_mean
    )
  print(CATE_diversity)
  # Save results
  saveRDS(CATE_edge, file = file.path(processed_dir, paste0('CATE_edge_', year, '_', area, '.RDS')))
  saveRDS(CATE_diversity, file = file.path(processed_dir, paste0('CATE_diversity_', year, '_', area, '.RDS')))
  cat('Landscape metrics and CATE analysis complete for year', year, '\n')
}