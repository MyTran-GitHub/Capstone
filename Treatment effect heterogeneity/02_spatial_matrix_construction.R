# 02_spatial_matrix_construction.R
# Purpose: Build spatial matrices (buffers, landscape context) around each pixel to analyze how treatment effect varies with local and neighborhood conditions.
#
# Why buffers? The 5km buffer lets us summarize the landscape context around each pixel, not just its own covariates. This helps us understand if treatment effects depend on the surrounding environment (e.g., forest density, elevation, climate).
#
# Treatment effect heterogeneity: We want to know if the effect of prescribed burning (or other treatment) varies depending on pixel-level and neighborhood-level covariates. For example, does tree cover loss differ in high-elevation vs. low-elevation areas, or in dense vs. sparse forests?
#
# By calculating buffer-level means, we can stratify and analyze treatment effects by both local and landscape context.

library(sf)
library(dplyr)

# Load prepared data (from 01_data_preparation.R)

years <- 2000:2009
area <- 'conifer'
processed_dir <- '../data/processed_data/'

for (year in years) {
  input_file <- file.path(processed_dir, 'rev_analysis_low', paste0('analysis_treated', year, '_', area, '.RDS'))
  if (!file.exists(input_file)) {
    cat('Missing analysis file for year', year, '- skipping.\n')
    next
  }
  df <- readRDS(input_file)
  # ...existing code...
  # Convert to sf object for spatial operations
  df_sf <- st_as_sf(df, coords = c('LONGITUDE', 'LATITUDE'), crs = 4326, remove = FALSE)
  df_sf <- st_transform(df_sf, 3310)
  df_sf$buffer_5km <- st_buffer(st_geometry(df_sf), dist = 5000)
  covariates <- intersect(names(df_sf), c('elev', paste0('tree_cover_', 2000:2020), 'precip', 'tmax', 'tmin', 'num.fire', 'disturbance'))
  gen_buffer_mean <- function(i, covar) {
    focal_geom <- df_sf$buffer_5km[i]
    idx <- st_intersects(focal_geom, df_sf$geometry, sparse = FALSE)[1,]
    mean(df_sf[[covar]][idx], na.rm = TRUE)
  }
  for (covar in covariates) {
    cat('Calculating buffer mean for', covar, '\n')
    df_sf[[paste0('mean_', covar, '_5km')]] <- sapply(seq_len(nrow(df_sf)), gen_buffer_mean, covar = covar)
  }
  saveRDS(df_sf, file = file.path(processed_dir, paste0('landscape_context_', year, '_', area, '.RDS')))
  cat('5km buffer context (neighbor means) calculated and saved for', nrow(df_sf), 'pixels for year', year, '\n')
}

# Convert to sf object for spatial operations
# Each pixel gets a geometry for spatial analysis
df_sf <- st_as_sf(df, coords = c('LONGITUDE', 'LATITUDE'), crs = 4326, remove = FALSE)

# Project to a suitable CRS for distance calculations (meters)
df_sf <- st_transform(df_sf, 3310)  # California Albers (EPSG:3310)

# Create 5km buffer around each pixel
# This defines the "neighborhood" for each pixel
df_sf$buffer_5km <- st_buffer(st_geometry(df_sf), dist = 5000)

# List relevant covariates to summarize in the buffer
# These are the variables whose neighborhood means we want to calculate
covariates <- intersect(names(df_sf), c('elev', paste0('tree_cover_', 2000:2020), 'precip', 'tmax', 'tmin', 'num.fire', 'disturbance'))

# Function to calculate mean of a covariate in buffer
# For each pixel, this finds all neighbors within 5km and averages the covariate
# This captures landscape context for each pixel
gen_buffer_mean <- function(i, covar) {
  focal_geom <- df_sf$buffer_5km[i]
  idx <- st_intersects(focal_geom, df_sf$geometry, sparse = FALSE)[1,]
  mean(df_sf[[covar]][idx], na.rm = TRUE)
}

# Calculate buffer means for each covariate
# This adds new columns like mean_elev_5km, mean_tree_cover_2000_5km, etc.
for (covar in covariates) {
  cat('Calculating buffer mean for', covar, '\n')
  df_sf[[paste0('mean_', covar, '_5km')]] <- sapply(seq_len(nrow(df_sf)), gen_buffer_mean, covar = covar)
}

# Save spatial context data for next step
saveRDS(df_sf, file = file.path(processed_dir, paste0('landscape_context_', year, '_', area, '.RDS')))

cat('5km buffer context (neighbor means) calculated and saved for', nrow(df_sf), 'pixels.\n')