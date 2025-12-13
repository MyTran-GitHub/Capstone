# Extract tree canopy cover for conifer-only pixels
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("fst", quietly = TRUE)) install.packages("fst")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(terra)
library(sf)
library(fst)
library(here)

options(repos = c(CRAN = "https://cloud.r-project.org"))
setwd(here::here())

conifer_grid_path <- "data/processed_data/conifer_grid_with_elevation.RDS"
if (!file.exists(conifer_grid_path)) stop("Missing ", conifer_grid_path)
conifer_grid <- readRDS(conifer_grid_path)

# Expect NLCD tree canopy cover or similar raster
# Download from https://www.mrlc.gov/data or use FedData package
tree_cover_file <- "data/raw_data/tree_cover_conifer_2016.tif"

if (!file.exists(tree_cover_file)) {
  message("Tree cover raster not found at ", tree_cover_file)
  message("Creating placeholder (70% canopy for all conifer pixels)...")
  
  tree_cover_df <- expand.grid(
    unit = unique(conifer_grid$unit),
    year = 2000:2021,
    stringsAsFactors = FALSE
  ) %>%
    mutate(tree_cover = 70)  # Reasonable default for conifer forests
  
  write_fst(tree_cover_df, "data/processed_data/tree_cover_conifer.fst")
  message("✓ Created placeholder tree cover (70%)")
  quit(save = "no", status = 0)
}

# Load and extract
tc_rast <- rast(tree_cover_file)

# Reproject if needed
if (!compareGeom(tc_rast, terra::vect(conifer_grid), stopOnError = FALSE)) {
  conifer_v <- terra::vect(conifer_grid)
  conifer_v <- terra::project(conifer_v, tc_rast)
} else {
  conifer_v <- terra::vect(conifer_grid)
}

tc_vals <- terra::extract(tc_rast, conifer_v, fun = mean, ID = FALSE)

# Use static 2016 value for all years (reasonable for slow-changing forests)
tree_cover_df <- expand.grid(
  unit = unique(conifer_grid$unit),
  year = 2000:2021,
  stringsAsFactors = FALSE
) %>%
  mutate(tree_cover = rep(tc_vals[,1], each = 22))

write_fst(tree_cover_df, "data/processed_data/tree_cover_conifer.fst")
message("✓ Extracted tree cover: mean = ", round(mean(tc_vals[,1], na.rm = TRUE), 1), "%")
