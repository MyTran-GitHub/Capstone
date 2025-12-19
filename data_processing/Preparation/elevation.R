library(terra)
library(tidyverse)
library(data.table)
library(sf)

Dir = "data/raw_data/"
outDir = "data/processed_data/"

# Load conifer-masked grid (sf object)
fveg_grid_ca_poly <- readRDS(file.path(outDir, "fveg_grid_ca_poly.RDS"))

# Read elevation raster with terra
elev <- terra::rast(file.path(Dir, "elevation_1KMmd_GMTEDmd.tif"))

# Ensure CRS matches
fveg_grid_ca_poly <- st_transform(fveg_grid_ca_poly, crs(elev))

# Extract mean elevation for each conifer polygon
extract_df <- terra::extract(elev, vect(fveg_grid_ca_poly), fun = mean, na.rm = TRUE)

# Combine with conifer grid
fveg_elev_grid_ca_poly <- cbind(fveg_grid_ca_poly, elev = extract_df[,2])

saveRDS(fveg_elev_grid_ca_poly, file = file.path(outDir, "fveg_elev_grid_ca_poly.RDS"))