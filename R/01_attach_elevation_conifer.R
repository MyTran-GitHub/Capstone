# Attach elevation to conifer-only grid using elevatr
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("elevatr", quietly = TRUE)) install.packages("elevatr")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(sf)
library(terra)
library(elevatr)
library(here)

options(repos = c(CRAN = "https://cloud.r-project.org"))
setwd(here::here())

conifer_grid_path <- "data/processed_data/conifer_grid_filtered.RDS"
if (!file.exists(conifer_grid_path)) stop("Missing ", conifer_grid_path)

conifer_grid <- readRDS(conifer_grid_path)

# Get elevation raster for extent of conifer grid
bbox <- st_bbox(conifer_grid)
roi <- st_as_sfc(bbox)

message("Downloading elevation for conifer extent (this may take a few minutes)...")
elev_rast <- get_elev_raster(locations = roi, z = 9, src = "aws")

# Convert to terra rast
tr <- terra::rast(elev_rast)

# Extract mean elevation per grid cell
vals <- terra::extract(tr, terra::vect(conifer_grid), fun = mean)
conifer_grid$elevation <- vals[,2]

saveRDS(conifer_grid, "data/processed_data/conifer_grid_with_elevation.RDS")
message("✓ Attached elevation to conifer grid (", sum(!is.na(conifer_grid$elevation)), " pixels)")
