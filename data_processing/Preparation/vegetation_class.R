
# Prepare forest vegetation grid for California based on conifer mask (robust, centroid-based, error-checked)
library(sf)
library(raster)
cat("[1/8] Libraries loaded.\n")



Dir = "data/raw_data"
outDir = "data/processed_data"
cat("[2/8] Directories set.\n")


# Check input files exist
grid_file <- file.path(Dir, "gpw_grid_ca.RDS")
mask_file <- file.path(Dir, "conifer_mask.tif")
if (!file.exists(grid_file)) stop("Grid file not found: ", grid_file)
if (!file.exists(mask_file)) stop("Conifer mask file not found: ", mask_file)
cat("[3/8] Input files found.\n")


gpw_grid_ca <- readRDS(grid_file)
cat("[4/8] Grid loaded.\n")


# Create polygons for each grid cell (optional, for later use)
cat("[5/8] Creating polygons for each grid cell...\n")
if (!requireNamespace("pbapply", quietly = TRUE)) {
  install.packages("pbapply", repos = "https://cloud.r-project.org")
}
library(pbapply)
gpw_grid_ca_poly <- gpw_grid_ca
int_lon <- min(abs(diff(sort(unique(gpw_grid_ca$LONGITUDE)))/2))
int_lat <- min(abs(diff(sort(unique(gpw_grid_ca$LATITUDE)))/2))
poly_list <- pbapply::pblapply(seq_len(nrow(gpw_grid_ca_poly)), function(row) {
  st_bbox(c(
    xmin = gpw_grid_ca_poly$LONGITUDE[row] - int_lon,
    xmax = gpw_grid_ca_poly$LONGITUDE[row] + int_lon,
    ymin = gpw_grid_ca_poly$LATITUDE[row] - int_lat,
    ymax = gpw_grid_ca_poly$LATITUDE[row] + int_lat
  ), crs = st_crs(4326)) %>%
    st_as_sfc() %>%
    .[[1]]
})
gpw_grid_ca_poly <- st_sf(gpw_grid_ca_poly, geometry = st_sfc(poly_list, crs = 4326))
cat("[5/8] Polygons created.\n")


# Read conifer mask raster (binary: 1 = conifer, 0 = not conifer)
conifer_mask <- raster(mask_file)
cat("[6/8] Conifer mask loaded.\n")


# Compute centroids for each grid cell
cat("[7/8] Computing centroids and extracting mask values...\n")
centroids <- st_centroid(gpw_grid_ca_poly)

# Reproject centroids if needed
if (!compareCRS(st_crs(centroids), crs(conifer_mask))) {
  centroids <- st_transform(centroids, crs = crs(conifer_mask))
}

# Extract mask value for each grid cell centroid (fast)
conifer_flag <- raster::extract(conifer_mask, as(centroids, "Spatial"))

# Handle NA values (e.g., outside raster extent)
conifer_flag[is.na(conifer_flag)] <- 0

gpw_grid_ca_poly$conifer <- conifer_flag

# Keep only conifer cells (or flag them)
fveg_grid_ca_poly <- gpw_grid_ca_poly[gpw_grid_ca_poly$conifer == 1, ]

# Create non-spatial version (data.frame) with only conifer cells
fveg_grid_ca <- as.data.frame(st_drop_geometry(fveg_grid_ca_poly))
cat("[7/8] Centroids processed and mask values extracted.\n")


# Ensure output directory exists
if (!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)


# Save both
cat("[8/8] Saving outputs...\n")
saveRDS(fveg_grid_ca_poly, file = file.path(outDir, "fveg_grid_ca_poly.RDS"))
saveRDS(fveg_grid_ca, file = file.path(outDir, "fveg_grid_ca.RDS"))

# save a CSV version as well
write.csv(fveg_grid_ca[, c("LONGITUDE", "LATITUDE")], file = file.path(outDir, "conifer_grid_points.csv"), row.names = FALSE)
cat("All done!\n")
## Visual check
plot(fveg_grid_ca_poly["conifer"])