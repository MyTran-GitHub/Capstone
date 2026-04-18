#' Prepare forest vegetation grid for California
#'
#' This script prepares a robust, centroid-based forest vegetation grid for California using a conifer mask and grid cell polygons.

# Prepare forest vegetation grid for California based on conifer mask (robust, centroid-based, error-checked)
library(sf)
library(terra)
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

# Validate grid has coordinates
if (!all(c("LONGITUDE", "LATITUDE") %in% names(gpw_grid_ca))) {
  stop("Grid must have LONGITUDE and LATITUDE columns")
}
cat(sprintf("[4/8] Grid loaded: %d points\n", nrow(gpw_grid_ca)))


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
conifer_mask <- terra::rast(mask_file)
cat(sprintf("[6/8] Conifer mask loaded: CRS = %s\n", as.character(terra::crs(conifer_mask))))


# Compute centroids for each grid cell
cat("[7/8] Computing centroids and extracting mask values...\n")
centroids <- st_centroid(gpw_grid_ca_poly)

# Reproject centroids if needed (use proper sf CRS comparison)
centroid_crs <- st_crs(centroids)
mask_crs <- st_crs(terra::crs(conifer_mask))

if (is.na(centroid_crs)) {
  stop("Centroids have no CRS - cannot proceed")
}

if (!identical(centroid_crs, mask_crs)) {
  cat(sprintf("  Transforming centroids: %s -> %s\n", 
              as.character(centroid_crs$input), 
              as.character(mask_crs$input)))
  centroids <- st_transform(centroids, crs = mask_crs)
}

# Extract mask value for each grid cell centroid (fast)
conifer_flag <- terra::extract(conifer_mask, terra::vect(centroids))[, 2]

# Handle NA values (e.g., outside raster extent) - REPORT BEFORE MASKING
na_count <- sum(is.na(conifer_flag))
na_pct <- 100 * na_count / length(conifer_flag)
cat(sprintf("  Extraction NA summary: %d cells (%.2f%%) outside mask extent\n", na_count, na_pct))
if (na_count > 0) {
  warning(sprintf("%d grid centroids fall outside conifer mask extent - treating as non-conifer", na_count))
}
conifer_flag[is.na(conifer_flag)] <- 0

gpw_grid_ca_poly$conifer <- conifer_flag

# Keep only conifer cells (or flag them)
fveg_grid_ca_poly <- gpw_grid_ca_poly[gpw_grid_ca_poly$conifer == 1, ]

# Create non-spatial version (data.frame) with only conifer cells
fveg_grid_ca <- as.data.frame(st_drop_geometry(fveg_grid_ca_poly))
cat(sprintf("[7/8] Conifer mask applied: %d conifer cells retained (%.1f%% of grid)\n", 
            nrow(fveg_grid_ca), 100*nrow(fveg_grid_ca)/nrow(gpw_grid_ca_poly)))


# Ensure output directory exists
if (!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)


# Save both
cat("[8/8] Saving outputs...\n")
saveRDS(fveg_grid_ca_poly, file = file.path(outDir, "fveg_grid_ca_poly.RDS"))
saveRDS(fveg_grid_ca, file = file.path(outDir, "fveg_grid_ca.RDS"))

# save a CSV version as well
write.csv(fveg_grid_ca[, c("LONGITUDE", "LATITUDE")], file = file.path(outDir, "conifer_grid_points.csv"), row.names = FALSE)

cat("\n========== CONIFER MASK SUMMARY ==========\n")
cat(sprintf("Total input grid cells: %d\n", nrow(gpw_grid_ca)))
cat(sprintf("Conifer cells retained: %d (%.1f%%)\n", nrow(fveg_grid_ca), 100*nrow(fveg_grid_ca)/nrow(gpw_grid_ca)))
cat(sprintf("Non-conifer cells: %d\n", nrow(gpw_grid_ca) - nrow(fveg_grid_ca)))
cat(sprintf("Coordinate range (conifer only):\n"))
cat(sprintf("  Longitude: [%.4f, %.4f]\n", min(fveg_grid_ca$LONGITUDE), max(fveg_grid_ca$LONGITUDE)))
cat(sprintf("  Latitude: [%.4f, %.4f]\n", min(fveg_grid_ca$LATITUDE), max(fveg_grid_ca$LATITUDE)))
cat("==========================================\n")
cat("All done!\n")
## Visual check
plot(fveg_grid_ca_poly["conifer"])