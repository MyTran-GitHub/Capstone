#' Prepare forest vegetation grid for California
#'
#' This script prepares a robust, centroid-based forest vegetation grid for California using a conifer mask and grid cell polygons.

library(sf)
library(terra)

run_vegetation_class <- function(Dir = "data/raw_data", outDir = "data/processed_data") {
  message("[1/8] Libraries loaded.")

  message("[2/8] Directories set: Dir=", Dir, ", outDir=", outDir)

  # Check input files exist
  grid_file <- file.path(Dir, "gpw_grid_ca.RDS")
  mask_file <- file.path(Dir, "conifer_mask.tif")
  if (!file.exists(grid_file)) stop("Grid file not found: ", grid_file)
  if (!file.exists(mask_file)) stop("Conifer mask file not found: ", mask_file)
  message("[3/8] Input files found.")

  gpw_grid_ca <- readRDS(grid_file)

  # Validate grid has coordinates
  if (!all(c("LONGITUDE", "LATITUDE") %in% names(gpw_grid_ca))) {
    stop("Grid must have LONGITUDE and LATITUDE columns")
  }
  message(sprintf("[4/8] Grid loaded: %d points", nrow(gpw_grid_ca)))

  # Create polygons for each grid cell (optional, for later use)
  message("[5/8] Creating polygons for each grid cell...")
  if (!requireNamespace("pbapply", quietly = TRUE)) {
    stop("Missing required package 'pbapply'. Install via install.packages('pbapply') or use the provided env/ environment.")
  }
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
  message("[5/8] Polygons created.")

  # Read conifer mask raster (binary: 1 = conifer, 0 = not conifer)
  conifer_mask <- terra::rast(mask_file)
  message(sprintf("[6/8] Conifer mask loaded: CRS = %s", as.character(terra::crs(conifer_mask))))

  # Compute centroids for each grid cell
  message("[7/8] Computing centroids and extracting mask values...")
  centroids <- st_centroid(gpw_grid_ca_poly)

  # Reproject centroids if needed (use proper sf CRS comparison)
  centroid_crs <- st_crs(centroids)
  mask_crs <- st_crs(terra::crs(conifer_mask))

  if (is.na(centroid_crs)) {
    stop("Centroids have no CRS - cannot proceed")
  }

  if (!identical(centroid_crs, mask_crs)) {
    message(sprintf("  Transforming centroids: %s -> %s", 
                    as.character(centroid_crs$input), 
                    as.character(mask_crs$input)))
    centroids <- st_transform(centroids, crs = mask_crs)
  }

  # Extract mask value for each grid cell centroid (fast)
  conifer_flag <- terra::extract(conifer_mask, terra::vect(centroids))[, 2]

  # Handle NA values (e.g., outside raster extent) - REPORT BEFORE MASKING
  na_count <- sum(is.na(conifer_flag))
  na_pct <- 100 * na_count / length(conifer_flag)
  message(sprintf("  Extraction NA summary: %d cells (%.2f%%) outside mask extent", na_count, na_pct))
  if (na_count > 0) {
    warning(sprintf("%d grid centroids fall outside conifer mask extent - treating as non-conifer", na_count))
  }
  conifer_flag[is.na(conifer_flag)] <- 0

  gpw_grid_ca_poly$conifer <- conifer_flag

  # Keep only conifer cells (or flag them)
  fveg_grid_ca_poly <- gpw_grid_ca_poly[gpw_grid_ca_poly$conifer == 1, ]

  # Create non-spatial version (data.frame) with only conifer cells
  fveg_grid_ca <- as.data.frame(st_drop_geometry(fveg_grid_ca_poly))
  message(sprintf("[7/8] Conifer mask applied: %d conifer cells retained (%.1f%% of grid)", 
                  nrow(fveg_grid_ca), 100*nrow(fveg_grid_ca)/nrow(gpw_grid_ca_poly)))

  # Ensure output directory exists
  if (!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)

  # Save both
  message("[8/8] Saving outputs...")
  saveRDS(fveg_grid_ca_poly, file = file.path(outDir, "fveg_grid_ca_poly.RDS"))
  saveRDS(fveg_grid_ca, file = file.path(outDir, "fveg_grid_ca.RDS"))

  # save a CSV version as well
  write.csv(fveg_grid_ca[, c("LONGITUDE", "LATITUDE")], file = file.path(outDir, "conifer_grid_points.csv"), row.names = FALSE)

  message("========== CONIFER MASK SUMMARY ==========")
  message(sprintf("Total input grid cells: %d", nrow(gpw_grid_ca)))
  message(sprintf("Conifer cells retained: %d (%.1f%%)", nrow(fveg_grid_ca), 100*nrow(fveg_grid_ca)/nrow(gpw_grid_ca)))
  message(sprintf("Non-conifer cells: %d", nrow(gpw_grid_ca) - nrow(fveg_grid_ca)))
  message(sprintf("Coordinate range (conifer only):"))
  message(sprintf("  Longitude: [%.4f, %.4f]", min(fveg_grid_ca$LONGITUDE), max(fveg_grid_ca$LONGITUDE)))
  message(sprintf("  Latitude: [%.4f, %.4f]", min(fveg_grid_ca$LATITUDE), max(fveg_grid_ca$LATITUDE)))
  message("==========================================")
  message("All done!")
  if (interactive()) {
    try(plot(fveg_grid_ca_poly["conifer"]), silent = TRUE)
  }

  invisible(TRUE)
}

# If executed directly, run and surface errors
if (!interactive()) {
  tryCatch(
    {
      run_vegetation_class()
    },
    error = function(e) {
      message("[ERROR] vegetation_class failed: ", conditionMessage(e))
      try({ tb <- utils::capture.output(traceback()); if (length(tb)>0) for (ln in tb) message(ln) }, silent = TRUE)
      quit(save = "no", status = 1, runLast = FALSE)
    }
  )
}