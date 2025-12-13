#!/usr/bin/env Rscript

# Lightweight local test runner for Daymet prep
# - Installs required packages if missing
# - Limits run to 2001 and variables available (minat, maxat, prcp, wvp)
# - Skips gracefully when NetCDFs are missing

ensure_pkg <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, repos = "https://cloud.r-project.org")
    }
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}

ensure_pkg(c("terra","tidyverse","data.table","sf","parallel","R.utils",
             "RCurl","raster","tigris","fst","ncdf4"))

Dir <- if (file.exists("data/raw_data/gpw_grid_ca.RDS")) "data/raw_data/" else "../data/raw_data/"
outDir <- if (dir.exists("data/processed_data")) "data/processed_data/" else "../data/processed_data/"

gpw_grid_ca <- readRDS(paste0(Dir, "gpw_grid_ca.RDS"))
## Ensure CRS is WGS84 for joining
if (inherits(gpw_grid_ca, "sf")) {
  if (is.na(sf::st_crs(gpw_grid_ca))) {
    sf::st_crs(gpw_grid_ca) <- 4326
  }
}

# Make geometries valid and normalize longitudes if needed
if (inherits(gpw_grid_ca, "sf")) {
  suppressWarnings({ gpw_grid_ca <- sf::st_make_valid(gpw_grid_ca) })
}
# If raw columns exist, ensure longitude within [-180, 180] and latitude within [-90, 90]
if ("LONGITUDE" %in% names(gpw_grid_ca)) {
  gpw_grid_ca$LONGITUDE <- ifelse(gpw_grid_ca$LONGITUDE > 180, gpw_grid_ca$LONGITUDE - 360, gpw_grid_ca$LONGITUDE)
}
if ("LATITUDE" %in% names(gpw_grid_ca)) {
  gpw_grid_ca$LATITUDE <- pmax(pmin(gpw_grid_ca$LATITUDE, 90), -90)
}
bb <- try(sf::st_bbox(gpw_grid_ca), silent = TRUE)
if (!inherits(bb, "try-error")) {
  message("gpw bbox: ", paste(unclass(bb), collapse = ", "))
}

# Local test: limit to variables you generated and year 2001
var_climate <- c("minat","maxat","prcp","wvp")
parameters <- expand.grid(2001:2001, var_climate)

gridclimate_mon <- data.frame(matrix(NA, nrow = nrow(gpw_grid_ca), ncol = 0))
gridclimate_mon$LATITUDE <- gpw_grid_ca$LATITUDE
gridclimate_mon$LONGITUDE <- gpw_grid_ca$LONGITUDE

# Optional: allow subsetting rows for lighter tests via env var N_ROWS
n_rows_env <- Sys.getenv("N_ROWS", unset = "")
if (nzchar(n_rows_env)) {
  n_subset <- suppressWarnings(as.integer(n_rows_env))
  if (!is.na(n_subset) && n_subset > 0) {
    message("Subsetting to first ", n_subset, " rows for a light run.")
    gpw_grid_ca <- gpw_grid_ca[seq_len(min(n_subset, nrow(gpw_grid_ca))), ]
    gridclimate_mon <- gridclimate_mon[seq_len(min(n_subset, nrow(gridclimate_mon))), ]
  }
}

for (par in seq_len(nrow(parameters))) {
  message("Processing year=", parameters[par, 1], " var=", parameters[par, 2])
  gridclimate_n_list <- lapply(1:12, function(months) {
    message("  Month ", months, " → reading and joining...")
    ncfile <- paste0(Dir, parameters[par, 2], "_", as.numeric(parameters[par, 1]), ".nc")
    if (!file.exists(ncfile)) {
      message("Skipping missing ", ncfile)
      return(rep(NA_real_, nrow(gpw_grid_ca)))
    }
    stk <- tryCatch({
      raster::stack(ncfile, varname = paste0("Band", months))
    }, error = function(e) NULL)
    if (is.null(stk)) {
      message("Unable to read ", ncfile, " Band", months, "; filling NAs")
      return(rep(NA_real_, nrow(gpw_grid_ca)))
    }
    layer_climate <- raster::mean(stk[[1]], na.rm = TRUE)
    layer_climate <- data.frame(raster::rasterToPoints(layer_climate))
    colnames(layer_climate)[1:2] <- c("LONGITUDE", "LATITUDE")
    ## Normalize longitudes to [-180, 180] if needed
    if (any(layer_climate$LONGITUDE > 180, na.rm = TRUE)) {
      layer_climate$LONGITUDE <- ifelse(layer_climate$LONGITUDE > 180,
                                        layer_climate$LONGITUDE - 360,
                                        layer_climate$LONGITUDE)
    }
    layer_climate <- sf::st_as_sf(layer_climate,
                                  coords = c("LONGITUDE", "LATITUDE"),
                                  crs = 4326, remove = FALSE)
    ## Ensure gpw_grid_ca has valid CRS and bounds
    if (is.na(sf::st_crs(gpw_grid_ca))) sf::st_crs(gpw_grid_ca) <- 4326
    ## Drop any invalid geometries
    layer_climate <- sf::st_make_valid(layer_climate)
    gpw_grid_ca <- sf::st_make_valid(gpw_grid_ca)
    ## Clamp lat/lon to valid ranges to avoid longlat warnings
    if ("LONGITUDE" %in% names(gpw_grid_ca)) {
      gpw_grid_ca$LONGITUDE <- ifelse(gpw_grid_ca$LONGITUDE > 180, gpw_grid_ca$LONGITUDE - 360, gpw_grid_ca$LONGITUDE)
    }
    if ("LATITUDE" %in% names(gpw_grid_ca)) {
      gpw_grid_ca$LATITUDE <- pmax(pmin(gpw_grid_ca$LATITUDE, 90), -90)
    }
    layer_climate_gridded <- sf::st_drop_geometry(sf::st_join(
      gpw_grid_ca, layer_climate, join = sf::st_nearest_feature, suffix = c("","_ignore")
    )[-c(3:4)])
    message("  Month ", months, " → join complete.")
    return(layer_climate_gridded[, 3])
  })
  gridclimate_n_df <- data.frame(do.call(cbind, gridclimate_n_list))
  rm(gridclimate_n_list)
  colnames(gridclimate_n_df) <- paste0(parameters[par, 2], "_", parameters[par, 1], "_", 1:12)
  gridclimate_mon <- cbind(gridclimate_mon, gridclimate_n_df)
}

fst::write_fst(gridclimate_mon, path = paste0(outDir, "gridClimate_mon2.fst"))
cat("\n✓ Wrote ", file.path(outDir, "gridClimate_mon2.fst"), "\n", sep = "")
