#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ncdf4)
  library(raster)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  cat("Usage: Rscript data_processing/Preparation/check_netcdf.R <path/to/file.nc>\n")
  quit(status = 1)
}

nc_path <- args[1]
if (!file.exists(nc_path)) {
  stop(paste("File not found:", nc_path))
}

cat("\nInspecting NetCDF:", nc_path, "\n\n")

# Open with ncdf4 for metadata
nc <- nc_open(nc_path)
on.exit(nc_close(nc))

# List variables and dimensions
var_names <- names(nc$var)
dim_names <- names(nc$dim)
cat("Variables (", length(var_names), "):\n", paste(var_names, collapse = ", "), "\n", sep = "")
cat("Dimensions (", length(dim_names), "):\n", paste(dim_names, collapse = ", "), "\n\n", sep = "")

# Expect 12 bands named Band1..Band12
expected_bands <- paste0("Band", 1:12)
missing_bands <- setdiff(expected_bands, var_names)
if (length(missing_bands) > 0) {
  cat("WARNING: Missing expected bands:", paste(missing_bands, collapse = ", "), "\n")
} else {
  cat("✓ All 12 Band* variables present.\n")
}

# Check lat/lon presence and dimensions
has_lat <- "lat" %in% var_names
has_lon <- "lon" %in% var_names
cat("lat present:", has_lat, "\n")
cat("lon present:", has_lon, "\n")

# Print dimension sizes for y/x if available
if ("y" %in% dim_names) cat("dim y size:", nc$dim$y$len, "\n")
if ("x" %in% dim_names) cat("dim x size:", nc$dim$x$len, "\n")

# Read small samples
read_var_sample <- function(vname) {
  v <- ncdf4::ncvar_get(nc, vname)
  # handle 2D lat/lon or 2D Band
  dims <- dim(v)
  cat("  ", vname, " dims:", paste(dims, collapse = "x"), "\n")
  # sample center if available
  iy <- ceiling(dims[1] / 2)
  ix <- ifelse(length(dims) >= 2, ceiling(dims[2] / 2), NA)
  val <- tryCatch({ if (!is.na(ix)) v[iy, ix] else v[iy] }, error = function(e) NA)
  cat("    sample value:", ifelse(is.na(val), "NA", as.character(val)), "\n")
}

cat("\nSamples:\n")
for (v in intersect(expected_bands, var_names)) read_var_sample(v)
if (has_lat) read_var_sample("lat")
if (has_lon) read_var_sample("lon")

# Quick raster check for one band
if ("Band1" %in% var_names) {
  cat("\nRaster check (Band1) ...\n")
  r <- tryCatch({
    raster::raster(nc_path, varname = "Band1")
  }, error = function(e) NULL)
  if (is.null(r)) {
    cat("  Unable to open Band1 as raster.\n")
  } else {
    cat("  Raster dims:", nrow(r), "x", ncol(r), "\n")
    cat("  NA count:", sum(is.na(values(r))), "\n")
    # mean ignoring NA (may be NaN if all NA)
    cat("  Mean (na.rm=TRUE):", suppressWarnings(mean(values(r), na.rm = TRUE)), "\n")
  }
}

cat("\nDone.\n")
