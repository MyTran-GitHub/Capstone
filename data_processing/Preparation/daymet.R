#!/usr/bin/env Rscript

library(terra)
library(dplyr)
library(fst)
library(parallel)
library(ncdf4)

# Directories
Dir <- "data/raw_data/daymet_data/"
outDir <- "data/processed_data/"

# Load conifer grid (already masked)
fveg_grid_ca <- readRDS(file.path(outDir, "fveg_grid_ca.RDS"))

# Years and variables
years <- 2000:2020
var_climate <- c("minat","maxat","prcp","wvp", "swe")

# Prepare output dataframe with coordinates only
gridclimate_mon <- data.frame(
  LONGITUDE = fveg_grid_ca$LONGITUDE,
  LATITUDE = fveg_grid_ca$LATITUDE
)
grid_points <- vect(gridclimate_mon, geom = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")

# Number of cores for parallel processing
n_cores <- min(detectCores() - 1, length(var_climate))

# Function to process one climate variable (self-contained for debugging)
process_variable <- function(varname) {
  message("Processing variable: ", varname)
  
  # Recreate grid inside function (avoid global scope issues)
  outDir <- "data/processed_data/"
  fveg_grid_ca <- readRDS(file.path(outDir, "fveg_grid_ca.RDS"))
  gridclimate_mon <- data.frame(
    LONGITUDE = fveg_grid_ca$LONGITUDE,
    LATITUDE = fveg_grid_ca$LATITUDE
  )
  var_df <- gridclimate_mon

  for (year in 2000:2020) {
    message("  Year: ", year)
    nc_file <- file.path("data/raw_data/daymet_data/", paste0(varname, "_", year, ".nc"))

    if (!file.exists(nc_file)) {
      warning("File does not exist: ", nc_file)
      next
    }

    tryCatch({
      # Read NetCDF manually
      nc <- ncdf4::nc_open(nc_file)
      lat_array <- ncdf4::ncvar_get(nc, "lat")  # [x=639, y=1136] WGS84 reference
      lon_array <- ncdf4::ncvar_get(nc, "lon")  # [x=639, y=1136] WGS84 reference
      
      # Read all 12 bands into a list
      bands <- lapply(1:12, function(m) {
        ncdf4::ncvar_get(nc, paste0("Band", m))  # [x=639, y=1136]
      })
      ncdf4::nc_close(nc)
      
      # Build KD-tree over curvilinear lat/lon grid and compute nearest pixel
      # Once per year, reuse indices for all 12 months
      if (!requireNamespace("RANN", quietly = TRUE)) {
        stop("Package 'RANN' not installed. Please run install.packages('RANN') and retry.")
      }
      coords_mat <- cbind(as.vector(lon_array), as.vector(lat_array))  # length = 639*1136
      mask_mat <- cbind(gridclimate_mon$LONGITUDE, gridclimate_mon$LATITUDE)
      k_neighbors <- 8
      nn <- RANN::nn2(coords_mat, mask_mat, k = k_neighbors)
      idx_mat <- nn$nn.idx  # [n_points, k]
      dist_mat <- nn$nn.dists

      # Extract month values using k-nearest inverse-distance weighting (IDW)
      month_values <- lapply(1:12, function(m) {
        band_vec <- as.vector(bands[[m]])  # flatten [639, 1136]
        vals_mat <- matrix(band_vec[idx_mat], nrow = nrow(idx_mat), ncol = ncol(idx_mat))
        # weights: inverse distance; handle zero distance, ignore NA neighbors
        w <- 1 / (dist_mat + 1e-9)
        w[is.na(vals_mat)] <- 0
        vals_mat[is.na(vals_mat)] <- 0
        denom <- rowSums(w)
        out <- rowSums(w * vals_mat) / denom
        out[denom == 0] <- NA
        out
      })
      
      # Check NAs
      for (m in 1:12) {
        na_count <- sum(is.na(month_values[[m]]))
        na_pct <- 100 * na_count / length(month_values[[m]])
        
        if (na_pct > 5) {
          message(sprintf("    Month %d: WARNING %.1f%% NA (n=%d)", m, na_pct, na_count))
        } else if (na_count > 0) {
          message(sprintf("    Month %d: %d NAs (%.1f%%)", m, na_count, na_pct))
        }
      }
      
      month_df <- as.data.frame(do.call(cbind, month_values))
      colnames(month_df) <- paste0(varname, "_", year, "_", 1:12)
      var_df <- cbind(var_df, month_df)

      rm(month_values, month_df, lat_array, lon_array, bands, coords_mat, mask_mat, nn, idx_mat, dist_mat)
      gc()
    }, error = function(e) {
      message(sprintf("ERROR %s year %d: %s", varname, year, e$message))
    })
  }

  # Return only climate columns
  return(var_df[, -c(1,2)])
}

# Sequential processing for debugging (switch to mclapply once working)
results <- lapply(var_climate, process_variable)

# Combine with coordinates
gridclimate_mon <- cbind(gridclimate_mon, do.call(cbind, results))

# Report NA summary before imputation
climate_cols <- setdiff(names(gridclimate_mon), c("LONGITUDE", "LATITUDE"))
na_summary <- sapply(gridclimate_mon[climate_cols], function(x) sum(is.na(x)))
cat("\nNA counts before imputation:\n")
print(sort(na_summary, decreasing=TRUE)[1:10])  # Top 10 columns with most NAs

# Impute only if NA count is small (< 1% per column)
gridclimate_mon[climate_cols] <- lapply(
  gridclimate_mon[climate_cols],
  function(x) {
    na_frac <- sum(is.na(x)) / length(x)
    if (na_frac > 0.01) {
      warning(sprintf("Column has %.1f%% NA - NOT imputing", 100*na_frac))
      return(x)
    }
    ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  }
)

# Final NA check
na_summary_final <- sapply(gridclimate_mon[climate_cols], function(x) sum(is.na(x)))
cat("\nNA counts after imputation:\n")
print(na_summary_final[na_summary_final > 0])

# Save final FST
write_fst(gridclimate_mon, path = file.path(outDir, "gridClimate_mon2_conifer.fst"))

message("Finished processing all Daymet variables with NA imputation!")