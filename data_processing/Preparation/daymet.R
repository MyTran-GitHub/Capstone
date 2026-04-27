#!/usr/bin/env Rscript

library(terra)
library(dplyr)
library(fst)
library(parallel)
library(ncdf4)

run_daymet <- function(Dir = "data/raw_data/daymet_data/", outDir = "data/processed_data/", years = 2000:2020, var_climate = c("minat","maxat","prcp","wvp", "swe")) {

  # Ensure RANN is available
  if (!requireNamespace("RANN", quietly = TRUE)) {
    stop("Package 'RANN' required but not installed. Install via install.packages('RANN') or use env/.")
  }

  # Directories
  data_dir <- Dir
  out_dir <- outDir

  # Load conifer grid (already masked)
  fveg_grid_ca <- readRDS(file.path(out_dir, "fveg_grid_ca.RDS"))

  # Prepare output dataframe with coordinates only
  gridclimate_mon <- data.frame(
    LONGITUDE = fveg_grid_ca$LONGITUDE,
    LATITUDE = fveg_grid_ca$LATITUDE
  )
  grid_points <- terra::vect(gridclimate_mon, geom = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")

  # Number of cores for parallel processing
  n_cores <- min(parallel::detectCores() - 1, length(var_climate))

  # Function to process one climate variable (self-contained for debugging)
  process_variable <- function(varname) {
    message("Processing variable: ", varname)

    # Recreate grid inside function (avoid global scope issues)
    outDir <- out_dir
    fveg_grid_ca <- readRDS(file.path(outDir, "fveg_grid_ca.RDS"))
    gridclimate_mon_local <- data.frame(
      LONGITUDE = fveg_grid_ca$LONGITUDE,
      LATITUDE = fveg_grid_ca$LATITUDE
    )
    var_df <- gridclimate_mon_local

    for (year in years) {
      message("  Year: ", year)
      nc_file <- file.path(data_dir, paste0(varname, "_", year, ".nc"))

      if (!file.exists(nc_file)) {
        warning("File does not exist: ", nc_file)
        next
      }

      tryCatch({
        # Read NetCDF manually
        nc <- ncdf4::nc_open(nc_file)
        lat_array <- ncdf4::ncvar_get(nc, "lat")
        lon_array <- ncdf4::ncvar_get(nc, "lon")

        # Read all 12 bands into a list
        bands <- lapply(1:12, function(m) {
          ncdf4::ncvar_get(nc, paste0("Band", m))
        })
        ncdf4::nc_close(nc)

        # Build KD-tree over curvilinear lat/lon grid and compute nearest pixel
        coords_mat <- cbind(as.vector(lon_array), as.vector(lat_array))
        mask_mat <- cbind(gridclimate_mon_local$LONGITUDE, gridclimate_mon_local$LATITUDE)
        k_neighbors <- 8
        nn <- RANN::nn2(coords_mat, mask_mat, k = k_neighbors)
        idx_mat <- nn$nn.idx
        dist_mat <- nn$nn.dists

        # Extract month values using k-nearest inverse-distance weighting (IDW)
        month_values <- lapply(1:12, function(m) {
          band_vec <- as.vector(bands[[m]])
          vals_mat <- matrix(band_vec[idx_mat], nrow = nrow(idx_mat), ncol = ncol(idx_mat))
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

  # Sequential processing for debugging
  results <- lapply(var_climate, process_variable)

  # Combine with coordinates
  gridclimate_mon <- cbind(gridclimate_mon, do.call(cbind, results))

  # Report NA summary before imputation
  climate_cols <- setdiff(names(gridclimate_mon), c("LONGITUDE", "LATITUDE"))
  na_summary <- sapply(gridclimate_mon[climate_cols], function(x) sum(is.na(x)))
  message("NA counts before imputation:")
  print(sort(na_summary, decreasing=TRUE)[1:10])

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
  message("NA counts after imputation:")
  print(na_summary_final[na_summary_final > 0])

  # Save final FST
  write_fst(gridclimate_mon, path = file.path(out_dir, "gridClimate_mon2_conifer.fst"))

  message("Finished processing all Daymet variables with NA imputation!")
  invisible(TRUE)
}

# If executed directly, run and surface errors
if (!interactive()) {
  tryCatch(
    {
      run_daymet()
    },
    error = function(e) {
      message("[ERROR] daymet processing failed: ", conditionMessage(e))
      try({ tb <- utils::capture.output(traceback()); if (length(tb)>0) for (ln in tb) message(ln) }, silent = TRUE)
      quit(save = "no", status = 1, runLast = FALSE)
    }
  )
}