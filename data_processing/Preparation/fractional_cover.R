library(tibble)
library(dplyr)
library(sf)
library(raster)
library(fst)

run_fractional_cover <- function(Dir = "data/raw_data", outDir = "data/processed_data", years = 2000:2020) {
  fracveg_dir <- file.path(Dir, "fractional_vegetation")
  message("Starting extraction for years: ", paste(years, collapse = ", "))

  # Ensure dependencies are available
  required_pkgs <- c("pbmcapply", "exactextractr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) stop(sprintf("Missing required packages: %s. Install via install.packages() or use env/.", paste(missing_pkgs, collapse = ", ")))

  # Read conifer grid polygons
  fveg_grid_ca_poly <- readRDS(file.path(outDir, "fveg_grid_ca_poly.RDS"))

  # Pre-transform grid CRS if all rasters share the same CRS
  test_raster <- raster(file.path(fracveg_dir, paste0("fractional_vegetation_", years[1], ".tif")))
  target_crs <- sf::st_crs(test_raster)
  grid <- fveg_grid_ca_poly
  if (!identical(sf::st_crs(grid), target_crs)) {
    message("Transforming grid CRS to match rasters...")
    grid <- sf::st_transform(grid, crs = target_crs)
  }
  message("Grid CRS transformation complete.")
  rm(test_raster); gc()

  # Use multicore parallelization and exactextractr for extraction
  tree_cover_summary.list <- pbmcapply::pbmclapply(years, function(year) {
    tryCatch({
      message("Processing year: ", year)
      raster_file <- file.path(fracveg_dir, paste0("fractional_vegetation_", year, ".tif"))
      r <- raster(raster_file)
      # Extract mean tree cover for each polygon using exactextractr
      tree_cover <- exactextractr::exact_extract(r, grid, 'mean')
      df <- st_drop_geometry(grid) %>% dplyr::select(LONGITUDE, LATITUDE)
      required_cols <- c("LONGITUDE", "LATITUDE")
      missing_cols <- setdiff(required_cols, colnames(df))
      if (length(missing_cols) > 0) {
        stop("Required column(s) missing from grid: ", paste(missing_cols, collapse = ", "))
      }
      df$tree_cover <- tree_cover
      colnames(df)[3] <- paste0("tree_cover_", year)
      message("  Finished year: ", year)
      rm(r); gc()
      df
    }, error = function(e) {
      message("Error in year ", year, ": ", conditionMessage(e))
      NULL
    })
  }, mc.cores = parallel::detectCores())
  message("Finished extraction for all years.")
  tree_cover_summary.list <- Filter(Negate(is.null), tree_cover_summary.list)
  gc()
  message("Merging all years by LONGITUDE, LATITUDE...")
  tree_cover_summary <- Reduce(function(x, y) merge(x, y, by =c("LONGITUDE", "LATITUDE"), all = TRUE), 
                               tree_cover_summary.list)
  message("Merge complete.")

  # Check NA summary before imputation
  climate_cols <- setdiff(names(tree_cover_summary), c("LONGITUDE", "LATITUDE"))
  na_summary <- sapply(tree_cover_summary[climate_cols], function(x) sum(is.na(x)))
  message("NA counts before imputation:")
  print(sort(na_summary, decreasing=TRUE)[1:10])

  # Impute only if NA count is small (< 1% per column)
  tree_cover_summary[climate_cols] <- lapply(
    tree_cover_summary[climate_cols],
    function(x) {
      na_frac <- sum(is.na(x)) / length(x)
      if (na_frac > 0.01) {
        warning(sprintf("Column has %.1f%% NA - NOT imputing, keeping as NA", 100*na_frac))
        return(x)
      }
      if (na_frac > 0) {
        message(sprintf("  Imputing %.2f%% NAs with column mean", 100*na_frac))
      }
      ifelse(is.na(x), mean(x, na.rm = TRUE), x)
    }
  )

  # Final NA check
  na_summary_final <- sapply(tree_cover_summary[climate_cols], function(x) sum(is.na(x)))
  message("NA counts after imputation:")
  print(na_summary_final[na_summary_final > 0])

  message("Saving output to: ", file.path(outDir, "tree_cover.fst"))
  write_fst(tree_cover_summary, path = file.path(outDir, "tree_cover.fst"))
  message("File saved.")

  # Example plot for verification (only interactive)
  if (interactive()) {
    df_joined <- st_drop_geometry(df)
    df_joined <- st_as_sf(df_joined,
                          coords = c("LONGITUDE", "LATITUDE"),
                          crs = 4326,
                          remove = FALSE)
    try(plot(df_joined["tree_cover"]), silent = TRUE)
    try(
      ggplot(data = df_joined) +
        geom_sf(aes(fill = tree_cover)) +
        scale_fill_gradient(low = "blue", high = "red") +
        theme_minimal(),
      silent = TRUE
    )
  }

  invisible(TRUE)
}

# If executed directly, run and surface errors
if (!interactive()) {
  tryCatch(
    {
      run_fractional_cover()
    },
    error = function(e) {
      message("[ERROR] fractional_cover failed: ", conditionMessage(e))
      try({ tb <- utils::capture.output(traceback()); if (length(tb)>0) for (ln in tb) message(ln) }, silent = TRUE)
      quit(save = "no", status = 1, runLast = FALSE)
    }
  )
}

