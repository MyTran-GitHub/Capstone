library("dataverse")
library("tibble") # to see dataframes in tidyverse-form
library(dplyr)
library(sf) 
library(parallel)
library(terra)
library(fst)


Dir = "data/raw_data/"
outDir = "data/processed_data/"

# Import the data contains the pixels of our interest in California.
fveg_grid_ca_poly <- readRDS(file.path(outDir, "fveg_grid_ca_poly.RDS"))
#st_transform(gpw_grid_ca[1:2,], crs = crs(fveg))

# slow!! run via HPC
# Import all disturbance data (they are in 5000 * 5000 meters tiles, all combines to California) 
# (See: https://www.usgs.gov/landsat-missions/landsat-shapefiles-and-kml-files)
f <- list.files(file.path(Dir, "disturbance"),
                pattern = "\\.tif",
                full.names = TRUE)

# combine all 5000 * 5000 meters tiles into a whole California map) 
# Different band means different years (band = 16 is Year 2000)
disturbance.list <- lapply(seq_along(f), function(i) terra::rast(f[[i]], lyrs = 16))
# project the map from meter coordinate reference system (CRS) to longitude-latitude system
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
st_crs(fveg_grid_ca_poly) <- llprj

start <- Sys.time()
fveg_grid_ca_tile.list <- mclapply(1:33, function(i) {
  # Get CRS from terra raster
  rast_crs <- terra::crs(disturbance.list[[i]])
  fveg_grid_ca_meter <- st_transform(fveg_grid_ca_poly, crs = rast_crs)
  # Use terra::crop and convert back to sf if needed
  fveg_grid_ca_tile <- st_crop(fveg_grid_ca_meter, terra::ext(disturbance.list[[i]]))
  # Extract values using terra::extract
  disturbance.fire <- terra::extract(disturbance.list[[i]], vect(fveg_grid_ca_tile))
  # Remove the ID column from terra extract
  disturbance.fire <- disturbance.fire[, -1, drop = FALSE]
  #print( Sys.time() - start)
  fveg_grid_ca_tile$fire = sapply(seq_len(nrow(disturbance.fire)), function(j) {sum(disturbance.fire[j,] == 1, na.rm = T)/ncol(disturbance.fire)})
  fveg_grid_ca_tile$timber = sapply(seq_len(nrow(disturbance.fire)), function(j) {sum(disturbance.fire[j,] == 2, na.rm = T)/ncol(disturbance.fire)})
  fveg_grid_ca_tile$drought = sapply(seq_len(nrow(disturbance.fire)), function(j) {sum(disturbance.fire[j,] == 3, na.rm = T)/ncol(disturbance.fire)})
  fveg_grid_ca_tile$greening = sapply(seq_len(nrow(disturbance.fire)), function(j) {sum(disturbance.fire[j,] == 4, na.rm = T)/ncol(disturbance.fire)})
  fveg_grid_ca_tile$browning = sapply(seq_len(nrow(disturbance.fire)), function(j) {sum(disturbance.fire[j,] == 5, na.rm = T)/ncol(disturbance.fire)})
  #print( Sys.time() - start )
  st_geometry(fveg_grid_ca_tile) <- NULL
  return(fveg_grid_ca_tile[,c(1:2,4:8)])
}, mc.cores = 12)
print( Sys.time() - start )

# after run through HPC
disturbance_summary.list <- mclapply(2000:2020, function(year) {
  tryCatch({
    f <- list.files(file.path(outDir, "disturbance", year),
                    pattern = "\\.rds",
                    full.names = TRUE)
    if (length(f) == 0) {
      cat("No files found for year", year, "\n")
      return(NULL)
    }
    disturbance.list <- lapply(f, readRDS)
    disturbance <- bind_rows(disturbance.list)
    if (nrow(disturbance) == 0) {
      cat("No disturbance data for year", year, "\n")
      return(NULL)
    }
    # Ensure we're working with data.frame without geometry
    base_grid <- st_drop_geometry(fveg_grid_ca_poly)
    base_grid <- base_grid[, c("LONGITUDE", "LATITUDE")]
    
    df_joined <- base_grid %>% 
      left_join(disturbance, by = c("LONGITUDE", "LATITUDE")) %>%
      group_by(LONGITUDE, LATITUDE) %>%
      summarise_at(.vars = c("fire", "timber", "drought", "greening","browning"), 
                   list(max = ~max(., na.rm = TRUE)), .groups = "drop")
    
    return(as.data.frame(df_joined))
  }, error = function(e) {
    cat("Error in year", year, ":", conditionMessage(e), "\n")
    return(NULL)
  })
}, mc.cores = 11)

# Remove NULL entries before merging
disturbance_summary.list <- Filter(Negate(is.null), disturbance_summary.list)

if (length(disturbance_summary.list) == 0) {
  stop("No valid disturbance data found for any year")
}

disturbance_summary <- Reduce(function(x, y) merge(x, y, by = c("LONGITUDE", "LATITUDE"), all = TRUE), 
                              disturbance_summary.list)
colnames(disturbance_summary)[3:ncol(disturbance_summary)] <- do.call(paste0, 
                                                                    expand.grid(c("fire_disturb_", "timber_", "drought_", "greening_","browning_"), 
                                                                                2000:2020))
sapply(disturbance_summary, function(x) sum(is.infinite(x)))
disturbance_summary[] <- lapply(disturbance_summary, function(x) replace(x, is.infinite(x) & x < 0, 0))

# disturbance data ready for analysis
write_fst(disturbance_summary, path = file.path(outDir, "disturbance.fst"))
