# Download and extract Daymet variables for conifer-only pixels
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("fst", quietly = TRUE)) install.packages("fst")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(terra)
library(sf)
library(fst)
library(here)

options(repos = c(CRAN = "https://cloud.r-project.org"))
setwd(here::here())

conifer_grid_path <- "data/processed_data/conifer_grid_with_elevation.RDS"
if (!file.exists(conifer_grid_path)) stop("Missing ", conifer_grid_path)
conifer_grid <- readRDS(conifer_grid_path)

# Expect NetCDF files already downloaded to data/raw_data/daymet/
# Filenames like: daymet_tmax_YYYY.nc, daymet_tmin_YYYY.nc, daymet_prcp_YYYY.nc
raw_dir <- "data/raw_data/daymet"
if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)

years <- 2000:2021
vars <- c("tmax", "tmin", "prcp")

out_list <- list()

for (yr in years) {
  files <- file.path(raw_dir, paste0("daymet_", vars, "_", yr, ".nc"))
  if (!all(file.exists(files))) {
    message("Missing Daymet files for ", yr, ". Place NetCDFs at ", raw_dir)
    next
  }
  r_stack <- rast(files)
  # Extract mean over the year for each variable
  vals <- terra::extract(r_stack, terra::vect(conifer_grid), fun = mean, ID = FALSE)
  out_list[[as.character(yr)]] <- data.frame(
    unit = conifer_grid$unit,
    year = yr,
    tmax_annual = vals[,1],
    tmin_annual = vals[,2],
    prcp_annual = vals[,3]
  )
}

daymet_df <- do.call(rbind, out_list)
write_fst(daymet_df, "data/processed_data/daymet_conifer.fst")
message("✓ Wrote Daymet conifer FST: ", nrow(daymet_df), " rows")
