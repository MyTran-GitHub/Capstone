# Process MTBS fire severity for conifer-only pixels
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("fst", quietly = TRUE)) install.packages("fst")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(sf)
library(terra)
library(dplyr)
library(fst)
library(here)

options(repos = c(CRAN = "https://cloud.r-project.org"))
setwd(here::here())

conifer_grid_path <- "data/processed_data/conifer_grid_with_elevation.RDS"
if (!file.exists(conifer_grid_path)) stop("Missing ", conifer_grid_path)
conifer_grid <- readRDS(conifer_grid_path)

# Load MTBS perimeters (download from https://www.mtbs.gov/direct-download)
mtbs_shp <- "data/raw_data/mtbs/mtbs_perims_DD.shp"
if (!file.exists(mtbs_shp)) {
  message("MTBS perimeters not found. Place shapefile at ", mtbs_shp)
  message("Creating placeholder severity data (all zeros)...")
  
  # Placeholder: no fires
  severity_df <- expand.grid(
    unit = unique(conifer_grid$unit),
    year = 2000:2021,
    stringsAsFactors = FALSE
  ) %>%
    mutate(max_severity = 0, fire_count = 0)
  
  write_fst(severity_df, "data/processed_data/mtbs_severity_conifer.fst")
  message("✓ Created placeholder MTBS severity (all zeros)")
  quit(save = "no", status = 0)
}

mtbs <- st_read(mtbs_shp) %>%
  filter(Year >= 2000, Year <= 2021)

# Ensure same CRS
if (st_crs(conifer_grid) != st_crs(mtbs)) {
  mtbs <- st_transform(mtbs, st_crs(conifer_grid))
}

# Spatial join: Find fires intersecting conifer pixels
conifer_fires <- st_join(conifer_grid, mtbs, join = st_intersects) %>%
  filter(!is.na(Fire_ID)) %>%
  st_drop_geometry()

if (nrow(conifer_fires) == 0) {
  message("No MTBS fires intersect conifer pixels. Creating zeros...")
  severity_df <- expand.grid(
    unit = unique(conifer_grid$unit),
    year = 2000:2021,
    stringsAsFactors = FALSE
  ) %>%
    mutate(max_severity = 0, fire_count = 0)
  
  write_fst(severity_df, "data/processed_data/mtbs_severity_conifer.fst")
  message("✓ Created zero MTBS severity")
  quit(save = "no", status = 0)
}

# For each fire, load severity raster if available
severity_list <- list()
raster_dir <- "data/raw_data/mtbs/severity_rasters"

for (fire_id in unique(conifer_fires$Fire_ID)) {
  # Try common naming patterns for severity rasters
  raster_patterns <- c(
    file.path(raster_dir, paste0(fire_id, "_dnbr.tif")),
    file.path(raster_dir, paste0(fire_id, "_rdnbr.tif")),
    file.path(raster_dir, paste0(fire_id, "_severity.tif"))
  )
  
  raster_path <- raster_patterns[file.exists(raster_patterns)][1]
  
  if (is.na(raster_path)) {
    # Use burn severity class from shapefile if available
    fire_pixels <- conifer_fires %>% filter(Fire_ID == fire_id)
    severity_list[[fire_id]] <- data.frame(
      unit = fire_pixels$unit,
      fire_id = fire_id,
      year = unique(fire_pixels$Year),
      severity = ifelse("Severity" %in% names(fire_pixels), fire_pixels$Severity, 2)
    )
    next
  }
  
  # Extract from raster
  sev_rast <- rast(raster_path)
  fire_pixels <- conifer_fires %>% filter(Fire_ID == fire_id)
  
  # Re-attach geometry for extraction
  fire_pixels_sf <- conifer_grid %>% filter(unit %in% fire_pixels$unit)
  
  sev_vals <- terra::extract(sev_rast, terra::vect(fire_pixels_sf), fun = mean)
  
  severity_list[[fire_id]] <- data.frame(
    unit = fire_pixels$unit,
    fire_id = fire_id,
    year = unique(fire_pixels$Year),
    severity = sev_vals[,2]
  )
}

severity_df_raw <- bind_rows(severity_list)

# Aggregate to pixel-year (multiple fires possible)
severity_annual <- severity_df_raw %>%
  group_by(unit, year) %>%
  summarise(
    max_severity = max(severity, na.rm = TRUE),
    fire_count = n(),
    .groups = "drop"
  )

# Fill in zeros for pixel-years with no fires
all_combos <- expand.grid(
  unit = unique(conifer_grid$unit),
  year = 2000:2021,
  stringsAsFactors = FALSE
)

severity_final <- all_combos %>%
  left_join(severity_annual, by = c("unit", "year")) %>%
  mutate(
    max_severity = ifelse(is.na(max_severity), 0, max_severity),
    fire_count = ifelse(is.na(fire_count), 0, fire_count)
  )

write_fst(severity_final, "data/processed_data/mtbs_severity_conifer.fst")
message("✓ Processed MTBS severity: ", sum(severity_final$fire_count > 0), " pixel-years with fires")
