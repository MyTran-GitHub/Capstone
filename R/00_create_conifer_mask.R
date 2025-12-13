# Create conifer-only mask and filtered grid
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(sf)
library(dplyr)
library(here)

options(repos = c(CRAN = "https://cloud.r-project.org"))
setwd(here::here())

# Paths
veg_zip <- "data/raw_data/fveg221gdb.zip"
veg_dir <- "data/raw_data/fveg22_1.gdb"
ca_grid_path <- "data/raw_data/gpw_grid_ca.RDS"

# Unzip vegetation if needed
if (file.exists(veg_zip) && !dir.exists(veg_dir)) {
  message("Unzipping CAL FIRE vegetation...")
  unzip(veg_zip, exdir = "data/raw_data/")
}

# Handle both .gdb and .shp formats
if (dir.exists(veg_dir)) {
  veg_path <- veg_dir
} else {
  veg_shp <- list.files("data/raw_data", pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(veg_shp) == 0) stop("No vegetation data found")
  veg_path <- veg_shp[1]
}

message("Reading vegetation data from: ", veg_path)
veg <- suppressMessages(st_read(veg_path))

# Try multiple attribute names for conifer types
attr_candidates <- c("WHR13_TYPE", "WHRTYPE", "COVERTYPE", "WHR_TYPE")
attr_name <- attr_candidates[which(attr_candidates %in% names(veg))[1]]
if (is.na(attr_name)) stop("Conifer type attribute not found in vegetation shapefile.")

# Determine conifer codes or labels
# Common coding: 31 (Montane Coniferous), 32 (Subalpine Coniferous)
vals <- veg[[attr_name]]
conifer_idx <- rep(FALSE, length(vals))

if (is.numeric(vals)) {
  conifer_idx <- vals %in% c(31, 32)
} else {
  # String labels fallback
  conifer_idx <- grepl("Conifer", vals, ignore.case = TRUE) |
                 grepl("Montane", vals, ignore.case = TRUE) |
                 grepl("Subalpine", vals, ignore.case = TRUE)
}

conifer_poly <- veg[conifer_idx, ]
if (nrow(conifer_poly) == 0) stop("No conifer polygons found. Check attribute mapping.")

# Save mask
saveRDS(conifer_poly, "data/processed_data/conifer_mask.rds")
message("Conifer polygons: ", nrow(conifer_poly))

# Load CA grid
if (!file.exists(ca_grid_path)) stop("Grid file missing: ", ca_grid_path)
ca_grid <- readRDS(ca_grid_path)

# Ensure same CRS
if (sf::st_crs(ca_grid) != sf::st_crs(conifer_poly)) {
  conifer_poly <- st_transform(conifer_poly, st_crs(ca_grid))
}

# Spatial filter: keep grid cells intersecting conifer areas
message("Filtering grid to conifer-only pixels...")
conifer_grid <- st_intersection(ca_grid, st_union(conifer_poly))

# Drop duplicates if any and keep core fields
if (!"unit" %in% names(conifer_grid)) {
  conifer_grid$unit <- paste0(round(conifer_grid$LATITUDE, 6), ",", round(conifer_grid$LONGITUDE, 6))
}
conifer_grid <- conifer_grid %>% distinct(unit, .keep_all = TRUE)

message("Conifer pixel count: ", nrow(conifer_grid))

saveRDS(conifer_grid, "data/processed_data/conifer_grid_filtered.RDS")
message("✓ Saved conifer mask and filtered grid.")
