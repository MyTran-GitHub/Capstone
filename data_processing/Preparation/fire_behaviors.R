library(fst)
library(sf)
library(tidyverse)

outDir <- "data/processed_data"

# Load conifer-only grid (created by vegetation_class.R)
fveg_grid_ca <- readRDS(file.path(outDir, "fveg_grid_ca.RDS"))
# If not an sf object, convert and set CRS
if (!inherits(fveg_grid_ca, "sf")) {
  fveg_grid_ca <- sf::st_as_sf(fveg_grid_ca, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)
}
# If CRS is missing, set to WGS84
if (is.na(sf::st_crs(fveg_grid_ca))) {
  sf::st_crs(fveg_grid_ca) <- sf::st_crs(4326)
}

# Load FIRMS and ensure CRS matches conifer grid
fire.df <- readRDS(file.path(outDir, "FIRMS.RDS"))
# If not an sf object, convert and set CRS
if (!inherits(fire.df, "sf")) {
  fire.df <- sf::st_as_sf(fire.df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)
}
# If CRS is missing, set to WGS84
if (is.na(sf::st_crs(fire.df))) {
  sf::st_crs(fire.df) <- sf::st_crs(4326)
}
if (!is.null(st_crs(fveg_grid_ca)) && !is.null(st_crs(fire.df)) && st_crs(fveg_grid_ca) != st_crs(fire.df)) {
  fire.df <- st_transform(fire.df, st_crs(fveg_grid_ca))
}
st_geometry(fire.df) <- NULL

# Add unit column for join (unique identifier by coordinates)
fveg_grid_ca$unit <- paste0(fveg_grid_ca$LATITUDE, fveg_grid_ca$LONGITUDE)
fire.df$unit <- paste0(fire.df$LATITUDE, fire.df$LONGITUDE)

# Filter FIRMS to conifer cells only (masking)
fire.df <- fire.df[fire.df$unit %in% fveg_grid_ca$unit, ]

# Use only conifer grid for output
df <- fveg_grid_ca[,c("LONGITUDE", "LATITUDE")]
df$unit <- paste0(df$LATITUDE, df$LONGITUDE)

# For each grid, assign yearly historical fire information
## Aggregate events to per-unit, per-year statistics
# compute: n_events, avg_BRIGHTNESS (mean), max_FRP (max)
agg <- fire.df %>%
  group_by(unit, year) %>%
  summarise(
    n_events = n(),
    # Use 0 when intensity is unavailable so no-fire/no-signal years do not propagate NAs.
    avg_BRIGHTNESS = if (all(is.na(avg_BRIGHTNESS))) 0 else mean(avg_BRIGHTNESS, na.rm = TRUE),
    max_FRP = if (all(is.na(max_FRP))) 0 else max(max_FRP, na.rm = TRUE),
    .groups = "drop"
  )

# create year-variable columns for 2000:2021 initialized to 0
years <- 2000:2021
for (y in years) {
  df[[paste0("fire_", y)]] <- 0
  df[[paste0("avg_BRIGHTNESS_", y)]] <- 0
  df[[paste0("max_FRP_", y)]] <- 0
}

# fill aggregated values into df
for (r in seq_len(nrow(agg))) {
  row <- agg[r, ]
  unit_idx <- which(df$unit == row$unit)
  if (length(unit_idx) == 0) next
  y <- as.character(row$year)
  # presence indicator
  df[unit_idx, paste0("fire_", y)] <- as.integer(row$n_events > 0)
  # intensity fields: keep 0 as no-fire/no-signal encoding to avoid downstream NA handling issues.
  df[unit_idx, paste0("avg_BRIGHTNESS_", y)] <- ifelse(is.na(row$avg_BRIGHTNESS), 0, row$avg_BRIGHTNESS)
  df[unit_idx, paste0("max_FRP_", y)] <- ifelse(is.na(row$max_FRP), 0, row$max_FRP)
}

# Enforce consistency: if no fire in a year, intensity variables must be 0.
for (y in years) {
  fire_col <- paste0("fire_", y)
  b_col <- paste0("avg_BRIGHTNESS_", y)
  f_col <- paste0("max_FRP_", y)
  no_fire_idx <- which(df[[fire_col]] == 0)
  if (length(no_fire_idx) > 0) {
    df[no_fire_idx, b_col] <- 0
    df[no_fire_idx, f_col] <- 0
  }
}

# Remove geometry column if present (in case df is still an sf object)
if ("geometry" %in% names(df)) {
  df$geometry <- NULL
}
# Ensure df is a pure data.frame (not sf)
df <- as.data.frame(df)
df <- df[,!names(df) %in% c("unit")]

# Check NA summary before saving
firedata_cols <- setdiff(names(df), c("LONGITUDE", "LATITUDE"))
na_summary <- sapply(df[firedata_cols], function(x) sum(is.na(x)))
na_pct_summary <- sapply(df[firedata_cols], function(x) 100*sum(is.na(x))/length(x))

cat("\nFire brightness/FRP NA summary:\n")
na_summary_df <- data.frame(
  Column = names(na_summary),
  NA_Count = as.numeric(na_summary),
  NA_Percent = round(as.numeric(na_pct_summary), 2)
)
print(na_summary_df[na_summary_df$NA_Count > 0, ])
if (all(na_summary == 0)) {
  cat("  ✓ No NA values found in fire data\n")
}

# Save conifer-masked fire brightness and FRP data
write_fst(df, path = file.path(outDir, "fire_brightness_frp_conifer.fst"))
message("✓ Created conifer-only fire brightness and FRP data")