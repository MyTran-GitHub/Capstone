dlibrary(fst)
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
Q <- list()
for (j in 2000:2021) {
    p <- subset(fire.df, year == j)
    p.u <- unique(p[c("unit")])
    index <- match(p.u$unit, p$unit)
    fire <- table(p$unit)
    fire <- as.numeric(fire)
    p.new <- cbind(p[index, ], fire)
    Q <- append(Q, list(p.new))
}
fire.df.new <- do.call(rbind, Q)

var <- c("fire", "avg_BRIGHTNESS", "max_FRP")
parameters <- expand.grid(2000:2021, var)

for (par in seq_len(nrow(parameters))) {
  dfn <- data.frame(matrix(0, nrow = nrow(df), ncol = 1))
  colnames(dfn) <- paste0(parameters[par, 2], "_", parameters[par, 1])
  df <- cbind(df, dfn)
}

for (j in 2000:2021) {
  p <- subset(fire.df.new, year == j)
  index <- match(p$unit, df$unit)
  df[index, paste0("fire_", j)] <- 1
  for (i in seq_len(length(index))) {
    df[index[i], paste0("avg_BRIGHTNESS_", j)] <- p$avg_BRIGHTNESS[i]
    df[index[i], paste0("max_FRP_", j)] <- p$max_FRP[i]
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