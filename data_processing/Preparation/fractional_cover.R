library(tibble)
library(dplyr)
library(sf)
library(raster)
library(fst)

Dir = "/data/raw_data/"
outDir = "/data/processed_data/"

# Read conifer grid polygons
fveg_grid_ca_poly <- readRDS(file.path(outDir, "fveg_grid_ca_poly.RDS"))

# Years to process
years <- 2000:2020
fracveg_dir <- file.path(Dir, "fractional_vegetation")

# Check all files exist
missing_files <- years[!file.exists(file.path(fracveg_dir, paste0("fractional_vegetation_", years, ".tif")))]
if (length(missing_files) > 0) stop("Missing raster files for years: ", paste(missing_files, collapse=", "))

# Extract mean tree cover for each grid cell and year
tree_cover_summary.list <- lapply(years, function(year) {
  raster_file <- file.path(fracveg_dir, paste0("fractional_vegetation_", year, ".tif"))
  r <- raster(raster_file)
  # Reproject grid if needed
  grid <- fveg_grid_ca_poly
  if (!compareCRS(st_crs(grid), crs(r))) {
    grid <- st_transform(grid, crs = crs(r))
  }
  # Extract mean tree cover for each polygon
  tree_cover <- raster::extract(r, grid, fun = mean, na.rm = TRUE)
  df <- st_drop_geometry(grid)[,c("LONGITUDE", "LATITUDE")]
  df$tree_cover <- tree_cover
  colnames(df)[3] <- paste0("tree_cover_", year)
  df
})

# Merge all years by LONGITUDE, LATITUDE
tree_cover_summary <- Reduce(function(x, y) merge(x, y, by =c("LONGITUDE", "LATITUDE"), all = TRUE), 
                             tree_cover_summary.list)

# Replace NA with 0 (if desired)
tree_cover_summary[] <- lapply(tree_cover_summary, function(x) replace(x, is.na(x), 0))

# Save for analysis
write_fst(tree_cover_summary, path = file.path(outDir, "tree_cover.fst"))

## Visual check
df_joined = st_drop_geometry(df_joined)
df_joined <- df_joined %>%
  group_by(LONGITUDE, LATITUDE) %>%
  summarise(tree_cover = max(tree_cover, na.rm = TRUE))
# 
df_joined = st_drop_geometry(df_joined)
df_joined <- st_as_sf(df_joined,
                      coords = c("LONGITUDE", "LATITUDE"),
                      crs = 4326,
                      remove = FALSE)
plot(df_joined["tree_cover"])
# 
ggplot(data = df_joined) +
  geom_sf(aes(fill = tree_cover)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()



