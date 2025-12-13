# Prepare forest vegetation grid for California based on conifer masklibrary(sf)
library(sf)
library(raster)

Dir = "data/raw_data"
outDir = "data/processed_data"

gpw_grid_ca <- readRDS(file.path(Dir, "gpw_grid_ca.RDS"))

# Create polygons for each grid cell as before
gpw_grid_ca_poly <- gpw_grid_ca
int_lon <- min(abs(diff(sort(unique(gpw_grid_ca$LONGITUDE)))/2))
int_lat <- min(abs(diff(sort(unique(gpw_grid_ca$LATITUDE)))/2))
poly_list <- lapply(1:nrow(gpw_grid_ca_poly), function(row) {
  st_bbox(c(
    xmin = gpw_grid_ca_poly$LONGITUDE[row] - int_lon,
    xmax = gpw_grid_ca_poly$LONGITUDE[row] + int_lon,
    ymin = gpw_grid_ca_poly$LATITUDE[row] - int_lat,
    ymax = gpw_grid_ca_poly$LATITUDE[row] + int_lat
  ), crs = st_crs(4326)) %>%
    st_as_sfc() %>%
    .[[1]]  # extract the sfg object
})
gpw_grid_ca_poly <- st_sf(gpw_grid_ca_poly, geometry = st_sfc(poly_list, crs = 4326))
# Read conifer mask raster (binary: 1 = conifer, 0 = not conifer)
conifer_mask <- raster(file.path(Dir, "conifer_mask.tif"))

# Reproject grid if needed
if (!compareCRS(st_crs(gpw_grid_ca_poly), crs(conifer_mask))) {
  gpw_grid_ca_poly <- st_transform(gpw_grid_ca_poly, crs = crs(conifer_mask))
}

# Extract mask value for each grid cell
conifer_flag <- raster::extract(conifer_mask, gpw_grid_ca_poly, fun = modal, na.rm = TRUE)
gpw_grid_ca_poly$conifer <- conifer_flag

# Keep only conifer cells (or flag them)
fveg_grid_ca_poly <- gpw_grid_ca_poly[gpw_grid_ca_poly$conifer == 1, ]

# Create non-spatial version (data.frame) with only conifer cells
fveg_grid_ca <- as.data.frame(st_drop_geometry(fveg_grid_ca_poly))

# Save both
saveRDS(fveg_grid_ca_poly, file = file.path(outDir, "fveg_grid_ca_poly.RDS"))
saveRDS(fveg_grid_ca, file = file.path(outDir, "fveg_grid_ca.RDS"))

## Visual check
plot(fveg_grid_ca_poly["conifer"])