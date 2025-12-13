# Merge all environmental layers into final conifer panel
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("fst", quietly = TRUE)) install.packages("fst")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(sf)
library(dplyr)
library(fst)
library(here)

options(repos = c(CRAN = "https://cloud.r-project.org"))
setwd(here::here())

# Load base grid with elevation
base_grid <- readRDS("data/processed_data/conifer_grid_with_elevation.RDS")

message("Base conifer pixels: ", nrow(base_grid))

# Create pixel-year skeleton
panel <- expand.grid(
  unit = base_grid$unit,
  year = 2000:2021,
  stringsAsFactors = FALSE
)

# Join static variables
panel <- panel %>%
  left_join(
    base_grid %>% 
      st_drop_geometry() %>% 
      select(unit, LATITUDE, LONGITUDE, elevation),
    by = "unit"
  )

# Join time-varying layers
layers <- list(
  daymet = "data/processed_data/daymet_conifer.fst",
  mtbs = "data/processed_data/mtbs_severity_conifer.fst",
  tree_cover = "data/processed_data/tree_cover_conifer.fst",
  prescribed = "data/processed_data/prescribed_conifer.fst"
)

for (layer_name in names(layers)) {
  fst_path <- layers[[layer_name]]
  if (file.exists(fst_path)) {
    message("Joining ", layer_name, "...")
    layer_data <- read_fst(fst_path)
    panel <- panel %>% left_join(layer_data, by = c("unit", "year"))
  } else {
    message("Warning: Missing ", fst_path)
  }
}

# Fill NAs with reasonable defaults
panel <- panel %>%
  mutate(
    max_severity = ifelse(is.na(max_severity), 0, max_severity),
    fire_count = ifelse(is.na(fire_count), 0, fire_count),
    prescribed_fire = ifelse(is.na(prescribed_fire), 0, prescribed_fire),
    tree_cover = ifelse(is.na(tree_cover), 70, tree_cover)  # Default for conifer
  )

# Load FIRMS fire data and join
firms_path <- "data/processed_data/FIRMS.RDS"
if (file.exists(firms_path)) {
  message("Joining FIRMS fire activity...")
  firms <- readRDS(firms_path)
  firms <- st_drop_geometry(firms)
  firms$unit <- paste0(firms$LATITUDE, firms$LONGITUDE)
  
  # Aggregate to pixel-year level
  firms_annual <- firms %>%
    group_by(unit, year) %>%
    summarise(
      max_FRP = max(FRP, na.rm = TRUE),
      fire_days = n(),
      .groups = "drop"
    )
  
  panel <- panel %>%
    left_join(firms_annual, by = c("unit", "year")) %>%
    mutate(
      max_FRP = ifelse(is.na(max_FRP), 0, max_FRP),
      fire_days = ifelse(is.na(fire_days), 0, fire_days)
    )
  
  # Classify fire intensity
  panel <- panel %>%
    mutate(
      fire_class = case_when(
        max_FRP == 0 ~ 0,
        max_FRP > 0 & max_FRP <= 100 ~ 1,   # Low intensity
        max_FRP > 100 & max_FRP <= 500 ~ 2, # Moderate
        max_FRP > 500 & max_FRP <= 1000 ~ 3,
        max_FRP > 1000 & max_FRP <= 1500 ~ 4,
        max_FRP > 1500 ~ 5
      )
    )
}

# Save final panel
saveRDS(panel, "data/processed_data/conifer_environmental_panel.RDS")

message("\n✓ Final conifer panel created:")
message("  Rows (pixel-years): ", nrow(panel))
message("  Pixels: ", length(unique(panel$unit)))
message("  Years: ", paste(range(panel$year), collapse = "-"))
message("  Variables: ", paste(names(panel), collapse = ", "))
message("\nSample statistics:")
print(summary(panel %>% select(-unit, -LATITUDE, -LONGITUDE)))
