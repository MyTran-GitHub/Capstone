# Process prescribed fire treatments for conifer-only pixels
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

conifer_grid_path <- "data/processed_data/conifer_grid_with_elevation.RDS"
if (!file.exists(conifer_grid_path)) stop("Missing ", conifer_grid_path)
conifer_grid <- readRDS(conifer_grid_path)

# Load USFS FACTS and CAL FIRE treatment data
facts_shp <- "data/raw_data/facts/S_USA.Activity_HazFuelTrt_PL.shp"
calfire_rx_shp <- "data/raw_data/calfire/calfire_rx.shp"

all_rx <- NULL

# USFS FACTS
if (file.exists(facts_shp)) {
  facts <- st_read(facts_shp, quiet = TRUE) %>%
    filter(STATE_ABBR == "CA" | STATE == "CA", 
           DATE_COMPLETED >= "2000-01-01" | ACTUAL_COMPLETION_DATE >= "2000-01-01")
  
  # Flexible date column
  date_col <- if ("DATE_COMPLETED" %in% names(facts)) "DATE_COMPLETED" else "ACTUAL_COMPLETION_DATE"
  facts <- facts %>%
    mutate(treatment_year = as.numeric(format(as.Date(.data[[date_col]]), "%Y"))) %>%
    select(treatment_year, geometry)
  
  all_rx <- facts
}

# CAL FIRE Rx
if (file.exists(calfire_rx_shp)) {
  calfire <- st_read(calfire_rx_shp, quiet = TRUE)
  
  year_col <- if ("Year" %in% names(calfire)) "Year" else if ("YEAR_" %in% names(calfire)) "YEAR_" else NULL
  if (!is.null(year_col)) {
    calfire <- calfire %>%
      mutate(treatment_year = as.numeric(.data[[year_col]])) %>%
      filter(treatment_year >= 2000) %>%
      select(treatment_year, geometry)
    
    if (is.null(all_rx)) {
      all_rx <- calfire
    } else {
      all_rx <- bind_rows(all_rx, calfire)
    }
  }
}

if (is.null(all_rx) || nrow(all_rx) == 0) {
  message("No prescribed fire data found. Creating zeros...")
  rx_df <- expand.grid(
    unit = unique(conifer_grid$unit),
    year = 2000:2021,
    stringsAsFactors = FALSE
  ) %>%
    mutate(prescribed_fire = 0)
  
  write_fst(rx_df, "data/processed_data/prescribed_conifer.fst")
  message("✓ Created zero prescribed fire data")
  quit(save = "no", status = 0)
}

# Ensure same CRS
if (st_crs(conifer_grid) != st_crs(all_rx)) {
  all_rx <- st_transform(all_rx, st_crs(conifer_grid))
}

# Spatial join
rx_conifer <- st_join(conifer_grid, all_rx, join = st_intersects) %>%
  filter(!is.na(treatment_year)) %>%
  st_drop_geometry() %>%
  group_by(unit, treatment_year) %>%
  summarise(prescribed_fire = 1, .groups = "drop") %>%
  rename(year = treatment_year)

# Fill with zeros for all pixel-years
all_combos <- expand.grid(
  unit = unique(conifer_grid$unit),
  year = 2000:2021,
  stringsAsFactors = FALSE
)

rx_final <- all_combos %>%
  left_join(rx_conifer, by = c("unit", "year")) %>%
  mutate(prescribed_fire = ifelse(is.na(prescribed_fire), 0, prescribed_fire))

write_fst(rx_final, "data/processed_data/prescribed_conifer.fst")
message("✓ Processed prescribed fire: ", sum(rx_final$prescribed_fire), " pixel-years treated")
