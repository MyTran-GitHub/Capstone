# Subset to Northern Sierra, focal year 2012, placeholder for conifer filter
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(dplyr)
library(here)

setwd(here::here())
panel_path <- file.path("data","processed_data","pixel_panel.rds")
stopifnot(file.exists(panel_path))
panel <- readRDS(panel_path)

# Bounding box Northern Sierra Nevada (approx)
lon_min <- -122; lon_max <- -119
lat_min <- 37.5; lat_max <- 40.0

subset_region <- panel %>% 
  filter(LONGITUDE >= lon_min, LONGITUDE <= lon_max,
         LATITUDE >= lat_min, LATITUDE <= lat_max)

focal_year <- 2012

# Placeholder conifer filter (needs vegetation layer). Keep all for now.
subset_region <- subset_region

# Treatment: low-intensity fire class 1 in focal year
treated_units <- subset_region %>% filter(year == focal_year, fire_class == 1) %>% pull(unit) %>% unique()
subset_region <- subset_region %>% mutate(treated = ifelse(year == focal_year & unit %in% treated_units, 1, 0))

saveRDS(subset_region, file = file.path("data","processed_data","subset_conifer2012_region.rds"))
message("Saved subset to data/processed_data/subset_conifer2012_region.rds")
