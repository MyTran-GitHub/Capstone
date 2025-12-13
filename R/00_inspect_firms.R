# Inspect FIRMS.RDS basic structure
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(sf)
library(dplyr)
library(here)

repo_root <- here::here() %>% normalizePath()
setwd(repo_root)

firms_path <- file.path("data", "processed_data", "FIRMS.RDS")
stopifnot(file.exists(firms_path))

firms <- readRDS(firms_path)

message("Column names:")
print(names(firms))
message("Sample rows:")
print(head(firms, 10))

firms$unit <- paste0(firms$LATITUDE, firms$LONGITUDE)

num_pixels <- dplyr::n_distinct(firms$unit)
years <- sort(unique(firms$year))

message("Unique pixels: ", num_pixels)
message("Year range: ", paste(range(years), collapse = " - "))
message("Total years: ", length(years))

saveRDS(firms, file = file.path("data", "processed_data", "FIRMS_with_unit.RDS"))
message("Saved augmented FIRMS to data/processed_data/FIRMS_with_unit.RDS")
