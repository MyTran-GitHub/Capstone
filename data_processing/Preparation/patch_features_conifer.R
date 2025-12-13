#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(data.table)
  library(fst)
})

# Usage:
# Rscript data_processing/Preparation/patch_features_conifer.R \
#   --patch_shp data/raw_data/fire_perimeters/CA_fire_perimeters.shp \
#   --veg_shp data/raw_data/vegetation/vegetation_class.shp \
#   --grid_fst data/processed_data/gridClimate_mon2.fst \
#   --out data/processed_data/patch_features_conifer.fst \
#   --conifer_codes 42,43 \
#   --min_conifer_pct 0.7

args <- commandArgs(trailingOnly = TRUE)
kv <- as.list(setNames(rep(NA_character_, length(args)), rep("", length(args))))
if (length(args) > 0) {
  for (i in seq(1, length(args), by = 2)) {
    if (i + 1 <= length(args)) kv[[sub("^--", "", args[i])]] <- args[i + 1]
  }
}

patch_shp <- kv[["patch_shp"]]
veg_shp   <- kv[["veg_shp"]]
grid_fst  <- kv[["grid_fst"]]
out_path  <- kv[["out"]]
codes_str <- kv[["conifer_codes"]]
min_pct   <- as.numeric(kv[["min_conifer_pct"]])
if (is.na(min_pct)) min_pct <- 0.7

stopifnot(!is.na(patch_shp), !is.na(veg_shp), !is.na(grid_fst), !is.na(out_path))
if (!file.exists(patch_shp)) stop("Missing patch shapefile: ", patch_shp)
if (!file.exists(veg_shp))   stop("Missing vegetation shapefile: ", veg_shp)
if (!file.exists(grid_fst))  stop("Missing grid fst: ", grid_fst)

conifer_codes <- if (!is.na(codes_str)) as.integer(strsplit(codes_str, ",")[[1]]) else integer()

message("Loading patches: ", patch_shp)
patches <- suppressMessages(st_read(patch_shp, quiet = TRUE))
if (is.na(st_crs(patches))) st_crs(patches) <- 4326
patches <- st_make_valid(patches)
patches$patch_id <- seq_len(nrow(patches))

message("Loading vegetation: ", veg_shp)
veg <- suppressMessages(st_read(veg_shp, quiet = TRUE))
if (is.na(st_crs(veg))) st_crs(veg) <- 4326
veg <- st_make_valid(veg)

# Expect a column with class codes; guess by common names
code_col <- intersect(c("class","code","veg_code","VEG_CODE"), names(veg))[1]
if (is.na(code_col)) stop("Vegetation class code column not found.")

if (length(conifer_codes) > 0) {
  message("Filtering vegetation to conifer codes: ", paste(conifer_codes, collapse = ","))
  veg_conifer <- veg[veg[[code_col]] %in% conifer_codes, ]
} else {
  stop("Provide --conifer_codes (comma-separated class codes for conifer).")
}

# Build conifer mask by intersecting patches and vegetation
message("Computing conifer coverage per patch...")
patches_area <- st_area(patches)
inter <- suppressWarnings(st_intersection(patches, veg_conifer))
inter_area <- st_area(inter)

coverage_dt <- data.table(patch_id = inter$patch_id,
                          conifer_area = as.numeric(inter_area))
coverage_sum <- coverage_dt[, .(conifer_area = sum(conifer_area, na.rm = TRUE)), by = patch_id]
patch_dt <- data.table(patch_id = patches$patch_id,
                       patch_area = as.numeric(patches_area))
cov_merged <- merge(patch_dt, coverage_sum, by = "patch_id", all.x = TRUE)
cov_merged[is.na(conifer_area), conifer_area := 0]
cov_merged[, conifer_pct := conifer_area / patch_area]

message("Keeping patches with conifer_pct >= ", min_pct)
keep_ids <- cov_merged[conifer_pct >= min_pct, patch_id]
patches_keep <- patches[patches$patch_id %in% keep_ids, ]

message("Loading grid climate: ", grid_fst)
grid_df <- fst::read_fst(grid_fst)
stopifnot(all(c("LATITUDE","LONGITUDE") %in% names(grid_df)))
grid_sf <- st_as_sf(grid_df, coords = c("LONGITUDE","LATITUDE"), crs = 4326, remove = FALSE)
grid_sf <- st_make_valid(grid_sf)

# Extract patch-level summaries from grid monthly columns (pre-treatment window optional)
clim_cols <- grep("^(minat|maxat|prcp|wvp)_[0-9]{4}_[0-9]{1,2}$", names(grid_df), value = TRUE)
if (length(clim_cols) == 0) stop("No climate monthly columns found in ", grid_fst)

message("Aggregating climate features per patch (means across points)...")
# Map points to patches
join_idx <- suppressWarnings(st_join(grid_sf[, c("LATITUDE","LONGITUDE")], patches_keep["patch_id"]))
grid_df$patch_id <- join_idx$patch_id
grid_df <- grid_df[!is.na(grid_df$patch_id), ]

# Compute simple means per patch for each monthly column
dt <- as.data.table(grid_df)
feat_dt <- dt[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = patch_id, .SDcols = clim_cols]

# Add conifer_pct and patch_area features
feat_dt <- merge(feat_dt, cov_merged[, .(patch_id, conifer_pct, patch_area)], by = "patch_id", all.x = TRUE)

message("Writing patch features → ", out_path)
fst::write_fst(as.data.frame(feat_dt), out_path, compress = 50)
cat("\n✓ Wrote ", out_path, "\n", sep = "")
