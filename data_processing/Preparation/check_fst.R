#' Inspect and summarize FST climate data files
#'
#' This script inspects FST files, checks for coordinate columns, and summarizes climate variables for quality control.
#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(fst)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  cat("Usage: Rscript data_processing/Preparation/check_fst.R processed_data/gridClimate_mon2.fst \n")
  quit(status = 1)
}

fst_path <- args[1]
if (!file.exists(fst_path)) {
  stop(paste("File not found:", fst_path))
}

cat("\nInspecting FST:", fst_path, "\n\n")

# Read with fst
df <- fst::read_fst(fst_path)

cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n\n")

# Show first 10 column names
cat("First columns:\n")
print(names(df)[seq_len(min(10, ncol(df)))])

# Ensure coordinate columns exist
coord_ok <- all(c("LATITUDE","LONGITUDE") %in% names(df))
cat("\nHas LATITUDE/LONGITUDE:", coord_ok, "\n")
if (coord_ok) {
  lat_rng <- range(df$LATITUDE, na.rm = TRUE)
  lon_rng <- range(df$LONGITUDE, na.rm = TRUE)
  cat("LAT range:", paste(lat_rng, collapse = " to "), "\n")
  cat("LON range:", paste(lon_rng, collapse = " to "), "\n")
}

# Find a few climate columns (use POSIX classes to avoid perl dependency)
climate_cols <- grep("^(minat|maxat|prcp|wvp)_[0-9]{4}_[0-9]{1,2}$", names(df), value = TRUE)
cat("\nDetected climate columns:", length(climate_cols), "\n")
print(head(climate_cols, 12))

# Summaries for the first 8 climate columns
if (length(climate_cols) > 0) {
  cat("\nColumn summaries (first 8):\n")
  for (cn in head(climate_cols, 8)) {
    rng <- range(df[[cn]], na.rm = TRUE)
    na_cnt <- sum(is.na(df[[cn]]))
    cat(sprintf("  %-20s range: [%s, %s], NA: %d\n", cn, format(rng[1], digits=6), format(rng[2], digits=6), na_cnt))
  }
}

# Sample a random row and show its monthly series for one variable/year
set.seed(42)
if (length(climate_cols) > 0) {
  row_id <- sample.int(nrow(df), 1)
  # pick one variable/year present (e.g., prcp_2001_*)
  base <- sub("_[0-9]{1,2}$", "", climate_cols[1])
  series_cols <- grep(paste0("^", base, "_[0-9]{1,2}$"), names(df), value = TRUE)
  cat("\nRow", row_id, "series for", base, ":\n")
  print(df[row_id, series_cols])
}

cat("\nDone.\n")
