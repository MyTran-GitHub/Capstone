#' Inspect and summarize FST climate data files
#'
#' This script inspects FST files, checks for coordinate columns, and summarizes climate variables for quality control.
#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(fst)
  library(dplyr)
})

run_check_fst <- function(fst_path) {
  if (!file.exists(fst_path)) stop(paste("File not found:", fst_path))

  message("Inspecting FST: ", fst_path)

  df <- fst::read_fst(fst_path)

  message("Rows: ", nrow(df))
  message("Columns: ", ncol(df))

  message("First columns:")
  message(paste(head(names(df), 10), collapse = ", "))

  coord_ok <- all(c("LATITUDE", "LONGITUDE") %in% names(df))
  message("Has LATITUDE/LONGITUDE: ", coord_ok)
  if (coord_ok) {
    lat_rng <- range(df$LATITUDE, na.rm = TRUE)
    lon_rng <- range(df$LONGITUDE, na.rm = TRUE)
    message("LAT range: ", paste(lat_rng, collapse = " to "))
    message("LON range: ", paste(lon_rng, collapse = " to "))
  }

  climate_cols <- grep("^(minat|maxat|prcp|wvp)_[0-9]{4}_[0-9]{1,2}$", names(df), value = TRUE)
  message("Detected climate columns: ", length(climate_cols))
  if (length(climate_cols) > 0) message(paste(head(climate_cols, 12), collapse = ", "))

  if (length(climate_cols) > 0) {
    message("Column summaries (first 8):")
    for (cn in head(climate_cols, 8)) {
      rng <- range(df[[cn]], na.rm = TRUE)
      na_cnt <- sum(is.na(df[[cn]]))
      message(sprintf("  %-20s range: [%s, %s], NA: %d", cn, format(rng[1], digits = 6), format(rng[2], digits = 6), na_cnt))
    }
  }

  set.seed(42)
  if (length(climate_cols) > 0 && nrow(df) > 0) {
    row_id <- sample.int(nrow(df), 1)
    base <- sub("_[0-9]{1,2}$", "", climate_cols[1])
    series_cols <- grep(paste0("^", base, "_[0-9]{1,2}$"), names(df), value = TRUE)
    message("Row ", row_id, " series for ", base, ":")
    print(df[row_id, series_cols])
  }

  message("Done.")
  invisible(TRUE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    message("Usage: Rscript data_processing/Preparation/check_fst.R processed_data/gridClimate_mon2.fst")
    quit(status = 1)
  }
  fst_path <- args[1]
  tryCatch(
    {
      run_check_fst(fst_path)
    },
    error = function(e) {
      message("[ERROR] check_fst failed: ", conditionMessage(e))
      quit(save = "no", status = 1, runLast = FALSE)
    }
  )
}
