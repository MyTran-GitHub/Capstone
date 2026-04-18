#!/usr/bin/env Rscript
# Plot selected-control locations for K experiments (2019 default)

suppressPackageStartupMessages({
  library(sf)
  library(tigris)
  library(readr)
  library(dplyr)
  library(scales)
})

# Simple CLI parsing to avoid extra package dependencies (accepts --flag value)
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  if (flag %in% args) {
    i <- match(flag, args)
    if (i < length(args)) return(args[i + 1])
  }
  return(default)
}

parse_unit <- function(unit) {
  # parse a string like "33.5208333333333-116.445833333333" into numeric lat, lon
  u <- gsub('"', '', unit)
  # replace last hyphen with a separator (handles negative lon)
  u2 <- sub('-(?=[^-]*$)', '|', u, perl=TRUE)
  parts <- strsplit(u2, '\\|')[[1]]
  lat <- as.numeric(parts[1])
  lon <- as.numeric(parts[2])
  return(data.frame(LATITUDE = lat, LONGITUDE = lon))
}

rescale_transparency <- function(x, floor = 0.1) {
  rng <- range(x, na.rm=TRUE)
  if (is.na(rng[1]) || is.na(rng[2]) || diff(rng) == 0) return(rep(1, length(x)))
  t <- (x - rng[1]) / (rng[2] - rng[1])
  t <- t + floor
  t[t > 1] <- 1
  return(t)
}


cbps_dir <- get_arg("--cbps_dir", "Embeddings/data/cbps_integration/2019/")
out_dir <- get_arg("--out_dir", "data/outputs/diagnostics/")
year <- as.integer(get_arg("--year", "2019"))
truncile <- as.numeric(get_arg("--truncile", "0.1"))
eps <- as.numeric(get_arg("--eps", "1e-12"))
ks <- strsplit(get_arg("--k_list", "20,100"), ",")[[1]] %>% trimws()

dir.create(file.path(out_dir, "maps"), recursive = TRUE, showWarnings = FALSE)

CA <- tryCatch({
  st_transform(states(cb = TRUE, year = 2020), crs = 4326) %>% filter(NAME == "California")
}, error = function(e) {
  message("Failed to fetch states via tigris: ", e$message)
  NULL
})

for (k in ks) {
  message("Processing K=", k)
  file_full <- file.path(cbps_dir, paste0("cbps_weights_full_k", k, "_", year, ".csv"))
  file_compact <- file.path(cbps_dir, paste0("cbps_weights_k", k, "_", year, ".csv"))
  sel_file <- file.path(cbps_dir, paste0("selected_controls_k", k, "_", year, ".csv"))

  if (file.exists(file_full)) {
    df <- read_csv(file_full, show_col_types = FALSE)
  } else if (file.exists(file_compact)) {
    df <- read_csv(file_compact, show_col_types = FALSE)
    # parse unit into LATITUDE/LONGITUDE
    if (!("LATITUDE" %in% names(df) && "LONGITUDE" %in% names(df))) {
      coords <- do.call(rbind, lapply(df$unit, parse_unit))
      df$LATITUDE <- coords[,"LATITUDE"]
      df$LONGITUDE <- coords[,"LONGITUDE"]
    }
  } else {
    warning("No weights file found for K=", k, " (tried: ", file_full, ", ", file_compact, ")")
    next
  }

  # read selected controls
  selected <- NULL
  if (file.exists(sel_file)) {
    selected <- read_csv(sel_file, show_col_types = FALSE)
    if (!("LATITUDE" %in% names(selected) && "LONGITUDE" %in% names(selected))) {
      coords_s <- do.call(rbind, lapply(selected$unit, parse_unit))
      selected$LATITUDE <- coords_s[,"LATITUDE"]
      selected$LONGITUDE <- coords_s[,"LONGITUDE"]
    }
    selected <- selected %>% mutate(selected = TRUE)
  } else {
    warning("Selected-controls file not found: ", sel_file)
  }

  # ensure unit column exists
  if (!"unit" %in% names(df)) df$unit <- paste0(df$LATITUDE, "-", df$LONGITUDE)

  # merge selected flag
  if (!is.null(selected)) {
    if (!"unit" %in% names(selected)) selected$unit <- paste0(selected$LATITUDE, "-", selected$LONGITUDE)
    df <- df %>% left_join(selected %>% select(unit, selected), by = "unit")
  } else {
    df$selected <- FALSE
  }

  # defensive weight handling
  if (!"weight" %in% names(df)) stop("weights file missing `weight` column for K=", k)
  df$weight <- as.numeric(df$weight)
  df$weight[is.na(df$weight) | df$weight <= 0] <- eps

  # compute log weight and truncate lower tail
  df$logwt <- log(df$weight)
  lowq <- quantile(df$logwt, truncile, na.rm = TRUE)
  df$logwt[is.na(df$logwt)] <- lowq
  df$logwt[df$logwt < lowq] <- lowq

  df$transparency <- rescale_transparency(df$logwt, floor = 0.1)

  # geometry
  if (!("LATITUDE" %in% names(df) && "LONGITUDE" %in% names(df))) stop("Missing LATITUDE/LONGITUDE for plotting for K=", k)
  pts <- st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

  out_file <- file.path(out_dir, "maps", paste0("maps_k", k, "_", year, ".jpeg"))
  message("Writing map to ", out_file)
  jpeg(out_file, width = 8.5*150, height = 11*150, quality = 90, res = 150)
  par(mar = c(0,0,2,0))
  if (!is.null(CA)) {
    plot(st_geometry(CA), col = "gray95", border = "gray60", main = paste0("Selected controls K=", k, " (", year, ")"))
  } else {
    plot(0, type = 'n', xlab = '', ylab = '', main = paste0("Selected controls K=", k, " (", year, ")"))
  }

  # plot selected controls in blue (preferred) and treated in red
  sel_controls <- pts %>% filter(!is.na(selected) & selected == TRUE & (is.na(treated) | treated == 0))
  if (nrow(sel_controls) > 0) {
    cols_sel <- alpha("blue", sel_controls$transparency)
    plot(st_geometry(sel_controls), pch = 16, cex = 0.15, col = cols_sel, add = TRUE)
  }

  # plot remaining controls (non-selected, non-treated) as muted points
  other_controls <- pts %>% filter(is.na(selected) | selected == FALSE)
  if (nrow(other_controls) > 0) {
    cols_other <- alpha("gray40", pmax(0.05, other_controls$transparency * 0.5))
    plot(st_geometry(other_controls), pch = 16, cex = 0.12, col = cols_other, add = TRUE)
  }

  # plot treated units in red
  treated_pts <- pts %>% filter(!is.na(treated) & treated == 1)
  if (nrow(treated_pts) > 0) {
    plot(st_geometry(treated_pts), pch = 16, cex = 0.25, col = "red", add = TRUE)
  }

  legend("bottomleft", legend = c("Treated","Selected controls"), pch = 16, col = c("red","blue"), cex = 1)
  dev.off()

  message("K=", k, " done; rows=", nrow(df), ", selected matched=", sum(df$selected, na.rm=TRUE))
}

message("All Ks processed.")
