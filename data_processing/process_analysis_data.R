#' Process data for covariate balance synthetic control analysis
#'
#' Combines active fire, historic fire behaviors, topography, meteorological, disturbance, and vegetation data into a single data frame per exposure year and land type.
## process data for the covariate balance synthetic control analysis
## combine active fire, historic trajectories on fire behaviors, topography, meteorological,
## disturbance, and vegetation data into single df
## the df is per exposure year (2008-2020) per land type ("conifer", "hardwood")
library("sf")
library("tidyverse")
library("mltools")
library("data.table")
library("fst")

run_process_analysis_data <- function(outDir = "data/processed_data/") {
  "Process data for the covariate balance synthetic control analysis."

  message(Sys.time(), " - Starting process_analysis_data")


# Optimized: Only use available/required layers (tree_cover.fst, gridClimate_mon2_conifer.fst, fire_brightness_frp_conifer.fst, fveg_elev_grid_ca_poly.RDS, FIRMS.RDS)

outDir = "data/processed_data/"
# Ensure output directories exist
dir.create(file.path(outDir, "rev_analysis_low"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(outDir, "rev_analysis_low", "fire_class"), showWarnings = FALSE, recursive = TRUE)

parameters <- data.frame(year = 2000:2020)
for (treated.year in parameters$year) {
  # Only conifer area is processed
  fveg_elev_grid_ca_poly <- readRDS(file.path(outDir, "fveg_elev_grid_ca_poly.RDS"))
  st_geometry(fveg_elev_grid_ca_poly) <- NULL
  # Use 'conifer' column to filter for conifer cells
  if (!"conifer" %in% names(fveg_elev_grid_ca_poly)) {
    print("Column names in fveg_elev_grid_ca_poly:")
    print(names(fveg_elev_grid_ca_poly))
    stop("Column 'conifer' not found in fveg_elev_grid_ca_poly. Please check your input data.")
  }
  if (!all(c("LATITUDE", "LONGITUDE") %in% names(fveg_elev_grid_ca_poly))) {
    stop("LATITUDE or LONGITUDE column missing in fveg_elev_grid_ca_poly.")
  }
  fveg_elev_grid_ca_poly <- fveg_elev_grid_ca_poly %>% filter(conifer == 1)
  fveg_elev_grid_ca_poly$unit <- paste0(fveg_elev_grid_ca_poly$LATITUDE, fveg_elev_grid_ca_poly$LONGITUDE)
  fveg_elev_grid_ca_poly$LATITUDE <- NULL
  fveg_elev_grid_ca_poly$LONGITUDE <- NULL

  # Import pre-exposure monthly meteorological covariates (conifer grid only)
  grid_climate <- read.fst(file.path(outDir, "gridClimate_mon2_conifer.fst"), from = 1, to = NULL)
  # Check for required columns
  if (!all(c("LATITUDE", "LONGITUDE") %in% names(grid_climate))) {
    stop("LATITUDE or LONGITUDE column missing in gridClimate_mon2_conifer.fst")
  }
  month_labels <- sprintf("%02d", 1:12)
  clim_vars <- c("minat", "maxat", "prcp", "swe", "wvp")
  clim_colnames <- c()
  for (var in clim_vars) {
    for (y in 2000:(treated.year - 1)) {
      for (m in month_labels) {
        clim_colnames <- c(clim_colnames, paste0(var, "_", y, "_", as.numeric(m)))
      }
    }
  }
  clim_colnames <- c("LATITUDE", "LONGITUDE", clim_colnames)
  clim_colnames <- intersect(clim_colnames, names(grid_climate))
  grid_climate <- grid_climate[, clim_colnames, drop = FALSE]

  # Import pre-exposure annual tree cover covariates
  tree_cover <- read.fst(file.path(outDir, "tree_cover.fst"), from = 1, to = NULL)
  if (!all(c("LATITUDE", "LONGITUDE") %in% names(tree_cover))) {
    stop("LATITUDE or LONGITUDE column missing in tree_cover.fst")
  }
  n_years <- (treated.year - 2000)
  col.idx_tree <- c(1, 2, 3:(2 + n_years))
  col.idx_tree <- col.idx_tree[col.idx_tree <= ncol(tree_cover)]
  tree_cover <- tree_cover[, col.idx_tree, drop = FALSE]

  # Import pre-exposure annual fire behaviors (frequency and intensity) covariates
  fire_brightness_frp <- read.fst(file.path(outDir, "fire_brightness_frp_conifer.fst"), from = 1, to = NULL)
  if (!all(c("LATITUDE", "LONGITUDE") %in% names(fire_brightness_frp))) {
    stop("LATITUDE or LONGITUDE column missing in fire_brightness_frp_conifer.fst")
  }
  n_fire_years <- (treated.year - 2000 + 1)
  fire_vars <- c("fire", "avg_BRIGHTNESS", "max_FRP")
  fire_colnames <- c()
  for (y in 2000:(treated.year - 1)) {
    for (v in fire_vars) {
      fire_colnames <- c(fire_colnames, paste0(v, "_", y))
    }
  }
  col.idx_fire <- c(1, 2, which(names(fire_brightness_frp) %in% fire_colnames))
  col.idx_fire <- col.idx_fire[col.idx_fire <= ncol(fire_brightness_frp)]
  fire_brightness_frp <- fire_brightness_frp[, col.idx_fire, drop = FALSE]

  # Merge all covariates by LATITUDE, LONGITUDE
  merge_keys <- intersect(intersect(names(grid_climate), names(tree_cover)), names(fire_brightness_frp))
  merge_keys <- merge_keys[merge_keys %in% c("LATITUDE", "LONGITUDE")]
  if (length(merge_keys) < 2) stop("LATITUDE and LONGITUDE must be present in all input tables for merging.")
  df <- merge(grid_climate, tree_cover, by = merge_keys, all.x = TRUE)
  df <- merge(df, fire_brightness_frp, by = merge_keys, all.x = TRUE)

  # Fire behavior files should encode no-fire years as 0, but keep a defensive
  # fill here so legacy/cached files with NA do not propagate into modeling.
  fire_related_cols <- grep('^(fire_|avg_BRIGHTNESS_|max_FRP_)', names(df), value = TRUE)
  if (length(fire_related_cols) > 0) {
    for (cc in fire_related_cols) {
      if (anyNA(df[[cc]])) df[[cc]][is.na(df[[cc]])] <- 0
    }
  }

  df$unit <- paste0(df$LATITUDE, df$LONGITUDE)

  # Create exposed and unexposed units based on fire intensity (FRP) at focal years
  FIRMS_ca_grouped <- readRDS(file.path(outDir, "FIRMS.RDS"))
  FIRMS_ca_grouped$unit <- paste0(FIRMS_ca_grouped$LATITUDE, FIRMS_ca_grouped$LONGITUDE)
  st_geometry(FIRMS_ca_grouped) <- NULL
  # Mask FIRMS to conifer units only
  conifer_units <- fveg_elev_grid_ca_poly$unit
  FIRMS_ca_grouped <- FIRMS_ca_grouped[FIRMS_ca_grouped$unit %in% conifer_units, ]
  fire.df <- subset(FIRMS_ca_grouped, year == treated.year)
  fire.index <- unique(fire.df[c("unit")])

  hist.fire.df <- subset(FIRMS_ca_grouped, year <= treated.year - 1)
  hist.fire.df$has.fire <- 1
  hist.fire.df <- hist.fire.df %>% group_by(unit) %>% summarise(num.fire = sum(has.fire))

  # Keep only covariates and treatment
  df <- merge(fveg_elev_grid_ca_poly, df, by = "unit", all.x = TRUE)
  if (!is.null(hist.fire.df) && nrow(hist.fire.df) > 0) {
    df <- merge(df, hist.fire.df, by = "unit", all.x = TRUE)
    df$num.fire[is.na(df$num.fire)] <- 0
  } else {
    df$num.fire <- 0
  }
  # treated = all units which had fire in treated.year
  df$treated = 0
  df[df$unit %in% fire.index$unit, "treated"] = 1

  fire.df_year <- fire.df %>% group_by(LATITUDE, LONGITUDE, year) %>% summarise(max_FRP = max(max_FRP))

  # classify the fire types by max FRP for each focal years
  # based on systems proposed in https://www.sciencedirect.com/science/article/abs/pii/S003442570800062X
  fire.df_year$class <- 0
  fire.df_year[fire.df_year$max_FRP == 0,]$class <- 0
  fire.df_year[0 < fire.df_year$max_FRP & fire.df_year$max_FRP < 100,]$class <- 1
  fire.df_year[100 <= fire.df_year$max_FRP & fire.df_year$max_FRP < 500,]$class <- 2
  fire.df_year[500 <= fire.df_year$max_FRP & fire.df_year$max_FRP < 1000,]$class <- 3
  fire.df_year[1000 <= fire.df_year$max_FRP & fire.df_year$max_FRP < 1500,]$class <- 4
  fire.df_year[fire.df_year$max_FRP >= 1500,]$class <- 5

  saveRDS(fire.df_year, file = paste0(outDir, "/rev_analysis_low/fire_class/fire.df", treated.year, "_conifer.RDS"))

  # df for the covariate balance analysis, only keep low intensity fire class 1 as the exposure
  fire.df$has.hifire <- 0
  fire.df[fire.df$max_FRP >= 100,]$has.hifire <- 1
  df <- subset(df, !(unit %in% subset(fire.df, has.hifire == 1)$unit))

  saveRDS(df, file = file.path(outDir, "rev_analysis_low", paste0("analysis_treated", treated.year, "_conifer.RDS")))
}
  message(Sys.time(), " - process_analysis_data complete")
  invisible(TRUE)
}

# If executed directly, run and surface errors
if (!interactive()) {
  tryCatch(
    {
      run_process_analysis_data()
    },
    error = function(e) {
      message("[ERROR] process_analysis_data failed: ", conditionMessage(e))
      try({ tb <- utils::capture.output(traceback()); if (length(tb)>0) for (ln in tb) message(ln) }, silent = TRUE)
      quit(save = "no", status = 1, runLast = FALSE)
    }
  )
}
