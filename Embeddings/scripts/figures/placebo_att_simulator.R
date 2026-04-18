#!/usr/bin/env Rscript
#Main placebo command (2019):
#Rscript placebo_att_simulator.R year=2019 B=1000 pre_years=2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018 post_years=2020 assignment_mode=control_only n_workers=1 seed_base=20260405 checkpoint_every=100 resume=true gate_prefit_mult=5.0 enforce_ratio_gate=false gate_ratio_max=20.0 donor_placebo_size=1 min_valid_draws=100 out_dir='Embeddings/data/k_selection/2019/placebo'


suppressPackageStartupMessages({
  pkgs <- c("data.table", "ggplot2", "dplyr")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(sprintf("Package %s is required but not installed.", p))
    }
  }
  library(data.table)
  library(ggplot2)
  library(dplyr)
})
invisible(utils::globalVariables(c("max_FRP", "treated", "has.hifire95", "weight", "denom", "sum.hifire95")))

args <- commandArgs(trailingOnly = TRUE)
arg_list <- list()
for (a in args) {
  if (grepl("=", a, fixed = TRUE)) {
    kv <- strsplit(a, "=", fixed = TRUE)[[1]]
    arg_list[[kv[1]]] <- kv[2]
  }
}

year <- ifelse(!is.null(arg_list$year), as.character(arg_list$year), "2019")
B <- ifelse(!is.null(arg_list$B), as.integer(arg_list$B), 1000)
post_years <- ifelse(!is.null(arg_list$post_years), arg_list$post_years, "2020")
post_years <- as.integer(strsplit(post_years, ",", fixed = TRUE)[[1]])
emb_file <- file.path("Embeddings", "data", "embeddings", sprintf("embeddings_%s.csv", year))
out_dir <- ifelse(!is.null(arg_list$out_dir), as.character(arg_list$out_dir), file.path("Embeddings", "data", "cbps_integration", year, "placebo"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(emb_file)) {
  stop("embeddings file not found: ", emb_file)
}
source("balancing/cbps_ATT.R")
source("balancing/cbps_lambda_utils.R")
cbps_att <- get("cbps_att", mode = "function")
compute_weights_metrics <- get("compute_weights_metrics", mode = "function")

# Read minimal columns: unit, LATITUDE, LONGITUDE, treated, band_*
hdr <- names(fread(emb_file, nrows = 0))
cols_needed <- intersect(c("unit", "LATITUDE", "LONGITUDE", "treated", grep("^band_", hdr, value = TRUE)), hdr)
DT <- fread(emb_file, select = cols_needed)

band_cols <- grep("^band_", names(DT), value = TRUE)
if (length(band_cols) == 0) {
  stop("no band cols present")
}

# dry run mode: skip heavy CBPS/ATT computation and run basic checks only
dry <- FALSE
if (!is.null(arg_list$dry)) {
  dry <- tolower(arg_list$dry) %in% c("1", "true", "t", "yes")
}
if (dry) {
  cat("Dry run mode enabled: performing lightweight checks and exiting.\n")
  DT_complete <- DT[!is.na(DT$treated)]
  W_obs <- as.integer(DT_complete$treated)
  n <- nrow(DT_complete)
  n1 <- sum(W_obs == 1)
  summary_checks <- data.frame(
    year = year,
    rows = nrow(DT),
    rows_with_treated = n,
    n_treated = n1,
    n_controls = n - n1,
    n_band_cols = length(band_cols),
    stringsAsFactors = FALSE
  )
  write.csv(summary_checks, file = file.path(out_dir, sprintf("placebo_dry_checks_%s.csv", year)), row.names = FALSE)
  cat("Wrote dry-run summary to", file.path(out_dir, sprintf("placebo_dry_checks_%s.csv", year)), "\n")
  quit(save = "no")
}

get_col <- function(dt, name) {
  if (name %in% names(dt)) return(dt[[name]])
  rep(NA, nrow(dt))
}

build_unit_key <- function(lat, lon, digits = 6L) {
  sprintf(paste0("%.", as.integer(digits), "f|%.", as.integer(digits), "f"), as.numeric(lat), as.numeric(lon))
}

as_firms_df <- function(firms_base) {
  if (inherits(firms_base, "sf")) {
    if (requireNamespace("sf", quietly = TRUE)) {
      coords_try <- try(sf::st_coordinates(firms_base), silent = TRUE)
      if (!inherits(coords_try, "try-error") && is.matrix(coords_try) && ncol(coords_try) >= 2) {
        if (!("LONGITUDE" %in% colnames(firms_base))) firms_base$LONGITUDE <- coords_try[, 1]
        if (!("LATITUDE" %in% colnames(firms_base))) firms_base$LATITUDE <- coords_try[, 2]
      }
      firms_base <- sf::st_drop_geometry(firms_base)
    } else {
      firms_base <- as.data.frame(firms_base)
    }
  }
  is_sfc_like <- sapply(
    firms_base,
    function(col) inherits(col, "sfc") || any(grepl("sfc", class(col), fixed = TRUE)) || is.list(col)
  )
  if (any(is_sfc_like)) {
    firms_base <- firms_base[, !is_sfc_like, drop = FALSE]
  }
  firms_base
}

prepare_unit_year_fire_panel <- function(all_units, all_years, firms_rds_path = "data/processed_data/FIRMS.RDS") {
  if (!file.exists(firms_rds_path)) {
    stop("FIRMS data not found: ", firms_rds_path)
  }
  firms_base <- readRDS(firms_rds_path)
  firms_base <- as_firms_df(firms_base)

  if (!("LATITUDE" %in% names(firms_base)) || !("LONGITUDE" %in% names(firms_base)) || !("year" %in% names(firms_base))) {
    stop("FIRMS data is missing LATITUDE/LONGITUDE/year columns")
  }

  firms_base$unit <- build_unit_key(firms_base$LATITUDE, firms_base$LONGITUDE)
  firms_base <- firms_base[firms_base$unit %in% all_units & firms_base$year %in% all_years, , drop = FALSE]

  if (!("max_FRP" %in% names(firms_base))) {
    firms_base$max_FRP <- NA_real_
  }

  if (nrow(firms_base) > 0) {
    fire_unit_year <- firms_base %>%
      dplyr::group_by(unit, year) %>%
      dplyr::summarise(
        has.fire = 1L,
        has.hifire95 = as.integer(any(!is.na(max_FRP) & max_FRP >= 1000)),
        .groups = "drop"
      )
  } else {
    fire_unit_year <- data.frame(unit = character(), year = integer(), has.fire = integer(), has.hifire95 = integer())
  }

  panel <- expand.grid(unit = all_units, year = all_years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  panel <- panel %>% dplyr::left_join(fire_unit_year, by = c("unit", "year"))
  panel$has.fire[is.na(panel$has.fire)] <- 0L
  panel$has.hifire95[is.na(panel$has.hifire95)] <- 0L

  panel
}

calc_weighted_fire_from_panel <- function(panel, treated_vec, weight_vec, post_years, pre_years) {
  unit_map <- data.frame(unit = names(treated_vec), treated = as.integer(treated_vec), weight = as.numeric(weight_vec), stringsAsFactors = FALSE)
  df <- panel %>% dplyr::left_join(unit_map, by = "unit")

  stats <- df %>%
    dplyr::group_by(year, treated) %>%
    dplyr::summarise(
      sum.hifire95 = sum(has.hifire95 * weight, na.rm = TRUE),
      denom = sum(weight, na.rm = TRUE),
      hifire95.frac = ifelse(denom == 0, NA_real_, sum.hifire95 / denom),
      .groups = "drop"
    )

  fw <- data.table::dcast(as.data.table(stats), year ~ treated, value.var = "hifire95.frac")
  if (!("0" %in% colnames(fw)) || !("1" %in% colnames(fw))) {
    return(list(att_pre = NA_real_, att_post = NA_real_, pre_rmspe = NA_real_, post_rmspe = NA_real_, post_pre_ratio = NA_real_, gap_pre_mean = NA_real_, gap_post_mean = NA_real_))
  }

  fw$gap <- fw[["1"]] - fw[["0"]]
  gap_post <- fw$gap[fw$year %in% post_years]
  gap_pre <- fw$gap[fw$year %in% pre_years]

  pre_rmspe <- if (length(gap_pre) > 0) sqrt(mean(gap_pre^2, na.rm = TRUE)) else NA_real_
  post_rmspe <- if (length(gap_post) > 0) sqrt(mean(gap_post^2, na.rm = TRUE)) else NA_real_
  ratio <- ifelse(is.na(pre_rmspe) || pre_rmspe <= 0, NA_real_, post_rmspe / pre_rmspe)

  list(
    att_pre = if (length(gap_pre) > 0) mean(gap_pre, na.rm = TRUE) else NA_real_,
    att_post = if (length(gap_post) > 0) mean(gap_post, na.rm = TRUE) else NA_real_,
    pre_rmspe = pre_rmspe,
    post_rmspe = post_rmspe,
    post_pre_ratio = ratio,
    gap_pre_mean = if (length(gap_pre) > 0) mean(gap_pre, na.rm = TRUE) else NA_real_,
    gap_post_mean = if (length(gap_post) > 0) mean(gap_post, na.rm = TRUE) else NA_real_
  )
}

# prepare X and indices
DT_complete <- DT[!is.na(DT$treated)]
W_obs <- as.integer(DT_complete$treated)
X_all <- as.matrix(DT_complete[, ..band_cols])

n <- nrow(X_all)
n1 <- sum(W_obs == 1)
if (n1 == 0) {
  stop("No treated units found for year ", year)
}
controls_idx <- which(W_obs == 0)
if (length(controls_idx) < n1) {
  stop("Insufficient controls for placebo assignment: controls=", length(controls_idx), " treated=", n1)
}

# Build cached fire panel once (for speed at large B)
all_units <- as.character(get_col(DT_complete, "unit"))
if (all(is.na(all_units))) {
  if (("LATITUDE" %in% names(DT_complete)) && ("LONGITUDE" %in% names(DT_complete))) {
    all_units <- build_unit_key(DT_complete$LATITUDE, DT_complete$LONGITUDE)
  } else {
    stop("Need unit or LATITUDE/LONGITUDE to build unit IDs")
  }
}
if (any(is.na(all_units) | !nzchar(all_units))) {
  stop("Invalid unit identifiers after normalization")
}
if (anyDuplicated(all_units) > 0) {
  stop("Duplicate unit identifiers found in embedding cohort")
}

all_years <- sort(unique(c(2000:as.integer(max(post_years)), post_years)))
if (!is.null(arg_list$all_years)) {
  all_years <- as.integer(strsplit(arg_list$all_years, ",", fixed = TRUE)[[1]])
}
pre_years <- all_years[all_years < min(post_years)]
if (!is.null(arg_list$pre_years)) {
  pre_years <- as.integer(strsplit(arg_list$pre_years, ",", fixed = TRUE)[[1]])
}
if (length(pre_years) == 0) {
  stop("No pre_years available. Provide pre_years=YYYY,YYYY,...")
}
if (any(pre_years >= as.integer(year))) {
  stop("pre_years must be strictly before the treated year")
}
cat("Using pre years:", paste(pre_years, collapse = ","), "\n")
cat("Using post years:", paste(post_years, collapse = ","), "\n")

cat("Preparing cached unit-year fire panel...\n")
fire_panel <- prepare_unit_year_fire_panel(all_units = all_units, all_years = all_years)

assignment_mode <- ifelse(!is.null(arg_list$assignment_mode), as.character(arg_list$assignment_mode), "control_only")
allowed_assignment_modes <- c("control_only", "full_sample_randomization", "donor_unit_placebo")
if (!(assignment_mode %in% allowed_assignment_modes)) {
  stop("assignment_mode must be one of: ", paste(allowed_assignment_modes, collapse = ", "))
}
allow_full_sample_randomization <- ifelse(!is.null(arg_list$allow_full_sample_randomization), tolower(as.character(arg_list$allow_full_sample_randomization)) %in% c("1", "true", "t", "yes"), FALSE)
if (assignment_mode == "full_sample_randomization" && !isTRUE(allow_full_sample_randomization)) {
  stop("assignment_mode=full_sample_randomization is disabled by default; set allow_full_sample_randomization=true to override")
}

n_workers <- ifelse(!is.null(arg_list$n_workers), as.integer(arg_list$n_workers), 1L)
n_workers <- max(1L, n_workers)
seed_base <- ifelse(!is.null(arg_list$seed_base), as.integer(arg_list$seed_base), 1L)
checkpoint_every <- ifelse(!is.null(arg_list$checkpoint_every), as.integer(arg_list$checkpoint_every), 100L)
checkpoint_every <- max(1L, checkpoint_every)
resume <- ifelse(!is.null(arg_list$resume), tolower(as.character(arg_list$resume)) %in% c("1", "true", "t", "yes"), TRUE)

gate_max_abs_smd <- ifelse(!is.null(arg_list$gate_max_abs_smd), as.numeric(arg_list$gate_max_abs_smd), 0.10)
gate_median_abs_smd <- ifelse(!is.null(arg_list$gate_median_abs_smd), as.numeric(arg_list$gate_median_abs_smd), 0.05)
gate_ess_frac <- ifelse(!is.null(arg_list$gate_ess_frac), as.numeric(arg_list$gate_ess_frac), 0.20)
gate_ess_mult_treated <- ifelse(!is.null(arg_list$gate_ess_mult_treated), as.numeric(arg_list$gate_ess_mult_treated), 2.0)
gate_max_weight_share <- ifelse(!is.null(arg_list$gate_max_weight_share), as.numeric(arg_list$gate_max_weight_share), 0.10)
gate_top10_share <- ifelse(!is.null(arg_list$gate_top10_share), as.numeric(arg_list$gate_top10_share), 0.60)
gate_prefit_mult <- ifelse(!is.null(arg_list$gate_prefit_mult), as.numeric(arg_list$gate_prefit_mult), 5.0)
gate_ratio_max <- ifelse(!is.null(arg_list$gate_ratio_max), as.numeric(arg_list$gate_ratio_max), 20.0)
enforce_ratio_gate <- ifelse(!is.null(arg_list$enforce_ratio_gate), tolower(as.character(arg_list$enforce_ratio_gate)) %in% c("1", "true", "t", "yes"), FALSE)

donor_placebo_size <- ifelse(!is.null(arg_list$donor_placebo_size), as.integer(arg_list$donor_placebo_size), 1L)
donor_placebo_size <- max(1L, donor_placebo_size)
min_valid_draws <- ifelse(!is.null(arg_list$min_valid_draws), as.integer(arg_list$min_valid_draws), max(50L, as.integer(0.10 * B)))
min_valid_draws <- max(1L, min_valid_draws)

checkpoint_rds <- file.path(out_dir, sprintf("placebo_checkpoint_%s.rds", year))
checkpoint_draws_csv <- file.path(out_dir, sprintf("placebo_draws_checkpoint_%s.csv", year))

obs_att <- NA_real_
obs_att_pre <- NA_real_
obs_pre_rmspe <- NA_real_
obs_post_rmspe <- NA_real_
obs_post_pre_ratio <- NA_real_

# compute observed ATT using cbps_att (treatments as in data)
cat("Computing observed CBPS weights (this may take a moment)...\n")
res_obs <- cbps_att(X_all, W_obs, intercept = TRUE, control = list(maxit = 2000))
weights_vec <- ifelse(W_obs == 1, 1.0, res_obs$weights.0)

obs_map_treated <- setNames(W_obs, all_units)
obs_map_weights <- setNames(weights_vec, all_units)
obs_stats <- calc_weighted_fire_from_panel(
  panel = fire_panel,
  treated_vec = obs_map_treated,
  weight_vec = obs_map_weights,
  post_years = post_years,
  pre_years = pre_years
)
obs_att <- obs_stats$att_post
obs_att_pre <- obs_stats$att_pre
obs_pre_rmspe <- obs_stats$pre_rmspe
obs_post_rmspe <- obs_stats$post_rmspe
obs_post_pre_ratio <- obs_stats$post_pre_ratio

cat("Observed ATT post (mean hifire95 gap):", obs_att, "\n")
cat("Observed ATT pre (mean hifire95 gap):", obs_att_pre, "\n")
cat("Observed pre RMSPE:", obs_pre_rmspe, " post RMSPE:", obs_post_rmspe, " ratio:", obs_post_pre_ratio, "\n")

init_draws_df <- function(B, year, assignment_mode) {
  data.frame(
    year = rep(as.integer(year), B),
    draw_id = seq_len(B),
    assignment_mode = rep(as.character(assignment_mode), B),
    pseudo_treated_n = rep(NA_integer_, B),
    completed = rep(FALSE, B),
    valid = rep(FALSE, B),
    reject_reason = rep(NA_character_, B),
    gate_balance = rep(NA, B),
    gate_weight = rep(NA, B),
    gate_concentration = rep(NA, B),
    gate_prefit = rep(NA, B),
    gate_ratio = rep(NA, B),
    gate_att_nonmissing = rep(NA, B),
    n_control_placebo = rep(NA_integer_, B),
    n_treated_placebo = rep(NA_integer_, B),
    ess_floor_required = rep(NA_real_, B),
    ess_control = rep(NA_real_, B),
    max_weight_share = rep(NA_real_, B),
    top10_share = rep(NA_real_, B),
    max_abs_smd = rep(NA_real_, B),
    median_abs_smd = rep(NA_real_, B),
    placebo_att_post = rep(NA_real_, B),
    placebo_att = rep(NA_real_, B),
    placebo_att_pre = rep(NA_real_, B),
    placebo_pre_rmspe = rep(NA_real_, B),
    placebo_post_rmspe = rep(NA_real_, B),
    placebo_post_pre_rmspe_ratio = rep(NA_real_, B),
    obs_att_post = rep(NA_real_, B),
    obs_att = rep(NA_real_, B),
    obs_att_pre = rep(NA_real_, B),
    obs_pre_rmspe = rep(NA_real_, B),
    obs_post_rmspe = rep(NA_real_, B),
    obs_post_pre_rmspe_ratio = rep(NA_real_, B),
    abs_ge_obs = rep(NA, B),
    stringsAsFactors = FALSE
  )
}

stamp_observed_cols <- function(df) {
  df$obs_att_post <- obs_att
  df$obs_att <- obs_att
  df$obs_att_pre <- obs_att_pre
  df$obs_pre_rmspe <- obs_pre_rmspe
  df$obs_post_rmspe <- obs_post_rmspe
  df$obs_post_pre_rmspe_ratio <- obs_post_pre_ratio
  df
}

save_checkpoint <- function(df) {
  write.csv(df, checkpoint_draws_csv, row.names = FALSE)
  saveRDS(
    list(
      draws_df = df,
      year = as.integer(year),
      B = as.integer(B),
      assignment_mode = as.character(assignment_mode),
      checkpoint_at = as.character(Sys.time())
    ),
    file = checkpoint_rds
  )
}

if (isTRUE(resume) && file.exists(checkpoint_rds)) {
  state <- tryCatch(readRDS(checkpoint_rds), error = function(e) NULL)
  if (!is.null(state) && !is.null(state$draws_df) && nrow(state$draws_df) == B) {
    draws_df <- state$draws_df
    cat("Resuming from checkpoint:", checkpoint_rds, "\n")
  } else {
    draws_df <- init_draws_df(B = B, year = year, assignment_mode = assignment_mode)
  }
} else {
  draws_df <- init_draws_df(B = B, year = year, assignment_mode = assignment_mode)
}
draws_df <- stamp_observed_cols(draws_df)

sample_placebo_assignment <- function(draw_id) {
  set.seed(seed_base + as.integer(draw_id))

  if (assignment_mode == "control_only") {
    idx <- sample(controls_idx, n1)
  } else if (assignment_mode == "full_sample_randomization") {
    idx <- sample(seq_len(n), n1)
  } else if (assignment_mode == "donor_unit_placebo") {
    k_placebo <- min(length(controls_idx), donor_placebo_size)
    idx <- sample(controls_idx, k_placebo)
  } else {
    stop("Unsupported assignment mode")
  }

  Wb <- integer(n)
  Wb[idx] <- 1L
  Wb
}

evaluate_draw <- function(draw_id) {
  Wb <- sample_placebo_assignment(draw_id)
  n_treated_b <- sum(Wb == 1)
  n_control_b <- sum(Wb == 0)

  res_b <- tryCatch(
    cbps_att(X_all, Wb, intercept = TRUE, control = list(maxit = 1500)),
    error = function(e) NULL
  )
  if (is.null(res_b)) {
    return(list(
      draw_id = draw_id,
      completed = TRUE,
      valid = FALSE,
      reject_reason = "cbps_error",
      n_treated_placebo = as.integer(n_treated_b),
      n_control_placebo = as.integer(n_control_b)
    ))
  }

  metrics <- compute_weights_metrics(res_b, Wb)
  if (is.null(metrics)) {
    return(list(
      draw_id = draw_id,
      completed = TRUE,
      valid = FALSE,
      reject_reason = "metrics_error",
      n_treated_placebo = as.integer(n_treated_b),
      n_control_placebo = as.integer(n_control_b)
    ))
  }

  ess_floor <- max(gate_ess_frac * n_control_b, gate_ess_mult_treated * n_treated_b)
  gate_balance <- is.finite(metrics$max_smd) && is.finite(metrics$median_smd) &&
    metrics$max_smd <= gate_max_abs_smd && metrics$median_smd <= gate_median_abs_smd
  gate_weight <- is.finite(metrics$ess) && metrics$ess >= ess_floor
  gate_concentration <- is.finite(metrics$max_weight) && is.finite(metrics$top10_share) &&
    metrics$max_weight <= gate_max_weight_share && metrics$top10_share <= gate_top10_share

  if (!gate_balance || !gate_weight || !gate_concentration) {
    reason <- if (!gate_balance) {
      if (!is.finite(metrics$max_smd) || metrics$max_smd > gate_max_abs_smd) "balance_max_smd" else "balance_median_smd"
    } else if (!gate_weight) {
      "weight_ess"
    } else {
      if (!is.finite(metrics$max_weight) || metrics$max_weight > gate_max_weight_share) "concentration_max_weight" else "concentration_top10"
    }

    return(list(
      draw_id = draw_id,
      completed = TRUE,
      valid = FALSE,
      reject_reason = reason,
      gate_balance = gate_balance,
      gate_weight = gate_weight,
      gate_concentration = gate_concentration,
      gate_prefit = NA,
      gate_ratio = NA,
      gate_att_nonmissing = NA,
      ess_control = as.numeric(metrics$ess),
      max_weight_share = as.numeric(metrics$max_weight),
      top10_share = as.numeric(metrics$top10_share),
      max_abs_smd = as.numeric(metrics$max_smd),
      median_abs_smd = as.numeric(metrics$median_smd),
      ess_floor_required = as.numeric(ess_floor),
      n_treated_placebo = as.integer(n_treated_b),
      n_control_placebo = as.integer(n_control_b),
      pseudo_treated_n = as.integer(n_treated_b)
    ))
  }

  weights_b <- ifelse(Wb == 1, 1.0, res_b$weights.0)
  map_treated <- setNames(Wb, all_units)
  map_weights <- setNames(weights_b, all_units)

  st <- calc_weighted_fire_from_panel(
    panel = fire_panel,
    treated_vec = map_treated,
    weight_vec = map_weights,
    post_years = post_years,
    pre_years = pre_years
  )

  gate_prefit <- TRUE
  if (is.finite(obs_pre_rmspe) && obs_pre_rmspe > 0) {
    gate_prefit <- is.finite(st$pre_rmspe) && (st$pre_rmspe <= gate_prefit_mult * obs_pre_rmspe)
  }
  gate_ratio <- if (isTRUE(enforce_ratio_gate)) {
    is.finite(st$post_pre_ratio) && (st$post_pre_ratio <= gate_ratio_max)
  } else {
    TRUE
  }
  gate_att_nonmissing <- !is.na(st$att_post)

  valid <- gate_balance && gate_weight && gate_concentration && gate_prefit && gate_ratio && gate_att_nonmissing
  reject_reason <- if (valid) {
    "pass"
  } else if (!gate_prefit) {
    "prefit_rmspe"
  } else if (!gate_ratio) {
    "post_pre_ratio"
  } else if (!gate_att_nonmissing) {
    "att_missing"
  } else {
    "unknown"
  }

  list(
    draw_id = draw_id,
    completed = TRUE,
    valid = valid,
    reject_reason = reject_reason,
    gate_balance = gate_balance,
    gate_weight = gate_weight,
    gate_concentration = gate_concentration,
    gate_prefit = gate_prefit,
    gate_ratio = gate_ratio,
    gate_att_nonmissing = gate_att_nonmissing,
    ess_control = as.numeric(metrics$ess),
    max_weight_share = as.numeric(metrics$max_weight),
    top10_share = as.numeric(metrics$top10_share),
    max_abs_smd = as.numeric(metrics$max_smd),
    median_abs_smd = as.numeric(metrics$median_smd),
    ess_floor_required = as.numeric(ess_floor),
    n_treated_placebo = as.integer(n_treated_b),
    n_control_placebo = as.integer(n_control_b),
    pseudo_treated_n = as.integer(n_treated_b),
    placebo_att_post = st$att_post,
    placebo_att = st$att_post,
    placebo_att_pre = st$att_pre,
    placebo_pre_rmspe = st$pre_rmspe,
    placebo_post_rmspe = st$post_rmspe,
    placebo_post_pre_rmspe_ratio = st$post_pre_ratio
  )
}

apply_draw_result <- function(df, res) {
  i <- as.integer(res$draw_id)
  for (nm in names(res)) {
    if (nm == "draw_id") next
    if (nm %in% names(df)) {
      df[i, nm] <- res[[nm]]
    }
  }
  df
}

pending_ids <- draws_df$draw_id[!draws_df$completed]
cat("Running", length(pending_ids), "pending placebo draws with mode=", assignment_mode,
    " workers=", n_workers, " checkpoint_every=", checkpoint_every, "\n", sep = "")

if (length(pending_ids) > 0) {
  chunk_starts <- seq(1L, length(pending_ids), by = checkpoint_every)
  for (chunk_idx in seq_along(chunk_starts)) {
    s <- chunk_starts[chunk_idx]
    e <- min(length(pending_ids), s + checkpoint_every - 1L)
    ids <- pending_ids[s:e]

    if (n_workers > 1L && length(ids) > 1L) {
      res_list <- parallel::mclapply(ids, evaluate_draw, mc.cores = n_workers)
    } else {
      res_list <- lapply(ids, evaluate_draw)
    }

    for (res in res_list) {
      draws_df <- apply_draw_result(draws_df, res)
    }
    draws_df <- stamp_observed_cols(draws_df)
    save_checkpoint(draws_df)

    cat(
      "Completed chunk ", chunk_idx, "/", length(chunk_starts),
      " (draws ", s, "-", e, "). Total completed=", sum(draws_df$completed),
      " valid=", sum(draws_df$valid, na.rm = TRUE), "\n",
      sep = ""
    )
  }
}

# summarize and plot
valid <- draws_df$completed & draws_df$valid & !is.na(draws_df$placebo_att_post)
if (sum(valid, na.rm = TRUE) < min_valid_draws) {
  stop(sprintf("Insufficient valid placebo draws: %d < min_valid_draws=%d", sum(valid, na.rm = TRUE), min_valid_draws))
}
placebo_vals <- draws_df$placebo_att_post[valid]
placebo_vals_abs <- abs(placebo_vals)
pval_rank <- if (length(placebo_vals) > 0 && !is.na(obs_att)) mean(placebo_vals_abs >= abs(obs_att), na.rm = TRUE) else NA_real_

median_abs_placebo <- if (length(placebo_vals_abs) > 0) median(placebo_vals_abs, na.rm = TRUE) else NA_real_
att_over_median_abs_placebo <- ifelse(is.na(median_abs_placebo) || median_abs_placebo == 0 || is.na(obs_att), NA_real_, obs_att / median_abs_placebo)

cat("Placebo draws (valid):", sum(valid), "rank p-value:", pval_rank, "\n")
cat("Observed ATT / median(|placebo ATT|):", att_over_median_abs_placebo, "\n")

# draw-level CSV for post-processing and external plotting
draws_df$valid <- valid
draws_df$abs_ge_obs <- ifelse(is.na(draws_df$placebo_att_post) | is.na(obs_att), NA, abs(draws_df$placebo_att_post) >= abs(obs_att))
write.csv(draws_df, file = file.path(out_dir, sprintf("placebo_draws_%s.csv", year)), row.names = FALSE)

reason_tab <- as.data.frame(table(ifelse(is.na(draws_df$reject_reason), "not_completed", draws_df$reject_reason)), stringsAsFactors = FALSE)
names(reason_tab) <- c("reject_reason", "n_draws")
reason_tab$share <- reason_tab$n_draws / max(1, nrow(draws_df))
write.csv(reason_tab, file = file.path(out_dir, sprintf("placebo_rejection_reasons_%s.csv", year)), row.names = FALSE)

accepted <- draws_df[draws_df$valid, , drop = FALSE]
diagnostic_summary <- data.frame(
  metric = c("ess_control", "max_weight_share", "top10_share", "max_abs_smd", "median_abs_smd", "placebo_pre_rmspe", "placebo_post_pre_rmspe_ratio"),
  mean = c(
    mean(accepted$ess_control, na.rm = TRUE),
    mean(accepted$max_weight_share, na.rm = TRUE),
    mean(accepted$top10_share, na.rm = TRUE),
    mean(accepted$max_abs_smd, na.rm = TRUE),
    mean(accepted$median_abs_smd, na.rm = TRUE),
    mean(accepted$placebo_pre_rmspe, na.rm = TRUE),
    mean(accepted$placebo_post_pre_rmspe_ratio, na.rm = TRUE)
  ),
  median = c(
    median(accepted$ess_control, na.rm = TRUE),
    median(accepted$max_weight_share, na.rm = TRUE),
    median(accepted$top10_share, na.rm = TRUE),
    median(accepted$max_abs_smd, na.rm = TRUE),
    median(accepted$median_abs_smd, na.rm = TRUE),
    median(accepted$placebo_pre_rmspe, na.rm = TRUE),
    median(accepted$placebo_post_pre_rmspe_ratio, na.rm = TRUE)
  ),
  p90 = c(
    as.numeric(stats::quantile(accepted$ess_control, probs = 0.90, na.rm = TRUE, names = FALSE)),
    as.numeric(stats::quantile(accepted$max_weight_share, probs = 0.90, na.rm = TRUE, names = FALSE)),
    as.numeric(stats::quantile(accepted$top10_share, probs = 0.90, na.rm = TRUE, names = FALSE)),
    as.numeric(stats::quantile(accepted$max_abs_smd, probs = 0.90, na.rm = TRUE, names = FALSE)),
    as.numeric(stats::quantile(accepted$median_abs_smd, probs = 0.90, na.rm = TRUE, names = FALSE)),
    as.numeric(stats::quantile(accepted$placebo_pre_rmspe, probs = 0.90, na.rm = TRUE, names = FALSE)),
    as.numeric(stats::quantile(accepted$placebo_post_pre_rmspe_ratio, probs = 0.90, na.rm = TRUE, names = FALSE))
  ),
  stringsAsFactors = FALSE
)
write.csv(diagnostic_summary, file = file.path(out_dir, sprintf("placebo_accepted_diagnostics_%s.csv", year)), row.names = FALSE)

# numeric summary and save CSV
summary_df <- data.frame(
  year = year,
  B = B,
  n_completed = sum(draws_df$completed),
  n_valid = sum(valid),
  valid_share = sum(valid) / max(1, B),
  assignment_mode = assignment_mode,
  n_workers = as.integer(n_workers),
  seed_base = as.integer(seed_base),
  gate_max_abs_smd = gate_max_abs_smd,
  gate_median_abs_smd = gate_median_abs_smd,
  gate_ess_frac = gate_ess_frac,
  gate_ess_mult_treated = gate_ess_mult_treated,
  gate_max_weight_share = gate_max_weight_share,
  gate_top10_share = gate_top10_share,
  gate_prefit_mult = gate_prefit_mult,
  enforce_ratio_gate = enforce_ratio_gate,
  gate_ratio_max = if (isTRUE(enforce_ratio_gate)) gate_ratio_max else NA_real_,
  gate_balance_pass_rate = mean(draws_df$gate_balance[draws_df$completed], na.rm = TRUE),
  gate_weight_pass_rate = mean(draws_df$gate_weight[draws_df$completed], na.rm = TRUE),
  gate_concentration_pass_rate = mean(draws_df$gate_concentration[draws_df$completed], na.rm = TRUE),
  gate_prefit_pass_rate = mean(draws_df$gate_prefit[draws_df$completed], na.rm = TRUE),
  gate_ratio_pass_rate = mean(draws_df$gate_ratio[draws_df$completed], na.rm = TRUE),
  obs_att_post = obs_att,
  obs_att = obs_att,
  obs_att_pre = obs_att_pre,
  obs_pre_rmspe = obs_pre_rmspe,
  obs_post_rmspe = obs_post_rmspe,
  obs_post_pre_rmspe_ratio = obs_post_pre_ratio,
  placebo_mean_post = ifelse(length(placebo_vals) > 0, mean(placebo_vals, na.rm = TRUE), NA),
  placebo_median_abs_post = median_abs_placebo,
  att_over_median_abs_placebo = att_over_median_abs_placebo,
  placebo_sd_post = ifelse(length(placebo_vals) > 0, sd(placebo_vals, na.rm = TRUE), NA),
  placebo_median_post = ifelse(length(placebo_vals) > 0, median(placebo_vals, na.rm = TRUE), NA),
  placebo_iqr_post = ifelse(length(placebo_vals) > 0, IQR(placebo_vals, na.rm = TRUE), NA),
  placebo_mean_pre_att = ifelse(any(!is.na(accepted$placebo_att_pre)), mean(accepted$placebo_att_pre, na.rm = TRUE), NA),
  placebo_median_pre_rmspe = ifelse(any(!is.na(accepted$placebo_pre_rmspe)), median(accepted$placebo_pre_rmspe, na.rm = TRUE), NA),
  placebo_median_post_pre_rmspe_ratio = ifelse(any(!is.na(accepted$placebo_post_pre_rmspe_ratio)), median(accepted$placebo_post_pre_rmspe_ratio, na.rm = TRUE), NA),
  pval_rank = pval_rank,
  interpretation = ifelse(!is.na(pval_rank), "Is observed ATT extreme under random assignment?", NA),
  stringsAsFactors = FALSE
)
write.csv(summary_df, file = file.path(out_dir, sprintf("placebo_summary_%s.csv", year)), row.names = FALSE)

# ggplot ECDF
if (length(placebo_vals) > 0 && !is.na(obs_att)) {
  dfp <- data.frame(ATT = placebo_vals)
  p_ecdf <- ggplot(dfp, aes(x = ATT)) +
    stat_ecdf(geom = "step") +
    geom_vline(xintercept = obs_att, color = "red", size = 0.8) +
    annotate("text", x = obs_att, y = 0.5, label = paste0("obs=", round(obs_att, 4)), color = "red", hjust = -0.1) +
    labs(
      title = sprintf("Placebo ECDF (%s) — valid=%d, p=%.3f", year, sum(valid), pval_rank),
      x = "ATT",
      y = "ECDF"
    ) +
    theme_minimal()
  ggsave(filename = file.path(out_dir, sprintf("placebo_ecdf_gg_%s.png", year)), plot = p_ecdf, width = 7, height = 5, dpi = 200)

  att_range <- range(dfp$ATT, na.rm = TRUE)
  span <- ifelse(all(is.finite(att_range)), att_range[2] - att_range[1], NA_real_)
  binwidth_scale <- ifelse(is.finite(span) && span > 0, span / 30, 1)
  p_hist <- ggplot(dfp, aes(x = ATT)) +
    geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "grey70", color = "black", alpha = 0.9) +
    geom_density(aes(y = after_stat(count) * binwidth_scale), color = "blue", linewidth = 0.8, adjust = 1) +
    geom_vline(xintercept = obs_att, color = "red", size = 0.8) +
    labs(
      title = sprintf("Placebo ATT histogram + density (%s) — valid=%d, p=%.3f", year, sum(valid), pval_rank),
      x = "ATT",
      y = "Count"
    ) +
    theme_minimal()
  ggsave(filename = file.path(out_dir, sprintf("placebo_hist_density_%s.png", year)), plot = p_hist, width = 7, height = 5, dpi = 200)

  p_box <- ggplot(dfp, aes(x = factor(1), y = ATT)) +
    geom_boxplot(width = 0.2, outlier.shape = NA) +
    geom_jitter(width = 0.15, height = 0, alpha = 0.7) +
    geom_point(aes(x = 1, y = obs_att), color = "red", size = 3) +
    labs(
      title = sprintf("Placebo ATT boxplot (%s) — valid=%d, p=%.3f", year, sum(valid), pval_rank),
      x = "",
      y = "ATT"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  ggsave(filename = file.path(out_dir, sprintf("placebo_box_%s.png", year)), plot = p_box, width = 4, height = 5, dpi = 200)
}

# save results (RDS)
saveRDS(
  list(
    placebo_atts_post = draws_df$placebo_att_post,
    placebo_atts_pre = draws_df$placebo_att_pre,
    placebo_pre_rmspe = draws_df$placebo_pre_rmspe,
    placebo_post_rmspe = draws_df$placebo_post_rmspe,
    placebo_post_pre_ratio = draws_df$placebo_post_pre_rmspe_ratio,
    obs_att_post = obs_att,
    obs_att_pre = obs_att_pre,
    obs_pre_rmspe = obs_pre_rmspe,
    obs_post_rmspe = obs_post_rmspe,
    obs_post_pre_ratio = obs_post_pre_ratio,
    att_over_median_abs_placebo = att_over_median_abs_placebo,
    pval_rank = pval_rank,
    rejection_reasons = reason_tab,
    accepted_diagnostics = diagnostic_summary,
    draws = draws_df,
    summary = summary_df
  ),
  file = file.path(out_dir, sprintf("placebo_results_%s.rds", year))
)
cat("Saved placebo plots and caches to", out_dir, "\n")
