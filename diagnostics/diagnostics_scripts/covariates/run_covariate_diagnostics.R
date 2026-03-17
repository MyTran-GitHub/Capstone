#!/usr/bin/env Rscript

# Central CLI entrypoint for covariate diagnostics.
# Runs diagnostics from existing prepared data + fitted CBPS results,
# and does not re-run the CBPS optimization.

source("balancing/balancing_config.R")
source("balancing/prepare_cbps_design.R")
source("diagnostics/diagnostics_scripts/covariates/run_covariate_exploration.R")

parse_flag_value <- function(args, flag, default = NULL) {
  flag_eq <- paste0(flag, "=")
  hit_eq <- args[startsWith(args, flag_eq)]
  if (length(hit_eq) > 0) return(sub(flag_eq, "", hit_eq[1], fixed = TRUE))
  idx <- which(args == flag)
  if (length(idx) > 0 && idx[1] < length(args)) return(args[idx[1] + 1])
  default
}

parse_bool <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  lx <- tolower(as.character(x)[1])
  if (lx %in% c("1", "true", "t", "yes", "y")) return(TRUE)
  if (lx %in% c("0", "false", "f", "no", "n")) return(FALSE)
  default
}

parse_numeric <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  v <- suppressWarnings(as.numeric(as.character(x)[1]))
  if (is.na(v)) default else v
}

parse_string <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  as.character(x)[1]
}

pick_latest_rho_fit <- function(year, area, base_dir = "data/processed_data/rev_analysis_low") {
  rho_pattern <- sprintf("^cbps_fit_%d_%s_rho.*\\.RDS$", year, area)
  rho_hits <- list.files(base_dir, pattern = rho_pattern, full.names = TRUE)
  if (length(rho_hits) == 0) return(NULL)
  rho_hits[which.max(file.info(rho_hits)$mtime)]
}

args <- commandArgs(trailingOnly = TRUE)

if ("--refit" %in% args || "--run-cbps" %in% args) {
  stop("No-refit contract violation: this script only computes diagnostics from existing fit artifacts.")
}

year <- as.integer(parse_flag_value(args, "--year", NA))
area <- parse_flag_value(args, "--area", "conifer")
data_file <- parse_flag_value(
  args,
  "--data-file",
  if (!is.na(year)) sprintf("data/processed_data/rev_analysis_low/analysis_treated%d_%s.RDS", year, area) else NULL
)
fit_file <- parse_flag_value(
  args,
  "--fit-file",
  if (!is.na(year)) {
    # Prefer area-specific fit if available; keep compatibility with legacy conifer naming.
    latest_fit <- pick_latest_rho_fit(year, area)
    if (!is.null(latest_fit)) latest_fit else sprintf("data/processed_data/rev_analysis_low/cbps_fit_%d_%s.RDS", year, area)
  } else {
    NULL
  }
)
out_dir <- parse_flag_value(args, "--out-dir", "diagnostics/diagnostics_results/covariates")
run_prefit_overlap <- parse_bool(parse_flag_value(args, "--run-prefit-overlap", "false"), FALSE)
write_distribution <- parse_bool(parse_flag_value(args, "--write-distribution", "false"), FALSE)
write_prepost_metrics <- parse_bool(parse_flag_value(args, "--write-prepost-metrics", "false"), FALSE)
write_block_summary <- parse_bool(parse_flag_value(args, "--write-block-summary", "false"), FALSE)
write_summary <- parse_bool(parse_flag_value(args, "--write-summary", "true"), TRUE)
progress_every <- parse_numeric(parse_flag_value(args, "--progress-every", NULL), NULL)
use_design_cache <- parse_bool(parse_flag_value(args, "--use-design-cache", "true"), TRUE)
design_cache_file <- parse_string(parse_flag_value(args, "--design-cache-file", NULL), NULL)
lambda_bundle_file <- parse_string(parse_flag_value(args, "--lambda-bundle-file", NULL), NULL)

if (is.na(year)) {
  stop("Missing required --year argument.")
}
if (is.null(data_file) || !file.exists(data_file)) {
  stop("Data file not found: ", data_file)
}

# Resolve fit file robustly from common patterns if user didn't provide an existing file.
if (is.null(fit_file) || !file.exists(fit_file)) {
  latest_fit <- pick_latest_rho_fit(year, area)
  fit_candidates <- c(
    latest_fit,
    sprintf("data/processed_data/rev_analysis_low/cbps_fit_%d_%s.RDS", year, area),
    pick_latest_rho_fit(year, "conifer"),
    sprintf("data/processed_data/rev_analysis_low/cbps_fit_%d_conifer.RDS", year)
  )
  fit_candidates <- unique(fit_candidates)
  fit_hit <- fit_candidates[file.exists(fit_candidates)]
  if (length(fit_hit) == 0) {
    stop("Could not find a fit RDS. Provide --fit-file explicitly.")
  }
  fit_file <- fit_hit[1]
}

cat("Covariate diagnostics run\n")
cat("  contract  : no-refit (existing fit artifacts only)\n")
cat("  year      : ", year, "\n", sep = "")
cat("  area      : ", area, "\n", sep = "")
cat("  data file : ", data_file, "\n", sep = "")
cat("  fit file  : ", fit_file, "\n", sep = "")
cat("  out dir   : ", out_dir, "\n", sep = "")
cat("  distribution: ", write_distribution, "\n", sep = "")
cat("  prepost metrics: ", write_prepost_metrics, "\n", sep = "")
cat("  block summary: ", write_block_summary, "\n", sep = "")
cat("  summary: ", write_summary, "\n", sep = "")
if (!is.null(progress_every)) {
  cat("  progress-every: ", progress_every, "\n", sep = "")
}
cat("  use-design-cache: ", use_design_cache, "\n", sep = "")
if (!is.null(design_cache_file)) {
  cat("  design-cache-file: ", design_cache_file, "\n", sep = "")
}

df <- readRDS(data_file)
fit <- readRDS(fit_file)
cfg <- get_diagnostics_config()

if (is.null(design_cache_file)) {
  design_cache_file <- sprintf("data/processed_data/rev_analysis_low/design_cache_%d_%s.RDS", year, area)
}
if (is.null(lambda_bundle_file)) {
  lambda_bundle_file <- sprintf("data/processed_data/rev_analysis_low/lambda_diagnostics_bundle_%d_%s.RDS", year, area)
}

prep <- NULL
if (isTRUE(use_design_cache) && file.exists(design_cache_file)) {
  prep <- readRDS(design_cache_file)
  if (!is.list(prep) || is.null(prep$X) || is.null(prep$W)) {
    warning("Invalid design cache; recomputing: ", design_cache_file)
    prep <- NULL
  }
}

if (is.null(prep)) {
  prep <- prepare_cbps_design(df, opts = list(default_winsor_p = cfg$preprocessing$default_winsor_p))
  if (isTRUE(use_design_cache)) {
    saveRDS(list(X = prep$X, W = prep$W), design_cache_file)
  }
}

bundle_obj <- if (file.exists(lambda_bundle_file)) readRDS(lambda_bundle_file) else NULL
cand_df <- NULL
selection_log <- NULL
if (!is.null(bundle_obj) && is.list(bundle_obj)) {
  if (!is.null(bundle_obj$cand_df) && is.data.frame(bundle_obj$cand_df)) cand_df <- bundle_obj$cand_df
  if (!is.null(bundle_obj$selection_log) && is.list(bundle_obj$selection_log)) selection_log <- bundle_obj$selection_log
  cat("  lambda bundle file : ", lambda_bundle_file, "\n", sep = "")
}

run_covariate_exploration(
  treated_year = year,
  area = area,
  X = prep$X,
  W = prep$W,
  res = fit,
  cand_df = cand_df,
  selection_log = selection_log,
  out_dir = out_dir,
  run_prefit_overlap = run_prefit_overlap,
  write_prepost_metrics = write_prepost_metrics,
  write_distribution = write_distribution,
  write_block_summary = write_block_summary,
  write_summary = write_summary,
  progress_every = progress_every
)

cat("Completed centralized covariate diagnostics.\n")
