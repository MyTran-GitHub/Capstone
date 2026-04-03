#!/usr/bin/env Rscript

# Lightweight CLI: lambda-path plot only.
# Reads lambda_run artifact and writes lambda diagnostics figure.
#
# Usage:
#   Rscript diagnostics/diagnostics_scripts/covariates/run_covariate_diagnostics.R \
#     --year 2019 --area conifer

source("balancing/cli_utils.R")
source("diagnostics/diagnostics_scripts/covariates/run_covariate_exploration.R")
parse_flag_value <- get("parse_flag_value", mode = "function")
parse_years_list <- get("parse_years_list", mode = "function")

args <- commandArgs(trailingOnly = TRUE)
year_vals <- parse_years_list(parse_flag_value(args, "--year", ""), "--year")
if (is.null(year_vals) || length(year_vals) != 1) {
  stop("Missing or invalid --year argument; provide exactly one year")
}
year <- as.integer(year_vals[1])
area <- parse_flag_value(args, "--area", "conifer")
if (!nzchar(trimws(area))) {
  stop("Invalid --area argument: must be a non-empty string")
}
out_dir <- parse_flag_value(args, "--out-dir", "diagnostics/diagnostics_results/covariates")
lambda_run_file <- parse_flag_value(
  args,
  "--lambda-run-file",
  sprintf("diagnostics/diagnostics_results/lambda_run/lambda_run_%d_%s.rds", year, area)
)

if (!file.exists(lambda_run_file)) {
  legacy_lambda_run <- sprintf("data/processed_data/lambda_run_%d_%s.rds", year, area)
  if (file.exists(legacy_lambda_run)) {
    lambda_run_file <- legacy_lambda_run
  }
}

if (!file.exists(lambda_run_file)) {
  default_lambda_run <- sprintf("diagnostics/diagnostics_results/lambda_run/lambda_run_%d_%s.rds", year, area)
  legacy_lambda_run <- sprintf("data/processed_data/lambda_run_%d_%s.rds", year, area)
  stop("lambda_run file not found. Checked: ",
       default_lambda_run,
       " and ",
       legacy_lambda_run)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cand_df <- tryCatch(
  readRDS(lambda_run_file),
  error = function(e) stop("Failed to read lambda_run file: ", lambda_run_file, " (", e$message, ")")
)
if (!is.data.frame(cand_df) || nrow(cand_df) == 0) {
  stop("lambda_run file does not contain candidate data.frame rows: ", lambda_run_file)
}
if (!"lambda" %in% colnames(cand_df)) {
  stop("lambda_run missing required column: lambda")
}

selected_lambda <- NA_real_
if ("chosen" %in% colnames(cand_df)) {
  chosen_rows <- cand_df[isTRUE(cand_df$chosen) | (!is.na(cand_df$chosen) & cand_df$chosen), , drop = FALSE]
  if (nrow(chosen_rows) > 0) {
    selected_lambda <- as.numeric(chosen_rows$lambda[1])
  }
}

if (!is.finite(selected_lambda)) {
  sel_ctx <- attr(cand_df, "selection_context")
  if (!is.null(sel_ctx) && !is.null(sel_ctx$selected_lambda) && is.finite(as.numeric(sel_ctx$selected_lambda))) {
    selected_lambda <- as.numeric(sel_ctx$selected_lambda)
  }
}

plot_file <- file.path(out_dir, paste0("lambda_diagnostics_", year, "_", area, ".png"))
plot_lambda_diagnostics(cand_df, selected_lambda = selected_lambda, out_file = plot_file)

cat("Lambda-path plotting run\n")
cat("  year      : ", year, "\n", sep = "")
cat("  area      : ", area, "\n", sep = "")
cat("  lambda run: ", lambda_run_file, "\n", sep = "")
if (is.finite(selected_lambda)) {
  cat("  selected  : ", selected_lambda, "\n", sep = "")
} else {
  cat("  selected  : not found (plot rendered without selected marker)\n")
}
cat("  output    : ", plot_file, "\n", sep = "")
