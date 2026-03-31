#!/usr/bin/env Rscript

# Lightweight CLI: lambda-path plot only.
# Reads lambda_run artifact and writes lambda diagnostics figure.
#
# Usage:
#   Rscript diagnostics/diagnostics_scripts/covariates/run_covariate_diagnostics.R \
#     --year 2019 --area conifer

source("diagnostics/diagnostics_scripts/covariates/run_covariate_exploration.R")

parse_flag_value <- function(args, flag, default = NULL) {
  flag_eq <- paste0(flag, "=")
  hit_eq <- args[startsWith(args, flag_eq)]
  if (length(hit_eq) > 0) return(sub(flag_eq, "", hit_eq[1], fixed = TRUE))
  idx <- which(args == flag)
  if (length(idx) > 0 && idx[1] < length(args)) return(args[idx[1] + 1])
  default
}

args <- commandArgs(trailingOnly = TRUE)
year <- as.integer(parse_flag_value(args, "--year", NA))
area <- parse_flag_value(args, "--area", "conifer")
out_dir <- parse_flag_value(args, "--out-dir", "diagnostics/diagnostics_results/covariates")
lambda_run_file <- parse_flag_value(
  args,
  "--lambda-run-file",
  sprintf("diagnostics/diagnostics_results/lambda_run/lambda_run_%d_%s.rds", year, area)
)

if (is.na(year)) {
  stop("Missing required --year argument.")
}

if (!file.exists(lambda_run_file)) {
  legacy_lambda_run <- sprintf("data/processed_data/rev_analysis_low/lambda_run_%d_%s.rds", year, area)
  if (file.exists(legacy_lambda_run)) {
    lambda_run_file <- legacy_lambda_run
  }
}

if (!file.exists(lambda_run_file)) {
  stop("lambda_run file not found. Checked: ",
       sprintf("diagnostics/diagnostics_results/lambda_run/lambda_run_%d_%s.rds", year, area),
       " and legacy path in data/processed_data/rev_analysis_low")
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cand_df <- readRDS(lambda_run_file)
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
