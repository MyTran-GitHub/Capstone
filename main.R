
#!/usr/bin/env Rscript
# Main orchestration script for Capstone pipeline
# This script runs the full pipeline using config/config.yaml for parameters.

# Parse command-line options to improve reproducibility and allow dry-runs
required_pkgs <- c("optparse", "yaml")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
	stop(sprintf("Missing required R packages: %s. Install via install.packages() or use the env/ environment.", paste(missing_pkgs, collapse = ", ")))
}
library(optparse)
library(yaml)

option_list <- list(
	make_option(c("-c", "--config"), type = "character", default = "config/config.yaml",
							help = "Path to YAML config file [default %default]"),
	make_option(c("-y", "--year"), type = "integer", default = NA,
							help = "Override year in config"),
	make_option(c("--dry-run"), action = "store_true", default = FALSE,
							help = "Print commands and sources without executing")
)
opt <- parse_args(OptionParser(option_list = option_list))

config_path <- opt$config
config <- yaml::read_yaml(config_path)

`%||%` <- function(a, b) if (!is.null(a)) a else b

if (!is.na(opt$year)) config$year <- opt$year

year <- config$year %||% 2019
output_prefix <- config$output_prefix %||% paste0("k", config$optimal_k %||% 30)
train_start <- config$train_start %||% 2000
train_end <- config$train_end %||% 2010
test_start <- config$test_start %||% 2011
test_end <- config$test_end %||% 2015
experiment_name <- config$experiment_name %||% "full_pool"
selected_units_csv <- config$selected_units_csv %||% paste0("Embeddings/data/k_selection/", year, "/selection_decision.json")

run_cmd <- function(cmd) {
	if (opt$`dry-run`) {
		message("[DRY-RUN] ", cmd)
	} else {
		message("[RUN] ", cmd)
		status <- system(cmd)
		if (status != 0) stop("Command failed: ", cmd)
	}
}

run_source <- function(path) {
	if (opt$`dry-run`) {
		message("[DRY-RUN] source(", path, ")")
	} else {
		message("[SOURCE] ", path)
		source(path)
	}
}

# ---- 1. Data Preparation ----
message("[1/7] Data preparation - running data processing scripts")
run_source('data_processing/process_analysis_data.R')
run_source('data_processing/Preparation/activeFIRMS.R')

# ---- 2. Embedding Extraction ----
message("[2/7] Extracting embeddings for year: ", year)
run_cmd(paste('python3 Embeddings/scripts/02_extract_embeddings_single_year.py', year))

# ---- 3. K Selection ----
message("[3/7] Running K selection pipeline for year: ", year)
run_cmd(paste('python3 Embeddings/scripts/03_select_optimal_k.py', year))

# ---- 4. CBPS with Selected Controls ----
message("[4/7] Running CBPS with selected controls for year: ", year)
cbps_cmd <- paste(
	'Rscript Embeddings/scripts/04_run_cbps_with_selected_controls.R',
	year,
	selected_units_csv,
	output_prefix,
	train_start,
	train_end,
	test_start,
	test_end,
	'--experiment-name', experiment_name
)
run_cmd(cbps_cmd)

# ---- 5. Analysis ----
message("[5/7] Running analysis scripts")
run_source('analysis/fire_regression_lag.R')
run_source('analysis/weighted_outcome_analysis.R')

# ---- 6. Diagnostics ----
message("[6/7] Running diagnostics scripts")
run_source('diagnostics/diagnostics_scripts/diagnostic_script.R')

# ---- 7. Placebo/Robustness (optional) ----
message("[7/7] Optional placebo/robustness checks")
run_cmd(paste('Rscript Embeddings/scripts/figures/placebo_att_simulator.R --experiment-name', experiment_name))

message("Pipeline completed successfully (or dry-run printed steps).")
