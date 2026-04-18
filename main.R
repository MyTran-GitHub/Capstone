
# Main orchestration script for Capstone pipeline
# This script runs the full pipeline using config/config.yaml for parameters.

# ---- Load configuration ----
if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml")
library(yaml)
config <- yaml::read_yaml('config/config.yaml')

# ---- Set parameters ----
year <- config$year %||% 2019
output_prefix <- config$output_prefix %||% paste0("k", config$optimal_k %||% 30)
train_start <- config$train_start %||% 2000
train_end <- config$train_end %||% 2010
test_start <- config$test_start %||% 2011
test_end <- config$test_end %||% 2015
experiment_name <- config$experiment_name %||% "full_pool"
selected_units_csv <- config$selected_units_csv %||% paste0("Embeddings/data/k_selection/", year, "/selection_decision.json")

# ---- 1. Data Preparation ----
message("[1/7] Running data preparation scripts...")
source('data_processing/process_analysis_data.R')
source('data_processing/Preparation/activeFIRMS.R')
# Add other preparation scripts as needed

# ---- 2. Embedding Extraction ----
message("[2/7] Extracting embeddings for year: ", year)
system(paste('python3 Embeddings/scripts/02_extract_embeddings_single_year.py', year))

# ---- 3. K Selection ----
message("[3/7] Running K selection pipeline for year: ", year)
system(paste('python3 Embeddings/scripts/03_select_optimal_k.py', year))

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
system(cbps_cmd)

# ---- 5. Analysis ----
message("[5/7] Running analysis scripts...")
source('analysis/fire_regression_lag.R')
source('analysis/weighted_outcome_analysis.R')
# Add other analysis scripts as needed

# ---- 6. Diagnostics ----
message("[6/7] Running diagnostics scripts...")
source('diagnostics/diagnostics_scripts/diagnostic_script.R')

# ---- 7. Placebo/Robustness (optional) ----
message("[7/7] (Optional) Running placebo/robustness checks...")
system(paste('Rscript Embeddings/scripts/figures/placebo_att_simulator.R --experiment-name', experiment_name))

message("Pipeline completed successfully.")
