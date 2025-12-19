# 03_merge_cbps_landscape.R
# Merge CBPS-weighted data with landscape context (buffer means) for each year
# Output: merged_cbps_landscape_{year}_conifer.RDS for downstream stratified CATE analysis

library(tidyverse)

years <- 2000:2009

cbps_dir <- '../data/processed_data/rev_analysis_low/'
context_dir <- '../data/processed_data/'
output_dir <- '../data/processed_data/rev_analysis_low/'

for (year in years) {
  cbps_file <- file.path(cbps_dir, paste0('cbps_weighted_', year, '_conifer.RDS'))
  context_file <- file.path(context_dir, paste0('landscape_context_', year, '_conifer.RDS'))
  if (!file.exists(cbps_file)) {
    cat('Missing CBPS-weighted file for year', year, '- skipping.\n')
    next
  }
  if (!file.exists(context_file)) {
    cat('Missing landscape context file for year', year, '- skipping.\n')
    next
  }
  df_cbps <- readRDS(cbps_file)
  df_ctx <- readRDS(context_file)
  # Check for 'unit' column
  if (!'unit' %in% names(df_cbps)) {
    stop(paste0("'unit' column missing in CBPS-weighted file for year ", year))
  }
  if (!'unit' %in% names(df_ctx)) {
    stop(paste0("'unit' column missing in landscape context file for year ", year))
  }
  # Remove duplicate 'unit' columns if present
  df_ctx <- df_ctx[, !duplicated(names(df_ctx))]
  # Only select mean_ columns that exist
  mean_cols <- grep('^mean_', names(df_ctx), value = TRUE)
  if (length(mean_cols) == 0) {
    cat('No buffer mean columns found for year', year, '- skipping.\n')
    next
  }
  # Remove duplicate rows in df_ctx by unit (keep first)
  df_ctx <- df_ctx[!duplicated(df_ctx$unit), c('unit', mean_cols)]
  merged <- left_join(df_cbps, df_ctx, by = 'unit')
  saveRDS(merged, file = file.path(output_dir, paste0('merged_cbps_landscape_', year, '_conifer.RDS')))
  cat('Merged and saved for year', year, '\n')
}
cat('All available years merged.\n')
