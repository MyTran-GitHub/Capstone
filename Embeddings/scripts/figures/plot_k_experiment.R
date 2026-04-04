#!/usr/bin/env Rscript
## Plot K-experiment results: RMSE, balance metrics, ESS, and weight distributions

suppressPackageStartupMessages({
  required <- c('ggplot2','dplyr','readr','tidyr')
  missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]
  if (length(missing)>0) stop('Missing packages: ', paste(missing, collapse=', '))
  lapply(required, library, character.only = TRUE)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop('Usage: Rscript plot_k_experiment.R <year> [cbps_integration_dir]')
}
year <- args[1]
cbps_dir <- ifelse(length(args) >= 2, args[2], paste0('Embeddings/data/cbps_integration/', year, '/'))
cbps_dir <- normalizePath(cbps_dir, mustWork = FALSE)

metrics_files <- list.files(cbps_dir, pattern = '^cbps_metrics_.*_.*\\.csv$', full.names = TRUE)
summary_file <- file.path(cbps_dir, paste0('k_selection_summary_', year, '.csv'))

if (file.exists(summary_file)) {
  cat('Using summary file:', summary_file, '\n')
  summary_df <- readr::read_csv(summary_file, show_col_types = FALSE)
} else if (length(metrics_files) > 0) {
  # read all metrics files and bind
  cat('Found', length(metrics_files), 'metrics files\n')
  metrics_list <- lapply(metrics_files, function(f) {
    d <- readr::read_csv(f, show_col_types = FALSE)
    # try to infer K from filename
    k_match <- regmatches(basename(f), regexpr('k\\d+', basename(f)))
    if (length(k_match)>0) d$K <- as.integer(gsub('k','',k_match))
    d
  })
  summary_df <- bind_rows(metrics_list)
} else if (file.exists(summary_file) == FALSE) {
  stop('No metrics or summary file found in ', cbps_dir)
}

# If summary file exists but no metrics, try to augment with metrics CSVs
if (exists('metrics_list')) {
  metrics_df <- summary_df
} else if (exists('summary_df') && nrow(summary_df)>0 && length(metrics_files)>0) {
  metrics_df <- summary_df
}

# Harmonize column names
if (!'K' %in% names(metrics_df) && 'K' %in% names(summary_df)) metrics_df$K <- as.integer(summary_df$K)

metrics_df <- metrics_df %>% dplyr::mutate(K = as.integer(K))

# Primary plot: RMSE test vs K
if ('rmse_test' %in% names(metrics_df)) {
  p1 <- metrics_df %>%
    dplyr::select(K, rmse_test) %>%
    dplyr::arrange(K) %>%
    ggplot(aes(x = K, y = rmse_test)) +
    geom_line() + geom_point() +
    labs(title = paste('K experiment: RMSE (test) —', year), x = 'K', y = 'RMSE (test)') + theme_minimal()
  ggsave(filename = file.path(cbps_dir, paste0('k_experiment_rmse_', year, '.png')), plot = p1, width = 8, height = 4)
  cat('Saved RMSE plot\n')
} else {
  cat('No rmse_test column found; skipping RMSE plot\n')
}

# Balance metrics plot: max_balance_std & mean_balance_std vs K
if (all(c('max_balance_std','mean_balance_std') %in% names(metrics_df))) {
  p2 <- metrics_df %>%
    dplyr::select(K, max_balance_std, mean_balance_std) %>%
    tidyr::pivot_longer(cols = c(max_balance_std, mean_balance_std), names_to = 'metric', values_to = 'value') %>%
    ggplot(aes(x = K, y = value, color = metric)) + geom_line() + geom_point() + theme_minimal() +
    labs(title = paste('Balance metrics vs K —', year), x = 'K', y = 'Std. mean diff')
  ggsave(filename = file.path(cbps_dir, paste0('k_experiment_balance_', year, '.png')), plot = p2, width = 8, height = 4)
  cat('Saved balance metrics plot\n')
} else {
  cat('Balance metrics columns missing; skipping balance plot\n')
}

# ESS / concentration metrics plot
conc_cols <- c()
if ('top10_share' %in% names(metrics_df)) {
  conc_cols <- c(conc_cols, 'top10_share')
} else if ('top50_share' %in% names(metrics_df)) {
  # Backward-compatibility with legacy outputs.
  conc_cols <- c(conc_cols, 'top50_share')
}
if ('max_weight' %in% names(metrics_df)) {
  conc_cols <- c(conc_cols, 'max_weight')
}

plot_cols <- c('ess', conc_cols)
if (all(plot_cols %in% names(metrics_df))) {
  p3 <- metrics_df %>%
    dplyr::select(K, dplyr::all_of(plot_cols)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(plot_cols), names_to = 'metric', values_to = 'value') %>%
    ggplot(aes(x = K, y = value, color = metric)) + geom_line() + geom_point() + theme_minimal() +
    labs(title = paste('ESS / concentration metrics vs K —', year), x = 'K')
  ggsave(filename = file.path(cbps_dir, paste0('k_experiment_ess_', year, '.png')), plot = p3, width = 8, height = 4)
  cat('Saved ESS/concentration plot\n')
} else {
  cat('ESS and concentration metrics missing; skipping ESS/concentration plot\n')
}

# Weight distribution plots for Ks that have weight CSVs
weight_files <- list.files(cbps_dir, pattern = '^cbps_weights_full_k\\d+_.*\\.csv$', full.names = TRUE)
if (length(weight_files) > 0) {
  # for each weight file, create a violin/boxplot of weights
  wlist <- lapply(weight_files, function(f) {
    d <- readr::read_csv(f, show_col_types = FALSE)
    k_match <- regmatches(basename(f), regexpr('k\\d+', basename(f)))
    d$K <- as.integer(gsub('k','', k_match))
    d
  })
  wdf <- dplyr::bind_rows(wlist)
  # filter control weights only
  wdf_ctrl <- wdf %>% dplyr::filter(treated == 0)
  p_w <- ggplot(wdf_ctrl, aes(x = factor(K), y = weight)) + geom_boxplot() + theme_minimal() +
    labs(title = paste('Control weight distribution by K —', year), x = 'K', y = 'Weight')
  ggsave(filename = file.path(cbps_dir, paste0('k_experiment_weights_', year, '.png')), plot = p_w, width = 10, height = 6)
  cat('Saved weight distribution plot\n')
} else {
  cat('No weight CSVs found; skipping weight distribution plots\n')
}

cat('K experiment plotting complete. Plots saved to', cbps_dir, '\n')
