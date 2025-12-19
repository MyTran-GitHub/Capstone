# 04_pixel_stratification_ATT.R
# Estimate CATE by pixel-level covariate strata (e.g., quartiles of elevation, tree cover, etc.)

library(tidyverse)
library(survey)

input_dir <- '../data/processed_data/rev_analysis_low/'
output_dir <- '../data/processed_data/rev_analysis_low/'

# Choose covariate for stratification (edit as needed)
strat_var <- 'elev'  # or any pixel-level covariate present in merged file

years <- 2000:2009
results <- list()

for (year in years) {
  file <- file.path(input_dir, paste0('merged_cbps_landscape_', year, '_conifer.RDS'))
  if (!file.exists(file)) next
  df <- readRDS(file)
  if (!strat_var %in% names(df)) next
  # Stratify into quartiles
  df$stratum <- cut(df[[strat_var]], breaks = quantile(df[[strat_var]], probs = 0:4/4, na.rm = TRUE), include.lowest = TRUE, labels = FALSE)
  # Estimate ATT in each stratum
  att_by_stratum <- df %>% group_by(stratum) %>% summarise(
    n = n(),
    att = {
      design <- svydesign(ids = ~1, weights = ~cbps_weight, data = cur_data())
      fit <- svyglm(outcome ~ treated, design = design)
      coef(fit)['treated']
    },
    se = {
      design <- svydesign(ids = ~1, weights = ~cbps_weight, data = cur_data())
      fit <- svyglm(outcome ~ treated, design = design)
      summary(fit)$coefficients['treated', 'Std. Error']
    }
  )
  results[[as.character(year)]] <- att_by_stratum
  cat('Year', year, 'done\n')
}
# Save results
saveRDS(results, file = file.path(output_dir, paste0('pixel_stratified_ATT_results.RDS')))
cat('Pixel-level stratified ATT estimation complete.\n')
