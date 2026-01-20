## create synthetic control regions using covariate balance weights
## calculate the relative risks of fire frequencies between exposed region and synthetic control region
## by land types and year lags
print(Sys.time())
rm(list = ls())
library("sf")
library("tidyverse")

outDir <- "data/processed_data/"
areas <- "conifer"                            # restrict analysis to conifer only
max_year <- 2020                               # last year with treatment cohorts
start_years <- c(2006, 2008, 2010)             # earliest cohort years; chosen so lags up to 9 fit within data window

firms_base <- readRDS(file.path(outDir, "FIRMS.RDS"))
firms_base$unit <- paste0(firms_base$LATITUDE, firms_base$LONGITUDE)
firms_base$has.fire <- 1
firms_base$has.hifire95 <- as.integer(firms_base$max_FRP >= 1000)
firms_base$has.hifire90 <- as.integer(firms_base$max_FRP >= 500)

for (start_year in start_years) {
  dir.create(file.path(outDir, "rev_result_low", start_year), recursive = TRUE, showWarnings = FALSE)

  parameters <- expand.grid(area = areas,
                            lagged = 1:9,
                            stringsAsFactors = FALSE)

  for (year_area in seq_len(nrow(parameters))) {
    area <- parameters$area[year_area]
    lagged <- parameters$lagged[year_area]

    treated_years <- start_year:(max_year - lagged)
    if (length(treated_years) == 0) {
      next
    }

    rate <- lapply(treated_years, function(treated.year) {
      cov_path <- file.path(outDir, "rev_analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS"))
      wgt_path <- file.path(outDir, "rev_analysis_low", paste0("cbps_weights_", treated.year, "_", area, ".RDS"))

      if (!file.exists(cov_path) || !file.exists(wgt_path)) {
        return(rep(NA_real_, 6))
      }

      df <- readRDS(cov_path)
      weights_df <- readRDS(wgt_path)

      df_weight <- merge(df, weights_df[, c("unit", "weight")], by = "unit", all.x = TRUE)

      firms <- firms_base

      control <- df_weight$unit[df_weight$treated == 0]
      treated <- df_weight$unit[df_weight$treated == 1]

      firms <- merge(firms,
                     df_weight[, c("LATITUDE", "LONGITUDE", "weight")],
                     by = c("LATITUDE", "LONGITUDE"),
                     all.x = TRUE)
      firms <- firms[firms$unit %in% c(control, treated), ]
      firms$treated <- 0
      firms[firms$unit %in% treated, ]$treated <- 1

      start.year <- min(firms$year)
      end.year <- max(firms$year)

      start.date <- as.Date(paste0(start.year, "-01-01"))
      end.date <- as.Date(paste0(end.year, "-12-01"))
      date.range <- seq(start.date, end.date, "years")
      df.date.panel <- data.frame(year = as.numeric(format(date.range, "%Y")), date.obj = date.range)

      ix.date <- rep(seq_len(nrow(df.date.panel)), length(unique(firms$unit)))
      ix.unit <- gl(length(unique(firms$unit)), nrow(df.date.panel))
      df.panel <- cbind(df.date.panel[ix.date, ], unit = unique(firms$unit)[ix.unit])

      df_final <- merge(df.panel,
                        firms[, c("year", "unit", "has.fire", "avg_BRIGHTNESS",
                                  "max_FRP", "treated", "has.hifire95", "has.hifire90")],
                        by = c("year", "unit"),
                        all.x = TRUE)
      df_final$has.fire[is.na(df_final$has.fire)] <- 0
      df_final$treated[df_final$unit %in% control] <- 0
      df_final$treated[df_final$unit %in% treated] <- 1
      df_final <- merge(df_final, df_weight[, c("unit", "weight")], by = "unit", all.x = TRUE)

      df_final$weight.fire <- df_final$has.fire * df_final$weight
      df_final$weight.hifire95 <- df_final$has.hifire95 * df_final$weight
      df_final$weight.hifire90 <- df_final$has.hifire90 * df_final$weight

      df.freq.year <- df_final %>%
        group_by(year, treated) %>%
        summarise(sum.fire = sum(weight.fire, na.rm = TRUE),
                  sum.hifire95 = sum(weight.hifire95, na.rm = TRUE),
                  sum.hifire90 = sum(weight.hifire90, na.rm = TRUE),
                  denom = sum(weight, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(fire.frac = ifelse(denom == 0, NA_real_, sum.fire / denom),
               hifire95.frac = ifelse(denom == 0, NA_real_, sum.hifire95 / denom),
               hifire90.frac = ifelse(denom == 0, NA_real_, sum.hifire90 / denom))

      saveRDS(df.freq.year, file = file.path(outDir, "rev_result_low", start_year, paste0("df.freq.year", treated.year, "_", area, ".RDS")))

      get_ratio <- function(df_freq, tr, outcome) {
        val <- df_freq %>% filter(treated == tr, year == treated.year + lagged) %>% pull({{ outcome }})
        if (length(val) == 0) return(NA_real_) else return(val[1])
      }

      ratio.fire.1 <- get_ratio(df.freq.year, 1, fire.frac)
      ratio.fire.0 <- get_ratio(df.freq.year, 0, fire.frac)
      ratio.hifire95.1 <- get_ratio(df.freq.year, 1, hifire95.frac)
      ratio.hifire95.0 <- get_ratio(df.freq.year, 0, hifire95.frac)
      ratio.hifire90.1 <- get_ratio(df.freq.year, 1, hifire90.frac)
      ratio.hifire90.0 <- get_ratio(df.freq.year, 0, hifire90.frac)

      return(c(ratio.fire.1, ratio.fire.0,
               ratio.hifire95.1, ratio.hifire95.0,
               ratio.hifire90.1, ratio.hifire90.0))
    })

    rate.df <- do.call(rbind, rate)
    colnames(rate.df) <- c("fire.1", "fire.0", "hifire95.1", "hifire95.0", "hifire90.1", "hifire90.0")
    saveRDS(rate.df, file = file.path(outDir, "rev_result_low", start_year, paste0(area, "_t", lagged, ".RDS")))
  }

  parameters <- expand.grid(area = areas,
                            lagged = as.character(seq(1, 9, 1)),
                            stringsAsFactors = FALSE)
  for (index in seq_len(nrow(parameters))) {
    rate <- data.frame(readRDS(file.path(outDir, "rev_result_low", start_year,
                                         paste0(parameters[index, 1], "_t", parameters[index, 2], ".RDS"))))
    rownames(rate) <- start_year:(start_year + nrow(rate) - 1)

    rate$pixels_burn <- sapply(start_year:(start_year + nrow(rate) - 1), function(treated.year) {
      df <- readRDS(file.path(outDir, "rev_analysis_low",
                              paste0("analysis_treated", treated.year, "_", parameters[index, 1], ".RDS")))
      return(sum(df$treated))
    })

    write.csv(rate, file = file.path(outDir, "rev_result_low", start_year,
                                     paste0(parameters[index, 1], "_lag", parameters[index, 2], ".csv")))
  }
}