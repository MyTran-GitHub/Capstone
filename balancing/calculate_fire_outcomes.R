## Shared function: Calculate weighted fire frequency outcomes
## Used by both weighted_outcome_analysis.R and run_cbps_with_selected_controls.R
##
## This function takes CBPS weights and calculates fire.frac (weighted fire frequency)
## for specified years, following the weighted outcome analysis methodology.
##
## IMPORTANT: FIRMS.RDS contains fire data for ALL areas (conifer + hardwood), but
## the merge operation with weights_df (which comes from analysis_treated{year}_conifer.RDS)
## automatically filters to conifer pixels only via coordinate matching.
## This is safe and correct - no manual filtering needed!
library(magrittr)  # Provides %>%
# Declare global variables used in dplyr/ggplot2 operations to avoid R CMD check NOTEs
utils::globalVariables(c(
  "year", "treated", "weight", "has.fire", "weight.fire",
  "sum.fire", "denom", "fire.frac", "group",
  "treated_0", "treated_1",  # For pivot_wider
  "date_obj", "unit", "fire.frac"  # For panel construction
))
calculate_fire_frequency <- function(weights_df, 
                                     firms_rds_path = "data/processed_data/FIRMS.RDS",
                                     years_to_include = NULL) {
  #' Calculate weighted fire frequency for treated vs control groups
  #' 
  #' CRITICAL: Matches weighted_outcome_analysis.R logic exactly:
  #' 1. Create full panel (all year-unit combinations)
  #' 2. Merge fire data with panel (units without fires get has.fire = 0)
  #' 3. Compute weighted fire frequency where denom = sum of ALL unit weights
  #' 
  #' @param weights_df Data frame with columns: unit, LATITUDE, LONGITUDE, treated, weight
  #' @param firms_rds_path Path to FIRMS.RDS file
  #' @param years_to_include Vector of years to include (NULL = all years)
  #' 
  #' @return Data frame with columns: year, treated, fire.frac, sum.fire, denom

  # DIAGNOSTICS: Print structure before merge
  cat("\n--- DIAGNOSTICS: weights_df structure ---\n")
  print(str(weights_df))
  cat("\n--- DIAGNOSTICS: firms_base structure ---\n")
  # Load FIRMS fire data
  if (!file.exists(firms_rds_path)) {
    stop(paste("FIRMS data not found:", firms_rds_path))
  }
  firms_base <- readRDS(firms_rds_path)
  # Fix: Drop geometry column and convert sf to data.frame if needed
  if (inherits(firms_base, "sf")) {
    if (requireNamespace("sf", quietly = TRUE)) {
      coords_try <- try(sf::st_coordinates(firms_base), silent = TRUE)
      if (!inherits(coords_try, "try-error") && is.matrix(coords_try) && ncol(coords_try) >= 2) {
        # st_coordinates returns X = LONG, Y = LAT
        if (!"LONGITUDE" %in% colnames(firms_base)) firms_base$LONGITUDE <- coords_try[,1]
        if (!"LATITUDE" %in% colnames(firms_base)) firms_base$LATITUDE <- coords_try[,2]
      }
      firms_base <- sf::st_drop_geometry(firms_base)
    } else {
      firms_base <- as.data.frame(firms_base)
    }
  }
  # Remove any remaining sfc or list-like geometry columns
  is_sfc_like <- sapply(firms_base, function(col) inherits(col, "sfc") || any(grepl("sfc", class(col), fixed = TRUE)) || is.list(col))
  if (any(is_sfc_like)) {
    firms_base <- firms_base[, !is_sfc_like, drop = FALSE]
  }
  print(str(firms_base))
  cat("\n--- DIAGNOSTICS: head(weights_df) ---\n")
  print(head(weights_df, 10))
  cat("\n--- DIAGNOSTICS: head(firms_base) ---\n")
  print(head(firms_base, 10))

  firms_base$unit <- paste0(firms_base$LATITUDE, firms_base$LONGITUDE)
  firms_base$has.fire <- 1
  firms_base$has.hifire95 <- as.integer(firms_base$max_FRP >= 1000)
  firms_base$has.hifire90 <- as.integer(firms_base$max_FRP >= 500)

    # Ensure weights_df contains unit and LAT/LONG coordinates for merging
    if (!('unit' %in% names(weights_df))) {
      if ('LATITUDE' %in% names(weights_df) && 'LONGITUDE' %in% names(weights_df)) {
        weights_df$unit <- paste0(weights_df$LATITUDE, weights_df$LONGITUDE)
      } else {
        stop('weights_df must contain either `unit` or both `LATITUDE` and `LONGITUDE`')
      }
    }
    # If LAT/LONG missing but unit present, try to parse unit string "lat<sep>long" where long begins with a minus sign
    if (!('LATITUDE' %in% names(weights_df) && 'LONGITUDE' %in% names(weights_df))) {
      if ('unit' %in% names(weights_df)) {
        parsed <- do.call(rbind, lapply(as.character(weights_df$unit), function(u) {
          m <- regexpr('-[0-9]+\\.?[0-9]*$', u)
          if (m[1] > 1) {
            lon <- substring(u, m[1])
            lat <- substring(u, 1, m[1]-1)
          } else {
            lat <- NA; lon <- NA
          }
          c(lat, lon)
        }))
        weights_df$LATITUDE <- as.numeric(parsed[,1])
        weights_df$LONGITUDE <- as.numeric(parsed[,2])
      }
    }

  # Extract control and treated units (matching weighted_outcome_analysis.R lines 47-48)
  control_units <- weights_df$unit[weights_df$treated == 0]
  treated_units <- weights_df$unit[weights_df$treated == 1]

  # Merge FIRMS with weights by coordinates (matching line 49-51)
  firms <- merge(firms_base,
                 weights_df[, c("LATITUDE", "LONGITUDE", "weight")],
                 by = c("LATITUDE", "LONGITUDE"),
                 all.x = TRUE)
  # After merging, create high-intensity fire indicators
  firms$has.hifire95 <- 0
  firms$has.hifire90 <- 0
  firms$has.hifire95[!is.na(firms$max_FRP) & firms$max_FRP >= 1000] <- 1
  firms$has.hifire90[!is.na(firms$max_FRP) & firms$max_FRP >= 500] <- 1
  # DIAGNOSTICS: Print structure after merge
  cat("\n--- DIAGNOSTICS: merged firms structure ---\n")
  print(str(firms))
  cat("\n--- DIAGNOSTICS: head(merged firms) ---\n")
  print(head(firms, 10))

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' required")
  }

  # Filter to study units and assign treated status (matching lines 53-55)
  firms <- firms[firms$unit %in% c(control_units, treated_units), ]
  firms$treated <- 0
  firms$treated[firms$unit %in% treated_units] <- 1

  # Filter to specified years if provided
  if (!is.null(years_to_include)) {
    firms <- firms[firms$year %in% years_to_include, ]
  }
  
  # ============================================================================
  # CRITICAL: Create full panel (matching weighted_outcome_analysis.R lines 57-68)
  # Without this, denom only includes units WITH fires → inflated fire.frac!
  # ============================================================================
  
  start_year <- min(firms$year)
  end_year <- max(firms$year)
  
  # Create date sequence
  start_date <- as.Date(paste0(start_year, "-01-01"))
  end_date <- as.Date(paste0(end_year, "-12-01"))
  date_range <- seq(start_date, end_date, "years")
  df_date_panel <- data.frame(
    year = as.numeric(format(date_range, "%Y")),
    date_obj = date_range
  )
  
  # Create full panel: all year-unit combinations
  all_units <- unique(c(control_units, treated_units))
  ix_date <- rep(seq_len(nrow(df_date_panel)), length(all_units))
  ix_unit <- gl(length(all_units), nrow(df_date_panel))
  df_panel <- cbind(df_date_panel[ix_date, ], unit = all_units[ix_unit])
  
  # Merge fire data with panel (all.x = TRUE keeps all panel rows)
  # Matching lines 70-79 of weighted_outcome_analysis.R
  # Include all relevant fire intensity columns for downstream hifire95.frac, hifire90.frac, etc.
  df_final <- merge(df_panel,
                    firms[, c("year", "unit", "has.fire", "avg_BRIGHTNESS", "max_FRP", "treated", "has.hifire95", "has.hifire90")],
                    by = c("year", "unit"),
                    all.x = TRUE)
  
  # Fill NAs: units without fires get has.fire = 0
  df_final$has.fire[is.na(df_final$has.fire)] <- 0
  df_final$treated[df_final$unit %in% control_units] <- 0
  df_final$treated[df_final$unit %in% treated_units] <- 1
  
  # Merge weights (now matches all panel rows)
  df_final <- merge(df_final, 
                    weights_df[, c("unit", "weight")], 
                    by = "unit", 
                    all.x = TRUE)
  
  if (nrow(df_final) == 0) {
    warning("No data after panel construction")
    return(data.frame(
      year = integer(),
      treated = integer(),
      fire.frac = numeric(),
      sum.fire = numeric(),
      denom = numeric()
    ))
  }
  
  # Calculate weighted fire frequency and high-intensity fire frequency (matching lines 81-92)
  df_final$weight.fire <- df_final$has.fire * df_final$weight
  df_final$weight.hifire95 <- df_final$has.hifire95 * df_final$weight
  df_final$weight.hifire90 <- df_final$has.hifire90 * df_final$weight

  fire_freq <- df_final %>%
    dplyr::group_by(year, treated) %>%
    dplyr::summarise(
      sum.fire = sum(weight.fire, na.rm = TRUE),
      sum.hifire95 = sum(weight.hifire95, na.rm = TRUE),
      sum.hifire90 = sum(weight.hifire90, na.rm = TRUE),
      denom = sum(weight, na.rm = TRUE),  # Now includes ALL units, not just those with fires
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      fire.frac = ifelse(denom == 0, NA_real_, sum.fire / denom),
      hifire95.frac = ifelse(denom == 0, NA_real_, sum.hifire95 / denom),
      hifire90.frac = ifelse(denom == 0, NA_real_, sum.hifire90 / denom)
    )

  return(fire_freq)
}

calculate_pretreatment_rmse <- function(weights_df,
                                        train_start,
                                        train_end,
                                        test_start,
                                        test_end,
                                        firms_rds_path = "data/processed_data/FIRMS.RDS") {
  #' Calculate pre-treatment RMSE on fire frequency outcomes
  #' 
  #' Measures how well treated and control groups match in pre-treatment fire frequency.
  #' Lower RMSE = better pre-treatment match = more credible counterfactual.
  #' 
  #' @param weights_df Data frame with columns: unit, LATITUDE, LONGITUDE, treated, weight
  #' @param train_start First year of training period (e.g., 2000)
  #' @param train_end Last year of training period (e.g., 2010)
  #' @param test_start First year of test period (e.g., 2011)
  #' @param test_end Last year of test period (e.g., 2015)
  #' @param firms_rds_path Path to FIRMS.RDS file
  #' 
  #' @return List with rmse_train, rmse_test, fire_freq_data
  
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' required")
  }
  
  train_years_vec <- seq(train_start, train_end)
  test_years_vec <- seq(test_start, test_end)
  all_years <- c(train_years_vec, test_years_vec)
  
  # Calculate fire frequency for all pre-treatment years
  fire_freq <- calculate_fire_frequency(
    weights_df = weights_df,
    firms_rds_path = firms_rds_path,
    years_to_include = all_years
  )
  
  if (nrow(fire_freq) == 0) {
    warning("No fire data in pre-treatment period")
    return(list(
      rmse_train = NA_real_,
      rmse_test = NA_real_,
      fire_freq_data = fire_freq
    ))
  }
  
  # Compute RMSE for training period (using hifire95.frac)
  fire_train <- fire_freq[fire_freq$year %in% train_years_vec, ]
  
  n_years_used_train <- 0
  rmse_train <- NA_real_
  rmse_train_norm <- NA_real_
  if (nrow(fire_train) > 0) {
    fire_train_wide <- fire_train %>%
      dplyr::select(year, treated, hifire95.frac) %>%
      tidyr::pivot_wider(names_from = treated, values_from = hifire95.frac, names_prefix = "treated_")

    # Compute RMSE only on years where both treated and control group values are present
    valid_idx_train <- which(!is.na(fire_train_wide$treated_1) & !is.na(fire_train_wide$treated_0))
    n_years_used_train <- length(valid_idx_train)
    if (n_years_used_train == 0) {
      rmse_train <- NA_real_
      rmse_train_norm <- NA_real_
      warning('No overlapping years with non-missing treated and control values for RMSE (train)')
    } else {
      diffs_train <- fire_train_wide$treated_1[valid_idx_train] - fire_train_wide$treated_0[valid_idx_train]
      rmse_train <- sqrt(mean(diffs_train^2, na.rm = TRUE))
      # normalized RMSE relative to mean treated outcome magnitude
      mean_treated_train <- mean(fire_train_wide$treated_1[valid_idx_train], na.rm = TRUE)
      if (is.na(mean_treated_train) || mean_treated_train == 0) {
        rmse_train_norm <- NA_real_
      } else {
        rmse_train_norm <- rmse_train / abs(mean_treated_train)
      }
    }
  } else {
    rmse_train <- NA_real_
  }
  
  # Compute RMSE for test period (using hifire95.frac)
  fire_test <- fire_freq[fire_freq$year %in% test_years_vec, ]
  
  n_years_used_test <- 0
  rmse_test <- NA_real_
  rmse_test_norm <- NA_real_
  if (nrow(fire_test) > 0) {
    fire_test_wide <- fire_test %>%
      dplyr::select(year, treated, hifire95.frac) %>%
      tidyr::pivot_wider(names_from = treated, values_from = hifire95.frac, names_prefix = "treated_")

    # Compute RMSE only on years where both treated and control group values are present
    valid_idx_test <- which(!is.na(fire_test_wide$treated_1) & !is.na(fire_test_wide$treated_0))
    n_years_used_test <- length(valid_idx_test)
    if (n_years_used_test == 0) {
      rmse_test <- NA_real_
      rmse_test_norm <- NA_real_
      warning('No overlapping years with non-missing treated and control values for RMSE (test)')
    } else {
      diffs_test <- fire_test_wide$treated_1[valid_idx_test] - fire_test_wide$treated_0[valid_idx_test]
      rmse_test <- sqrt(mean(diffs_test^2, na.rm = TRUE))
      mean_treated_test <- mean(fire_test_wide$treated_1[valid_idx_test], na.rm = TRUE)
      if (is.na(mean_treated_test) || mean_treated_test == 0) {
        rmse_test_norm <- NA_real_
      } else {
        rmse_test_norm <- rmse_test / abs(mean_treated_test)
      }
    }
  } else {
    rmse_test <- NA_real_
  }
  
  return(list(
    rmse_train = rmse_train,
    rmse_test = rmse_test,
    rmse_train_norm = rmse_train_norm,
    rmse_test_norm = rmse_test_norm,
    n_years_used_train = n_years_used_train,
    n_years_used_test = n_years_used_test,
    fire_freq_data = fire_freq
  ))
}

plot_pretreatment_trajectory <- function(weights_df,
                                         train_start,
                                         train_end,
                                         test_start,
                                         test_end,
                                         output_path = NULL,
                                         treatment_year = NULL,
                                         firms_rds_path = "data/processed_data/FIRMS.RDS") {
  #' Plot pre-treatment fire frequency trajectory for treated vs control
  #' 
  #' Visual check of parallel trends assumption. Lines should track closely
  #' in pre-treatment period if matching is successful.
  #' 
  #' @param weights_df Data frame with columns: unit, LATITUDE, LONGITUDE, treated, weight
  #' @param train_start First year of training period (e.g., 2000)
  #' @param train_end Last year of training period (e.g., 2010)
  #' @param test_start First year of test period (e.g., 2011)
  #' @param test_end Last year of test period (e.g., 2015)
  #' @param output_path Optional path to save plot (PNG or PDF)
  #' @param treatment_year Optional year to mark treatment start with vertical line
  #' @param firms_rds_path Path to FIRMS.RDS file
  #' 
  #' @return Data frame with trajectory data (for further analysis)
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Package 'ggplot2' not available - trajectory data returned without plot")
    plot_enabled <- FALSE
  } else {
    plot_enabled <- TRUE
  }
  
  # Calculate fire frequency for pre-treatment period
  train_years_vec <- seq(train_start, train_end)
  test_years_vec <- seq(test_start, test_end)
  all_years <- c(train_years_vec, test_years_vec)
  
  fire_freq <- calculate_fire_frequency(
    weights_df = weights_df,
    firms_rds_path = firms_rds_path,
    years_to_include = all_years
  )
  
  if (nrow(fire_freq) == 0) {
    warning("No fire data for trajectory plot")
    return(fire_freq)
  }
  
  # Reshape for plotting: pivot to wide format
  fire_wide <- fire_freq %>%
    dplyr::select(year, treated, hifire95.frac) %>%
    tidyr::pivot_wider(names_from = treated, 
                      values_from = hifire95.frac, 
                      names_prefix = "treated_")
  
  # Compute gap (treated - control)
  fire_wide$gap <- fire_wide$treated_1 - fire_wide$treated_0
  
  # Create plot if ggplot2 available
  if (plot_enabled) {
    # Reshape for ggplot (long format)
    fire_long <- fire_freq %>%
      dplyr::mutate(group = ifelse(treated == 1, "Treated", "Control"))
    
    p <- ggplot2::ggplot(fire_long, ggplot2::aes(x = year, y = hifire95.frac, color = group)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_color_manual(values = c("Treated" = "#E41A1C", "Control" = "#377EB8")) +
      ggplot2::labs(
        title = "Pre-treatment High-Intensity Fire Frequency Trajectory",
        subtitle = "Parallel trends = successful matching",
        x = "Year",
        y = "Weighted High-Intensity Fire Frequency",
        color = "Group"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 10, color = "gray30")
      )
    
    # Add vertical line for treatment year if provided
    if (!is.null(treatment_year)) {
      p <- p + ggplot2::geom_vline(xintercept = treatment_year, 
                                   linetype = "dashed", 
                                   color = "gray50",
                                   linewidth = 0.5) +
        ggplot2::annotate("text", 
                         x = treatment_year, 
                         y = max(fire_long$hifire95.frac, na.rm = TRUE) * 0.95,
                         label = paste("Treatment:", treatment_year),
                         color = "gray30",
                         hjust = -0.1,
                         size = 3)
    }
    
    # Add shaded regions for train vs test
    p <- p + ggplot2::annotate("rect",
                              xmin = train_start, xmax = train_end,
                              ymin = -Inf, ymax = Inf,
                              alpha = 0.1, fill = "blue") +
      ggplot2::annotate("text",
                       x = mean(c(train_start, train_end)),
                       y = min(fire_long$hifire95.frac, na.rm = TRUE),
                       label = "Train",
                       color = "blue",
                       size = 3) +
      ggplot2::annotate("rect",
                       xmin = test_start, xmax = test_end,
                       ymin = -Inf, ymax = Inf,
                       alpha = 0.1, fill = "green") +
      ggplot2::annotate("text",
                       x = mean(c(test_start, test_end)),
                       y = min(fire_long$hifire95.frac, na.rm = TRUE),
                       label = "Test",
                       color = "darkgreen",
                       size = 3)
    
    # Save plot if path provided
    if (!is.null(output_path)) {
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      ggplot2::ggsave(output_path, plot = p, width = 10, height = 6, dpi = 300)
      cat("✓ Trajectory plot saved to:", output_path, "\n")
    } else {
      print(p)
    }
  }
  
  # Return trajectory data for further analysis
  return(fire_wide)
}


#' Pooled jackknife estimator for ratio ATT (adapted from analysis/fire_regression_lag.R)
#'
#' Computes a pooled (cohort-pooled across years) ratio estimator of treated vs control
#' outcomes using a leave-one-year-out jackknife to estimate variance. Returns a
#' relative-risk-style ratio with jackknife standard error and CI.
#'
compute_pooled_jackknife_att <- function(weights_df,
                                         outcome_years,
                                         outcome_var = "hifire95.frac",
                                         firms_rds_path = "data/processed_data/FIRMS.RDS",
                                         alpha = 0.05) {
  # Build per-year treated/control outcome table using existing helper
  fire_freq <- calculate_fire_frequency(
    weights_df = weights_df,
    firms_rds_path = firms_rds_path,
    years_to_include = outcome_years
  )

  if (nrow(fire_freq) == 0) stop("No fire-frequency data available for pooled jackknife")

  years_present <- sort(unique(fire_freq$year))
  raw.plot <- data.frame(Year = years_present,
                         ratio = NA_real_,
                         Baseline = NA_real_)

  for (i in seq_along(years_present)) {
    yr <- years_present[i]
    sub <- fire_freq[fire_freq$year == yr, ]
    treated_val <- sub[[outcome_var]][sub$treated == 1]
    control_val <- sub[[outcome_var]][sub$treated == 0]

    if (length(treated_val) == 0 || length(control_val) == 0 || is.na(treated_val) || is.na(control_val) || control_val == 0) {
      raw.plot$ratio[i] <- NA_real_
      raw.plot$Baseline[i] <- NA_real_
    } else {
      raw.plot$ratio[i] <- treated_val / control_val
      raw.plot$Baseline[i] <- control_val
    }
  }

  raw.plot <- raw.plot[!is.na(raw.plot$ratio), , drop = FALSE]
  if (nrow(raw.plot) == 0) stop("No valid years with both treated and control outcomes for pooled jackknife")

  jackfun <- function(end.years) {
    jack.data <- subset(raw.plot, Year %in% end.years)
    reg.jack <- stats::glm(ratio ~ 1, family = quasipoisson, weights = Baseline, data = jack.data)
    as.numeric(coef(reg.jack)[1])
  }

  full.reg <- jackfun(unique(raw.plot$Year))
  yrs <- unique(raw.plot$Year)
  jackreps <- sapply(yrs, function(yy) jackfun(setdiff(yrs, yy)))

  # Jackknife variance (standard leave-one-out jackknife variance formula)
  n <- length(jackreps)
  jackvar <- var(jackreps) * (n - 1)^2 / n
  jackse <- sqrt(jackvar)

  rat <- exp(full.reg)
  zcrit <- qnorm(1 - alpha / 2)
  ub <- exp(full.reg + zcrit * jackse)
  lb <- exp(full.reg - zcrit * jackse)

  data.frame(
    method = "pooled_jackknife",
    att_ratio = rat,
    se = jackse,
    ci_lower = lb,
    ci_upper = ub,
    n_years = nrow(raw.plot),
    stringsAsFactors = FALSE
  )
}

#' Embedding-weighted cohort-pooled per-lag estimator
#'
#' Computes per-lag pooled jackknife estimates using user-supplied weights.
#' This reproduces the jackknife + quasi-Poisson regression logic from
#' `analysis/fire_regression_lag.R::compute_results()` but sources outcomes
#' from `calculate_fire_frequency()` using the provided `weights_df`.
compute_pooled_jackknife_per_lag_weights <- function(weights_df,
                                                     focal_years,
                                                     outcome_var = "hifire95.frac",
                                                     ci_type = "two") {
  all.lags <- 1:9
  all.end.years <- 2009:2021

  raw.plot <- expand.grid(Year = all.end.years, Lag = all.lags)
  raw.plot$ratio <- NA_real_
  raw.plot$Baseline <- NA_real_

  for (i in seq_len(nrow(raw.plot))) {
    YYY <- raw.plot$Year[i]
    LLL <- raw.plot$Lag[i]
    start_year <- YYY - LLL
    if (!(start_year %in% focal_years)) next

    ff <- tryCatch(
      calculate_fire_frequency(weights_df = weights_df, years_to_include = start_year),
      error = function(e) NULL
    )
    if (is.null(ff) || nrow(ff) == 0) next

    treated_val <- ff[[outcome_var]][ff$treated == 1]
    control_val <- ff[[outcome_var]][ff$treated == 0]
    if (length(treated_val) == 0 || length(control_val) == 0 || is.na(treated_val) || is.na(control_val) || control_val == 0) {
      next
    }

    raw.plot$ratio[i] <- treated_val / control_val
    raw.plot$Baseline[i] <- control_val
  }

  raw.plot <- subset(raw.plot, !is.na(ratio))
  if (nrow(raw.plot) == 0) stop("No valid years with both treated and control outcomes for pooled jackknife (weights)")

  jackfun <- function(end.years) {
    jack.data <- subset(raw.plot, Year %in% end.years)
    reg.jack <- glm(ratio ~ Lag, family = quasipoisson, weights = Baseline, data = jack.data)
    coef(reg.jack)[1] + all.lags * coef(reg.jack)[2]
  }

  full.reg <- jackfun(unique(raw.plot$Year))
  yrs <- unique(raw.plot$Year)
  jackreps <- t(sapply(yrs, function(yy) jackfun(setdiff(yrs, yy))))

  colnames(jackreps) <- all.lags
  jackvar <- apply(jackreps, 2, function(xx) var(xx) * (length(xx) - 1)^2 / length(xx))
  jackse <- sqrt(jackvar)

  rat <- exp(full.reg)
  if (ci_type == "two") {
    ub <- exp(full.reg + qnorm(0.975) * jackse)
    lb <- exp(full.reg - qnorm(0.975) * jackse)
  } else {
    ub <- exp(full.reg + qnorm(0.95) * jackse)
    lb <- exp(full.reg - 1000 * jackse)
  }

  data.frame(year = 1:length(rat), rate = rat, lower = lb, upper = ub)
}




estimate_att_with_ci <- function(weights_df,
                                 outcome_years,
                                 treatment_year,
                                 firms_rds_path = "data/processed_data/FIRMS.RDS",
                                 cluster_by_unit = TRUE,
                                 alpha = 0.05,
                                 method = c("per_year", "pooled_jackknife_per_lag", "pooled_jackknife")) {
  #' Estimate ATT with confidence intervals
  #' 
  #' Computes Average Treatment Effect on the Treated with robust/clustered standard errors:
  #' - ATT: Weighted difference in fire frequency (treated - control)
  #' - Standard errors: Robust (HC3) or clustered by pixel 
  #' - 95% confidence intervals
  #' 
  #' @param weights_df Data frame with columns: unit, LATITUDE, LONGITUDE, treated, weight
  #' @param outcome_years Vector of post-treatment years to analyze
  #' @param treatment_year Year of treatment
  #' @param firms_rds_path Path to FIRMS.RDS file
  #' @param cluster_by_unit Whether to cluster standard errors by unit (default: TRUE)
  #' @param alpha Significance level for CI (default: 0.05 for 95% CI)
  #' 
  #' @return Data frame with columns: year, method, att, se, ci_lower, ci_upper, ci_width, n_treated, n_control
  
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' required for robust standard errors. Install with: install.packages('sandwich')")
  }
  
  if (!requireNamespace("lmtest", quietly = TRUE)) {
    stop("Package 'lmtest' required for regression testing. Install with: install.packages('lmtest')")
  }
  
  # Calculate post-treatment fire frequency
  fire_freq <- calculate_fire_frequency(
    weights_df = weights_df,
    firms_rds_path = firms_rds_path,
    years_to_include = outcome_years
  )
  
  if (nrow(fire_freq) == 0) {
    warning("No fire data in post-treatment period")
    return(data.frame(
      year = integer(),
      method = character(),
      att = numeric(),
      se = numeric(),
      ci_lower = numeric(),
      ci_upper = numeric(),
      ci_width = numeric(),
      n_treated = integer(),
      n_control = integer()
    ))
  }
  
  # Count units
  n_treated <- sum(weights_df$treated == 1)
  n_control <- sum(weights_df$treated == 0)

  method <- match.arg(method)
  if (method == "pooled_jackknife_per_lag") {
    # Reuse the analysis script's pooled per-lag estimator
    res <- compute_pooled_jackknife_per_lag(focal_years = outcome_years,
                                            outcome_var = "hifire95.frac",
                                            ci_type = ifelse(alpha == 0.05, "two", "one"))

    # res has columns: year (lag index), rate, lower, upper
    zcrit <- qnorm(1 - alpha / 2)
    se_log <- (log(res$upper) - log(res$lower)) / (2 * zcrit)

    results_df <- data.frame(
      year = res$year,
      method = "pooled_jackknife_per_lag",
      att = res$rate,
      se = se_log,
      ci_lower = res$lower,
      ci_upper = res$upper,
      ci_width = res$upper - res$lower,
      n_treated = n_treated,
      n_control = n_control,
      stringsAsFactors = FALSE
    )
    cat("✓ Computed pooled jackknife per-lag ATT (ratio) via analysis/fire_regression_lag.R\n")
    return(results_df)
  }
  
  # Merge weights with fire frequency for regression
  fire_freq_with_weights <- merge(
    fire_freq,
    weights_df[, c("unit", "treated", "weight")],
    by = "treated",
    all.x = TRUE
  )
  
  # Estimate ATT for each year
  results_list <- list()
  
  for (yr in outcome_years) {
    fire_year <- fire_freq[fire_freq$year == yr, ]
    
    if (nrow(fire_year) == 0) {
      warning(paste("No fire data for year", yr))
      next
    }
    
    if (nrow(fire_year) < 2) {
      warning(paste("Insufficient data for year", yr, "- need both treated and control"))
      next
    }
    
    # Create dataset for regression: one row per unit
    # Merge fire.frac with weights_df
    fire_year_full <- merge(
      weights_df[, c("unit", "treated", "weight")],
      fire_year[, c("treated", "hifire95.frac")],
      by = "treated",
      all.x = TRUE
    )
    
    # Handle missing fire.frac (units not in fire data get fire.frac from their group mean)
    # This is safe because calculate_fire_frequency creates full panel
    if (anyNA(fire_year_full$hifire95.frac)) {
      # Use group mean for missing values
      fire_year_full$hifire95.frac[is.na(fire_year_full$hifire95.frac) & fire_year_full$treated == 0] <- 
        fire_year$hifire95.frac[fire_year$treated == 0]
      fire_year_full$hifire95.frac[is.na(fire_year_full$hifire95.frac) & fire_year_full$treated == 1] <- 
        fire_year$hifire95.frac[fire_year$treated == 1]
    }
    
    # Weighted regression: E[Y|D=1] - E[Y|D=0] with CBPS weights
    model <- lm(hifire95.frac ~ treated, data = fire_year_full, weights = weight)
    
    # Extract ATT (coefficient on 'treated')
    att <- coef(model)["treated"]
    
    # Compute robust/clustered standard errors
    if (cluster_by_unit) {
      # Clustered by unit (accounts for within-unit correlation)
      vcov_robust <- sandwich::vcovCL(model, cluster = fire_year_full$unit, type = "HC3")
    } else {
      # Heteroskedasticity-robust (HC3)
      vcov_robust <- sandwich::vcovHC(model, type = "HC3")
    }
    
    se <- sqrt(vcov_robust["treated", "treated"])
    
    # Compute confidence interval
    t_crit <- qt(1 - alpha/2, df = nrow(fire_year_full) - 2)
    ci_lower <- att - t_crit * se
    ci_upper <- att + t_crit * se
    ci_width <- ci_upper - ci_lower
    
    # Store results
    results_list[[length(results_list) + 1]] <- data.frame(
      year = yr,
      treatment_year = treatment_year,
      att = att,
      se = se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      ci_width = ci_width,
      n_treated = n_treated,
      n_control = n_control,
      stringsAsFactors = FALSE
    )
  }
  
  if (length(results_list) == 0) {
    warning("No ATT estimates computed")
    return(data.frame(
      year = integer(),
      treatment_year = integer(),
      att = numeric(),
      se = numeric(),
      ci_lower = numeric(),
      ci_upper = numeric(),
      ci_width = numeric(),
      n_treated = integer(),
      n_control = integer()
    ))
  }
  
  results_df <- do.call(rbind, results_list)
  
  cat("✓ Computed ATT for high-intensity fire (hifire95.frac) with", ifelse(cluster_by_unit, "clustered", "robust"), "standard errors\n")
  cat("  Post-treatment years:", paste(outcome_years, collapse = ", "), "\n")
  cat("  Mean CI width:", round(mean(results_df$ci_width, na.rm = TRUE), 4), "\n")
  return(results_df)
}
