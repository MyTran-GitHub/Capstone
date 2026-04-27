message(Sys.time())
library(fst)
library("sf")
library("tidyverse")
library("mltools")
library("data.table")
library(sp)
library("parallel")
library("tigris")
library("EValue")
library("gridExtra")
library("scales")  # For alpha() function in maps
options(tigris_use_cache = TRUE)


args <- commandArgs(trailingOnly = TRUE)
EXPERIMENT_NAME <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "full_pool"
message("Using experiment: ", EXPERIMENT_NAME)

RUN_BALANCE_PLOTS <- TRUE
RUN_EVALUES <- TRUE

outDir <- "data/processed_data"
analysisDir <- file.path(outDir, "rev_analysis_low", EXPERIMENT_NAME)
resultDir <- file.path("data/outputs")

# Helper: prefer *_full.RDS weights if available (produced by selection diagnostics)
choose_weights_path <- function(analysis_dir, treated_year, area) {
  base <- file.path(analysis_dir, paste0("cbps_weights_", treated_year, "_", area, ".RDS"))
  full <- file.path(analysis_dir, paste0("cbps_weights_", treated_year, "_", area, "_full.RDS"))
  if (file.exists(full)) return(full)
  return(base)
}

dir.create(file.path(resultDir, "balance", EXPERIMENT_NAME), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(resultDir, "main_results", EXPERIMENT_NAME), recursive = TRUE, showWarnings = FALSE)
# Figure 3 covariate balance check
# Setting c(2006:2020) temporarily to match treated years with available covariate data; adjust as needed for other analyses/years
if (RUN_BALANCE_PLOTS) {
parameters <- expand.grid(c(2006:2020), c("conifer"))
# NOTE: Disturbance data (fire_disturb_, timber_, drought_, greening_, browning_) not included - only 9 covariates available
covariates <- c("minat_", "maxat_", "prcp_", "swe_", "wvp_", "fire_", "avg_BRIGHTNESS_", "max_FRP_", "tree_cover_")
cat("\nGenerating", nrow(parameters), "balance plots...\n")
for (year_area in seq_len(nrow(parameters))) {
  if (year_area %% 5 == 0) cat("  Plot", year_area, "/", nrow(parameters), "\n")
  #year_area <- 1
  treated.year = as.numeric(parameters[year_area, 1])
  area = as.character(parameters[year_area, 2])
  df = readRDS(file.path(analysisDir,
      paste0("analysis_treated", treated.year, "_", area, ".RDS")))
  
  if (is.null(df) || nrow(df) == 0) {
    cat("  ERROR: Empty analysis data for year", treated.year, "area", area, "\n")
    next
  }
  
  weights_path <- choose_weights_path(analysisDir, treated.year, area)
  weights = tryCatch(readRDS(weights_path), error = function(e) NULL)
  if (is.null(weights) || nrow(weights) == 0) {
    cat("  ERROR: Empty weights data for year", treated.year, "area", area, " (path:", weights_path, ")\n")
    next
  }

  # Ensure join keys are same type to avoid accidental NAs
  if ("unit" %in% names(df)) df$unit <- as.character(df$unit)
  if ("unit" %in% names(weights)) weights$unit <- as.character(weights$unit)

  df_weight <- merge(df, weights[, c("unit", "weight")], by = "unit", all.x = TRUE)
  na_count <- sum(is.na(df_weight$weight))
  if (na_count > 0) {
    if (grepl("_full\\.RDS$", weights_path)) {
      cat("  WARNING:", na_count, "units still missing weights for year", treated.year, "even after using full weights - replacing with 1\n")
    } else {
      cat("  WARNING:", na_count, "units missing weights for year", treated.year, "(using donor-only weights at:", weights_path, ") - replacing with 1\n")
    }
  }
  if (any(is.na(df_weight$weight))) {
    cat("  WARNING: Some weights are NA for year", treated.year, "- replacing with 1\n")
    df_weight$weight[is.na(df_weight$weight)] <- 1
  }
  
  post_bal <- NULL
  pre_bal <- NULL
  ncov = length(covariates)
  for (i in 1:(ncov)) {
    covariate = covariates[i]
    matching_cols <- grep(covariate, colnames(df_weight))
    
    if (length(matching_cols) == 0) {
      cat("  WARNING: No columns matching pattern '", covariate, "' for year", treated.year, "\n", sep="")
      post_bal[i] <- NA
      pre_bal[i] <- NA
      next
    }
    
    tryCatch({
      post_bal[i] <- (mean(rowMeans(subset(df_weight, treated == 1)[, matching_cols]))  - 
        sum(rowMeans(subset(df_weight, treated == 0)[, matching_cols]) * 
            subset(df_weight, treated == 0)$weight)/sum(subset(df_weight, treated == 0)$weight))/
        sd(rowMeans(subset(df_weight, treated == 1)[, matching_cols]))
      
      pre_bal[i] <- (mean(rowMeans(subset(df_weight, treated == 1)[, matching_cols]))  - 
        mean(rowMeans(subset(df_weight, treated == 0)[, matching_cols])))/
        sd(rowMeans(subset(df_weight, treated == 1)[, matching_cols]))
    }, error = function(e) {
      cat("  ERROR calculating balance for covariate '", covariate, "': ", e$message, "\n", sep="")
      post_bal[i] <<- NA
      pre_bal[i] <<- NA
    })
  }
  
      if (is.null(weights) || nrow(weights) == 0) {
        cat("  ERROR: Empty weights for map year", treated.year, " (path:", weights_path, ")\n")
        next
      }
  post_bal <- replace(post_bal, is.infinite(post_bal), 0)
  pre_bal <- replace(pre_bal, is.infinite(pre_bal), 0)
  post_bal[is.na(post_bal)] <- 0
  pre_bal[is.na(pre_bal)] <- 0
  
  order <- order(pre_bal, na.last = TRUE)
  balance <- data.frame(matrix(NA, nrow = ncov * 2, ncol = 0))
  balance$Covariates <- rep(1:ncov, 2)
  balance$SMD <- c(sort(pre_bal, na.last = TRUE), post_bal[order])
  balance$covariates_name <- rep(covariates[order], 2)
  balance$Scenarios <- c(rep("Unweighted: Orginal Data", ncov), rep("Weighted: Synthetic Control", ncov))
  balance$Scenarios <- factor(balance$Scenarios, levels = c("Unweighted: Orginal Data", "Weighted: Synthetic Control"))
  if (area == "forestland") {area = "forest"}
  balance_p <- ggplot(balance, aes(x=SMD, y=Covariates, colour=Scenarios)) + 
    scale_y_continuous(breaks = 1:ncov,
                       labels = c("Min air temperature", "Max air temperature", "Precipitation", "Snow water equivalent",
                                  "Water vapor pressure", "Fire frequency", "Avg fire brightness", "Max fire radiative power",
                                  "Vegetation: tree cover")[order]) +
    scale_color_manual(breaks = c("Unweighted: Orginal Data", "Weighted: Synthetic Control"),
                       values=c("red", "blue")) + 
    geom_point() +
    geom_path() +
    theme_bw() +
    theme(plot.margin = unit(c(0.2, 1, 0.2, 0.2), "lines"),
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          text = element_text(size = 16),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position="bottom",
          legend.box="vertical",
          axis.text.y = element_text(angle = 30, hjust = 1)) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  ggtitle(paste0(treated.year, ", ", str_to_title(area))) + 
    xlab("Standardized Mean Differences") +
    xlim(min(balance$SMD) - 0.01, max(balance$SMD) + 0.01)
  ggsave(file.path(resultDir, "balance", EXPERIMENT_NAME, paste0("Covariate_Balance" , treated.year, "across",  area, ".jpeg")), 
         balance_p, 
         width = 8.5 / 1.6,
         height = 11 / 1.6,
         units = "in")
}
} else { cat("\nSkipping balance plots (RUN_BALANCE_PLOTS=FALSE)\n") }

## Figure 3 California maps for exposed and control regions

if (RUN_MAPS) {

parameters <- expand.grid(c(2006:2020), c("conifer"))
cat("\nGenerating", nrow(parameters), "California maps...\n")
# Load CA boundary once (cache for reuse across all maps)
CA_bound = subset(states(cb = TRUE, resolution = "500k", year = 2020), STATEFP == "06")
CA_bound = st_transform(CA_bound, crs = 4326)

for (year_area in seq_len(nrow(parameters))) {
  if (year_area %% 5 == 0) cat("  Map", year_area, "/", nrow(parameters), "\n")
  #year_area <- 1
  treated.year = as.numeric(parameters[year_area, 1])
  area = as.character(parameters[year_area, 2])

  df = readRDS(file.path(outDir, "rev_analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS")))
  if (is.null(df) || nrow(df) == 0) {
    cat("  ERROR: Empty data for map year", treated.year, "\n")
    next
  }
  
  weights_path <- choose_weights_path(outDir, treated.year, area)
  weights = tryCatch(readRDS(weights_path), error = function(e) NULL)
  if (is.null(weights) || nrow(weights) == 0) {
    cat("  ERROR: Empty weights for map year", treated.year, " (path:", weights_path, ")\n")
    next
  }
  if ("unit" %in% names(df)) df$unit <- as.character(df$unit)
  if ("unit" %in% names(weights)) weights$unit <- as.character(weights$unit)
  df_weight <- merge(df, weights[, c("unit", "weight")], by = "unit", all.x = TRUE)
  na_count2 <- sum(is.na(df_weight$weight))
  if (na_count2 > 0) {
    if (grepl("_full\\.RDS$", weights_path)) {
      cat("  WARNING:", na_count2, "units still missing weights for map year", treated.year, "even after using full weights - replacing with 1\n")
    } else {
      cat("  WARNING:", na_count2, "units missing weights for map year", treated.year, "(using donor-only weights at:", weights_path, ") - replacing with 1\n")
    }
  }
  if (any(is.na(df_weight$weight))) {
    df_weight$weight[is.na(df_weight$weight)] <- 1
  }
  
  # Validate required columns for map
  if (!all(c("LATITUDE", "LONGITUDE", "weight") %in% colnames(df_weight))) {
    cat("  ERROR: Missing required columns (LATITUDE/LONGITUDE/weight) for year", treated.year, "\n")
    next
  }
  
  df_weight2 <- st_as_sf(df_weight[,c("LATITUDE", "LONGITUDE","weight")],
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = 4326,
                         remove = FALSE)
  df_weight2$logwt <- log(df_weight2$weight)
  df_weight2$logwt[df_weight2$logwt < quantile(df_weight2$logwt,0.1, na.rm = T)] <- quantile(df_weight2$logwt,0.1, na.rm = T)
  
  df_weight2$transparency <- (df_weight2$logwt- min(df_weight2$logwt))/(max(df_weight2$logwt)-min(df_weight2$logwt)) + 10^{-1}
  
  jpeg(file.path(resultDir, "maps", paste0("logweight_histfire", treated.year, "_", area, ".jpeg")),
       width = 8.5*150, height = 11*150, quality = 90, res = 150)
  par(mar=c(0.1,1,2,1))
  if (area == "forestland") {area = "forest"}
  plot(CA_bound$geometry, main = paste0(treated.year, ", ", str_to_title(area)), cex.main= 3)
  control = subset(df_weight2, weight != 1)[c("logwt","geometry","transparency")]
  plot(subset(df_weight2, weight != 1)[c("logwt","geometry")], pch=16, cex = 0.15, col = alpha("blue", control$transparency), add = TRUE)
  plot(subset(df_weight2, weight == 1)[c("logwt","geometry")], pch=16, cex = 0.2, col = "red", add = TRUE)
  legend("bottomleft", legend = c("Exposed", "Control"), fill = c("red", "blue"), cex=2)
  dev.off()
}
} else { cat("\nSkipping maps (RUN_MAPS=FALSE)\n") }

# Table 1 for fire frequency

if (RUN_FIRE_FREQ_TABLE) {

FIRMS_ca_grouped = readRDS(file.path(outDir, "FIRMS.RDS"))
FIRMS_ca_grouped$unit = paste0(FIRMS_ca_grouped$LATITUDE, FIRMS_ca_grouped$LONGITUDE)
st_geometry(FIRMS_ca_grouped) <- NULL
fire.df <- FIRMS_ca_grouped
fire.index = unique(fire.df[c("unit")])

fire.df_year <- fire.df %>% 
  group_by(LATITUDE, LONGITUDE, year) %>% 
  summarise(max_FRP = max(max_FRP), .groups = "drop")

# classify the fire types by max FRP for each years
# based on systems proposed in https://www.sciencedirect.com/science/article/abs/pii/S003442570800062X
fire.df_year$class <- 0 
fire.df_year[fire.df_year$max_FRP == 0,]$class <- 0
fire.df_year[0 < fire.df_year$max_FRP & fire.df_year$max_FRP < 100,]$class <- 1
fire.df_year[100 <= fire.df_year$max_FRP & fire.df_year$max_FRP < 500,]$class <- 2
fire.df_year[500 <= fire.df_year$max_FRP,]$class <- 3
fire.df_year$unit <- paste0(fire.df_year$LATITUDE, fire.df_year$LONGITUDE)

fveg_elev_grid_ca_poly <- readRDS(file.path(outDir, "fveg_elev_grid_ca_poly.RDS"))

st_geometry(fveg_elev_grid_ca_poly) <- NULL

results <- merge(fire.df_year, fveg_elev_grid_ca_poly, by = c("LATITUDE", "LONGITUDE"), all.x = TRUE)

results = results %>%
  filter(class > 0, conifer == 1)

df.list = lapply(2001:2020, function(treated.year) {
  tbl <- table(subset(results, year == treated.year)$class)
  round(as.vector(tbl) * 0.6390217, digits = 0)
})
df <- do.call(rbind, df.list)

write.csv(df, "fire_freq.csv", row.names = F)
} else { cat("\nSkipping fire frequency table (RUN_FIRE_FREQ_TABLE=FALSE)\n") }

### Table for covariate balance of indivudal covariates

if (RUN_BALANCE_SUMMARY) {

cat("\nGenerating covariate balance summary table...\n")
parameters <- expand.grid(c(2006:2020), c("conifer"))
balance_summary.df <- data.frame(area = rep(NA, nrow(parameters)),
                                 year = rep(NA, nrow(parameters)),
                                 pre = rep(NA, nrow(parameters)),
                                 post = rep(NA, nrow(parameters)))

for (year_area in seq_len(nrow(parameters))) {
  
treated.year = as.numeric(parameters[year_area, 1])
area = as.character(parameters[year_area, 2])
df = readRDS(file.path(outDir, "rev_analysis_low", paste0("analysis_treated", treated.year, "_", area, ".RDS")))

weights = readRDS(file.path(outDir, "rev_analysis_low", paste0("cbps_weights_", treated.year, "_", area, ".RDS")))
  if ("unit" %in% names(df)) df$unit <- as.character(df$unit)
  if ("unit" %in% names(weights)) weights$unit <- as.character(weights$unit)
  df_weight <- merge(df, weights[, c("unit", "weight")], by = "unit", all.x = TRUE)
  na_count3 <- sum(is.na(df_weight$weight))
  if (na_count3 > 0) cat("  WARNING:", na_count3, "units missing weights for summary year", treated.year, "- will replace with 1\n")

df_weight$unit <- NULL
df_weight$LATITUDE <- NULL
df_weight$LONGITUDE <- NULL
df_weight$num.fire <- NULL

df_weight <- df_weight[, -grep("V1_", colnames(df_weight))]
post_bal <- NULL
pre_bal <- NULL

# Keep only numeric columns except treated and weight (use integer indices for safety)
numeric_cols <- sapply(df_weight, is.numeric)
numeric_cols <- as.logical(numeric_cols)
numeric_cols[is.na(numeric_cols)] <- FALSE
numeric_cols[names(numeric_cols) %in% c("treated", "weight")] <- FALSE
numeric_idx <- which(numeric_cols)

if (length(numeric_idx) == 0) {
  next  # Skip if no numeric covariates
}

df_weight_numeric <- df_weight[, numeric_idx, drop = FALSE]

bal.list <- lapply(seq_len(ncol(df_weight_numeric)), function(i) {
  tryCatch({
    c((mean(subset(df_weight, treated == 1)[[names(df_weight_numeric)[i]]])  - 
         sum(subset(df_weight, treated == 0)[[names(df_weight_numeric)[i]]] * 
               subset(df_weight, treated == 0)$weight) / sum(subset(df_weight, treated == 0)$weight)) /
        sd(subset(df_weight, treated == 1)[[names(df_weight_numeric)[i]]]),
      (mean(subset(df_weight, treated == 1)[[names(df_weight_numeric)[i]]])  - 
         mean(subset(df_weight, treated == 0)[[names(df_weight_numeric)[i]]])) /
        sd(subset(df_weight, treated == 1)[[names(df_weight_numeric)[i]]]))
  }, error = function(e) c(NA, NA))
})
bal = do.call("rbind", bal.list)

balance_summary.df$area[year_area] = area
balance_summary.df$year[year_area] = treated.year
balance_summary.df$pre[year_area] = paste0(sum(bal[,2] > 0.1, na.rm = T), " (", round(mean(bal[,2] > 0.1, na.rm = T)*100, digits=2), ")")
balance_summary.df$post[year_area] = paste0(sum(bal[,1] > 0.1, na.rm = T), " (", round(mean(bal[,1] > 0.1, na.rm = T)*100, digits=2), ")")
}
write.csv(balance_summary.df, "balance_summary_frp.csv")
} else { cat("\nSkipping balance summary table (RUN_BALANCE_SUMMARY=FALSE)\n") }

## Figure for E-values

if (RUN_EVALUES) {

start_years <- c(2006, 2008, 2010)  # Match years from weighted_outcome_analysis.R

for (start_year in start_years) {
  cat("\n=== Processing start year:", start_year, "===\n")
  res <- list()
  k = 1  
  for (outcome in c("fire_all", "fire_90", "fire_95")) {
    cat("  Processing outcome:", outcome, "\n")
    for (biome in c("conifer")) {
      # Check if data files exist
      test_file <- file.path(outDir, "rev_result_low", as.character(start_year), 
                            paste0(biome, "_lag1.csv"))
      if (!file.exists(test_file)) {
        cat("    WARNING: Data files not found for start_year", start_year, "- skipping\n")
        next
      }
      
      data.raw = tryCatch({
        Reduce(rbind, lapply(1:9, function(ll) {
          lag_file <- file.path(outDir, "rev_result_low", as.character(start_year),
                                paste0(biome, "_lag", ll, ".csv"))
          if (!file.exists(lag_file)) {
            cat("    ERROR: Missing lag file:", lag_file, "\n")
            return(NULL)
          }
          XX = read.csv(lag_file)
          XX$lag = ll
          XX
        }))
      }, error = function(e) {
        cat("    ERROR reading lag files for start_year", start_year, ":", e$message, "\n")
        NULL
      })
      
      if (is.null(data.raw) || nrow(data.raw) == 0) {
        cat("    ERROR: No valid data loaded for start_year", start_year, "outcome", outcome, "\n")
        next
      }
      
      # Validate required columns
      required_cols <- c("fire.0", "fire.1", "hifire90.0", "hifire90.1", "hifire95.0", "hifire95.1", "pixels_burn")
      missing_cols <- setdiff(required_cols, colnames(data.raw))
      if (length(missing_cols) > 0) {
        cat("    ERROR: Missing required columns in lag data:", paste(missing_cols, collapse=", "), "\n")
        cat("    Available columns:", paste(colnames(data.raw), collapse=", "), "\n")
        next
      }
      
      names(data.raw)[1] = "year"
      data.raw$end.year = data.raw$year + data.raw$lag
      
      data.reg = data.raw[c("lag", "year", "end.year")]
      data.reg = rbind(data.reg, data.reg)
      data.reg$treat = c(rep(0, nrow(data.raw)), rep(1, nrow(data.raw)))
      data.reg$fire_all = c(data.raw$fire.0, data.raw$fire.1) * data.raw$pixels_burn
      data.reg$fire_90 = c(data.raw$hifire90.0, data.raw$hifire90.1) * data.raw$pixels_burn
      data.reg$fire_95 = c(data.raw$hifire95.0, data.raw$hifire95.1) * data.raw$pixels_burn
      
      # FIXED TIME WINDOW: Use same end-years for all start_year analyses
      # Constrained by: covariate data ends 2020 + max lag 9 → max year is 2020+1=2021
      # All analyses use 2009-2021 window for consistency/comparability
      all.end.years = 2009:2021
      all.lags = 1:9
    
    jackfun = function(end.years) {
      #regform = paste0(outcome, " ~ factor(end.year) * factor(lag) + treat + treat:poly(lag, 3)")
      #reg.jack = glm(formula(regform),
      #            family = quasipoisson,
      #            data = subset(data.reg, end.year %in% end.years))
      regform = paste0(outcome, " ~ factor(end.year) * factor(lag) + treat + treat:poly(lag, 1)")
      #regform = paste0(outcome, " ~ poly(end.year,1) * poly(lag, 1) + treat + treat:poly(lag, 1)")
      reg.jack = glm(formula(regform),
                     family = quasipoisson,
                     data = subset(data.reg, end.year %in% end.years))
      
      Xpred0 = data.frame(end.year=end.years[1], lag=all.lags, lag2=all.lags^2, treat=0)
      Xpred1 = data.frame(end.year=end.years[1], lag=all.lags, lag2=all.lags^2, treat=1)
      
      yy0 = predict(reg.jack, Xpred0)
      yy1 = predict(reg.jack, Xpred1)
      
      yy1 - yy0
    }
    
    full.reg = jackfun(all.end.years)
    
    cat("    Running jackknife resampling (", length(all.end.years), "iterations)...\n")
    jackreps = t(sapply(seq_along(all.end.years), function(ii) { 
      if (ii %% 5 == 0) cat("      Iteration", ii, "/", length(all.end.years), "\n")
      jackfun(all.end.years[-ii]) 
    }))
    colnames(jackreps) = all.lags
    
    jackvar = apply(jackreps, 2, function(xx) { var(xx) * (length(xx) - 1)^2 / length(xx) })
    jackse = sqrt(jackvar)
    
    rat = exp(full.reg)
    ub.rat = exp(full.reg + 1.96 * jackse)
    lb.rat = exp(full.reg - 1.96 * jackse)
    results <-  data.frame(year = seq_along(rat), rate = rat, lower = lb.rat, upper = ub.rat, "land_type" = rep(str_to_title(biome), length(rat)))
    
    if (outcome == "fire_all") {fire_type <- "all fires"} else 
      if (outcome == "fire_90") {fire_type <- "class 2-5 fires"} else 
        if (outcome == "fire_95") {fire_type <- "class 3-5 fires"} 
    if (biome == "forestland") {biome = "forest"}
    
    results_eval <- cbind(results[,c(1:2,5)], t(sapply(seq_len(nrow(results)), function(i) {
      tryCatch({
        evalue_result <- evalues.RR(est = results[i,2], lo = results[i,3], hi = results[i,4])
        if (is.null(evalue_result) || nrow(evalue_result) < 2) {
          c(NA, NA)  # Return NA if evalues.RR fails
        } else {
          evalue_result[2,]
        }
      }, error = function(e) {
        cat("      WARNING: evalues.RR failed for lag", i, ":", e$message, "\n")
        c(NA, NA)
      })
    })))
    
    results_eval2 = results_eval[,c(1,6)]
    colnames(results_eval2) <- colnames(results_eval[,c(1,4)])
    results_eval2 = rbind(results_eval2, results_eval[,c(1,4)])
    results_eval2$Types <- c(rep("95% CI upper limit", nrow(results_eval)),rep("Main Effect", nrow(results_eval)))
    results_eval2$point[is.na(results_eval2$point)] <- 1
    
      res[[k]] =   ggplot(data = results_eval2, aes(x = year, y = point, color = Types)) +
      
        geom_line(aes(x= year, y= point, color = Types), linetype="dashed" , lwd=2) +
        geom_hline(yintercept=1) +
        scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10)) +
        labs(color = "E-value types") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 24),
              legend.position="bottom",
              text = element_text(size=24),
              axis.title.x = element_text(size = 24),
              axis.title.y = element_text(size = 24)) +
        ggtitle(paste0("Effect on ", fire_type, ", ", str_to_title(biome), " (", start_year, ")")) + 
        xlab("Years since fire") +
        ylab("E-values") +
        coord_cartesian(ylim = c(0.9, 5))

  k = k + 1
    }
  }
  
  # Only save if we have all 3 plots
  if (length(res) == 3) {
    cat("  Combining and saving plots for start_year", start_year, "\n")
    res_combined <- grid.arrange(res[[1]], res[[2]], res[[3]],
                                 nrow = 3)
    
    ggsave(file.path(resultDir, "main_results", paste0("evalue_combined_rev_linear_", start_year, ".jpeg")), 
           res_combined, 
           width = 14 / 1.6,
           height = 8.5 / 1.6*3,
           units = "in")
  } else {
    cat("  WARNING: Only generated", length(res), "plots for start_year", start_year, "- skipping combined plot\n")
  }
}
} else { cat("\nSkipping E-values (RUN_EVALUES=FALSE)\n") }

cat("\n=== Figures.R completed ===")
message(Sys.time())