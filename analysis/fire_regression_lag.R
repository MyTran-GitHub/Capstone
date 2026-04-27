## Cohort-pooled estimator for fire regression lag analysis
## This script computes pooled estimates for fire outcomes across cohorts and lags.
## Required packages must be installed beforehand (see REPRODUCE.md).
required_packages <- c("grid", "pBrackets", "gridExtra", "Hmisc", "ggplot2")
missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(sprintf("Missing required R packages: %s. Install them via install.packages() or use the conda env provided in env/.", paste(missing_pkgs, collapse = ", ")))
}
library(ggplot2)
library(grid)
library(pBrackets)
library(gridExtra)
library(Hmisc)

run_fire_regression_lag <- function(outDir = "data/processed_data", resDir = "data/outputs", biome = "conifer") {

  # ---------------------------------------
  # SETTINGS
  # ---------------------------------------

  windows <- list(
    "2008-2020"  = 2008:2020,
    "2006-2020" = 2006:2020,
    "2010-2020" = 2010:2020
  )

  outcomes <- c("fire_all", "fire_90", "fire_95")

  all.lags <- 1:9
  all.end.years <- 2009:2021

  # ---------------------------------------
  # FUNCTION: Cohort-pooled estimator
  # ---------------------------------------

  compute_results <- function(focal.years, outcome, ci_type = "two") {

    # Identify which cohort folder to use
    cohort_start <- min(focal.years)

    data.raw <- Reduce(rbind, lapply(1:9, function(ll) {
      file_path <- file.path(
        outDir,
        "rev_result_low",
        as.character(cohort_start),
        paste0(biome, "_lag", ll, ".csv")
      )

      XX <- read.csv(file_path)
      XX$lag <- ll
      XX
    }))

    names(data.raw)[1] <- "year"
    data.raw$end.year <- data.raw$year + data.raw$lag

    data.raw <- subset(data.raw, year %in% focal.years)

    data.reg <- rbind(
      transform(data.raw, treat = 0),
      transform(data.raw, treat = 1)
    )

    data.reg$fire_all <- c(data.raw$fire.0, data.raw$fire.1) * data.raw$pixels_burn
    data.reg$fire_90  <- c(data.raw$hifire90.0, data.raw$hifire90.1) * data.raw$pixels_burn
    data.reg$fire_95  <- c(data.raw$hifire95.0, data.raw$hifire95.1) * data.raw$pixels_burn

    raw.plot <- expand.grid(Year = all.end.years, Lag = all.lags)
    raw.plot$ratio.estimte <- NA
    raw.plot$Baseline <- NA

    for (i in 1:nrow(raw.plot)) {
      YYY <- raw.plot$Year[i]
      LLL <- raw.plot$Lag[i]

      DDD <- subset(data.reg, end.year == YYY & lag == LLL)
      if (nrow(DDD) != 0) {
        raw.plot$ratio.estimte[i] <-
          DDD[DDD$treat == 1, outcome] /
          DDD[DDD$treat == 0, outcome]

        raw.plot$Baseline[i] <- DDD[DDD$treat == 0, outcome]
      }
    }

    raw.plot <- subset(raw.plot, !is.na(ratio.estimte))
    raw.plot$Year <- factor(raw.plot$Year)

    jackfun <- function(end.years) {
      jack.data <- subset(raw.plot, Year %in% end.years)
      reg.jack <- glm(ratio.estimte ~ Lag,
                      family = quasipoisson,
                      weights = Baseline,
                      data = jack.data)
      coef(reg.jack)[1] + all.lags * coef(reg.jack)[2]
    }

    full.reg <- jackfun(unique(raw.plot$Year))
    jackreps <- t(sapply(unique(raw.plot$Year), function(yy) {
      jackfun(setdiff(unique(raw.plot$Year), yy))
    }))

    colnames(jackreps) <- all.lags

    jackvar <- apply(jackreps, 2,
                     function(xx) var(xx) * (length(xx) - 1)^2 / length(xx))

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

  # ---------------------------------------
  # FUNCTION: Build 3x3 Grid
  # ---------------------------------------

  build_grid <- function(ci_type = "two") {
    plot_list <- list()
    k <- 1
    for (w_name in names(windows)) {
      focal.years <- windows[[w_name]]
      for (outcome in outcomes) {
        results <- compute_results(focal.years, outcome, ci_type)
        fire_label <- switch(outcome,
                             fire_all = "All fires",
                             fire_90  = "Class 2 fires",
                             fire_95  = "Class 3 fires")
        plot_list[[k]] <-
          ggplot(results, aes(x = year, y = rate)) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.3) +
          geom_line(size = 1.1) +
          geom_line(aes(y = upper), linetype = "dashed", size = 1) +
          geom_line(aes(y = lower), linetype = "dashed", size = 1) +
          geom_hline(yintercept = 1) + theme_bw() +
          ggtitle(paste0(w_name, " | ", fire_label)) +
          xlab("Years since fire") + ylab("Relative risk") + coord_cartesian(ylim = c(0, 1.8))
        k <- k + 1
      }
    }
    grid.arrange(grobs = plot_list, nrow = 3, ncol = 3)
  }

  # Generate figures
  grid_two <- build_grid(ci_type = "two")
  ggsave(file.path(resDir, "results", "conifer_all_windows_two_sided.jpeg"), grid_two, width = 15, height = 12, units = "in")
  grid_one <- build_grid(ci_type = "one")
  ggsave(file.path(resDir, "results", "conifer_all_windows_one_sided.jpeg"), grid_one, width = 15, height = 12, units = "in")
  invisible(TRUE)
}

# If executed directly, run and surface errors
if (!interactive()) {
  tryCatch(
    {
      run_fire_regression_lag()
    },
    error = function(e) {
      message("[ERROR] fire_regression_lag failed: ", conditionMessage(e))
      try({ tb <- utils::capture.output(traceback()); if (length(tb) > 0) for (ln in tb) message(ln) }, silent = TRUE)
      quit(save = "no", status = 1, runLast = FALSE)
    }
  )
}

