#!/usr/bin/env Rscript
# Distributional balance diagnostics (mirrors Figures.R aggregation)
# Usage: Rscript diagnostics/diagnostics_scripts/overlap/distributional_balance_plots.R <year> <area> [out_dir]

args <- commandArgs(trailingOnly = TRUE)

parse_flag_value <- function(args, flag, default = NULL) {
  flag_eq <- paste0(flag, "=")
  hit_eq <- args[startsWith(args, flag_eq)]
  if (length(hit_eq) > 0) return(sub(flag_eq, "", hit_eq[1], fixed = TRUE))
  idx <- which(args == flag)
  if (length(idx) > 0 && idx[1] < length(args)) return(args[idx[1] + 1])
  default
}
out_dir <- parse_flag_value(args, "--out-dir", "diagnostics/diagnostics_results/covariate_distribution_balance")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
year <- as.integer(parse_flag_value(args, "--year", NA))
area <- parse_flag_value(args, "--area", "conifer")
max_controls <- as.integer(parse_flag_value(args, "--max-controls", 25000))
ecdf_grid_n <- as.integer(parse_flag_value(args, "--ecdf-grid-n", 200))

positional <- args[!startsWith(args, "--")]
if (is.na(year) && length(positional) >= 1) year <- as.integer(positional[1])
if (is.na(area) && length(positional) >= 2) area <- positional[2]
if (is.na(year) || is.na(area) || !nzchar(area)) stop("Usage: --year <year> --area <area> [--out-dir <dir>] or positional <year> <area>")


suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("scales", quietly = TRUE)) stop("Package 'scales' is required.")
  library(dplyr)
  library(ggplot2)
  library(scales)
})

data_file <- file.path("data/processed_data/rev_analysis_low", paste0("analysis_treated", year, "_", area, ".RDS"))
weights_file <- file.path("data/processed_data/rev_analysis_low", paste0("cbps_weights_", year, "_", area, ".RDS"))
if (!file.exists(data_file)) stop("Data file not found: ", data_file)
if (!file.exists(weights_file)) stop("Weights file not found: ", weights_file)

df <- readRDS(data_file)
w <- readRDS(weights_file)

# Merge weights
dfw <- df %>% left_join(w %>% select(unit, weight), by = "unit")
# treated rows usually have weight==1; fill NAs conservatively
dfw$weight[is.na(dfw$weight) & dfw$treated == 1] <- 1
dfw$weight[is.na(dfw$weight)] <- 0

# Optional control downsampling to keep runtime predictable on very large pools.
if (is.finite(max_controls) && max_controls > 0) {
  ctrl_idx <- which(dfw$treated == 0)
  if (length(ctrl_idx) > max_controls) {
    set.seed(42)
    keep_ctrl <- sample(ctrl_idx, max_controls)
    keep_idx <- sort(c(which(dfw$treated == 1), keep_ctrl))
    dfw <- dfw[keep_idx, , drop = FALSE]
  }
}

# Covariate groups (same as Figures.R)
covariates <- c("minat_", "maxat_", "prcp_", "swe_", "wvp_", "fire_", "avg_BRIGHTNESS_", "max_FRP_", "tree_cover_")

# Helpers: aggregate group to per-unit scalar (rowMean for monthly groups)
aggregate_group <- function(df, pattern) {
  cols <- grep(pattern, colnames(df), value = TRUE)
  if (length(cols) == 0) return(NULL)
  vals <- rowMeans(df[, cols, drop = FALSE], na.rm = TRUE)
  vals[is.nan(vals)] <- NA_real_
  return(vals)
}

# Weighted empirical CDF builder
weighted_ecdf_fun <- function(x, w = NULL) {
  if (is.null(w)) {
    x <- x[is.finite(x)]
    if (length(x) == 0) {
      return(function(v) rep(NA_real_, length(v)))
    }
    f <- ecdf(x)
    return(function(v) f(v))
  }

  n <- min(length(x), length(w))
  if (n == 0) {
    return(function(v) rep(NA_real_, length(v)))
  }
  x <- x[seq_len(n)]
  w <- w[seq_len(n)]

  keep <- is.finite(x) & is.finite(w)
  x <- x[keep]
  w <- w[keep]
  if (length(x) == 0) {
    return(function(v) rep(NA_real_, length(v)))
  }

  w[is.na(w) | w < 0] <- 0
  if (sum(w) == 0) {
    f <- ecdf(x)
    return(function(v) f(v))
  }
  ord <- order(x)
  x_s <- x[ord]
  w_s <- w[ord]
  cw <- cumsum(w_s) / sum(w_s)
  function(v) {
    sapply(v, function(vv) {
      idx <- max(which(x_s <= vv), 0)
      if (idx == 0) return(0)
      return(cw[idx])
    })
  }
}

# Weighted KS (sup norm between two weighted ECDFs)
weighted_ks <- function(x1, w1 = NULL, x2, w2 = NULL) {
  x1 <- x1[is.finite(x1)]
  x2 <- x2[is.finite(x2)]
  if (length(x1) == 0 || length(x2) == 0) return(NA_real_)
  f1 <- weighted_ecdf_fun(x1, w1)
  f2 <- weighted_ecdf_fun(x2, w2)
  grid <- sort(unique(c(x1, x2)))
  d <- abs(f1(grid) - f2(grid))
  return(max(d, na.rm = TRUE))
}

# 1D Wasserstein (L1 distance between CDFs)
wasserstein_1d <- function(x1, w1 = NULL, x2, w2 = NULL) {
  x1 <- x1[is.finite(x1)]
  x2 <- x2[is.finite(x2)]
  if (length(x1) == 0 || length(x2) == 0) return(NA_real_)
  f1 <- weighted_ecdf_fun(x1, w1)
  f2 <- weighted_ecdf_fun(x2, w2)
  grid <- sort(unique(c(x1, x2)))
  Fd <- abs(f1(grid) - f2(grid))
  dx <- c(diff(grid), 0)
  return(sum(Fd * dx, na.rm = TRUE))
}

metrics <- list()
ecdf_curves <- list()
for (pat in covariates) {
  vals <- aggregate_group(dfw, pat)
  if (is.null(vals)) next
  treat_mask <- dfw$treated == 1
  x_t_all <- vals[treat_mask]
  x_c_all <- vals[!treat_mask]
  w_c_all <- dfw$weight[!treat_mask]

  keep_t <- is.finite(x_t_all)
  keep_c <- is.finite(x_c_all)
  x_t <- x_t_all[keep_t]
  x_c <- x_c_all[keep_c]
  w_c <- w_c_all[keep_c]

  if (length(x_t) == 0 || length(x_c) == 0) next

  sd_t <- sd(x_t, na.rm = TRUE)
  if (!is.finite(sd_t) || sd_t <= 0) {
    smd_un <- NA_real_
  } else {
    smd_un <- (mean(x_t, na.rm = TRUE) - mean(x_c, na.rm = TRUE)) / sd_t
  }
  if (sum(w_c, na.rm = TRUE) > 0) {
    mean_c_w <- sum(x_c * w_c, na.rm = TRUE) / sum(w_c, na.rm = TRUE)
  } else mean_c_w <- mean(x_c, na.rm = TRUE)
  if (!is.finite(sd_t) || sd_t <= 0) {
    smd_w <- NA_real_
  } else {
    smd_w <- (mean(x_t, na.rm = TRUE) - mean_c_w) / sd_t
  }

  ks_un <- weighted_ks(x_t, NULL, x_c, NULL)
  ks_w  <- weighted_ks(x_t, NULL, x_c, w_c)
  wass  <- wasserstein_1d(x_t, NULL, x_c, w_c)

  probs <- seq(0.01, 0.99, by = 0.01)
  qt_t <- quantile(x_t, probs = probs, na.rm = TRUE)
  wt_c <- ifelse(is.na(w_c) | w_c < 0, 0, w_c)
  if (sum(wt_c) == 0) qt_c_w <- quantile(x_c, probs = probs, na.rm = TRUE) else {
    ord <- order(x_c)
    xc <- x_c[ord]; wc <- wt_c[ord]
    cw <- cumsum(wc) / sum(wc)
    qt_c_w <- sapply(probs, function(p) xc[which.max(cw >= p)])
  }
  qdiff_med <- median(qt_t - qt_c_w, na.rm = TRUE)

  metrics[[pat]] <- data.frame(pattern = pat, smd_un = smd_un, smd_w = smd_w,
                               ks_un = ks_un, ks_w = ks_w, wass = wass, qdiff_med = qdiff_med,
                               stringsAsFactors = FALSE)

  # Lightweight faceted ECDF curves (single output file).
  lo <- suppressWarnings(stats::quantile(c(x_t, x_c), probs = 0.01, na.rm = TRUE, names = FALSE))
  hi <- suppressWarnings(stats::quantile(c(x_t, x_c), probs = 0.99, na.rm = TRUE, names = FALSE))
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    lo <- min(c(x_t, x_c), na.rm = TRUE)
    hi <- max(c(x_t, x_c), na.rm = TRUE)
  }
  if (is.finite(lo) && is.finite(hi) && hi > lo) {
    grid <- seq(lo, hi, length.out = max(50, ecdf_grid_n))
    f_t <- weighted_ecdf_fun(x_t, NULL)
    f_cw <- weighted_ecdf_fun(x_c, w_c)
    ecdf_curves[[pat]] <- data.frame(
      x = rep(grid, 2),
      cdf = c(f_cw(grid), f_t(grid)),
      group = rep(c("Control (weighted)", "Treated"), each = length(grid)),
      pattern = pat,
      stringsAsFactors = FALSE
    )
  }
}

if (length(metrics) == 0) {
  stop("No distributional balance metrics were produced.")
}
metrics_df <- do.call(rbind, metrics)
metrics_df$year <- year
metrics_df$area <- area
metrics_df$ks_reduction_pct <- ifelse(
  is.finite(metrics_df$ks_un) & metrics_df$ks_un > 0,
  100 * (metrics_df$ks_un - metrics_df$ks_w) / metrics_df$ks_un,
  NA_real_
)
metrics_df$abs_smd_reduction_pct <- ifelse(
  is.finite(metrics_df$smd_un) & abs(metrics_df$smd_un) > 0,
  100 * (abs(metrics_df$smd_un) - abs(metrics_df$smd_w)) / abs(metrics_df$smd_un),
  NA_real_
)
metrics_df <- metrics_df[, c(
  "year", "area", "pattern", "smd_un", "smd_w", "ks_un", "ks_w",
  "ks_reduction_pct", "abs_smd_reduction_pct", "wass", "qdiff_med"
)]
out_file <- file.path(out_dir, paste0("distributional_balance_metrics_", year, "_", area, ".csv"))
write.csv(metrics_df, out_file, row.names = FALSE)

if (length(ecdf_curves) > 0) {
  ecdf_df <- do.call(rbind, ecdf_curves)
  p_fac <- ggplot(ecdf_df, aes(x = x, y = cdf, colour = group)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~ pattern, scales = "free_x", ncol = 3) +
    theme_minimal(base_size = 11) +
    labs(
      title = paste0("Distributional Balance (Weighted Control vs Treated): ", year, " ", area),
      subtitle = "Faceted ECDFs by covariate family",
      x = "Aggregated covariate value",
      y = "CDF"
    )
  fac_file <- file.path(out_dir, paste0("distributional_balance_ecdf_faceted_", year, "_", area, ".png"))
  ggsave(fac_file, p_fac, width = 13, height = 9, dpi = 220)
}

cat("Saved distributional diagnostics to", out_dir, "\n")
cat("  - metrics:", out_file, "\n")
if (length(ecdf_curves) > 0) {
  cat("  - plot:", file.path(out_dir, paste0("distributional_balance_ecdf_faceted_", year, "_", area, ".png")), "\n")
}