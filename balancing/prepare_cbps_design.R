# Shared design preparation for CBPS fitting and diagnostics.

source("balancing/balancing_config.R")
get_diagnostics_config <- get("get_diagnostics_config", mode = "function")

#' Prepare design matrix for CBPS from raw df
#' Returns a list with elements: X (raw transformed), X.scl (robust standardized matrix), W (treatment vector)
prepare_cbps_design <- function(df, opts = list()) {
  # opts may supply defaults: default_winsor_p, block_winsor_map, troublesome_winsor_p
  cfg <- get_diagnostics_config()
  if (is.null(opts$default_winsor_p)) opts$default_winsor_p <- cfg$preprocessing$default_winsor_p
  if (is.null(opts$block_winsor_map)) opts$block_winsor_map <- list()
  if (is.null(opts$troublesome_winsor_p)) opts$troublesome_winsor_p <- 0.999

  W <- df$treated

  X <- df
  X$unit <- NULL
  X$LATITUDE <- NULL
  X$LONGITUDE <- NULL
  X$treated <- NULL
  X$num.fire <- NULL

  # Keep numeric columns only and drop zero-variance
  X <- X[, sapply(X, is.numeric), drop = FALSE]
  X <- X[, apply(X, 2, sd, na.rm = TRUE) > 0, drop = FALSE]

  # PRCP/SWE anomaly construction (monthly z-scores based on controls only)
  prcp_cols <- grep('^prcp_\\d{4}_\\d{1,2}$', colnames(X), value = TRUE)
  swe_cols <- grep('^swe_\\d{4}_\\d{1,2}$', colnames(X), value = TRUE)
  if (length(prcp_cols) > 0 || length(swe_cols) > 0) {
    get_month <- function(nm) as.integer(sub('^.*_(\\d{1,2})$', '\\1', nm))
    ctrl_idx <- W == 0

    compute_anoms <- function(cols) {
      if (length(cols) == 0) return(NULL)
      anom <- matrix(NA_real_, nrow = nrow(X), ncol = length(cols))
      colnames(anom) <- cols
      for (i in seq_along(cols)) {
        x <- X[[cols[i]]]
        base_vals <- x[ctrl_idx & !is.na(x)]
        mu <- mean(base_vals, na.rm = TRUE)
        sdv <- sd(base_vals, na.rm = TRUE)
        if (is.na(sdv) || sdv == 0) {
          anom[, i] <- 0
        } else {
          anom[, i] <- (x - mu) / sdv
        }
      }
      anom
    }

    sum_if_any <- function(mat, idx) {
      if (is.null(mat) || length(idx) == 0) return(rep(NA_real_, nrow(X)))
      rowSums(mat[, idx, drop = FALSE], na.rm = TRUE)
    }

    prcp_anom <- compute_anoms(prcp_cols)
    swe_anom <- compute_anoms(swe_cols)

    # Aggregate anomalies into water-year seasons
    if (!is.null(prcp_anom)) {
      prcp_months <- sapply(prcp_cols, get_month)
      X$prcp_coolwet_anom <- sum_if_any(prcp_anom, prcp_months %in% c(11, 12, 1, 2, 3))
      X$prcp_transition_anom <- sum_if_any(prcp_anom, prcp_months %in% c(4, 5))
      X$prcp_fireseason_anom <- sum_if_any(prcp_anom, prcp_months %in% c(6, 7, 8, 9, 10))

      prcp_mean <- rowMeans(prcp_anom, na.rm = TRUE)
      prcp_sd <- apply(prcp_anom, 1, sd, na.rm = TRUE)
      prcp_mean[is.nan(prcp_mean)] <- NA_real_
      prcp_sd[is.nan(prcp_sd)] <- NA_real_
      X$prcp_mean_annual <- prcp_mean
      X$prcp_sd_annual <- prcp_sd
    }

    if (!is.null(swe_anom)) {
      swe_months <- sapply(swe_cols, get_month)
      X$swe_accum_anom <- sum_if_any(swe_anom, swe_months %in% c(11, 12, 1, 2, 3))
      X$swe_melt_anom <- sum_if_any(swe_anom, swe_months %in% c(4, 5, 6, 7))
    }

    # Drop monthly PRCP/SWE columns after aggregation
    X <- X[, !colnames(X) %in% c(prcp_cols, swe_cols), drop = FALSE]
  }

  # avg_BRIGHTNESS drop when fire_ present
  fire_cols_init <- grep('^fire_', colnames(X), value = TRUE)
  if (length(fire_cols_init) > 0 && any(grepl('^avg_BRIGHTNESS_', colnames(X)))) {
    cols_to_drop <- grep('^avg_BRIGHTNESS_', colnames(X), value = TRUE)
    X <- X[, !colnames(X) %in% cols_to_drop, drop = FALSE]
  }

  # wvp log1p+winsor
  wvp_cols <- grep('^wvp_', colnames(X), value = TRUE)
  if (length(wvp_cols) > 0) {
    for (col in wvp_cols) {
      x <- X[[col]]
      if (all(is.na(x))) next
      x <- log1p(x)
      p995 <- quantile(x, 0.995, na.rm = TRUE)
      if (!is.na(p995)) x[x > p995] <- p995
      X[[col]] <- x
    }
  }

  # fire two-part
  fire_cols <- grep('^fire_', colnames(X), value = TRUE)
  if (length(fire_cols) > 0) {
    sparse_fire <- c(); cols_converted <- 0
    for (col in fire_cols) {
      p_one <- mean(X[[col]] > 0, na.rm = TRUE)
      if (!is.na(p_one) && p_one < 0.005) {
        sparse_fire <- c(sparse_fire, col)
      } else {
        x <- X[[col]]
        X[[paste0(col, '_present')]] <- as.numeric(x > 0)
        x_pos <- ifelse(x > 0, log1p(x), 0)
        pos_vals <- x_pos[x_pos > 0]
        if (length(pos_vals) > 0) {
          p995 <- quantile(pos_vals, 0.995, na.rm = TRUE)
          if (!is.na(p995)) x_pos[x_pos > p995] <- p995
        }
        X[[col]] <- x_pos
        cols_converted <- cols_converted + 1
      }
    }
    if (length(sparse_fire) > 0) X <- X[, !colnames(X) %in% sparse_fire]
  }

  # max_FRP two-part
  frp_cols <- grep('^max_FRP_', colnames(X), value = TRUE)
  if (length(frp_cols) > 0) {
    drop_frp <- c()
    for (col in frp_cols) {
      x <- X[[col]]
      if (all(is.na(x))) { drop_frp <- c(drop_frp, col); next }
      pct_zero <- sum(x == 0 | is.na(x), na.rm = TRUE) / length(x)
      if (pct_zero > 0.95) { drop_frp <- c(drop_frp, col); next }
      X[[paste0(col, '_present')]] <- as.numeric(x > 0)
      x_pos <- ifelse(x > 0, log1p(x), 0)
      pos_vals <- x_pos[x_pos > 0 & !is.na(x_pos)]
      if (length(pos_vals) > 0) {
        p_block <- if ('max_FRP_' %in% names(opts$block_winsor_map)) opts$block_winsor_map[['max_FRP_']] else NULL
        p <- if (!is.null(p_block)) p_block else opts$default_winsor_p
        q <- quantile(pos_vals, p, na.rm = TRUE)
        if (!is.na(q)) x_pos[x_pos > q] <- q
      }
      X[[col]] <- x_pos
    }
    if (length(drop_frp) > 0) X <- X[, !colnames(X) %in% drop_frp]
  }

  # remove near-constant
  near_constant_threshold <- 1e-10
  vars <- apply(X, 2, var, na.rm = TRUE)
  keep_variance <- vars >= near_constant_threshold
  X <- X[, keep_variance, drop = FALSE]

  # robust standardize median/MAD
  X.center <- apply(X, 2, median, na.rm = TRUE)
  X.scale <- apply(X, 2, function(z) { m <- mad(z, na.rm = TRUE); if (is.na(m) || m == 0) 1 else m })
  X.scl <- sweep(sweep(X, 2, X.center, "-"), 2, X.scale, "/")

  return(list(X = X, X.scl = as.matrix(X.scl), W = W))
}
