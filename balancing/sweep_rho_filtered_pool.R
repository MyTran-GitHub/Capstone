#!/usr/bin/env Rscript
# Usage: Rscript balancing/sweep_rho_filtered_pool.R <year> <selected_units_csv|weights_rds> <out_csv> [max_controls] [sampling_method]
# sampling_method: "random" (default) or "prob" (prob-weighted using provided weights)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) stop("Usage: <year> <selected_units_csv|weights_rds> <out_csv> [max_controls] [sampling_method]")
year <- as.integer(args[1]); sel_path <- args[2]; out_csv <- args[3]

suppressPackageStartupMessages({
  library(dplyr)
})

if (!interactive()) {
    options(error = function() {
        tb <- utils::capture.output(traceback())
        if (length(tb) > 0) for (ln in tb) message(ln)
        quit(save = "no", status = 1, runLast = FALSE)
    })
}

source("balancing/cbps_ATT.R")    # must exist
# Load and prepare filtered pool (same transforms as run_cbps... simplified)
df <- readRDS(sprintf("data/processed_data/rev_analysis_low/analysis_treated%d_conifer.RDS", year))
# Support either a CSV of selected units or an RDS of weights (e.g. cbps_weights_2019_conifer.RDS)
sampling_method <- if (length(args) >= 5) tolower(args[5]) else "random"

# Read selector: if RDS contains weights, capture them for prob sampling
sel_weights_df <- NULL
if (grepl("\\.rds$", sel_path, ignore.case = TRUE)) {
    sel_obj <- readRDS(sel_path)
    if (is.data.frame(sel_obj) && "unit" %in% names(sel_obj)) {
        sel_units <- sel_obj$unit
        if ("weight" %in% names(sel_obj)) sel_weights_df <- sel_obj[, c("unit", "weight")]
    } else if (is.list(sel_obj) && !is.null(sel_obj$unit)) {
        sel_units <- sel_obj$unit
        if (!is.null(sel_obj$weight)) sel_weights_df <- data.frame(unit = sel_obj$unit, weight = sel_obj$weight)
    } else {
        stop("RDS file did not contain a data.frame/list with a 'unit' column")
    }
} else {
    sel_csv <- read.csv(sel_path, stringsAsFactors = FALSE)
    if (!"unit" %in% names(sel_csv)) stop("Selected units CSV must have a 'unit' column")
    sel_units <- sel_csv$unit
    if ("weight" %in% names(sel_csv)) sel_weights_df <- sel_csv[, c("unit", "weight")]
}

df_filtered <- df %>% filter(treated == 1 | unit %in% sel_units)
if (nrow(df_filtered) == 0) stop("No filtered data")

# Optional: limit controls for memory-constrained exploratory sweeps
# Usage: pass optional 4th arg as max_controls (e.g. 10000). Default = 10000.
max_controls <- if (length(args) >= 4) as.integer(args[4]) else 10000
n_ctrl_total <- sum(df_filtered$treated == 0)
if (!is.na(max_controls) && n_ctrl_total > max_controls) {
    set.seed(1)
    ctrl_units <- df_filtered$unit[df_filtered$treated == 0]
    if (sampling_method == "prob" && !is.null(sel_weights_df)) {
        # merge weights to control units
        weight_map <- sel_weights_df
        weight_map$weight <- as.numeric(weight_map$weight)
        prob_df <- data.frame(unit = ctrl_units, stringsAsFactors = FALSE) %>%
            left_join(weight_map, by = "unit")
        probs <- prob_df$weight
        # handle missing/zero weights
        if (all(is.na(probs) | probs <= 0)) {
            message("⚠ WARNING: no positive weights found for prob sampling; falling back to uniform random sampling")
            keep_ctrl <- sample(ctrl_units, max_controls)
        } else {
            probs[is.na(probs) | probs < 0] <- 0
            # add tiny jitter to avoid all zeros
            probs <- probs + .Machine$double.eps
            probs <- probs / sum(probs)
            keep_ctrl <- sample(ctrl_units, max_controls, prob = probs)
            message("⚙ Prob-weighted sampling using provided weights")
        }
    } else {
        keep_ctrl <- sample(ctrl_units, max_controls)
        if (sampling_method == "prob" && is.null(sel_weights_df)) {
            message("⚠ WARNING: requested prob sampling but no weights found in selector; used uniform random instead")
        }
    }
    df_filtered <- df_filtered %>% filter(treated == 1 | unit %in% keep_ctrl)
    message("⚠ NOTE: Sampled", max_controls, "controls from", n_ctrl_total, "for memory-limited sweep")
}

W <- df_filtered$treated
X <- df_filtered
X$unit <- NULL; X$LATITUDE <- NULL; X$LONGITUDE <- NULL; X$treated <- NULL; X$num.fire <- NULL
X <- X[, sapply(X, is.numeric), drop = FALSE]
X <- X[, apply(X, 2, sd, na.rm = TRUE) > 0, drop = FALSE]

# Apply same basic preprocessing as run_cbps (two-part SWE + winsorize) if needed.
# Two-part SWE: presence indicator + log-intensity (winsorized)
swe_cols <- grep("^swe_", colnames(X), value = TRUE)
if (length(swe_cols) > 0) {
cols_to_remove <- c()
cols_converted <- 0

for (col in swe_cols) {
    x <- X[[col]]
    pct_zero <- sum(x == 0 | is.na(x), na.rm = TRUE) / length(x)

    # If >95% zero, remove entirely (no discriminatory power for covariate balance)
    if (pct_zero > 0.95) {
    cols_to_remove <- c(cols_to_remove, col)
    } else {
    X[[paste0(col, "_present")]] <- as.numeric(x > 0)

    x_pos <- ifelse(x > 0, log1p(x), 0)
    pos_vals <- x_pos[x_pos > 0]
    if (length(pos_vals) > 0) {
        p995 <- quantile(pos_vals, 0.995, na.rm = TRUE)
        if (!is.na(p995)) {
        x_pos[x_pos > p995] <- p995
        }
    }

    X[[col]] <- x_pos
    cols_converted <- cols_converted + 1
    }
}

if (length(cols_to_remove) > 0) {
    X <- X[, !colnames(X) %in% cols_to_remove]
    message("  Removed", length(cols_to_remove), "sparse SWE columns (>95% zero)")
}

if (cols_converted > 0) {
    message("  SWE two-part: converted", cols_converted, "columns (presence + log-intensity)")
}
}

# Log1p + winsorize max_FRP_* to preserve intensity ordering and reduce tail risk
frp_cols <- grep("^max_FRP_", colnames(X), value = TRUE)
if (length(frp_cols) > 0) {
for (col in frp_cols) {
    x <- X[[col]]
    if (all(is.na(x))) {
    next
    }
    x <- log1p(x)
    p995 <- quantile(x, 0.995, na.rm = TRUE)
    if (!is.na(p995)) {
    x[x > p995] <- p995
    }
    X[[col]] <- x
}
message("  Log+winsorized", length(frp_cols), "max_FRP columns (99.5%)")
}

# Log1p + winsorize prcp_* and avg_BRIGHTNESS_* to tame heavy tails
prcp_cols <- grep("^prcp_", colnames(X), value = TRUE)
bright_cols <- grep("^avg_BRIGHTNESS_", colnames(X), value = TRUE)
# prcp: log1p + winsorize
for (col in prcp_cols) {
    x <- X[[col]]
    if (all(is.na(x))) next
    x <- log1p(x)
    p995 <- quantile(x, 0.995, na.rm = TRUE)
    if (!is.na(p995)) x[x > p995] <- p995
    X[[col]] <- x
}
if (length(prcp_cols) > 0) message("  Log+winsorized", length(prcp_cols), "prcp columns (99.5%)")

# add explicit prcp variants: _tr (transformed), _rnk (rank 0-1), _q4 (quartile)
if (length(prcp_cols) > 0) {
    for (col in prcp_cols) {
        x <- X[[col]]
        if (all(is.na(x))) next
        X[[paste0(col, '_tr')]] <- x
        non_na_idx <- which(!is.na(x))
        if (length(non_na_idx) > 0) {
            ranks <- rep(NA_real_, length(x))
            ranks[non_na_idx] <- rank(x[non_na_idx], ties.method = 'average') / length(non_na_idx)
            X[[paste0(col, '_rnk')]] <- ranks
            breaks <- unique(quantile(x[non_na_idx], probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
            if (length(breaks) >= 2) {
                X[[paste0(col, '_q4')]] <- as.numeric(cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE))
            } else {
                X[[paste0(col, '_q4')]] <- rep(NA_real_, length(x))
            }
        } else {
            X[[paste0(col, '_rnk')]] <- rep(NA_real_, length(x))
            X[[paste0(col, '_q4')]] <- rep(NA_real_, length(x))
        }
    }
}

# avg_BRIGHTNESS: two-part encoding (presence + log-intensity winsorized)
if (length(bright_cols) > 0) {
    cols_converted <- 0
    cols_removed <- c()
    for (col in bright_cols) {
        x <- X[[col]]
        if (all(is.na(x))) { cols_removed <- c(cols_removed, col); next }
        pct_zero <- sum(x == 0 | is.na(x), na.rm = TRUE) / length(x)
        if (pct_zero > 0.999) { cols_removed <- c(cols_removed, col); next }
        X[[paste0(col, "_present")]] <- as.numeric(x > 0)
        x_pos <- ifelse(x > 0, log1p(x), 0)
        pos_vals <- x_pos[x_pos > 0]
        if (length(pos_vals) > 0) {
            p995 <- quantile(pos_vals, 0.995, na.rm = TRUE)
            if (!is.na(p995)) x_pos[x_pos > p995] <- p995
        }
        X[[col]] <- x_pos
        cols_converted <- cols_converted + 1
    }
    if (length(cols_removed) > 0) {
        X <- X[, !colnames(X) %in% cols_removed]
        message("  Removed", length(cols_removed), "extremely sparse avg_BRIGHTNESS columns")
    }
    if (cols_converted > 0) message("  avg_BRIGHTNESS two-part: converted", cols_converted, "columns (presence + log-intensity)")
}

# Drop extremely sparse fire_* columns to avoid huge z-scores from rare events
fire_cols <- grep("^fire_", colnames(X), value = TRUE)
if (length(fire_cols) > 0) {
    sparse_fire <- c(); cols_converted <- 0
    any_present_vec <- as.numeric(rowSums(X[, fire_cols, drop = FALSE] > 0, na.rm = TRUE) > 0)
    n_years_present_vec <- as.numeric(rowSums(X[, fire_cols, drop = FALSE] > 0, na.rm = TRUE))
    for (col in fire_cols) {
        p_one <- mean(X[[col]] > 0, na.rm = TRUE)
        if (!is.na(p_one) && p_one < 0.005) {
            sparse_fire <- c(sparse_fire, col)
        } else {
            x <- X[[col]]
            X[[paste0(col, "_present")]] <- as.numeric(x > 0)
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
    # SMD heuristic to decide if aggregate any-year presence is preferable
    treated_idx <- which(W == 1); ctrl_idx <- which(W == 0)
    smd_raws <- c()
    for (col in fire_cols) {
        pres_t <- mean((X[[paste0(col, '_present')]])[treated_idx], na.rm = TRUE)
        pres_c <- mean((X[[paste0(col, '_present')]])[ctrl_idx], na.rm = TRUE)
        sd_c <- sd((X[[paste0(col, '_present')]])[ctrl_idx], na.rm = TRUE)
        if (is.na(sd_c) || sd_c == 0) sd_c <- 1
        smd_raws <- c(smd_raws, (pres_t - pres_c) / sd_c)
    }
    any_pres_t <- mean(any_present_vec[treated_idx], na.rm = TRUE)
    any_pres_c <- mean(any_present_vec[ctrl_idx], na.rm = TRUE)
    sd_c_any <- sd(any_present_vec[ctrl_idx], na.rm = TRUE)
    if (is.na(sd_c_any) || sd_c_any == 0) sd_c_any <- 1
    smd_any <- (any_pres_t - any_pres_c) / sd_c_any
    prefer_any <- FALSE
    if (!is.na(any_pres_c) && any_pres_c >= 0.002 && length(smd_raws) > 0) {
        if (abs(smd_any) <= max(abs(smd_raws), na.rm = TRUE)) prefer_any <- TRUE
    }
    if (prefer_any) {
        X <- X[, !colnames(X) %in% fire_cols, drop = FALSE]
        X[['fire_any_present']] <- as.numeric(any_present_vec > 0)
        X[['fire_n_years_present']] <- as.numeric(n_years_present_vec)
        message('  Replaced yearly fire_* with aggregate fire_any_present and fire_n_years_present')
    } else {
        if (length(sparse_fire) > 0) {
            X <- X[, !colnames(X) %in% sparse_fire]
            message('  Dropped', length(sparse_fire), 'sparse fire_* columns (<0.5% ones)')
        }
        if (cols_converted > 0) message('  fire two-part: converted', cols_converted, 'columns (presence + log-intensity)')
    }
}
  
# Preprocessing mirrors `run_cbps_with_selected_controls.R` for parity.
# Standardize
X_mean <- colMeans(X, na.rm = TRUE)
X_sd <- apply(X, 2, sd, na.rm = TRUE)
X_sd[is.na(X_sd) | X_sd == 0] <- 1
X_scl <- scale(X, center = X_mean, scale = X_sd)

# Preserve original variances and remove near-constant covariates (parity with run_cbps)
X_var_original <- apply(X, 2, var, na.rm = TRUE)
near_constant_threshold <- 1e-10
keep_variance <- X_var_original >= near_constant_threshold
n_removed <- sum(!keep_variance)
if (n_removed > 0) {
    message("  Removed", n_removed, "near-constant covariates (var <", near_constant_threshold, ")")
    X_scl <- X_scl[, keep_variance, drop = FALSE]
}

# Choose rho grid based on obs:covariate ratio (same logic as run_cbps)
n_control <- sum(W == 0)
obs_per_cov <- if (ncol(X_scl) > 0) n_control / ncol(X_scl) else 0

if (obs_per_cov < 10) {
    message("Using STRONG regularization grid (obs:cov =", round(obs_per_cov,1), ")")
    rho_exps <- -4:5
} else {
    message("Using STANDARD regularization grid (obs:cov =", round(obs_per_cov,1), ")")
    rho_exps <- -6:1
}
rho_vals <- unique(10^rho_exps)

results <- list()
for (rho in rho_vals) {
    message("Trying rho =", rho, "...")
  res_try <- tryCatch({
    res <- cbps_att(as.matrix(X_scl),
                    W,
                    theta.init = rep(0, ncol(X_scl) + 1),
                    control = list(trace = 0, maxit = 6000),
                    lambda = rep(rho, ncol(X_scl)))
    list(ok = TRUE, res = res)
  }, error = function(e) list(ok = FALSE, err = e$message))
  if (!res_try$ok) {
    results[[length(results)+1]] <- data.frame(rho = rho, converged = FALSE, err = res_try$err,
                                               max_balance_std = NA, mean_balance_std = NA,
                                                                                             ess = NA, top10_share = NA, max_weight = NA, n_ctrl = sum(W==0),
                                               stringsAsFactors = FALSE)
    next
  }
  res <- res_try$res
  converged <- (res$convergence == 0)
  # build weights vector same order as df_filtered
  wvec <- ifelse(df_filtered$treated == 1, res$weights.1, res$weights.0)
  # controls only
  ctrl_w <- wvec[df_filtered$treated == 0]
  total_ctrl_weight <- sum(ctrl_w, na.rm = TRUE)
  ess <- ifelse(total_ctrl_weight == 0, NA, (total_ctrl_weight^2) / sum(ctrl_w^2, na.rm = TRUE))
    k_top10 <- ceiling(0.10 * length(ctrl_w))
    top10_share <- ifelse(total_ctrl_weight == 0, NA,
                                                sum(sort(ctrl_w, decreasing = TRUE)[1:k_top10], na.rm = TRUE) / total_ctrl_weight)
    max_weight <- ifelse(total_ctrl_weight == 0, NA, max(ctrl_w, na.rm = TRUE) / total_ctrl_weight)
  max_balance_std <- ifelse(is.null(res$balance.std), NA, max(abs(res$balance.std), na.rm = TRUE))
  mean_balance_std <- ifelse(is.null(res$balance.std), NA, mean(abs(res$balance.std), na.rm = TRUE))
  results[[length(results)+1]] <- data.frame(rho = rho, converged = converged, err = NA,
                                             max_balance_std = max_balance_std,
                                             mean_balance_std = mean_balance_std,
                                             ess = ess,
                                                                                         top10_share = top10_share,
                                                                                         max_weight = max_weight,
                                             n_ctrl = sum(W==0),
                                             stringsAsFactors = FALSE)
}

df_res <- do.call(rbind, results)
write.csv(df_res, out_csv, row.names = FALSE)
print(df_res)
message("Saved sweep results to", out_csv)