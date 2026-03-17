#!/usr/bin/env Rscript

# Unified covariate diagnostics for a completed CBPS run.
#
# This script is designed to be sourced and called from implement_cbps.R so
# diagnostics are computed from an existing fit (no second CBPS run).

suppressPackageStartupMessages({
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package 'stats' is required.")
  }
})

source("balancing/balancing_config.R")
get_diagnostics_config <- get("get_diagnostics_config", mode = "function")

validate_output_df <- function(df, required_cols, key_cols, label) {
  if (!is.data.frame(df)) stop(label, " must be a data.frame.")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(label, " missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  if (nrow(df) == 0) stop(label, " has zero rows.")
  if (length(key_cols) > 0) {
    for (nm in key_cols) {
      if (all(is.na(df[[nm]]))) stop(label, " has all-NA key column: ", nm)
    }
  }
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & w >= 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]
  w <- w[ok]
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0) return(NA_real_)
  sum(x * w) / sw
}

weighted_sd_safe <- function(x, w) {
  mu <- weighted_mean_safe(x, w)
  if (is.na(mu)) return(NA_real_)
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & w >= 0
  x <- x[ok]
  w <- w[ok]
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0) return(NA_real_)
  v <- sum(w * (x - mu)^2) / sw
  sqrt(max(v, 0))
}

weighted_quantile_safe <- function(x, w, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & w >= 0
  if (!any(ok)) return(rep(NA_real_, length(probs)))
  x <- x[ok]
  w <- w[ok]
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0) return(rep(NA_real_, length(probs)))

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w) / sw
  sapply(probs, function(p) {
    idx <- which(cw >= p)[1]
    if (is.na(idx)) return(tail(x, 1))
    x[idx]
  })
}

weighted_ecdf_fun <- function(x, w = NULL) {
  if (is.null(w)) {
    f <- ecdf(x)
    return(function(v) f(v))
  }
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & w >= 0
  x <- x[ok]
  w <- w[ok]
  if (length(x) == 0 || sum(w) <= 0) {
    return(function(v) rep(NA_real_, length(v)))
  }
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w) / sum(w)
  function(v) {
    sapply(v, function(vv) {
      idx <- max(which(x <= vv), 0)
      if (idx == 0) return(0)
      cw[idx]
    })
  }
}

weighted_ks <- function(x_t, x_c, w_c = NULL) {
  if (length(x_t) == 0 || length(x_c) == 0) return(NA_real_)
  ft <- weighted_ecdf_fun(x_t, NULL)
  fc <- weighted_ecdf_fun(x_c, w_c)
  grid <- sort(unique(c(x_t, x_c)))
  d <- abs(ft(grid) - fc(grid))
  max(d, na.rm = TRUE)
}

safe_smd <- function(mean_t, mean_c, sd_ref) {
  if (is.na(mean_t) || is.na(mean_c) || is.na(sd_ref) || sd_ref <= 0) return(NA_real_)
  (mean_t - mean_c) / sd_ref
}

covariate_block <- function(x) {
  blk <- sub("_.*$", "", x)
  no_sep <- !grepl("_", x)
  blk[no_sep] <- x[no_sep]
  blk
}

plot_lambda_diagnostics <- function(df, selected_lambda = NULL, out_file = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("patchwork", quietly = TRUE)) {
    warning("Skipping lambda plot: packages 'ggplot2' and 'patchwork' are required.")
    return(invisible(NULL))
  }
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))

  df <- df[order(df$lambda), , drop = FALSE]
  df$feasible_strict <- with(df,
    !is.na(max_smd) & !is.na(top10_share) & !is.na(max_weight) &
      max_smd <= 0.10 & top10_share <= 0.75 & max_weight <= 0.10
  )

  x_scale <- ggplot2::scale_x_log10()

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = rlang::.data$lambda, y = rlang::.data$max_smd)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(color = rlang::.data$feasible_strict)) +
    ggplot2::geom_hline(yintercept = 0.10, linetype = "dashed") +
    ggplot2::labs(title = "Max SMD", y = "max SMD", x = "lambda") +
    x_scale +
    ggplot2::theme_minimal()

  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = rlang::.data$lambda, y = rlang::.data$top10_share)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = c(0.75, 0.80, 0.85), linetype = "dashed") +
    ggplot2::labs(title = "Top 10% Weight Share", y = "share", x = "lambda") +
    x_scale +
    ggplot2::theme_minimal()

  p3 <- ggplot2::ggplot(df, ggplot2::aes(x = rlang::.data$lambda, y = rlang::.data$max_weight)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = c(0.10, 0.15, 0.20), linetype = "dashed") +
    ggplot2::labs(title = "Max Weight", y = "max weight", x = "lambda") +
    x_scale +
    ggplot2::theme_minimal()

  p4 <- ggplot2::ggplot(df, ggplot2::aes(x = rlang::.data$lambda, y = rlang::.data$ess)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Effective Sample Size", y = "ESS", x = "lambda") +
    x_scale +
    ggplot2::theme_minimal()

  if (!is.null(selected_lambda) && is.finite(selected_lambda)) {
    p1 <- p1 + ggplot2::geom_vline(xintercept = selected_lambda, linetype = "dotted")
    p2 <- p2 + ggplot2::geom_vline(xintercept = selected_lambda, linetype = "dotted")
    p3 <- p3 + ggplot2::geom_vline(xintercept = selected_lambda, linetype = "dotted")
    p4 <- p4 + ggplot2::geom_vline(xintercept = selected_lambda, linetype = "dotted")
  }

  plt <- (p1 | p2) / (p3 | p4)
  if (!is.null(out_file)) {
    ggplot2::ggsave(filename = out_file, plot = plt, width = 11, height = 8)
  }
  invisible(plt)
}

run_covariate_exploration <- function(treated_year,
                                      area,
                                      X,
                                      W,
                                      res,
                                      cand_df = NULL,
                                      selection_log = NULL,
                                      out_dir = "diagnostics/diagnostics_results/covariates",
                                      run_prefit_overlap = TRUE,
                                      prefit_if_missing = TRUE,
                                      write_prepost_metrics = FALSE,
                                      write_distribution = FALSE,
                                      progress_every = NULL,
                                      write_block_summary = FALSE,
                                      write_summary = TRUE) {
  cfg <- get_diagnostics_config()

  if (is.null(dim(X))) stop("X must be a matrix/data.frame.")
  X <- as.data.frame(X)
  if (nrow(X) != length(W)) stop("X rows must match length(W).")
  if (is.null(res) || is.null(res$weights.0)) stop("res with weights.0 is required.")
  if (!is.null(cand_df) && !is.data.frame(cand_df)) stop("cand_df must be a data.frame when provided.")

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Keep lambda diagnostics output minimal: plot only.
  if (!is.null(cand_df) && nrow(cand_df) > 0) {
    selected_lambda <- NULL
    if (!is.null(selection_log) && !is.null(selection_log$selected_lambda)) {
      selected_lambda <- as.numeric(selection_log$selected_lambda)
    }
    plot_lambda_diagnostics(
      cand_df,
      selected_lambda = selected_lambda,
      out_file = file.path(out_dir, paste0("lambda_diagnostics_", treated_year, "_", area, ".png"))
    )
  }

  prefit_overlap_file <- file.path(out_dir, paste0("covariate_overlap_", treated_year, "_", area, ".csv"))
  prefit_summary_file <- file.path(out_dir, paste0("overlap_screen_summary_", treated_year, "_", area, ".csv"))
  prefit_block_file <- file.path(out_dir, paste0("overlap_block_summary_", treated_year, "_", area, ".csv"))
  prefit_missing <- !file.exists(prefit_overlap_file) || !file.exists(prefit_summary_file)

  overlap_screen <- NULL
  if (run_prefit_overlap && exists("screen_prefit_overlap", mode = "function") && (!prefit_if_missing || prefit_missing)) {
    overlap_fun <- get("screen_prefit_overlap", mode = "function")
    overlap_screen <- tryCatch(
      overlap_fun(X, W, thresholds = cfg$overlap_thresholds),
      error = function(e) NULL
    )
    if (!is.null(overlap_screen)) {
      write.csv(
        overlap_screen$overlap,
        file = prefit_overlap_file,
        row.names = FALSE
      )
      write.csv(
        overlap_screen$summary,
        file = prefit_summary_file,
        row.names = FALSE
      )
      if (!is.null(overlap_screen$block_summary) && nrow(overlap_screen$block_summary) > 0) {
        write.csv(
          overlap_screen$block_summary,
          file = prefit_block_file,
          row.names = FALSE
        )
      }
    }
  }

  treated_idx <- which(W == 1)
  control_idx <- which(W == 0)
  if (length(treated_idx) == 0 || length(control_idx) == 0) {
    stop("Both treated and control groups are required.")
  }

  # res$weights.0 is full-length in this implementation; if not, align to controls.
  w_all <- res$weights.0
  if (length(w_all) == length(control_idx)) {
    w_ctrl <- w_all
  } else if (length(w_all) == length(W)) {
    w_ctrl <- w_all[control_idx]
  } else {
    stop("Unexpected weights length in res$weights.0.")
  }

  ctrl_total <- sum(w_ctrl, na.rm = TRUE)
  ctrl_ess <- ifelse(ctrl_total > 0, (ctrl_total^2) / sum(w_ctrl^2, na.rm = TRUE), NA_real_)
  topN <- max(1, ceiling(0.10 * length(w_ctrl)))
  top10_share <- ifelse(ctrl_total > 0, sum(sort(w_ctrl, decreasing = TRUE)[1:topN], na.rm = TRUE) / ctrl_total, NA_real_)
  max_weight <- ifelse(ctrl_total > 0, max(w_ctrl, na.rm = TRUE) / ctrl_total, NA_real_)

  probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  cov_rows <- vector("list", ncol(X))
  dist_rows <- list()

  bs_pre <- res$balance.std.pre
  bs_post <- res$balance.std

  total_covariates <- ncol(X)
  log_progress <- !is.null(progress_every) && is.finite(progress_every) && progress_every > 0
  if (log_progress) {
    cat("  Diagnostics progress: ", total_covariates, " covariates\n", sep = "")
  }

  for (j in seq_len(ncol(X))) {
    if (log_progress && j %% progress_every == 0) {
      cat("    Processed ", j, "/", total_covariates, " covariates\n", sep = "")
    }
    nm <- colnames(X)[j]
    x <- as.numeric(X[[j]])
    xt <- x[treated_idx]
    xc <- x[control_idx]

    mt <- mean(xt, na.rm = TRUE)
    mcp <- mean(xc, na.rm = TRUE)
    mcw <- weighted_mean_safe(xc, w_ctrl)

    sdt <- sd(xt, na.rm = TRUE)
    sdcp <- sd(xc, na.rm = TRUE)
    sdcw <- weighted_sd_safe(xc, w_ctrl)
    if (is.na(sdt) || sdt <= 0) {
      sdt <- sd(c(xt, xc), na.rm = TRUE)
      if (is.na(sdt) || sdt <= 0) sdt <- 1
    }

    mean_shift_pre <- mt - mcp
    mean_shift_post <- mt - mcw
    smd_pre <- safe_smd(mt, mcp, sdt)
    smd_post <- safe_smd(mt, mcw, sdt)

    ks_pre <- weighted_ks(xt, xc, w_c = w_ctrl)
    ks_post <- weighted_ks(xt, xc, w_c = w_ctrl)

    cbps_smd_pre <- NA_real_
    cbps_smd_post <- NA_real_
    if (!is.null(names(bs_pre)) && nm %in% names(bs_pre)) cbps_smd_pre <- as.numeric(bs_pre[nm])
    if (!is.null(names(bs_post)) && nm %in% names(bs_post)) cbps_smd_post <- as.numeric(bs_post[nm])

    cov_rows[[j]] <- data.frame(
      year = treated_year,
      area = area,
      covariate = nm,
      block = covariate_block(nm),
      n_treated = sum(!is.na(xt)),
      n_control = sum(!is.na(xc)),
      treated_mean = mt,
      control_mean_pre = mcp,
      control_mean_post = mcw,
      treated_sd = sdt,
      control_sd_pre = sdcp,
      control_sd_post = sdcw,
      mean_shift_pre = mean_shift_pre,
      mean_shift_post = mean_shift_post,
      abs_mean_shift_pre = abs(mean_shift_pre),
      abs_mean_shift_post = abs(mean_shift_post),
      smd_pre = smd_pre,
      smd_post = smd_post,
      abs_smd_pre = abs(smd_pre),
      abs_smd_post = abs(smd_post),
      cbps_smd_pre = cbps_smd_pre,
      cbps_smd_post = cbps_smd_post,
      ks_pre = ks_pre,
      ks_post = ks_post,
      stringsAsFactors = FALSE
    )

    if (write_distribution) {
      q_t <- stats::quantile(xt, probs = probs, na.rm = TRUE, names = FALSE)
      q_cp <- stats::quantile(xc, probs = probs, na.rm = TRUE, names = FALSE)
      q_cw <- weighted_quantile_safe(xc, w_ctrl, probs = probs)

      dist_rows[[length(dist_rows) + 1]] <- data.frame(
        year = treated_year,
        area = area,
        covariate = nm,
        group = "treated",
        mean = mt,
        sd = sdt,
        q05 = q_t[1], q25 = q_t[2], q50 = q_t[3], q75 = q_t[4], q95 = q_t[5],
        stringsAsFactors = FALSE
      )
      dist_rows[[length(dist_rows) + 1]] <- data.frame(
        year = treated_year,
        area = area,
        covariate = nm,
        group = "control_pre",
        mean = mcp,
        sd = sdcp,
        q05 = q_cp[1], q25 = q_cp[2], q50 = q_cp[3], q75 = q_cp[4], q95 = q_cp[5],
        stringsAsFactors = FALSE
      )
      dist_rows[[length(dist_rows) + 1]] <- data.frame(
        year = treated_year,
        area = area,
        covariate = nm,
        group = "control_post",
        mean = mcw,
        sd = sdcw,
        q05 = q_cw[1], q25 = q_cw[2], q50 = q_cw[3], q75 = q_cw[4], q95 = q_cw[5],
        stringsAsFactors = FALSE
      )
    }
  }

  cov_df <- do.call(rbind, cov_rows)
  dist_df <- if (write_distribution && length(dist_rows) > 0) do.call(rbind, dist_rows) else NULL

  block_df <- do.call(
    rbind,
    lapply(split(cov_df, cov_df$block), function(g) {
      data.frame(
        year = treated_year,
        area = area,
        block = g$block[1],
        n_covariates = nrow(g),
        mean_abs_smd_pre = mean(g$abs_smd_pre, na.rm = TRUE),
        mean_abs_smd_post = mean(g$abs_smd_post, na.rm = TRUE),
        max_abs_smd_pre = max(g$abs_smd_pre, na.rm = TRUE),
        max_abs_smd_post = max(g$abs_smd_post, na.rm = TRUE),
        mean_abs_mean_shift_pre = mean(g$abs_mean_shift_pre, na.rm = TRUE),
        mean_abs_mean_shift_post = mean(g$abs_mean_shift_post, na.rm = TRUE),
        mean_ks_pre = mean(g$ks_pre, na.rm = TRUE),
        mean_ks_post = mean(g$ks_post, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
  )
  block_df <- block_df[order(-block_df$max_abs_smd_post, -block_df$mean_abs_smd_post), , drop = FALSE]

  summary_df <- data.frame(
    year = treated_year,
    area = area,
    n_covariates = nrow(cov_df),
    n_treated = length(treated_idx),
    n_control = length(control_idx),
    ess_control = ctrl_ess,
    top10_share = top10_share,
    max_weight = max_weight,
    max_abs_smd_pre = max(cov_df$abs_smd_pre, na.rm = TRUE),
    max_abs_smd_post = max(cov_df$abs_smd_post, na.rm = TRUE),
    median_abs_smd_pre = median(cov_df$abs_smd_pre, na.rm = TRUE),
    median_abs_smd_post = median(cov_df$abs_smd_post, na.rm = TRUE),
    mean_abs_mean_shift_pre = mean(cov_df$abs_mean_shift_pre, na.rm = TRUE),
    mean_abs_mean_shift_post = mean(cov_df$abs_mean_shift_post, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  if (isTRUE(cfg$outputs$validate_before_write)) {
    validate_output_df(cov_df,
      required_cols = c("year", "area", "covariate", "block", "abs_smd_pre", "abs_smd_post", "ks_pre", "ks_post"),
      key_cols = c("covariate", "abs_smd_post"),
      label = "covariate_prepost_metrics"
    )
    if (write_distribution && !is.null(dist_df) && nrow(dist_df) > 0) {
      validate_output_df(dist_df,
        required_cols = c("year", "area", "covariate", "group", "mean", "q50"),
        key_cols = c("covariate", "group", "mean"),
        label = "covariate_distribution"
      )
    }
    validate_output_df(block_df,
      required_cols = c("year", "area", "block", "n_covariates", "max_abs_smd_post"),
      key_cols = c("block", "max_abs_smd_post"),
      label = "covariate_block_summary"
    )
    validate_output_df(summary_df,
      required_cols = c("year", "area", "n_covariates", "ess_control", "max_abs_smd_post"),
      key_cols = c("year", "area", "max_abs_smd_post"),
      label = "covariate_summary"
    )
  }

  if (write_prepost_metrics) {
    write.csv(
      cov_df,
      file = file.path(out_dir, paste0("covariate_prepost_metrics_", treated_year, "_", area, ".csv")),
      row.names = FALSE
    )
  }
  if (write_distribution && !is.null(dist_df) && nrow(dist_df) > 0) {
    write.csv(
      dist_df,
      file = file.path(out_dir, paste0("covariate_distribution_", treated_year, "_", area, ".csv")),
      row.names = FALSE
    )
  }
  if (write_block_summary) {
    write.csv(
      block_df,
      file = file.path(out_dir, paste0("covariate_block_summary_", treated_year, "_", area, ".csv")),
      row.names = FALSE
    )
  }
  if (write_summary) {
    write.csv(
      summary_df,
      file = file.path(out_dir, paste0("covariate_summary_", treated_year, "_", area, ".csv")),
      row.names = FALSE
    )
  }

  cat("  Covariate diagnostics saved to ", out_dir, "\n", sep = "")
  cat("    ESS(control) = ", round(ctrl_ess, 2),
      "; top10_share = ", round(top10_share, 3),
      "; max_weight = ", round(max_weight, 3),
      "; max |SMD| pre/post = ",
      round(summary_df$max_abs_smd_pre, 3), "/", round(summary_df$max_abs_smd_post, 3),
      "\n", sep = "")

  invisible(list(summary = summary_df, by_covariate = cov_df, by_block = block_df, distribution = dist_df))
}
