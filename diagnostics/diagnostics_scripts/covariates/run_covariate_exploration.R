#!/usr/bin/env Rscript

# Unified covariate diagnostics for a completed CBPS run.
#
# This script is designed to be sourced and called from implement_cbps.R so
# diagnostics are computed from an existing fit (no second CBPS run).
#
# Default output is a single centralized scorecard file with:
# - one overall row for the year
# - one row per covariate block (fire, prcp, swe, etc.)

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

  cfg <- get_diagnostics_config()
  hard_gate <- cfg$lambda_selection$hard_gates
  hard_max_smd <- if (!is.null(hard_gate$max_smd)) as.numeric(hard_gate$max_smd) else 0.10
  hard_top10 <- if (!is.null(hard_gate$top10_share)) as.numeric(hard_gate$top10_share) else 0.75
  hard_max_weight <- if (!is.null(hard_gate$max_weight)) as.numeric(hard_gate$max_weight) else 0.10

  df$lambda <- as.numeric(df$lambda)
  df <- df[order(df$lambda), , drop = FALSE]
  df$feasible_strict <-
    !is.na(df$max_smd) & !is.na(df$top10_share) & !is.na(df$max_weight) &
    df$max_smd <= hard_max_smd & df$top10_share <= hard_top10 & df$max_weight <= hard_max_weight
  tol <- if (!is.null(selected_lambda) && is.finite(selected_lambda)) {
    max(.Machine$double.eps * 10, abs(selected_lambda) * 1e-8)
  } else {
    0
  }
  df$is_selected <- if (!is.null(selected_lambda) && is.finite(selected_lambda)) {
    abs(df$lambda - selected_lambda) <= tol
  } else {
    rep(FALSE, nrow(df))
  }

  x_breaks <- sort(unique(df$lambda))
  x_labels <- vapply(x_breaks, function(x) formatC(x, format = "e", digits = 1), character(1))

  df$label_smd <- sprintf("%.3f", df$max_smd)
  df$label_top10 <- sprintf("%.3f", df$top10_share)
  df$label_maxw <- sprintf("%.3f", df$max_weight)
  df$label_ess <- format(round(df$ess, 0), big.mark = ",", scientific = FALSE, trim = TRUE)

  add_labels <- function(p, label_col) {
    label_df <- p$data
    label_df$.label <- label_df[[label_col]]
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p + ggrepel::geom_text_repel(
        data = label_df,
        ggplot2::aes_string(label = ".label"),
        size = 3,
        box.padding = 0.18,
        point.padding = 0.16,
        max.overlaps = Inf,
        min.segment.length = 0,
        segment.alpha = 0.5,
        seed = 1,
        show.legend = FALSE
      )
    } else {
      p + ggplot2::geom_text(
        data = label_df,
        ggplot2::aes_string(label = ".label"),
        size = 2.8,
        vjust = -0.7,
        check_overlap = FALSE,
        show.legend = FALSE
      )
    }
  }

  x_scale <- ggplot2::scale_x_log10(
    breaks = x_breaks,
    labels = x_labels
  )

  base_theme <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey88", linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      plot.title = ggplot2::element_text(face = "bold")
    )

  point_layers <- list(
    ggplot2::geom_line(color = "grey35", linewidth = 0.55),
    ggplot2::geom_point(ggplot2::aes_string(shape = "is_selected", color = "is_selected"), size = 2.5),
    ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none"),
    ggplot2::scale_color_manual(values = c(`FALSE` = "#2C3E50", `TRUE` = "#C0392B"), guide = "none")
  )

  p1 <- ggplot2::ggplot(df, ggplot2::aes_string(x = "lambda", y = "max_smd")) +
    point_layers +
    ggplot2::geom_hline(yintercept = hard_max_smd, linetype = "dashed") +
    ggplot2::labs(title = "Max SMD", y = "Max SMD", x = "Lambda (log scale)") +
    x_scale +
    base_theme
  p1 <- add_labels(p1, "label_smd")

  p2 <- ggplot2::ggplot(df, ggplot2::aes_string(x = "lambda", y = "top10_share")) +
    point_layers +
    ggplot2::geom_hline(yintercept = c(0.75, 0.80, 0.85), linetype = "dashed") +
    ggplot2::labs(title = "Top 10% Weight Share", y = "Share", x = "Lambda (log scale)") +
    x_scale +
    base_theme
  p2 <- add_labels(p2, "label_top10")

  p3 <- ggplot2::ggplot(df, ggplot2::aes_string(x = "lambda", y = "max_weight")) +
    point_layers +
    ggplot2::geom_hline(yintercept = c(0.10, 0.15, 0.20), linetype = "dashed") +
    ggplot2::labs(title = "Max Control Weight", y = "Max Weight", x = "Lambda (log scale)") +
    x_scale +
    base_theme
  p3 <- add_labels(p3, "label_maxw")

  p4 <- ggplot2::ggplot(df, ggplot2::aes_string(x = "lambda", y = "ess")) +
    point_layers +
    ggplot2::labs(title = "Effective Sample Size", y = "ESS (controls)", x = "Lambda (log scale)") +
    x_scale +
    base_theme
  p4 <- add_labels(p4, "label_ess")

  plt <- (p1 | p2) / (p3 | p4) +
    patchwork::plot_annotation(
      title = "Lambda Diagnostics",
      subtitle = if (!is.null(selected_lambda) && is.finite(selected_lambda)) {
        paste("Selected lambda:", formatC(selected_lambda, format = "e", digits = 2), "(red triangles)")
      } else {
        NULL
      },
      caption = "Point labels show exact metric values at each lambda"
    )

  if (!is.null(out_file)) {
    ggplot2::ggsave(filename = out_file, plot = plt, width = 12, height = 8.5, dpi = 320, bg = "white")
  }
  invisible(plt)
}

run_covariate_exploration <- function(treated_year,
                                      area,
                                      X,
                                      W,
                                      res,
                                      cand_df = NULL,
                                      selected_lambda = NULL,
                                      out_dir = "diagnostics/diagnostics_results/covariates",
                                      run_prefit_overlap = FALSE,
                                      prefit_if_missing = TRUE,
                                      write_prepost_metrics = FALSE,
                                      write_distribution = FALSE,
                                      progress_every = NULL,
                                      # Optional duplicate block-level export. Block rows are
                                      # already included in the centralized scorecard file.
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
    marker_lambda <- selected_lambda

    # Primary source: unified lambda_run file with explicit chosen column.
    if (is.null(marker_lambda) && "chosen" %in% colnames(cand_df)) {
      chosen_rows <- cand_df[isTRUE(cand_df$chosen) | (!is.na(cand_df$chosen) & cand_df$chosen), , drop = FALSE]
      if (nrow(chosen_rows) > 0 && "lambda" %in% colnames(chosen_rows)) {
        marker_lambda <- as.numeric(chosen_rows$lambda[1])
      }
    }

    plot_lambda_diagnostics(
      cand_df,
      selected_lambda = marker_lambda,
      out_file = file.path(out_dir, paste0("lambda_diagnostics_", treated_year, "_", area, ".png"))
    )
  }

  # Advanced-only prefit overlap screening. Keep off for standard runs.
  prefit_overlap_file <- file.path(out_dir, paste0("covariate_overlap_", treated_year, "_", area, ".csv"))
  prefit_summary_file <- file.path(out_dir, paste0("overlap_screen_summary_", treated_year, "_", area, ".csv"))
  prefit_block_file <- file.path(out_dir, paste0("overlap_block_summary_", treated_year, "_", area, ".csv"))
  prefit_missing <- !file.exists(prefit_overlap_file) || !file.exists(prefit_summary_file)
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

  summarize_slice <- function(g, row_type, block_name, include_weight_metrics = FALSE) {
    qsafe <- function(x, p) {
      x <- x[is.finite(x)]
      if (length(x) == 0) return(NA_real_)
      as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))
    }
    mean_pre <- mean(g$abs_smd_pre, na.rm = TRUE)
    mean_post <- mean(g$abs_smd_post, na.rm = TRUE)
    denom <- ifelse(is.finite(mean_pre) && mean_pre > 0, mean_pre, NA_real_)
    reduction_pct <- ifelse(is.na(denom), NA_real_, 100 * (mean_pre - mean_post) / denom)

    data.frame(
      year = treated_year,
      area = area,
      row_type = row_type,
      block = block_name,
      n_covariates = nrow(g),
      n_treated = length(treated_idx),
      n_control = length(control_idx),
      ess_control = if (include_weight_metrics) ctrl_ess else NA_real_,
      top10_share = if (include_weight_metrics) top10_share else NA_real_,
      max_weight = if (include_weight_metrics) max_weight else NA_real_,
      abs_smd_pre_p90 = qsafe(g$abs_smd_pre, 0.90),
      abs_smd_pre_max = ifelse(any(is.finite(g$abs_smd_pre)), max(g$abs_smd_pre, na.rm = TRUE), NA_real_),
      abs_smd_post_p50 = qsafe(g$abs_smd_post, 0.50),
      abs_smd_post_p90 = qsafe(g$abs_smd_post, 0.90),
      abs_smd_post_p95 = qsafe(g$abs_smd_post, 0.95),
      abs_smd_post_max = max(g$abs_smd_post, na.rm = TRUE),
      pct_cov_abs_smd_le_0_10 = mean(g$abs_smd_post <= 0.10, na.rm = TRUE),
      pct_cov_abs_smd_le_0_05 = mean(g$abs_smd_post <= 0.05, na.rm = TRUE),
      mean_abs_smd_reduction_pct = reduction_pct,
      stringsAsFactors = FALSE
    )
  }

  overall_row <- summarize_slice(cov_df, row_type = "overall", block_name = "all", include_weight_metrics = TRUE)
  block_df <- do.call(
    rbind,
    lapply(split(cov_df, cov_df$block), function(g) {
      summarize_slice(g, row_type = "block", block_name = g$block[1], include_weight_metrics = FALSE)
    })
  )
  if (!is.null(block_df) && nrow(block_df) > 0) {
    block_df <- block_df[order(-block_df$abs_smd_post_p90, -block_df$abs_smd_post_max), , drop = FALSE]
    rownames(block_df) <- NULL
  }
  scorecard_df <- rbind(overall_row, block_df)

  if (isTRUE(cfg$outputs$validate_before_write)) {
    validate_output_df(cov_df,
      required_cols = c("year", "area", "covariate", "block", "abs_smd_pre", "abs_smd_post"),
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
    validate_output_df(scorecard_df,
      required_cols = c("year", "area", "row_type", "block", "n_covariates", "ess_control", "abs_smd_post_max"),
      key_cols = c("row_type", "block", "abs_smd_post_max"),
      label = "covariate_scorecard"
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
    block_rows <- scorecard_df[scorecard_df$row_type == "block", , drop = FALSE]
    if (nrow(block_rows) > 0) {
      write.csv(
        block_rows,
        file = file.path(out_dir, paste0("covariate_block_summary_", treated_year, "_", area, ".csv")),
        row.names = FALSE
      )
    }
  }
  if (write_summary) {
    write.csv(
      scorecard_df,
      file = file.path(out_dir, paste0("covariate_summary_", treated_year, "_", area, ".csv")),
      row.names = FALSE
    )
  }

  summary_df <- scorecard_df[scorecard_df$row_type == "overall", , drop = FALSE]

  cat("  Covariate diagnostics saved to ", out_dir, "\n", sep = "")
  cat("    ESS(control) = ", round(ctrl_ess, 2),
      "; top10_share = ", round(top10_share, 3),
      "; max_weight = ", round(max_weight, 3),
      "; |SMD| p90/max (post) = ",
      round(summary_df$abs_smd_post_p90, 3), "/", round(summary_df$abs_smd_post_max, 3),
      "; pct<=0.10 = ", round(100 * summary_df$pct_cov_abs_smd_le_0_10, 1), "%",
      "\n", sep = "")

  invisible(list(summary = summary_df, by_covariate = cov_df, by_block = block_df, distribution = dist_df))
}
