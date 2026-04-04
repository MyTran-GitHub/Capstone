#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  pkgs <- c("data.table", "ggplot2")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(sprintf("Package %s is required but not installed.", p))
    }
  }
  library(data.table)
  library(ggplot2)
})

parse_key_value_args <- function(args) {
  out <- list()
  for (a in args) {
    if (grepl("=", a, fixed = TRUE)) {
      kv <- strsplit(a, "=", fixed = TRUE)[[1]]
      out[[kv[1]]] <- kv[2]
    }
  }
  out
}

parse_years <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(integer(0))
  as.integer(strsplit(x, ",", fixed = TRUE)[[1]])
}

arg_list <- parse_key_value_args(commandArgs(trailingOnly = TRUE))

treated_year <- ifelse(!is.null(arg_list$treated_year), as.integer(arg_list$treated_year), 2019L)
B <- ifelse(!is.null(arg_list$B), as.integer(arg_list$B), 1000L)
placebo_years <- parse_years(arg_list$placebo_years)
pre_start <- ifelse(!is.null(arg_list$pre_start), as.integer(arg_list$pre_start), 2008L)
pre_end <- ifelse(!is.null(arg_list$pre_end), as.integer(arg_list$pre_end), treated_year - 2L)

if (length(placebo_years) == 0) {
  placebo_years <- seq.int(pre_start, pre_end)
}

if (length(placebo_years) == 0) {
  stop("No placebo_years provided and inferred range is empty")
}

post_lag <- ifelse(!is.null(arg_list$post_lag), as.integer(arg_list$post_lag), 1L)
post_year_count <- ifelse(!is.null(arg_list$post_year_count), as.integer(arg_list$post_year_count), 1L)
post_year_count <- max(1L, post_year_count)

out_dir <- ifelse(
  !is.null(arg_list$out_dir),
  as.character(arg_list$out_dir),
  file.path("Embeddings", "data", "cbps_integration", as.character(treated_year), "temporal_placebo")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Pass-through settings for placebo simulator (matching research-grade defaults).
assignment_mode <- ifelse(!is.null(arg_list$assignment_mode), as.character(arg_list$assignment_mode), "control_only")
n_workers <- ifelse(!is.null(arg_list$n_workers), as.integer(arg_list$n_workers), 1L)
seed_base <- ifelse(!is.null(arg_list$seed_base), as.integer(arg_list$seed_base), 1L)
checkpoint_every <- ifelse(!is.null(arg_list$checkpoint_every), as.integer(arg_list$checkpoint_every), 100L)
resume <- ifelse(!is.null(arg_list$resume), as.character(arg_list$resume), "true")
gate_prefit_mult <- ifelse(!is.null(arg_list$gate_prefit_mult), as.numeric(arg_list$gate_prefit_mult), 5.0)
enforce_ratio_gate <- ifelse(!is.null(arg_list$enforce_ratio_gate), as.character(arg_list$enforce_ratio_gate), "false")
gate_ratio_max <- ifelse(!is.null(arg_list$gate_ratio_max), as.numeric(arg_list$gate_ratio_max), 20.0)
donor_placebo_size <- ifelse(!is.null(arg_list$donor_placebo_size), as.integer(arg_list$donor_placebo_size), 1L)
dry <- ifelse(!is.null(arg_list$dry), as.character(arg_list$dry), "false")
allow_full_sample_randomization <- ifelse(!is.null(arg_list$allow_full_sample_randomization), as.character(arg_list$allow_full_sample_randomization), "false")
min_valid_draws <- ifelse(!is.null(arg_list$min_valid_draws), as.integer(arg_list$min_valid_draws), max(50L, as.integer(0.10 * B)))
max_false_positive_rate_05 <- ifelse(!is.null(arg_list$max_false_positive_rate_05), as.numeric(arg_list$max_false_positive_rate_05), 0.15)
enforce_fp_rate_gate <- ifelse(!is.null(arg_list$enforce_fp_rate_gate), tolower(as.character(arg_list$enforce_fp_rate_gate)) %in% c("1", "true", "t", "yes"), FALSE)

gate_max_abs_smd <- ifelse(!is.null(arg_list$gate_max_abs_smd), as.numeric(arg_list$gate_max_abs_smd), 0.10)
gate_median_abs_smd <- ifelse(!is.null(arg_list$gate_median_abs_smd), as.numeric(arg_list$gate_median_abs_smd), 0.05)
gate_ess_frac <- ifelse(!is.null(arg_list$gate_ess_frac), as.numeric(arg_list$gate_ess_frac), 0.20)
gate_ess_mult_treated <- ifelse(!is.null(arg_list$gate_ess_mult_treated), as.numeric(arg_list$gate_ess_mult_treated), 2.0)
gate_max_weight_share <- ifelse(!is.null(arg_list$gate_max_weight_share), as.numeric(arg_list$gate_max_weight_share), 0.10)
gate_top10_share <- ifelse(!is.null(arg_list$gate_top10_share), as.numeric(arg_list$gate_top10_share), 0.60)

run_one_year <- function(fake_year, run_idx) {
  fake_out_dir <- file.path(out_dir, paste0("fake_treated", fake_year))
  dir.create(fake_out_dir, recursive = TRUE, showWarnings = FALSE)

  post_years <- seq.int(fake_year + post_lag, length.out = post_year_count)
  post_years_str <- paste(post_years, collapse = ",")

  # Offset seed per year for deterministic and independent draws across placebo years.
  this_seed_base <- as.integer(seed_base + run_idx * 100000L)

  cmd_args <- c(
    "Embeddings/scripts/figures/placebo_att_simulator.R",
    paste0("year=", fake_year),
    paste0("B=", as.integer(B)),
    paste0("post_years=", post_years_str),
    paste0("out_dir=", fake_out_dir),
    paste0("assignment_mode=", assignment_mode),
    paste0("n_workers=", as.integer(n_workers)),
    paste0("seed_base=", this_seed_base),
    paste0("checkpoint_every=", as.integer(checkpoint_every)),
    paste0("resume=", resume),
    paste0("gate_prefit_mult=", gate_prefit_mult),
    paste0("enforce_ratio_gate=", enforce_ratio_gate),
    paste0("gate_ratio_max=", gate_ratio_max),
    paste0("donor_placebo_size=", as.integer(donor_placebo_size)),
    paste0("allow_full_sample_randomization=", allow_full_sample_randomization),
    paste0("min_valid_draws=", as.integer(min_valid_draws)),
    paste0("dry=", dry),
    paste0("gate_max_abs_smd=", gate_max_abs_smd),
    paste0("gate_median_abs_smd=", gate_median_abs_smd),
    paste0("gate_ess_frac=", gate_ess_frac),
    paste0("gate_ess_mult_treated=", gate_ess_mult_treated),
    paste0("gate_max_weight_share=", gate_max_weight_share),
    paste0("gate_top10_share=", gate_top10_share)
  )

  cat("[temporal-placebo] fake_year=", fake_year, " post_years=", post_years_str, "\n", sep = "")
  status <- 0L
  out_lines <- character(0)
  out_lines <- tryCatch(
    system2("Rscript", args = cmd_args, stdout = TRUE, stderr = TRUE),
    warning = function(w) {
      c("WARNING", conditionMessage(w))
    },
    error = function(e) {
      status <<- 1L
      c("ERROR", conditionMessage(e))
    }
  )

  if (!is.null(attr(out_lines, "status"))) {
    status <- as.integer(attr(out_lines, "status"))
  }

  summary_path <- file.path(fake_out_dir, sprintf("placebo_summary_%s.csv", fake_year))
  rejection_path <- file.path(fake_out_dir, sprintf("placebo_rejection_reasons_%s.csv", fake_year))

  row <- data.frame(
    treated_year = as.integer(treated_year),
    placebo_year = as.integer(fake_year),
    post_years = post_years_str,
    status = as.integer(status),
    B = as.integer(B),
    stringsAsFactors = FALSE
  )

  if (status == 0L && file.exists(summary_path)) {
    s <- tryCatch(fread(summary_path), error = function(e) NULL)
    if (!is.null(s) && nrow(s) > 0) {
      row$obs_att_post <- if ("obs_att_post" %in% names(s)) as.numeric(s$obs_att_post[1]) else NA_real_
      row$pval_rank <- if ("pval_rank" %in% names(s)) as.numeric(s$pval_rank[1]) else NA_real_
      row$n_valid <- if ("n_valid" %in% names(s)) as.integer(s$n_valid[1]) else NA_integer_
      row$valid_share <- if ("valid_share" %in% names(s)) as.numeric(s$valid_share[1]) else NA_real_
      row$gate_balance_pass_rate <- if ("gate_balance_pass_rate" %in% names(s)) as.numeric(s$gate_balance_pass_rate[1]) else NA_real_
      row$gate_weight_pass_rate <- if ("gate_weight_pass_rate" %in% names(s)) as.numeric(s$gate_weight_pass_rate[1]) else NA_real_
      row$gate_concentration_pass_rate <- if ("gate_concentration_pass_rate" %in% names(s)) as.numeric(s$gate_concentration_pass_rate[1]) else NA_real_
      row$gate_prefit_pass_rate <- if ("gate_prefit_pass_rate" %in% names(s)) as.numeric(s$gate_prefit_pass_rate[1]) else NA_real_
      row$gate_ratio_pass_rate <- if ("gate_ratio_pass_rate" %in% names(s)) as.numeric(s$gate_ratio_pass_rate[1]) else NA_real_
      row$false_positive_05 <- is.finite(row$pval_rank) && row$pval_rank < 0.05
      row$false_positive_10 <- is.finite(row$pval_rank) && row$pval_rank < 0.10
    }
  }

  if (file.exists(rejection_path)) {
    rr <- tryCatch(fread(rejection_path), error = function(e) NULL)
    if (!is.null(rr) && nrow(rr) > 0) {
      rr$treated_year <- as.integer(treated_year)
      rr$placebo_year <- as.integer(fake_year)
      fwrite(rr, file.path(fake_out_dir, sprintf("temporal_rejections_%s.csv", fake_year)))
    }
  }

  log_path <- file.path(fake_out_dir, sprintf("temporal_runner_log_%s.txt", fake_year))
  writeLines(out_lines, con = log_path)
  row
}

all_rows <- lapply(seq_along(placebo_years), function(i) run_one_year(placebo_years[i], i))
summary_df <- rbindlist(all_rows, fill = TRUE)
summary_df <- as.data.frame(summary_df)

summary_csv <- file.path(out_dir, sprintf("temporal_placebo_summary_%s.csv", treated_year))
fwrite(summary_df, summary_csv)

if (nrow(summary_df) > 0 && any(!is.na(summary_df$false_positive_05))) {
  fp_rate_05 <- mean(as.logical(summary_df$false_positive_05), na.rm = TRUE)
  if (isTRUE(enforce_fp_rate_gate) && is.finite(fp_rate_05) && fp_rate_05 > max_false_positive_rate_05) {
    stop(sprintf("Temporal placebo false-positive rate too high: %.3f > %.3f", fp_rate_05, max_false_positive_rate_05))
  }
}

if (nrow(summary_df) > 0 && any(is.finite(summary_df$pval_rank))) {
  p <- ggplot(summary_df, aes(x = placebo_year, y = pval_rank)) +
    geom_line(color = "#1f77b4", linewidth = 0.8) +
    geom_point(aes(color = false_positive_05), size = 2) +
    scale_color_manual(values = c("TRUE" = "#d62728", "FALSE" = "#2ca02c"), guide = "none") +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "#d62728") +
    geom_hline(yintercept = 0.10, linetype = "dotted", color = "#ff7f0e") +
    scale_x_continuous(breaks = sort(unique(summary_df$placebo_year))) +
    labs(
      title = sprintf("Temporal placebo falsification (treated year %s)", treated_year),
      subtitle = "Red points indicate p < 0.05 false positives",
      x = "Fake treatment year",
      y = "Placebo rank p-value"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(out_dir, sprintf("temporal_placebo_pvalues_%s.png", treated_year)),
    plot = p,
    width = 9,
    height = 5,
    dpi = 220
  )
}

cat("Temporal placebo summary written to ", summary_csv, "\n", sep = "")
cat("Completed years: ", paste(placebo_years, collapse = ","), "\n", sep = "")
