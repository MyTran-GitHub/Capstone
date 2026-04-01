#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("utils", quietly = TRUE)) stop("Package 'utils' is required")
})

parse_flag <- function(args, flag, default = NULL) {
  flag_eq <- paste0(flag, "=")
  hit_eq <- args[startsWith(args, flag_eq)]
  if (length(hit_eq) > 0) return(sub(flag_eq, "", hit_eq[1], fixed = TRUE))
  idx <- which(args == flag)
  if (length(idx) > 0 && idx[1] < length(args)) return(args[idx[1] + 1])
  default
}

safe_numeric <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 0) return(NA_real_)
  out[1]
}

safe_integer <- function(x, default) {
  out <- suppressWarnings(as.integer(x))
  if (length(out) == 0 || is.na(out[1])) return(default)
  out[1]
}

safe_bool <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) return(default)
  val <- tolower(trimws(as.character(x[1])))
  if (val %in% c("1", "true", "t", "yes", "y")) return(TRUE)
  if (val %in% c("0", "false", "f", "no", "n")) return(FALSE)
  default
}

safe_ratio <- function(num, den) {
  if (!is.finite(num) || !is.finite(den) || den <= 0) return(NA_real_)
  as.numeric(num) / as.numeric(den)
}

safe_quantile <- function(x, p) {
  vals <- x[is.finite(x)]
  if (length(vals) == 0) return(NA_real_)
  as.numeric(stats::quantile(vals, probs = p, names = FALSE, na.rm = TRUE))
}

alert_label <- function(code) {
  lbl <- c(
    alert_no_chosen = "No chosen lambda row",
    alert_emergency = "Emergency lambda selection",
    alert_fallback = "Fallback gate used",
    alert_smd_over_hard = "Selected lambda exceeds hard max SMD",
    alert_top10_over_hard = "Top-10 weight share exceeds hard gate",
    alert_maxw_over_hard = "Max single weight exceeds hard gate",
    alert_ess_below_floor = "ESS below required floor",
    alert_ess_near_floor = "ESS near required floor",
    alert_hard_has_no_feasible = "No hard-gate-feasible lambda candidate",
    alert_stage2_not_run = "Stage-2 lambda search did not run",
    alert_too_few_candidates = "Too few lambda candidates",
    alert_cov_missing = "Covariate summary missing",
    alert_cov_max_smd_over_hard = "Covariate post max SMD exceeds threshold",
    alert_cov_low_pct_le_0_10 = "Covariate share <=0.10 SMD below threshold",
    alert_joint_balance_fail = "Joint lambda/covariate balance failure",
    alert_block_failure = "At least one covariate block failed threshold"
  )
  out <- unname(lbl[code])
  ifelse(is.na(out), as.character(code), out)
}

find_files <- function(root, pattern) {
  if (!dir.exists(root)) return(character(0))
  unique(list.files(root, pattern = pattern, recursive = TRUE, full.names = TRUE))
}

extract_meta <- function(path, prefix) {
  nm <- basename(path)
  m <- regexec(paste0("^", prefix, "_([0-9]{4})_(.+)\\.csv$"), nm)
  g <- regmatches(nm, m)[[1]]
  if (length(g) == 3) {
    return(list(year = as.integer(g[2]), area = g[3]))
  }

  m2 <- regexec(paste0("^", prefix, "_([0-9]{4})_(.+)\\.rds$"), nm)
  g2 <- regmatches(nm, m2)[[1]]
  if (length(g2) == 3) {
    return(list(year = as.integer(g2[2]), area = g2[3]))
  }

  list(year = NA_integer_, area = NA_character_)
}

infer_n_control <- function(chosen_row, sel_ctx) {
  if (!is.null(sel_ctx) && is.finite(as.numeric(sel_ctx$n_control))) {
    return(as.numeric(sel_ctx$n_control))
  }
  if ("ess_ratio" %in% colnames(chosen_row) && is.finite(chosen_row$ess_ratio[1]) && chosen_row$ess_ratio[1] > 0) {
    return(as.numeric(chosen_row$ess[1] / chosen_row$ess_ratio[1]))
  }
  if ("ess_frac" %in% colnames(chosen_row) && is.finite(chosen_row$ess_frac[1]) && chosen_row$ess_frac[1] > 0) {
    return(as.numeric(chosen_row$ess[1] / chosen_row$ess_frac[1]))
  }
  NA_real_
}

infer_n_treated <- function(sel_ctx) {
  if (!is.null(sel_ctx) && is.finite(as.numeric(sel_ctx$n_treated))) {
    return(as.numeric(sel_ctx$n_treated))
  }
  NA_real_
}

gate_required_ess <- function(gate, n_ctrl, n_treated) {
  ess_candidates <- c()
  if (!is.null(gate$ess_frac) && is.finite(n_ctrl) && n_ctrl > 0) {
    ess_candidates <- c(ess_candidates, as.numeric(gate$ess_frac) * n_ctrl)
  }
  if (!is.null(gate$ess_abs)) {
    ess_candidates <- c(ess_candidates, as.numeric(gate$ess_abs))
  }
  if (!is.null(gate$ess_mult_treated) && is.finite(n_treated) && n_treated > 0) {
    ess_candidates <- c(ess_candidates, as.numeric(gate$ess_mult_treated) * n_treated)
  }
  ess_candidates <- ess_candidates[is.finite(ess_candidates)]
  if (length(ess_candidates) == 0) return(0)
  max(ess_candidates)
}

compute_pass <- function(d, gate, n_ctrl, n_treated) {
  gate_max_weight <- if (!is.null(gate$max_weight)) as.numeric(gate$max_weight) else Inf
  ess_required <- gate_required_ess(gate, n_ctrl, n_treated)

  !is.na(d$max_smd) &
    !is.na(d$median_smd) &
    !is.na(d$top10_share) &
    !is.na(d$max_weight) &
    !is.na(d$ess) &
    d$max_smd <= as.numeric(gate$max_smd) &
    d$median_smd <= as.numeric(gate$median_smd) &
    d$top10_share <= as.numeric(gate$top10_share) &
    d$max_weight <= gate_max_weight &
    d$ess >= ess_required
}

read_covariate_overall <- function(path) {
  df <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  if ("row_type" %in% colnames(df)) {
    overall <- df[df$row_type == "overall", , drop = FALSE]
    if (nrow(overall) == 0) overall <- df[1, , drop = FALSE]
  } else {
    overall <- df[1, , drop = FALSE]
  }

  meta <- extract_meta(path, "covariate_summary")

  data.frame(
    cov_file = path,
    year = if ("year" %in% colnames(overall)) safe_integer(overall$year[1], meta$year) else meta$year,
    area = if ("area" %in% colnames(overall)) as.character(overall$area[1]) else as.character(meta$area),
    cov_n_covariates = if ("n_covariates" %in% colnames(overall)) safe_integer(overall$n_covariates[1], NA_integer_) else NA_integer_,
    cov_n_control = if ("n_control" %in% colnames(overall)) safe_numeric(overall$n_control[1]) else NA_real_,
    cov_ess_control = if ("ess_control" %in% colnames(overall)) safe_numeric(overall$ess_control[1]) else NA_real_,
    cov_top10_share = if ("top10_share" %in% colnames(overall)) safe_numeric(overall$top10_share[1]) else NA_real_,
    cov_max_weight = if ("max_weight" %in% colnames(overall)) safe_numeric(overall$max_weight[1]) else NA_real_,
    cov_abs_smd_post_p90 = if ("abs_smd_post_p90" %in% colnames(overall)) safe_numeric(overall$abs_smd_post_p90[1]) else NA_real_,
    cov_abs_smd_post_max = if ("abs_smd_post_max" %in% colnames(overall)) safe_numeric(overall$abs_smd_post_max[1]) else NA_real_,
    cov_pct_cov_abs_smd_le_0_10 = if ("pct_cov_abs_smd_le_0_10" %in% colnames(overall)) safe_numeric(overall$pct_cov_abs_smd_le_0_10[1]) else NA_real_,
    cov_pct_cov_abs_smd_le_0_05 = if ("pct_cov_abs_smd_le_0_05" %in% colnames(overall)) safe_numeric(overall$pct_cov_abs_smd_le_0_05[1]) else NA_real_,
    cov_mean_abs_smd_reduction_pct = if ("mean_abs_smd_reduction_pct" %in% colnames(overall)) safe_numeric(overall$mean_abs_smd_reduction_pct[1]) else NA_real_,
    stringsAsFactors = FALSE
  )
}

read_covariate_block_summary <- function(path, hard_max_smd, min_pct_le_0_10) {
  df <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  if (!("row_type" %in% colnames(df)) || !("block" %in% colnames(df))) return(NULL)

  blk <- df[df$row_type == "block", , drop = FALSE]
  if (nrow(blk) == 0) return(NULL)

  meta <- extract_meta(path, "covariate_summary")
  year_val <- if ("year" %in% colnames(blk)) safe_integer(blk$year[1], meta$year) else meta$year
  area_val <- if ("area" %in% colnames(blk)) as.character(blk$area[1]) else as.character(meta$area)

  blk$abs_smd_post_max_num <- if ("abs_smd_post_max" %in% colnames(blk)) suppressWarnings(as.numeric(blk$abs_smd_post_max)) else NA_real_
  blk$pct_cov_abs_smd_le_0_10_num <- if ("pct_cov_abs_smd_le_0_10" %in% colnames(blk)) suppressWarnings(as.numeric(blk$pct_cov_abs_smd_le_0_10)) else NA_real_

  fail_smd <- is.finite(blk$abs_smd_post_max_num) & (blk$abs_smd_post_max_num > hard_max_smd)
  fail_pct <- is.finite(blk$pct_cov_abs_smd_le_0_10_num) & (blk$pct_cov_abs_smd_le_0_10_num < min_pct_le_0_10)
  blk_fail <- fail_smd | fail_pct

  worst_idx <- if (any(is.finite(blk$abs_smd_post_max_num))) which.max(blk$abs_smd_post_max_num) else NA_integer_
  worst_block <- if (is.finite(worst_idx)) as.character(blk$block[worst_idx]) else NA_character_
  worst_smd <- if (is.finite(worst_idx)) safe_numeric(blk$abs_smd_post_max_num[worst_idx]) else NA_real_

  fail_blocks <- as.character(unique(blk$block[blk_fail]))
  fail_blocks <- fail_blocks[!is.na(fail_blocks) & nzchar(fail_blocks)]

  data.frame(
    year = year_val,
    area = area_val,
    block_worst_name = worst_block,
    block_worst_abs_smd_post_max = worst_smd,
    block_n_fail = length(fail_blocks),
    block_fail_blocks = if (length(fail_blocks) > 0) paste(fail_blocks, collapse = ";") else "",
    block_any_fail = length(fail_blocks) > 0,
    stringsAsFactors = FALSE
  )
}

args <- commandArgs(trailingOnly = TRUE)
lambda_dir <- parse_flag(args, "--lambda-dir", "diagnostics/diagnostics_results/lambda_run")
covariate_dir <- parse_flag(args, "--covariate-dir", "diagnostics/diagnostics_results/covariates")
config_file <- parse_flag(args, "--config", "balancing/balancing_config.R")
out_dir <- parse_flag(args, "--out-dir", "diagnostics/diagnostics_results/batch_diagnostics")
experiment_name <- parse_flag(args, "--experiment-name", "full_pool")
lambda_pattern <- parse_flag(args, "--lambda-pattern", "^lambda_run_.*\\.rds$")
covariate_pattern <- parse_flag(args, "--covariate-pattern", "^covariate_summary_.*\\.csv$")
top_n <- safe_integer(parse_flag(args, "--top-n", 10), 10)
if (!is.finite(top_n) || top_n < 1) top_n <- 10
save_top_alerts <- safe_integer(parse_flag(args, "--save-top-alerts", 20), 20)
if (!is.finite(save_top_alerts) || save_top_alerts < 1) save_top_alerts <- 20
save_detailed <- safe_bool(parse_flag(args, "--save-detailed", "false"), FALSE)

cov_hard_max_smd <- safe_numeric(parse_flag(args, "--cov-hard-max-smd", 0.10))
cov_min_pct_le_0_10 <- safe_numeric(parse_flag(args, "--cov-min-pct-le-0-10", 0.80))

resolve_experiment_dir <- function(base_dir, experiment_name) {
  base_norm <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  if (basename(base_norm) == experiment_name) return(base_dir)
  file.path(base_dir, experiment_name)
}

lambda_dir <- resolve_experiment_dir(lambda_dir, experiment_name)
covariate_dir <- resolve_experiment_dir(covariate_dir, experiment_name)
out_dir <- resolve_experiment_dir(out_dir, experiment_name)

if (!file.exists(config_file)) stop("config file not found: ", config_file)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

source(config_file)
if (!exists("get_diagnostics_config", mode = "function")) {
  stop("get_diagnostics_config() not found after sourcing config file")
}

cfg <- get_diagnostics_config()
lambda_cfg <- cfg$lambda_selection
if (is.null(lambda_cfg) || is.null(lambda_cfg$hard_gates)) {
  stop("lambda_selection hard_gates not found in config")
}

gate_profiles <- c(list(c(name = "hard", lambda_cfg$hard_gates)), lambda_cfg$fallback_gates)

lambda_files <- find_files(lambda_dir, lambda_pattern)
if (length(lambda_files) == 0) {
  stop("No lambda files found under: ", lambda_dir, " with pattern: ", lambda_pattern)
}

cov_files <- find_files(covariate_dir, covariate_pattern)

required_cols <- c("lambda", "ess", "top10_share", "max_weight", "max_smd")
lambda_rows <- list()

for (f in sort(lambda_files)) {
  meta <- extract_meta(f, "lambda_run")
  rec <- list(
    lambda_file = f,
    year = meta$year,
    area = meta$area,
    lambda_status = "ok",
    n_candidates = NA_integer_,
    n_control = NA_real_,
    n_treated = NA_real_,
    stage2_ran = NA,
    selected_lambda = NA_real_,
    selected_gate = NA_character_,
    selected_stage = NA_character_,
    selected_is_emergency = NA,
    selected_max_smd = NA_real_,
    selected_median_smd = NA_real_,
    selected_top10_share = NA_real_,
    selected_max_weight = NA_real_,
    selected_ess = NA_real_,
    selected_ess_frac = NA_real_,
    required_ess_floor = NA_real_,
    hard_pass_count = NA_integer_,
    fallback_total_pass_count = NA_integer_,
    best_max_smd = NA_real_,
    best_lambda = NA_real_,
    delta_selected_vs_best_smd = NA_real_,
    alert_no_chosen = FALSE,
    alert_emergency = FALSE,
    alert_fallback = FALSE,
    alert_smd_over_hard = FALSE,
    alert_top10_over_hard = FALSE,
    alert_maxw_over_hard = FALSE,
    alert_ess_below_floor = FALSE,
    alert_ess_near_floor = FALSE,
    alert_hard_has_no_feasible = FALSE,
    alert_stage2_not_run = FALSE,
    alert_too_few_candidates = FALSE,
    alert_any_lambda = FALSE,
    alert_count_lambda = 0L
  )

  cand <- tryCatch(readRDS(f), error = function(e) e)
  if (inherits(cand, "error") || !is.data.frame(cand) || nrow(cand) == 0) {
    rec$lambda_status <- "read_error_or_empty"
    lambda_rows[[length(lambda_rows) + 1]] <- as.data.frame(rec, stringsAsFactors = FALSE)
    next
  }

  rec$n_candidates <- nrow(cand)
  if (length(setdiff(required_cols, colnames(cand))) > 0) {
    rec$lambda_status <- "missing_required_columns"
    lambda_rows[[length(lambda_rows) + 1]] <- as.data.frame(rec, stringsAsFactors = FALSE)
    next
  }

  if (!"median_smd" %in% colnames(cand)) cand$median_smd <- cand$max_smd
  if (!"stage" %in% colnames(cand)) cand$stage <- NA_character_
  if (!"chosen" %in% colnames(cand)) cand$chosen <- FALSE

  rec$stage2_ran <- any(cand$stage == "stage2", na.rm = TRUE)
  rec$alert_stage2_not_run <- !isTRUE(rec$stage2_ran)
  rec$alert_too_few_candidates <- isTRUE(rec$n_candidates < 5)

  chosen_idx <- which(isTRUE(cand$chosen) | (!is.na(cand$chosen) & cand$chosen))
  sel_ctx <- attr(cand, "selection_context")

  if (length(chosen_idx) == 0 && !is.null(sel_ctx) && is.finite(as.numeric(sel_ctx$selected_lambda))) {
    target <- as.numeric(sel_ctx$selected_lambda)
    tol <- max(.Machine$double.eps * 10, abs(target) * 1e-8)
    chosen_idx <- which(abs(cand$lambda - target) <= tol)
  }

  if (length(chosen_idx) == 0) {
    rec$alert_no_chosen <- TRUE
    rec$lambda_status <- "no_chosen_row"
  } else {
    chosen <- cand[chosen_idx[1], , drop = FALSE]

    rec$selected_lambda <- safe_numeric(chosen$lambda)
    rec$selected_stage <- as.character(chosen$stage[1])
    rec$selected_gate <- if ("gate_used" %in% colnames(chosen) && !is.na(chosen$gate_used[1])) {
      as.character(chosen$gate_used[1])
    } else if (!is.null(sel_ctx) && !is.null(sel_ctx$selected_gate)) {
      as.character(sel_ctx$selected_gate)
    } else {
      NA_character_
    }

    rec$selected_is_emergency <- if ("selected_is_emergency" %in% colnames(chosen)) {
      isTRUE(chosen$selected_is_emergency[1])
    } else {
      identical(rec$selected_gate, "emergency")
    }

    rec$selected_max_smd <- safe_numeric(chosen$max_smd)
    rec$selected_median_smd <- safe_numeric(chosen$median_smd)
    rec$selected_top10_share <- safe_numeric(chosen$top10_share)
    rec$selected_max_weight <- safe_numeric(chosen$max_weight)
    rec$selected_ess <- safe_numeric(chosen$ess)

    rec$required_ess_floor <- if ("required_ess_floor" %in% colnames(chosen)) {
      safe_numeric(chosen$required_ess_floor)
    } else if (!is.null(sel_ctx) && is.finite(as.numeric(sel_ctx$required_ess_floor))) {
      as.numeric(sel_ctx$required_ess_floor)
    } else {
      NA_real_
    }

    rec$n_control <- infer_n_control(chosen, sel_ctx)
    rec$n_treated <- infer_n_treated(sel_ctx)
    if (is.finite(rec$n_control) && rec$n_control > 0) {
      rec$selected_ess_frac <- rec$selected_ess / rec$n_control
    }

    rec$alert_emergency <- isTRUE(rec$selected_is_emergency)
    rec$alert_fallback <- isTRUE(!is.na(rec$selected_gate) && rec$selected_gate != "hard" && rec$selected_gate != "emergency")
    rec$alert_smd_over_hard <- isTRUE(is.finite(rec$selected_max_smd) && rec$selected_max_smd > as.numeric(lambda_cfg$hard_gates$max_smd))
    rec$alert_top10_over_hard <- isTRUE(is.finite(rec$selected_top10_share) && rec$selected_top10_share > as.numeric(lambda_cfg$hard_gates$top10_share))
    rec$alert_maxw_over_hard <- isTRUE(is.finite(rec$selected_max_weight) && rec$selected_max_weight > as.numeric(lambda_cfg$hard_gates$max_weight))
    rec$alert_ess_below_floor <- isTRUE(is.finite(rec$required_ess_floor) && is.finite(rec$selected_ess) && rec$selected_ess < rec$required_ess_floor)
    rec$alert_ess_near_floor <- isTRUE(is.finite(rec$required_ess_floor) && is.finite(rec$selected_ess) && rec$selected_ess >= rec$required_ess_floor && rec$selected_ess < 1.2 * rec$required_ess_floor)
  }

  if (any(is.finite(cand$max_smd))) {
    best_idx <- which.min(cand$max_smd)
    rec$best_max_smd <- safe_numeric(cand$max_smd[best_idx[1]])
    rec$best_lambda <- safe_numeric(cand$lambda[best_idx[1]])
    if (is.finite(rec$selected_max_smd) && is.finite(rec$best_max_smd)) {
      rec$delta_selected_vs_best_smd <- rec$selected_max_smd - rec$best_max_smd
    }
  }

  if (is.finite(rec$n_control) && rec$n_control > 0) {
    for (g in gate_profiles) {
      nm <- as.character(g$name)
      pass_col <- paste0("pass_", nm)
      if (!pass_col %in% colnames(cand)) {
        cand[[pass_col]] <- compute_pass(cand, g, rec$n_control, rec$n_treated)
      }
    }
  }

  rec$hard_pass_count <- if ("pass_hard" %in% colnames(cand)) sum(cand$pass_hard, na.rm = TRUE) else NA_integer_
  fb_cols <- grep("^pass_", colnames(cand), value = TRUE)
  fb_cols <- setdiff(fb_cols, "pass_hard")
  rec$fallback_total_pass_count <- if (length(fb_cols) > 0) {
    # Count unique lambda candidates that pass at least one fallback gate.
    rowSums(as.data.frame(lapply(cand[fb_cols], function(x) as.integer(!is.na(x) & x))), na.rm = TRUE) |> 
      (`>`)(0L) |> 
      sum(na.rm = TRUE)
  } else {
    NA_integer_
  }

  rec$alert_hard_has_no_feasible <- isTRUE(!is.na(rec$hard_pass_count) && rec$hard_pass_count == 0)

  alert_cols <- grep("^alert_", names(rec), value = TRUE)
  alert_cols <- setdiff(alert_cols, c("alert_any_lambda", "alert_count_lambda"))
  rec$alert_count_lambda <- sum(vapply(alert_cols, function(nm) isTRUE(rec[[nm]]), logical(1)))
  rec$alert_any_lambda <- rec$alert_count_lambda > 0

  lambda_rows[[length(lambda_rows) + 1]] <- as.data.frame(rec, stringsAsFactors = FALSE)
}

lambda_df <- do.call(rbind, lambda_rows)
lambda_df <- lambda_df[order(lambda_df$year, lambda_df$area), , drop = FALSE]

cov_df <- data.frame()
if (length(cov_files) > 0) {
  cov_rows <- lapply(sort(cov_files), read_covariate_overall)
  cov_rows <- Filter(Negate(is.null), cov_rows)
  if (length(cov_rows) > 0) {
    cov_df <- do.call(rbind, cov_rows)
    cov_df <- cov_df[order(cov_df$year, cov_df$area), , drop = FALSE]

    cov_df$cov_has_core <- as.integer(
      is.finite(cov_df$cov_abs_smd_post_max) |
      is.finite(cov_df$cov_pct_cov_abs_smd_le_0_10)
    )
    cov_df <- cov_df[order(cov_df$year, cov_df$area, -cov_df$cov_has_core, cov_df$cov_file), , drop = FALSE]

    dup_idx <- duplicated(cov_df[, c("year", "area")])
    if (any(dup_idx)) {
      cat("Warning: duplicate covariate summaries for year-area keys found; keeping one deterministic row per key. Dropped:", sum(dup_idx), "\n")
      cov_df <- cov_df[!dup_idx, , drop = FALSE]
    }
    cov_df$cov_has_core <- NULL
  }
}

block_df <- data.frame()
if (length(cov_files) > 0) {
  block_rows <- lapply(sort(cov_files), function(p) read_covariate_block_summary(p, cov_hard_max_smd, cov_min_pct_le_0_10))
  block_rows <- Filter(Negate(is.null), block_rows)
  if (length(block_rows) > 0) {
    block_df <- do.call(rbind, block_rows)
    block_df <- block_df[order(block_df$year, block_df$area), , drop = FALSE]
    block_dup <- duplicated(block_df[, c("year", "area")])
    if (any(block_dup)) {
      block_df <- block_df[!block_dup, , drop = FALSE]
    }
  }
}

if (nrow(cov_df) == 0) {
  merged_df <- lambda_df
  merged_df$cov_file <- NA_character_
  merged_df$cov_n_covariates <- NA_integer_
  merged_df$cov_ess_control <- NA_real_
  merged_df$cov_top10_share <- NA_real_
  merged_df$cov_max_weight <- NA_real_
  merged_df$cov_abs_smd_post_p90 <- NA_real_
  merged_df$cov_abs_smd_post_max <- NA_real_
  merged_df$cov_pct_cov_abs_smd_le_0_10 <- NA_real_
  merged_df$cov_pct_cov_abs_smd_le_0_05 <- NA_real_
  merged_df$cov_mean_abs_smd_reduction_pct <- NA_real_
} else {
  merged_df <- merge(lambda_df, cov_df, by = c("year", "area"), all.x = TRUE, sort = TRUE)
}

if (nrow(block_df) > 0) {
  merged_df <- merge(merged_df, block_df, by = c("year", "area"), all.x = TRUE, sort = TRUE)
} else {
  merged_df$block_worst_name <- NA_character_
  merged_df$block_worst_abs_smd_post_max <- NA_real_
  merged_df$block_n_fail <- NA_integer_
  merged_df$block_fail_blocks <- NA_character_
  merged_df$block_any_fail <- FALSE
}

merged_df$cov_ess_ratio <- ifelse(
  is.finite(merged_df$cov_ess_control) & is.finite(merged_df$cov_n_control) & merged_df$cov_n_control > 0,
  merged_df$cov_ess_control / merged_df$cov_n_control,
  NA_real_
)
merged_df$experiment_name <- experiment_name

merged_df$alert_cov_missing <- is.na(merged_df$cov_file)
merged_df$alert_cov_max_smd_over_hard <- is.finite(merged_df$cov_abs_smd_post_max) & merged_df$cov_abs_smd_post_max > cov_hard_max_smd
merged_df$alert_cov_low_pct_le_0_10 <- is.finite(merged_df$cov_pct_cov_abs_smd_le_0_10) & merged_df$cov_pct_cov_abs_smd_le_0_10 < cov_min_pct_le_0_10
merged_df$alert_joint_balance_fail <- isTRUE(FALSE)
merged_df$alert_joint_balance_fail <- (
  (is.finite(merged_df$selected_max_smd) & merged_df$selected_max_smd > as.numeric(lambda_cfg$hard_gates$max_smd)) |
  (is.finite(merged_df$cov_abs_smd_post_max) & merged_df$cov_abs_smd_post_max > cov_hard_max_smd)
)
merged_df$alert_block_failure <- !is.na(merged_df$block_any_fail) & merged_df$block_any_fail

merged_df$alert_any <- (
  merged_df$alert_any_lambda |
  merged_df$alert_cov_missing |
  merged_df$alert_cov_max_smd_over_hard |
  merged_df$alert_cov_low_pct_le_0_10 |
  merged_df$alert_joint_balance_fail |
  merged_df$alert_block_failure |
  merged_df$lambda_status != "ok"
)

merged_df$alert_count <- merged_df$alert_count_lambda +
  as.integer(merged_df$alert_cov_missing) +
  as.integer(merged_df$alert_cov_max_smd_over_hard) +
  as.integer(merged_df$alert_cov_low_pct_le_0_10) +
  as.integer(merged_df$alert_joint_balance_fail) +
  as.integer(merged_df$alert_block_failure) +
  as.integer(merged_df$lambda_status != "ok")

alert_df <- merged_df[(merged_df$alert_any | merged_df$lambda_status != "ok"), , drop = FALSE]
alert_df <- alert_df[order(-alert_df$alert_count, alert_df$year, alert_df$area), , drop = FALSE]

gate_table <- as.data.frame(table(ifelse(is.na(merged_df$selected_gate), "NA", merged_df$selected_gate)), stringsAsFactors = FALSE)
colnames(gate_table) <- c("selected_gate", "n_years")
gate_table <- gate_table[order(-gate_table$n_years), , drop = FALSE]

coverage <- data.frame(
  n_lambda_files = nrow(lambda_df),
  n_covariate_summaries = if (nrow(cov_df) > 0) nrow(cov_df) else 0L,
  n_joined_year_area = sum(!is.na(merged_df$cov_file), na.rm = TRUE),
  pct_joined_year_area = ifelse(nrow(merged_df) > 0, 100 * mean(!is.na(merged_df$cov_file)), NA_real_),
  stringsAsFactors = FALSE
)

overall <- data.frame(
  n_year_area = nrow(merged_df),
  n_lambda_ok = sum(merged_df$lambda_status == "ok", na.rm = TRUE),
  n_any_alert = sum(merged_df$alert_any, na.rm = TRUE),
  n_emergency = sum(merged_df$alert_emergency, na.rm = TRUE),
  n_fallback = sum(merged_df$alert_fallback, na.rm = TRUE),
  n_cov_missing = sum(merged_df$alert_cov_missing, na.rm = TRUE),
  n_cov_max_smd_over_hard = sum(merged_df$alert_cov_max_smd_over_hard, na.rm = TRUE),
  n_cov_low_pct_le_0_10 = sum(merged_df$alert_cov_low_pct_le_0_10, na.rm = TRUE),
  n_block_failure = sum(merged_df$alert_block_failure, na.rm = TRUE),
  mean_selected_max_smd = mean(merged_df$selected_max_smd, na.rm = TRUE),
  p90_selected_max_smd = safe_quantile(merged_df$selected_max_smd, 0.90),
  mean_selected_ess = mean(merged_df$selected_ess, na.rm = TRUE),
  mean_selected_ess_frac = mean(merged_df$selected_ess_frac, na.rm = TRUE),
  p10_selected_ess_frac = safe_quantile(merged_df$selected_ess_frac, 0.10),
  mean_cov_ess_control = mean(merged_df$cov_ess_control, na.rm = TRUE),
  mean_cov_ess_ratio = mean(merged_df$cov_ess_ratio, na.rm = TRUE),
  mean_cov_abs_smd_post_max = mean(merged_df$cov_abs_smd_post_max, na.rm = TRUE),
  p90_cov_abs_smd_post_max = safe_quantile(merged_df$cov_abs_smd_post_max, 0.90),
  mean_cov_pct_le_0_10 = mean(merged_df$cov_pct_cov_abs_smd_le_0_10, na.rm = TRUE),
  stringsAsFactors = FALSE
)

overall$share_any_alert_pct <- 100 * safe_ratio(overall$n_any_alert[1], overall$n_year_area[1])
overall$share_emergency_pct <- 100 * safe_ratio(overall$n_emergency[1], overall$n_year_area[1])
overall$share_fallback_pct <- 100 * safe_ratio(overall$n_fallback[1], overall$n_year_area[1])
overall$share_cov_missing_pct <- 100 * safe_ratio(overall$n_cov_missing[1], overall$n_year_area[1])

alert_driver_cols <- c(
  "alert_no_chosen",
  "alert_emergency",
  "alert_fallback",
  "alert_smd_over_hard",
  "alert_top10_over_hard",
  "alert_maxw_over_hard",
  "alert_ess_below_floor",
  "alert_ess_near_floor",
  "alert_hard_has_no_feasible",
  "alert_stage2_not_run",
  "alert_too_few_candidates",
  "alert_cov_missing",
  "alert_cov_max_smd_over_hard",
  "alert_cov_low_pct_le_0_10",
  "alert_joint_balance_fail",
  "alert_block_failure"
)
alert_driver_cols <- alert_driver_cols[alert_driver_cols %in% colnames(merged_df)]

alert_driver <- data.frame(
  alert = alert_driver_cols,
  n = vapply(alert_driver_cols, function(nm) sum(as.integer(merged_df[[nm]]), na.rm = TRUE), numeric(1)),
  stringsAsFactors = FALSE
)
alert_driver$alert_label <- alert_label(alert_driver$alert)
alert_driver$pct_year_area <- if (nrow(merged_df) > 0) 100 * alert_driver$n / nrow(merged_df) else NA_real_
alert_driver <- alert_driver[order(-alert_driver$n, alert_driver$alert), , drop = FALSE]

priority_order <- c(
  "alert_no_chosen",
  "alert_emergency",
  "alert_ess_below_floor",
  "alert_smd_over_hard",
  "alert_cov_max_smd_over_hard",
  "alert_cov_low_pct_le_0_10",
  "alert_block_failure",
  "alert_hard_has_no_feasible",
  "alert_fallback",
  "alert_stage2_not_run",
  "alert_too_few_candidates",
  "alert_joint_balance_fail",
  "alert_cov_missing",
  "alert_ess_near_floor",
  "alert_top10_over_hard",
  "alert_maxw_over_hard"
)
priority_order <- priority_order[priority_order %in% colnames(merged_df)]

merged_df$primary_alert <- NA_character_
if (length(priority_order) > 0 && nrow(merged_df) > 0) {
  for (i in seq_len(nrow(merged_df))) {
    hits <- priority_order[which(vapply(priority_order, function(nm) isTRUE(merged_df[[nm]][i]), logical(1)))]
    if (length(hits) > 0) merged_df$primary_alert[i] <- hits[1]
  }
}
merged_df$primary_alert_label <- ifelse(is.na(merged_df$primary_alert), NA_character_, alert_label(merged_df$primary_alert))

priority_df <- merged_df[(merged_df$alert_any | merged_df$lambda_status != "ok"), c(
  "experiment_name",
  "year", "area", "lambda_status", "selected_gate", "selected_lambda",
  "selected_max_smd", "selected_ess", "required_ess_floor", "selected_ess_frac", "cov_abs_smd_post_max",
  "cov_pct_cov_abs_smd_le_0_10", "block_worst_name", "block_worst_abs_smd_post_max", "block_n_fail",
  "alert_count", "primary_alert", "primary_alert_label"
), drop = FALSE]
priority_df <- priority_df[order(-priority_df$alert_count, priority_df$year, priority_df$area), , drop = FALSE]

n_hard <- sum(merged_df$selected_gate == "hard", na.rm = TRUE)
n_joint_fail <- sum(merged_df$alert_joint_balance_fail, na.rm = TRUE)
paper_summary <- data.frame(
  experiment_name = experiment_name,
  n_year_area = nrow(merged_df),
  n_lambda_ok = sum(merged_df$lambda_status == "ok", na.rm = TRUE),
  pct_lambda_ok = 100 * safe_ratio(sum(merged_df$lambda_status == "ok", na.rm = TRUE), nrow(merged_df)),
  n_any_alert = sum(merged_df$alert_any, na.rm = TRUE),
  pct_any_alert = 100 * safe_ratio(sum(merged_df$alert_any, na.rm = TRUE), nrow(merged_df)),
  n_selected_hard = n_hard,
  pct_selected_hard = 100 * safe_ratio(n_hard, nrow(merged_df)),
  n_emergency = sum(merged_df$alert_emergency, na.rm = TRUE),
  pct_emergency = 100 * safe_ratio(sum(merged_df$alert_emergency, na.rm = TRUE), nrow(merged_df)),
  n_joint_balance_fail = n_joint_fail,
  pct_joint_balance_fail = 100 * safe_ratio(n_joint_fail, nrow(merged_df)),
  n_block_failure = sum(merged_df$alert_block_failure, na.rm = TRUE),
  pct_block_failure = 100 * safe_ratio(sum(merged_df$alert_block_failure, na.rm = TRUE), nrow(merged_df)),
  mean_selected_max_smd = mean(merged_df$selected_max_smd, na.rm = TRUE),
  p90_selected_max_smd = safe_quantile(merged_df$selected_max_smd, 0.90),
  mean_selected_ess = mean(merged_df$selected_ess, na.rm = TRUE),
  mean_selected_ess_frac = mean(merged_df$selected_ess_frac, na.rm = TRUE),
  p10_selected_ess_frac = safe_quantile(merged_df$selected_ess_frac, 0.10),
  n_ess_near_floor = sum(merged_df$alert_ess_near_floor, na.rm = TRUE),
  pct_ess_near_floor = 100 * safe_ratio(sum(merged_df$alert_ess_near_floor, na.rm = TRUE), nrow(merged_df)),
  mean_cov_ess_control = mean(merged_df$cov_ess_control, na.rm = TRUE),
  mean_cov_ess_ratio = mean(merged_df$cov_ess_ratio, na.rm = TRUE),
  mean_cov_abs_smd_post_max = mean(merged_df$cov_abs_smd_post_max, na.rm = TRUE),
  p90_cov_abs_smd_post_max = safe_quantile(merged_df$cov_abs_smd_post_max, 0.90),
  mean_cov_pct_le_0_10 = mean(merged_df$cov_pct_cov_abs_smd_le_0_10, na.rm = TRUE),
  lambda_hard_gate_max_smd = as.numeric(lambda_cfg$hard_gates$max_smd),
  covariate_hard_max_smd = cov_hard_max_smd,
  covariate_min_pct_le_0_10 = cov_min_pct_le_0_10,
  stringsAsFactors = FALSE
)

paper_priority <- utils::head(priority_df, n = save_top_alerts)

insights <- data.frame(
  insight = c(
    "experiment_name",
    "lambda_pattern",
    "covariate_pattern",
    "lambda_hard_gate_max_smd",
    "covariate_hard_max_smd",
    "covariate_min_pct_le_0_10",
    "share_years_with_any_alert_pct",
    "share_years_emergency_pct",
    "share_years_missing_cov_summary_pct"
  ),
  value = c(
    experiment_name,
    lambda_pattern,
    covariate_pattern,
    as.numeric(lambda_cfg$hard_gates$max_smd),
    cov_hard_max_smd,
    cov_min_pct_le_0_10,
    ifelse(nrow(merged_df) > 0, 100 * mean(merged_df$alert_any, na.rm = TRUE), NA_real_),
    ifelse(nrow(merged_df) > 0, 100 * mean(merged_df$alert_emergency, na.rm = TRUE), NA_real_),
    ifelse(nrow(merged_df) > 0, 100 * mean(merged_df$alert_cov_missing, na.rm = TRUE), NA_real_)
  ),
  stringsAsFactors = FALSE
)

paper_summary_file <- file.path(out_dir, "paper_cbps_batch_summary.csv")
paper_priority_file <- file.path(out_dir, "paper_cbps_priority_alerts.csv")
paper_brief_file <- file.path(out_dir, "paper_cbps_results_brief.md")

year_file <- file.path(out_dir, "batch_year_diagnostics.csv")
alerts_file <- file.path(out_dir, "batch_alerts.csv")
overall_file <- file.path(out_dir, "batch_overall_summary.csv")
gate_file <- file.path(out_dir, "batch_gate_distribution.csv")
coverage_file <- file.path(out_dir, "batch_data_coverage.csv")
insights_file <- file.path(out_dir, "batch_insights.csv")
driver_file <- file.path(out_dir, "batch_alert_driver_summary.csv")
priority_file <- file.path(out_dir, "batch_priority_table.csv")

utils::write.csv(paper_summary, paper_summary_file, row.names = FALSE)
utils::write.csv(paper_priority, paper_priority_file, row.names = FALSE)

brief_lines <- c(
  "# CBPS Batch Diagnostics Results Brief",
  "",
  sprintf("- Experiment: %s", experiment_name),
  sprintf("- Lambda dir: %s", lambda_dir),
  sprintf("- Covariate dir: %s", covariate_dir),
  sprintf("- Lambda file pattern: %s", lambda_pattern),
  sprintf("- Covariate file pattern: %s", covariate_pattern),
  "",
  sprintf("- Total year-area units analyzed: %d", paper_summary$n_year_area[1]),
  sprintf("- Lambda status OK: %.1f%% (%d/%d)", paper_summary$pct_lambda_ok[1], paper_summary$n_lambda_ok[1], paper_summary$n_year_area[1]),
  sprintf("- Any alert triggered: %.1f%% (%d/%d)", paper_summary$pct_any_alert[1], paper_summary$n_any_alert[1], paper_summary$n_year_area[1]),
  sprintf("- Selected hard gate: %.1f%% (%d/%d)", paper_summary$pct_selected_hard[1], paper_summary$n_selected_hard[1], paper_summary$n_year_area[1]),
  sprintf("- Emergency selections: %.1f%% (%d/%d)", paper_summary$pct_emergency[1], paper_summary$n_emergency[1], paper_summary$n_year_area[1]),
  sprintf("- Joint balance failures: %.1f%% (%d/%d)", paper_summary$pct_joint_balance_fail[1], paper_summary$n_joint_balance_fail[1], paper_summary$n_year_area[1]),
  sprintf("- Block-level covariate failures: %.1f%% (%d/%d)", paper_summary$pct_block_failure[1], paper_summary$n_block_failure[1], paper_summary$n_year_area[1]),
  "",
  "## Balance Quality",
  sprintf("- Mean selected max SMD: %.4f", paper_summary$mean_selected_max_smd[1]),
  sprintf("- P90 selected max SMD: %.4f", paper_summary$p90_selected_max_smd[1]),
  sprintf("- Mean selected ESS (absolute): %.1f", paper_summary$mean_selected_ess[1]),
  sprintf("- Mean selected ESS fraction: %.4f", paper_summary$mean_selected_ess_frac[1]),
  sprintf("- P10 selected ESS fraction: %.4f", paper_summary$p10_selected_ess_frac[1]),
  sprintf("- ESS near-floor years: %.1f%% (%d/%d)", paper_summary$pct_ess_near_floor[1], paper_summary$n_ess_near_floor[1], paper_summary$n_year_area[1]),
  sprintf("- Mean covariate ESS control: %.1f", paper_summary$mean_cov_ess_control[1]),
  sprintf("- Mean covariate ESS ratio: %.4f", paper_summary$mean_cov_ess_ratio[1]),
  sprintf("- Mean covariate post max SMD: %.4f", paper_summary$mean_cov_abs_smd_post_max[1]),
  sprintf("- P90 covariate post max SMD: %.4f", paper_summary$p90_cov_abs_smd_post_max[1]),
  sprintf("- Mean share of covariates with abs(SMD)<=0.10: %.4f", paper_summary$mean_cov_pct_le_0_10[1]),
  "",
  "## Threshold Context",
  sprintf("- Lambda hard gate max SMD threshold: %.4f", paper_summary$lambda_hard_gate_max_smd[1]),
  sprintf("- Covariate hard max SMD threshold: %.4f", paper_summary$covariate_hard_max_smd[1]),
  sprintf("- Covariate minimum share abs(SMD)<=0.10: %.4f", paper_summary$covariate_min_pct_le_0_10[1]),
  "",
  "## Top Priority Alerts"
)

if (nrow(paper_priority) > 0) {
  top_k <- min(5, nrow(paper_priority))
  for (i in seq_len(top_k)) {
    brief_lines <- c(
      brief_lines,
      sprintf(
        "- %d) year=%s area=%s primary_alert=%s alert_count=%s selected_ess=%s req_ess_floor=%s selected_max_smd=%s cov_post_max_smd=%s worst_block=%s worst_block_post_max_smd=%s",
        i,
        as.character(paper_priority$year[i]),
        as.character(paper_priority$area[i]),
        as.character(paper_priority$primary_alert_label[i]),
        as.character(paper_priority$alert_count[i]),
        as.character(signif(paper_priority$selected_ess[i], 5)),
        as.character(signif(paper_priority$required_ess_floor[i], 5)),
        as.character(signif(paper_priority$selected_max_smd[i], 4)),
        as.character(signif(paper_priority$cov_abs_smd_post_max[i], 4)),
        as.character(paper_priority$block_worst_name[i]),
        as.character(signif(paper_priority$block_worst_abs_smd_post_max[i], 4))
      )
    )
  }
} else {
  brief_lines <- c(brief_lines, "- No priority alert rows found.")
}

writeLines(brief_lines, con = paper_brief_file)

if (isTRUE(save_detailed)) {
  utils::write.csv(merged_df, year_file, row.names = FALSE)
  utils::write.csv(alert_df, alerts_file, row.names = FALSE)
  utils::write.csv(overall, overall_file, row.names = FALSE)
  utils::write.csv(gate_table, gate_file, row.names = FALSE)
  utils::write.csv(coverage, coverage_file, row.names = FALSE)
  utils::write.csv(insights, insights_file, row.names = FALSE)
  utils::write.csv(alert_driver, driver_file, row.names = FALSE)
  utils::write.csv(priority_df, priority_file, row.names = FALSE)
}

cat("\n=== Batch Multi-Year Diagnostics ===\n")
cat("experiment:", experiment_name, "\n")
cat("lambda files scanned:", nrow(lambda_df), "\n")
cat("covariate summaries found:", ifelse(nrow(cov_df) > 0, nrow(cov_df), 0), "\n")
cat("year-area rows in final batch table:", nrow(merged_df), "\n")
cat("rows with alerts:", nrow(alert_df), "\n")
cat("share with any alert (%):", sprintf("%.1f", overall$share_any_alert_pct[1]), "\n")

cat("\nTop alert drivers:\n")
alert_driver_nonzero <- alert_driver[alert_driver$n > 0, , drop = FALSE]
if (nrow(alert_driver_nonzero) > 0) {
  show_driver <- utils::head(alert_driver_nonzero[, c("alert_label", "n", "pct_year_area"), drop = FALSE], n = min(6, nrow(alert_driver_nonzero)))
  print(show_driver, row.names = FALSE)
} else {
  cat("No non-zero alert drivers found.\n")
}

cat("\nTop", top_n, "rows by alert_count:\n")
if (nrow(alert_df) > 0) {
  show_cols <- c(
    "year", "area", "lambda_status", "selected_gate", "selected_lambda",
    "selected_max_smd", "cov_abs_smd_post_max", "cov_pct_cov_abs_smd_le_0_10",
    "alert_count"
  )
  show_cols <- show_cols[show_cols %in% colnames(alert_df)]
  print(utils::head(alert_df[, show_cols, drop = FALSE], n = top_n), row.names = FALSE)
} else {
  cat("No alert rows found.\n")
}

cat("\nSaved files:\n")
cat("- ", paper_summary_file, "\n", sep = "")
cat("- ", paper_priority_file, "\n", sep = "")
cat("- ", paper_brief_file, "\n", sep = "")

if (isTRUE(save_detailed)) {
  cat("\nDetailed files (--save-detailed=true):\n")
  cat("- ", year_file, "\n", sep = "")
  cat("- ", alerts_file, "\n", sep = "")
  cat("- ", overall_file, "\n", sep = "")
  cat("- ", gate_file, "\n", sep = "")
  cat("- ", coverage_file, "\n", sep = "")
  cat("- ", insights_file, "\n", sep = "")
  cat("- ", driver_file, "\n", sep = "")
  cat("- ", priority_file, "\n", sep = "")
}
