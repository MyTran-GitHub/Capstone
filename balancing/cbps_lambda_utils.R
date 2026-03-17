# Utility helpers for cbps lambda grid search and diagnostics
source("balancing/balancing_config.R")
get_diagnostics_config <- get("get_diagnostics_config", mode = "function")

make_lambda_grid <- function(level = c("very_coarse", "coarse", "full")) {
  # Return a lambda grid tuned for runtime vs coverage.
  # level: "very_coarse" -> minimal set (fastest),
  #        "coarse"      -> default balanced grid,
  #        "full"        -> original fine grid for exhaustive search.
  level <- match.arg(level)
  if (level == "very_coarse") {
    # very small set to drastically reduce runtime while keeping a wide dynamic range
    # Default requested by user: include smaller values for a single-shot attempt
    grid <- c(10, 1, 0.1, 0.01, 0.001, 1e-4)
  } else if (level == "coarse") {
    grid <- c(10, 3, 1, 0.3, 0.1, 0.03, 0.01, 0.003)
  } else {
    grid <- c(10, 3, 1, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001, 1e-4)
  }
  unique(grid)
}

# Convenience accessors
make_full_lambda_grid <- function() make_lambda_grid("full")
make_very_coarse_lambda_grid <- function() make_lambda_grid("very_coarse")

# Default inflation factor for problematic covariates. Increase this
# to give more chance for convergence by de-emphasizing flagged covariates.
get_lambda_inflation_factor <- function() {
  100
}

compute_weights_metrics <- function(res, W) {
  if (is.null(res) || is.null(res$weights.0)) return(NULL)
  w_all <- res$weights.0
  w_ctrl <- w_all[W == 0]
  if (length(w_ctrl) == 0) return(NULL)
  if (any(!is.finite(w_ctrl))) return(NULL)

  total_ctrl <- sum(w_ctrl, na.rm = TRUE)
  if (!is.finite(total_ctrl) || total_ctrl <= 0) return(NULL)
  ess_ctrl <- ifelse(total_ctrl == 0, NA, (total_ctrl^2) / sum(w_ctrl^2))
  k <- ceiling(0.10 * length(w_ctrl))
  top10_share <- ifelse(total_ctrl == 0, NA, sum(sort(w_ctrl, decreasing = TRUE)[1:k]) / total_ctrl)
  max_weight <- ifelse(total_ctrl == 0, NA, max(w_ctrl, na.rm = TRUE) / total_ctrl)

  max_smd <- if (!is.null(res$balance.std)) max(abs(res$balance.std), na.rm = TRUE) else NA

  list(ess = ess_ctrl, top10_share = top10_share, max_weight = max_weight, max_smd = max_smd)
}

run_lambda_selection <- function(results_df, n_ctrl) {
  tiers <- get_diagnostics_config()$selection_thresholds$tiers
  if (is.null(tiers) || length(tiers) == 0) {
    stop("run_lambda_selection requires selection_thresholds$tiers in diagnostics config.")
  }

  selection_log <- list(
    tier_used = NULL,
    n_candidates_total = if (!is.null(results_df)) nrow(results_df) else 0,
    n_feasible_by_tier = list(),
    selected_lambda = NULL,
    selected_metrics = NULL,
    ess_ratio = NULL,
    warnings = character()
  )

  required_cols <- c("lambda", "ess", "top10_share", "max_weight", "max_smd")
  if (is.null(results_df) || nrow(results_df) == 0) {
    stop("No feasible lambda found under any threshold tier. Check overlap or relax constraints. Diagnostic summary: no converged lambda candidates available.")
  }
  missing_cols <- setdiff(required_cols, colnames(results_df))
  if (length(missing_cols) > 0) {
    stop("run_lambda_selection missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.finite(n_ctrl) || n_ctrl <= 0) {
    stop("run_lambda_selection requires n_ctrl > 0.")
  }

  for (tier in tiers) {
    feasible <- results_df[
      !is.na(results_df$max_smd) &
        !is.na(results_df$top10_share) &
        !is.na(results_df$max_weight) &
        results_df$max_smd <= tier$max_smd &
        results_df$top10_share <= tier$top10 &
        results_df$max_weight <= tier$max_weight,
      ,
      drop = FALSE
    ]

    selection_log$n_feasible_by_tier[[tier$name]] <- nrow(feasible)

    if (nrow(feasible) == 0) next

    ess_max <- max(feasible$ess, na.rm = TRUE)
    if (!is.finite(ess_max)) next
    stable <- feasible[!is.na(feasible$ess) & feasible$ess >= 0.9 * ess_max, , drop = FALSE]
    if (nrow(stable) == 0) next

    min_smd <- min(stable$max_smd, na.rm = TRUE)
    finalists <- stable[stable$max_smd == min_smd, , drop = FALSE]
    finalists <- finalists[order(finalists$lambda), , drop = FALSE]
    selected <- finalists[1, , drop = FALSE]

    ess_ratio <- as.numeric(selected$ess / n_ctrl)
    selection_log$tier_used <- tier$name
    selection_log$selected_lambda <- as.numeric(selected$lambda)
    selection_log$selected_metrics <- list(
      ess = as.numeric(selected$ess),
      top10_share = as.numeric(selected$top10_share),
      max_weight = as.numeric(selected$max_weight),
      max_smd = as.numeric(selected$max_smd)
    )
    selection_log$ess_ratio <- ess_ratio

    if (tier$name != "strict") {
      selection_log$warnings <- c(selection_log$warnings, paste("Fallback tier used:", tier$name))
    }
    if (is.finite(ess_ratio) && ess_ratio < 0.1) {
      selection_log$warnings <- c(selection_log$warnings, "Low ESS/N (<0.1)")
    }

    return(list(selected_row = selected, selection_log = selection_log))
  }

  feasible_summary <- paste(
    vapply(names(selection_log$n_feasible_by_tier), function(nm) {
      paste0(nm, "=", selection_log$n_feasible_by_tier[[nm]])
    }, character(1)),
    collapse = ", "
  )
  stop(
    "No feasible lambda found under any threshold tier. Check overlap or relax constraints. ",
    "Diagnostic summary: n_candidates_total=", selection_log$n_candidates_total,
    "; n_feasible_by_tier=[", feasible_summary, "]"
  )
}

compute_covariate_overlap <- function(X, W) {
  # Return per-covariate diagnostics: pre-SMD, pct_outside, ks_stat
  treated_idx <- which(W == 1)
  ctrl_idx <- which(W == 0)
  p <- ncol(X)
  smd_pre <- rep(NA, p)
  pct_outside <- rep(0, p)
  ks_stat <- rep(0, p)
  names(smd_pre) <- colnames(X)
  names(pct_outside) <- colnames(X)
  names(ks_stat) <- colnames(X)

  for (j in seq_len(p)) {
    x <- X[, j]
    xt <- x[treated_idx]
    xc <- x[ctrl_idx]
    if (length(xt) == 0 || length(xc) == 0) next
    sd_c <- sd(xc, na.rm = TRUE)
    if (is.na(sd_c) || sd_c == 0) sd_c <- sd(c(xc, xt), na.rm = TRUE)
    if (is.na(sd_c) || sd_c == 0) sd_c <- 1
    smd_pre[j] <- (mean(xt, na.rm = TRUE) - mean(xc, na.rm = TRUE)) / sd_c
    pct_outside[j] <- mean(xt < min(xc, na.rm = TRUE) | xt > max(xc, na.rm = TRUE), na.rm = TRUE)
    # KS test (two-sample) — use try in case of constant vectors
    ks <- tryCatch(ks.test(xt, xc)$statistic, error = function(e) NA)
    ks_stat[j] <- ifelse(is.null(ks) || length(ks) == 0, NA, as.numeric(ks))
  }
  data.frame(covariate = colnames(X), smd_pre = smd_pre, pct_outside = pct_outside, ks = ks_stat, stringsAsFactors = FALSE)
}

infer_covariate_block <- function(covariate_names) {
  if (length(covariate_names) == 0) return(character(0))
  # Use leading token as block (e.g., prcp_2007_autumn -> prcp)
  block <- sub("_.*$", "", covariate_names)
  # Preserve full name when no underscore exists
  no_sep <- !grepl("_", covariate_names)
  block[no_sep] <- covariate_names[no_sep]
  block
}

default_overlap_thresholds <- function() {
  # Practical pre-fit overlap thresholds used in applied causal inference workflows.
  get_diagnostics_config()$overlap_thresholds
}

screen_prefit_overlap <- function(X, W, thresholds = default_overlap_thresholds()) {
  overlap <- compute_covariate_overlap(X, W)
  if (nrow(overlap) == 0) {
    return(list(
      overlap = overlap,
      flagged = character(0),
      severe = character(0),
      summary = data.frame(stringsAsFactors = FALSE),
      block_summary = data.frame(stringsAsFactors = FALSE),
      feasible = TRUE
    ))
  }

  abs_smd <- abs(overlap$smd_pre)
  warn_any <- (abs_smd > thresholds$smd_warn) |
    (overlap$pct_outside > thresholds$pct_outside_warn) |
    (overlap$ks > thresholds$ks_warn)
  fail_any <- (abs_smd > thresholds$smd_fail) |
    (overlap$pct_outside > thresholds$pct_outside_fail) |
    (overlap$ks > thresholds$ks_fail)

  overlap$abs_smd_pre <- abs_smd
  overlap$flag_warn <- warn_any
  overlap$flag_fail <- fail_any
  overlap$block <- infer_covariate_block(overlap$covariate)

  n_cov <- nrow(overlap)
  n_warn <- sum(overlap$flag_warn, na.rm = TRUE)
  n_fail <- sum(overlap$flag_fail, na.rm = TRUE)
  fail_fraction <- if (n_cov > 0) n_fail / n_cov else 0

  summary <- data.frame(
    n_covariates = n_cov,
    n_warn = n_warn,
    n_fail = n_fail,
    warn_fraction = ifelse(n_cov > 0, n_warn / n_cov, 0),
    fail_fraction = fail_fraction,
    max_abs_smd_pre = max(overlap$abs_smd_pre, na.rm = TRUE),
    max_pct_outside = max(overlap$pct_outside, na.rm = TRUE),
    max_ks = max(overlap$ks, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  block_summary <- do.call(rbind, lapply(split(overlap, overlap$block), function(g) {
    data.frame(
      block = unique(g$block)[1],
      n_covariates = nrow(g),
      n_warn = sum(g$flag_warn, na.rm = TRUE),
      n_fail = sum(g$flag_fail, na.rm = TRUE),
      max_abs_smd_pre = max(g$abs_smd_pre, na.rm = TRUE),
      max_pct_outside = max(g$pct_outside, na.rm = TRUE),
      max_ks = max(g$ks, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  if (!is.null(block_summary) && nrow(block_summary) > 0) {
    block_summary <- block_summary[order(-block_summary$n_fail, -block_summary$max_abs_smd_pre), , drop = FALSE]
    rownames(block_summary) <- NULL
  }

  feasible <- fail_fraction <= thresholds$max_fail_fraction

  list(
    overlap = overlap,
    flagged = overlap$covariate[overlap$flag_warn],
    severe = overlap$covariate[overlap$flag_fail],
    summary = summary,
    block_summary = block_summary,
    feasible = feasible
  )
}

select_lambda_from_candidates <- function(cands_df) {
  # cands_df must contain columns: lambda, ess, top10_share, max_weight, max_smd
  if (is.null(cands_df) || nrow(cands_df) == 0) return(NULL)
  # prefer smallest lambda as a deterministic tiebreaker
  best_row <- cands_df[which.min(cands_df$lambda), , drop = FALSE]
  best_row
}
