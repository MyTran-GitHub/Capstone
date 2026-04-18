#' Utility helpers for CBPS lambda grid search and diagnostics
#'
#' This script provides functions for lambda grid construction, gate evaluation, and diagnostics in CBPS workflows.
# Utility helpers for cbps lambda grid search and diagnostics
source("balancing/balancing_config.R")
get_diagnostics_config <- get("get_diagnostics_config", mode = "function")

compute_gate_required_ess <- function(gate, n_ctrl, n_treated) {
  ess_candidates <- c()
  if (!is.null(gate$ess_frac)) ess_candidates <- c(ess_candidates, as.numeric(gate$ess_frac) * n_ctrl)
  if (!is.null(gate$ess_abs)) ess_candidates <- c(ess_candidates, as.numeric(gate$ess_abs))
  if (!is.null(gate$ess_mult_treated) && is.finite(n_treated) && n_treated > 0) {
    ess_candidates <- c(ess_candidates, as.numeric(gate$ess_mult_treated) * n_treated)
  }
  ess_candidates <- ess_candidates[is.finite(ess_candidates)]
  if (length(ess_candidates) == 0) return(0)
  max(ess_candidates)
}

evaluate_gate_pass <- function(d, gate, n_ctrl, n_treated) {
  gate_max_weight <- if (!is.null(gate$max_weight)) as.numeric(gate$max_weight) else Inf
  gate_ess_required <- compute_gate_required_ess(gate, n_ctrl, n_treated)
  pass <-
    !is.na(d$max_smd) &
    !is.na(d$median_smd) &
    !is.na(d$top10_share) &
    !is.na(d$max_weight) &
    !is.na(d$ess) &
    d$max_smd <= as.numeric(gate$max_smd) &
    d$median_smd <= as.numeric(gate$median_smd) &
    d$top10_share <= as.numeric(gate$top10_share) &
    d$max_weight <= gate_max_weight &
    d$ess >= gate_ess_required

  list(pass = pass, required_ess = gate_ess_required)
}

make_lambda_grid <- function(level = c("very_coarse", "coarse", "full")) {
  # Return a lambda grid tuned for runtime vs coverage.
  # level: "very_coarse" -> minimal set (fastest),
  #        "coarse"      -> default balanced grid,
  #        "full"        -> original fine grid for exhaustive search.
  level <- match.arg(level)
  if (level == "very_coarse") {
    # Focus stage-1 search near the empirical balance/overlap frontier.
    grid <- c(0.1, 0.03, 0.02, 0.01, 0.006, 0.003, 0.001)
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

make_refined_lambda_grid <- function(lower_lambda,
                                     upper_lambda,
                                     n_inner = 6,
                                     min_ratio = 1.05) {
  # Build an interior log-spaced grid between two lambda values.
  # Returns descending values and excludes endpoints.
  if (!is.finite(lower_lambda) || !is.finite(upper_lambda)) return(numeric(0))
  if (lower_lambda <= 0 || upper_lambda <= 0) return(numeric(0))
  if (upper_lambda <= lower_lambda) return(numeric(0))
  if (!is.finite(n_inner) || n_inner < 1) return(numeric(0))

  ratio <- upper_lambda / lower_lambda
  if (!is.finite(ratio) || ratio < min_ratio) return(numeric(0))

  grid <- exp(seq(log(upper_lambda), log(lower_lambda), length.out = n_inner + 2))
  # Drop endpoints; keep unique values sorted high to low for warm-start stability.
  refined <- sort(unique(grid[2:(length(grid) - 1)]), decreasing = TRUE)
  refined
}

# Default inflation factor for problematic covariates. Increase this
# to give more chance for convergence by de-emphasizing flagged covariates.
get_lambda_inflation_factor <- function() {
  100
}

compute_weights_metrics <- function(res, W) {
  if (is.null(res) || is.null(res$weights.0)) return(NULL)
  w_all <- res$weights.0
  n_ctrl <- sum(W == 0)
  if (length(w_all) == n_ctrl) {
    w_ctrl <- w_all
  } else if (length(w_all) == length(W)) {
    w_ctrl <- w_all[W == 0]
  } else {
    return(NULL)
  }
  if (length(w_ctrl) == 0) return(NULL)
  if (any(!is.finite(w_ctrl))) return(NULL)

  total_ctrl <- sum(w_ctrl, na.rm = TRUE)
  if (!is.finite(total_ctrl) || total_ctrl <= 0) return(NULL)
  ess_ctrl <- ifelse(total_ctrl == 0, NA, (total_ctrl^2) / sum(w_ctrl^2))
  k <- min(length(w_ctrl), max(1L, ceiling(0.10 * length(w_ctrl))))
  # Partial sort keeps this O(n) average for top-k extraction instead of full O(n log n).
  topk <- if (k >= length(w_ctrl)) w_ctrl else -sort(-w_ctrl, partial = seq_len(k))[seq_len(k)]
  top10_share <- ifelse(total_ctrl == 0, NA, sum(topk, na.rm = TRUE) / total_ctrl)
  max_weight <- ifelse(total_ctrl == 0, NA, max(w_ctrl, na.rm = TRUE) / total_ctrl)

  abs_smd <- if (!is.null(res$balance.std)) abs(res$balance.std) else NA
  max_smd <- if (all(is.na(abs_smd))) NA_real_ else max(abs_smd, na.rm = TRUE)
  median_smd <- if (all(is.na(abs_smd))) NA_real_ else stats::median(abs_smd, na.rm = TRUE)

  list(
    ess = ess_ctrl,
    top10_share = top10_share,
    max_weight = max_weight,
    max_smd = max_smd,
    median_smd = median_smd
  )
}

annotate_lambda_gate_diagnostics <- function(results_df, n_ctrl, n_treated = NA_real_, cfg = get_diagnostics_config()) {
  if (is.null(results_df) || nrow(results_df) == 0) return(results_df)
  d <- results_df
  if (!"median_smd" %in% colnames(d)) d$median_smd <- d$max_smd
  d$ess_frac <- d$ess / n_ctrl

  lambda_cfg <- cfg$lambda_selection
  gate_profiles <- c(list(c(name = "hard", lambda_cfg$hard_gates)), lambda_cfg$fallback_gates)

  for (gate in gate_profiles) {
    nm <- as.character(gate$name)
    gate_eval <- evaluate_gate_pass(d, gate, n_ctrl, n_treated)
    pass_col <- paste0("pass_", nm)
    d[[paste0("required_ess_", nm)]] <- gate_eval$required_ess
    d[[pass_col]] <- gate_eval$pass
  }

  # Ordered reject reason for the hard gate.
  if ("pass_hard" %in% colnames(d)) {
    d$hard_fail_reason <- ifelse(
      d$pass_hard,
      "pass",
      ifelse(is.na(d$max_smd) | d$max_smd > cfg$lambda_selection$hard_gates$max_smd, "max_smd",
        ifelse(is.na(d$median_smd) | d$median_smd > cfg$lambda_selection$hard_gates$median_smd, "median_smd",
          ifelse(is.na(d$top10_share) | d$top10_share > cfg$lambda_selection$hard_gates$top10_share, "top10_share",
            ifelse(is.na(d$max_weight) | d$max_weight > cfg$lambda_selection$hard_gates$max_weight, "max_weight",
              ifelse(is.na(d$ess) | d$ess < d$required_ess_hard, "ess_floor", "unknown")
            )
          )
        )
      )
    )
  }

  pass_cols <- grep("^pass_", colnames(d), value = TRUE)
  if (length(pass_cols) > 0) {
    d$n_gates_passed <- rowSums(as.data.frame(lapply(d[pass_cols], function(x) as.integer(!is.na(x) & x))), na.rm = TRUE)
  }
  d
}

run_lambda_selection <- function(results_df, n_ctrl, n_treated = NA_real_) {
  cfg <- get_diagnostics_config()
  lambda_cfg <- cfg$lambda_selection
  if (!is.null(lambda_cfg) && !is.null(lambda_cfg$hard_gates)) {
    gate_profiles <- c(list(c(name = "hard", lambda_cfg$hard_gates)), lambda_cfg$fallback_gates)
  } else {
    # Backward compatibility with older config format.
    tiers <- cfg$selection_thresholds$tiers
    if (is.null(tiers) || length(tiers) == 0) {
      stop("run_lambda_selection requires lambda_selection config (or legacy selection_thresholds tiers).")
    }
    gate_profiles <- lapply(tiers, function(t) {
      list(
        name = t$name,
        max_smd = t$max_smd,
        median_smd = 0.05,
        top10_share = t$top10,
        ess_frac = 0.0
      )
    })
    lambda_cfg <- list(ess_plateau_frac = 0.90)
  }

  selection_log <- list(
    rule = "hard-gates + branch-specific plateau + lexicographic",
    tier_used = NULL,
    gate_used = NULL,
    n_candidates_total = if (!is.null(results_df)) nrow(results_df) else 0,
    n_feasible_by_tier = list(),
    selected_lambda = NULL,
    selected_metrics = NULL,
    required_ess_floor = NULL,
    ess_ratio = NULL,
    ess_frac = NULL,
    warnings = character()
  )

  required_cols <- c("lambda", "ess", "top10_share", "max_weight", "max_smd")
  if (is.null(results_df) || nrow(results_df) == 0) {
    stop("No lambda candidates available: no converged candidates returned.")
  }
  missing_cols <- setdiff(required_cols, colnames(results_df))
  if (length(missing_cols) > 0) {
    stop("run_lambda_selection missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.finite(n_ctrl) || n_ctrl <= 0) {
    stop("run_lambda_selection requires n_ctrl > 0.")
  }

  d <- results_df
  if (!"median_smd" %in% colnames(d)) {
    d$median_smd <- d$max_smd
  }
  d$ess_frac <- d$ess / n_ctrl

  near_equal <- function(x, target, rel_tol = 1e-8) {
    abs(x - target) <= pmax(1, abs(target)) * rel_tol
  }

  append_ess_floor_warnings <- function(warnings, ess_value, ess_floor) {
    if (!is.finite(ess_value) || !is.finite(ess_floor) || ess_floor <= 0) return(warnings)
    if (ess_value < ess_floor) {
      return(c(warnings, paste0("ESS below required floor: ess=", round(ess_value, 2), " < required=", round(ess_floor, 2))))
    }
    if (ess_value < 1.2 * ess_floor) {
      return(c(warnings, paste0("ESS near floor (soft caution): ess=", round(ess_value, 2),
        " < 1.2x required=", round(1.2 * ess_floor, 2))))
    }
    warnings
  }

  for (gate in gate_profiles) {
    gate_eval <- evaluate_gate_pass(d, gate, n_ctrl, n_treated)
    gate_ess_required <- gate_eval$required_ess
    feasible <- d[gate_eval$pass, , drop = FALSE]

    selection_log$n_feasible_by_tier[[as.character(gate$name)]] <- nrow(feasible)
    if (nrow(feasible) == 0) next

    # Soft anti-boundary filter: when possible, avoid selecting lambdas whose
    # ESS sits right on the hard floor. Keep feasibility by falling back to
    # the full feasible set if this filter would drop everything.
    soft_floor_mult <- ifelse(is.null(lambda_cfg$ess_soft_floor_mult), 1.0, as.numeric(lambda_cfg$ess_soft_floor_mult))
    if (is.finite(soft_floor_mult) && soft_floor_mult > 1 && is.finite(gate_ess_required) && gate_ess_required > 0) {
      soft_floor <- soft_floor_mult * gate_ess_required
      feasible_soft <- feasible[!is.na(feasible$ess) & feasible$ess >= soft_floor, , drop = FALSE]
      if (nrow(feasible_soft) > 0) {
        feasible <- feasible_soft
        selection_log$warnings <- c(
          selection_log$warnings,
          paste0(
            "Applied ESS soft-floor filter in ", as.character(gate$name),
            ": ess >= ", round(soft_floor, 2),
            " (", round(soft_floor_mult, 2), "x required floor)."
          )
        )
      }
    }

    use_plateau <- !identical(as.character(gate$name), "hard")
    if (use_plateau) {
      ess_max <- max(feasible$ess, na.rm = TRUE)
      if (!is.finite(ess_max)) next
      plateau_frac <- ifelse(is.null(lambda_cfg$ess_plateau_frac), 0.90, as.numeric(lambda_cfg$ess_plateau_frac))
      plateau <- feasible[!is.na(feasible$ess) & feasible$ess >= plateau_frac * ess_max, , drop = FALSE]
      if (nrow(plateau) == 0) next
    } else {
      # Option B: in hard gate, optimize balance directly over all hard-feasible lambdas.
      plateau <- feasible
    }

    min_max_smd <- min(plateau$max_smd, na.rm = TRUE)
    lvl1 <- plateau[near_equal(plateau$max_smd, min_max_smd), , drop = FALSE]
    min_med_smd <- min(lvl1$median_smd, na.rm = TRUE)
    lvl2 <- lvl1[near_equal(lvl1$median_smd, min_med_smd), , drop = FALSE]
    min_top10 <- min(lvl2$top10_share, na.rm = TRUE)
    lvl3 <- lvl2[near_equal(lvl2$top10_share, min_top10), , drop = FALSE]

    if (nrow(lvl3) == 1) {
      selected <- lvl3
    } else {
      med_lambda <- stats::median(lvl3$lambda, na.rm = TRUE)
      lvl3$._med_dist <- abs(lvl3$lambda - med_lambda)
      lvl3 <- lvl3[order(lvl3$._med_dist, lvl3$lambda), , drop = FALSE]
      selected <- lvl3[1, setdiff(colnames(lvl3), "._med_dist"), drop = FALSE]
    }

    ess_ratio <- as.numeric(selected$ess / n_ctrl)
    selection_log$tier_used <- as.character(gate$name)
    selection_log$gate_used <- as.character(gate$name)
    selection_log$selected_lambda <- as.numeric(selected$lambda)
    selection_log$selected_metrics <- list(
      ess = as.numeric(selected$ess),
      ess_frac = as.numeric(selected$ess_frac),
      top10_share = as.numeric(selected$top10_share),
      max_weight = as.numeric(selected$max_weight),
      max_smd = as.numeric(selected$max_smd),
      median_smd = as.numeric(selected$median_smd)
    )
    selection_log$required_ess_floor <- as.numeric(gate_ess_required)
    selection_log$ess_ratio <- ess_ratio
    selection_log$ess_frac <- ess_ratio

    if (!identical(as.character(gate$name), "hard")) {
      selection_log$warnings <- c(selection_log$warnings, paste("Fallback gate used:", gate$name))
    }
    selection_log$warnings <- append_ess_floor_warnings(
      selection_log$warnings,
      ess_value = as.numeric(selected$ess),
      ess_floor = as.numeric(gate_ess_required)
    )

    return(list(selected_row = selected, selection_log = selection_log))
  }

  feasible_summary <- paste(
    vapply(names(selection_log$n_feasible_by_tier), function(nm) {
      paste0(nm, "=", selection_log$n_feasible_by_tier[[nm]])
    }, character(1)),
    collapse = ", "
  )

  emergency_cfg <- lambda_cfg$emergency_selection
  emergency_enabled <- is.null(emergency_cfg$enabled) || isTRUE(emergency_cfg$enabled)
  emergency_ess_floor <- ifelse(is.null(emergency_cfg$ess_frac_floor), 0, as.numeric(emergency_cfg$ess_frac_floor))
  emergency_gate <- list(
    ess_frac = emergency_ess_floor,
    ess_abs = emergency_cfg$ess_abs_floor,
    ess_mult_treated = emergency_cfg$ess_mult_treated
  )
  emergency_ess_required <- compute_gate_required_ess(emergency_gate, n_ctrl, n_treated)

  if (emergency_enabled) {
    emergency_pool <- d[
      !is.na(d$max_smd) &
        !is.na(d$median_smd) &
        !is.na(d$top10_share) &
        !is.na(d$ess) &
        d$ess >= emergency_ess_required,
      ,
      drop = FALSE
    ]

    if (nrow(emergency_pool) > 0) {
      prioritize_balance <- isTRUE(emergency_cfg$prioritize_balance)
      rank_pool <- emergency_pool

      if (!prioritize_balance) {
        ess_max <- max(emergency_pool$ess, na.rm = TRUE)
        plateau_frac <- ifelse(is.null(lambda_cfg$ess_plateau_frac), 0.90, as.numeric(lambda_cfg$ess_plateau_frac))
        plateau <- emergency_pool[!is.na(emergency_pool$ess) & emergency_pool$ess >= plateau_frac * ess_max, , drop = FALSE]
        if (nrow(plateau) > 0) rank_pool <- plateau
      }

      min_max_smd <- min(rank_pool$max_smd, na.rm = TRUE)
      lvl1 <- rank_pool[near_equal(rank_pool$max_smd, min_max_smd), , drop = FALSE]
      min_med_smd <- min(lvl1$median_smd, na.rm = TRUE)
      lvl2 <- lvl1[near_equal(lvl1$median_smd, min_med_smd), , drop = FALSE]
      min_top10 <- min(lvl2$top10_share, na.rm = TRUE)
      lvl3 <- lvl2[near_equal(lvl2$top10_share, min_top10), , drop = FALSE]

      if (nrow(lvl3) == 1) {
        selected <- lvl3
      } else {
        # For balance-first emergency ranking, prefer higher ESS among equally balanced lambdas.
        if (prioritize_balance) {
          lvl3 <- lvl3[order(-lvl3$ess, lvl3$lambda), , drop = FALSE]
        } else {
          med_lambda <- stats::median(lvl3$lambda, na.rm = TRUE)
          lvl3$._med_dist <- abs(lvl3$lambda - med_lambda)
          lvl3 <- lvl3[order(lvl3$._med_dist, lvl3$lambda), , drop = FALSE]
        }
        selected <- lvl3[1, setdiff(colnames(lvl3), "._med_dist"), drop = FALSE]
      }

      ess_ratio <- as.numeric(selected$ess / n_ctrl)
      selection_log$tier_used <- "emergency"
      selection_log$gate_used <- "emergency"
      selection_log$selected_lambda <- as.numeric(selected$lambda)
      selection_log$selected_metrics <- list(
        ess = as.numeric(selected$ess),
        ess_frac = as.numeric(selected$ess_frac),
        top10_share = as.numeric(selected$top10_share),
        max_weight = as.numeric(selected$max_weight),
        max_smd = as.numeric(selected$max_smd),
        median_smd = as.numeric(selected$median_smd)
      )
      selection_log$required_ess_floor <- as.numeric(emergency_ess_required)
      selection_log$ess_ratio <- ess_ratio
      selection_log$ess_frac <- ess_ratio
      selection_log$warnings <- c(
        selection_log$warnings,
        paste0("Emergency selection used: no candidate passed hard/fallback gates; summary=[", feasible_summary, "]")
      )
      selection_log$warnings <- append_ess_floor_warnings(
        selection_log$warnings,
        ess_value = as.numeric(selected$ess),
        ess_floor = as.numeric(emergency_ess_required)
      )

      return(list(selected_row = selected, selection_log = selection_log))
    }
  }

  stop(
    "No feasible lambda found under hard/fallback gates. ",
    "Diagnostic summary: n_candidates_total=", selection_log$n_candidates_total,
    "; n_feasible_by_tier=[", feasible_summary, "]"
  )
}

select_lambda_with_hard_gates <- function(results_df, n_ctrl, n_treated = NA_real_) {
  # Explicit alias used by filtered and baseline runners to emphasize gate parity.
  run_lambda_selection(results_df = results_df, n_ctrl = n_ctrl, n_treated = n_treated)
}

compute_covariate_overlap <- function(X, W) {
  # Overlap and KS diagnostics intentionally disabled for runtime optimization.
  data.frame(
    covariate = character(0),
    smd_pre = numeric(0),
    pct_outside = numeric(0),
    ks = numeric(0),
    stringsAsFactors = FALSE
  )
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
  n_cov <- if (!is.null(X)) ncol(as.data.frame(X)) else 0
  summary <- data.frame(
    n_covariates = n_cov,
    n_warn = 0,
    n_fail = 0,
    warn_fraction = 0,
    fail_fraction = 0,
    max_abs_smd_pre = NA_real_,
    max_pct_outside = NA_real_,
    max_ks = NA_real_,
    stringsAsFactors = FALSE
  )
  block_summary <- data.frame(
    block = character(0),
    n_covariates = integer(0),
    n_warn = integer(0),
    n_fail = integer(0),
    max_abs_smd_pre = numeric(0),
    max_pct_outside = numeric(0),
    max_ks = numeric(0),
    stringsAsFactors = FALSE
  )
  list(
    overlap = overlap,
    flagged = character(0),
    severe = character(0),
    summary = summary,
    block_summary = block_summary,
    feasible = TRUE
  )
}

select_lambda_from_candidates <- function(cands_df) {
  # cands_df must contain columns: lambda, ess, top10_share, max_weight, max_smd
  if (is.null(cands_df) || nrow(cands_df) == 0) return(NULL)
  # prefer smallest lambda as a deterministic tiebreaker
  best_row <- cands_df[which.min(cands_df$lambda), , drop = FALSE]
  best_row
}
