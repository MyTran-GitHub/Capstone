#!/usr/bin/env Rscript
# Driver: run pooled jackknife per-lag ATT for embedding-selected CBPS weight files
# Scans Embeddings data folders for weight files, calls estimate_att_with_ci(..., method="pooled_jackknife_per_lag"), and writes CSV results.

args <- commandArgs(trailingOnly = TRUE)
# optional: first arg = path to search root (default: Embeddings/data)
search_root <- if (length(args) >= 1) args[[1]] else file.path("Embeddings", "data")
# optional: output dir
out_dir <- if (length(args) >= 2) args[[2]] else file.path("Embeddings", "data", "pj_results")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# windows to evaluate (matches analysis/fire_regression_lag.R)
windows <- list(
  "2008-2020" = 2008:2020,
  "2006-2020" = 2006:2020,
  "2010-2020" = 2010:2020
)

# find candidate weight files (RDS or CSV) containing 'cbps' or 'weights' in name
candidates <- list.files(search_root, pattern = "(cbps|weights)", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
if (length(candidates) == 0) {
  message("No candidate weight files found under: ", search_root)
  quit(status = 0)
}

# Source shared outcome helpers
helpers_path <- file.path("balancing", "calculate_fire_outcomes.R")
if (!file.exists(helpers_path)) stop("Required file not found: ", helpers_path)
source(helpers_path)

# helper to load weights
load_weights <- function(path) {
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    readRDS(path)
  } else {
    read.csv(path, stringsAsFactors = FALSE)
  }
}

## (Moved) compute_pooled_jackknife_per_lag_weights now lives in balancing/calculate_fire_outcomes.R
## The driver will call that function after sourcing the helpers file.

# Helper: build 3x3 grid using a function that returns per-lag results (rate, lower, upper)
build_grid_for_weights <- function(focal_years_list, outcomes_vars, ci_type = "two", result_fn, title_prefix = NULL) {
  plot_list <- list()
  k <- 1
  for (w_name in names(focal_years_list)) {
    focal_years <- focal_years_list[[w_name]]
    for (outcome_var in outcomes_vars) {
      res <- result_fn(focal_years = focal_years, outcome_var = outcome_var, ci_type = ci_type)
      res$year <- factor(res$year)
      fire_label <- switch(outcome_var,
                           "fire.frac" = "All fires",
                           "hifire90.frac" = "Class 2 ^`^s5 fires",
                           "hifire95.frac" = "Class 3 ^`^s5 fires",
                           outcome_var)

      p <- ggplot2::ggplot(res, ggplot2::aes(x = year, y = rate)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.3) +
        ggplot2::geom_line(size = 1.1) +
        ggplot2::geom_line(ggplot2::aes(y = upper), linetype = "dashed", size = 1) +
        ggplot2::geom_line(ggplot2::aes(y = lower), linetype = "dashed", size = 1) +
        ggplot2::geom_hline(yintercept = 1) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste0(w_name, " | ", fire_label)) +
        ggplot2::xlab("Years since fire") + ggplot2::ylab("Relative risk") +
        ggplot2::coord_cartesian(ylim = c(0, 1.8))

      plot_list[[k]] <- p
      k <- k + 1
    }
  }

  grid_grob <- gridExtra::arrangeGrob(grobs = plot_list, nrow = 3, ncol = 3)
  return(grid_grob)
}

for (f in candidates) {
  message("Processing: ", f)
  ok <- TRUE
  wt <- tryCatch(load_weights(f), error = function(e) { message("  failed to load: ", e$message); ok <<- FALSE; NULL })
  if (!ok) next

  # ensure required columns exist
  if (!all(c("unit", "treated", "weight") %in% colnames(wt))) {
    message("  skipping (missing required columns 'unit','treated','weight')")
    next
  }

  # run windows
  base <- tools::file_path_sans_ext(basename(f))
  for (w_name in names(windows)) {
    yrs <- windows[[w_name]]
    message("  window: ", w_name)
    # Compute embedding-selected pooled per-lag (two-sided) and write CSV
    res_embed_two <- tryCatch(
      compute_pooled_jackknife_per_lag_weights(weights_df = wt, focal_years = yrs, outcome_var = "hifire95.frac", ci_type = "two"),
      error = function(e) { message("    estimator error: ", e$message); NULL }
    )
    if (is.null(res_embed_two)) next

    out_file <- file.path(out_dir, paste0(base, "_", w_name, "_embedding_two_sided.csv"))
    write.csv(res_embed_two, out_file, row.names = FALSE)
    message("    wrote: ", out_file)
  }

  # After per-window CSVs, produce comparable 3x3 grid figures:
  outcomes_vars <- c("fire.frac", "hifire90.frac", "hifire95.frac")

  # Embedding-selected grids (two- and one-sided)
  result_fn_embed <- function(focal_years, outcome_var, ci_type) {
    compute_pooled_jackknife_per_lag_weights(weights_df = wt, focal_years = focal_years, outcome_var = outcome_var, ci_type = ci_type)
  }

  grid_embed_two <- tryCatch(build_grid_for_weights(windows, outcomes_vars, ci_type = "two", result_fn = result_fn_embed), error = function(e) { message("  embed grid error: ", e$message); NULL })
  if (!is.null(grid_embed_two)) {
    fname_embed_two <- file.path(out_dir, paste0(base, "_embedding_all_windows_two_sided.jpeg"))
    ggplot2::ggsave(fname_embed_two, plot = grid_embed_two, width = 15, height = 12, units = "in")
    message("  wrote: ", fname_embed_two)
  }

  grid_embed_one <- tryCatch(build_grid_for_weights(windows, outcomes_vars, ci_type = "one", result_fn = result_fn_embed), error = function(e) { message("  embed one-sided grid error: ", e$message); NULL })
  if (!is.null(grid_embed_one)) {
    fname_embed_one <- file.path(out_dir, paste0(base, "_embedding_all_windows_one_sided.jpeg"))
    ggplot2::ggsave(fname_embed_one, plot = grid_embed_one, width = 15, height = 12, units = "in")
    message("  wrote: ", fname_embed_one)
  }
}

message("Done.")
