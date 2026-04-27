#!/usr/bin/env Rscript
# Extract embedding-selected vs full-pool CBPS comparison matrix
# Usage: Rscript scripts/extract_embedding_vs_full_comparison.R

suppressPackageStartupMessages({
  library(tidyverse)
})

emb_csv <- "diagnostics/k_selection_synthesis/emb_selected_by_year.csv"
out_csv <- "diagnostics/k_selection_synthesis/emb_vs_full_comparison.csv"

run_extract_embedding_vs_full_comparison <- function(full_pool_dir = NULL) {
  # allow overriding the full_pool directory via argument
  if (is.null(full_pool_dir) || !nzchar(full_pool_dir)) {
    full_pool_dir <- "data/processed_data/rev_analysis_low/full_pool"
  }

  if (!file.exists(emb_csv)) stop("Embedding selected CSV not found: ", emb_csv)
  if (!dir.exists(full_pool_dir)) stop(paste0("Full-pool directory not found: ", full_pool_dir, "\nRun this script on the TACC environment or pass the full_pool path as the first argument."))

  emb <- read_csv(emb_csv, show_col_types = FALSE)

  # helper: attempt to find a value in recursive lists by name patterns
  find_field <- function(obj, patterns) {
    if (is.null(obj)) return(NULL)
    if (!is.list(obj)) return(NULL)
    names_lower <- tolower(names(obj))
    for (pat in patterns) {
      idx <- which(grepl(pat, names_lower))
      if (length(idx) > 0) {
        val <- obj[[idx[1]]]
        if (!is.list(val) && length(val) == 1) return(val)
        if (is.numeric(val) && length(val) >= 1) return(val[1])
      }
    }
    for (nm in names(obj)) {
      sub <- obj[[nm]]
      if (is.list(sub)) {
        res <- find_field(sub, patterns)
        if (!is.null(res)) return(res)
      }
    }
    return(NULL)
  }

emb <- read_csv(emb_csv, show_col_types = FALSE)

# helper: attempt to find a value in recursive lists by name patterns
find_field <- function(obj, patterns) {
  # obj: list or atomic
  if (is.null(obj)) return(NULL)
  if (!is.list(obj)) return(NULL)
  names_lower <- tolower(names(obj))
  for (pat in patterns) {
    idx <- which(grepl(pat, names_lower))
    if (length(idx) > 0) {
      val <- obj[[idx[1]]]
      # if val is length 1 atomic, return it
      if (!is.list(val) && length(val) == 1) return(val)
      # if it's numeric vector, return first element
      if (is.numeric(val) && length(val) >= 1) return(val[1])
      # otherwise continue
    }
  }
  # recurse into sublists
  for (nm in names(obj)) {
    sub <- obj[[nm]]
    if (is.list(sub)) {
      res <- find_field(sub, patterns)
      if (!is.null(res)) return(res)
    }
  }
  return(NULL)
}

out_df <- bind_rows(rows) %>% arrange(year)
  rows <- list()
  for (i in seq_len(nrow(emb))) {
    r <- emb[i, ]
    yr <- r$year
    pattern <- paste0("cbps_fit_.*", yr, ".*\\.RDS$")
    fits <- list.files(full_pool_dir, pattern = pattern, full.names = TRUE)
    if (length(fits) == 0) {
      wpat <- paste0("cbps_weights_", yr, "_conifer.*\\.RDS$")
      fits <- list.files(full_pool_dir, pattern = wpat, full.names = TRUE)
    }
    chosen_fit <- if (length(fits) > 0) fits[1] else NA_character_

    full_prefit <- NA_real_
    full_ess <- NA_real_
    full_median_smd <- NA_real_
    full_max_smd <- NA_real_
    full_top10 <- NA_real_

    if (!is.na(chosen_fit)) {
      obj <- tryCatch(readRDS(chosen_fit), error = function(e) NULL)
      if (!is.null(obj)) {
        if (!is.null(obj$balance.std) && is.numeric(obj$balance.std)) {
          vals <- abs(as.numeric(obj$balance.std))
          full_median_smd <- median(vals, na.rm = TRUE)
          full_max_smd <- max(vals, na.rm = TRUE)
        }
        if (!is.null(obj$balance.std.pre) && is.numeric(obj$balance.std.pre)) {
          pre_vals <- abs(as.numeric(obj$balance.std.pre))
          full_prefit <- median(pre_vals, na.rm = TRUE)
        }
        if (!is.null(obj$weights.0) && is.numeric(obj$weights.0)) {
          w <- as.numeric(obj$weights.0)
          w <- w[is.finite(w) & w >= 0]
          if (length(w) > 0) {
            s <- sum(w); sq <- sum(w^2)
            if (sq > 0) full_ess <- (s^2) / sq
            topn <- min(10, length(w))
            full_top10 <- sum(sort(w, decreasing = TRUE)[1:topn]) / s
          }
        }
        if (is.na(full_prefit)) {
          f <- find_field(obj, c('prefit_rmse', 'prefit', 'rmse', 'rmspe'))
          if (!is.null(f)) full_prefit <- as.numeric(f)
        }
      }
    }

    rows[[length(rows) + 1]] <- tibble(
      year = yr,
      selected_k = r$selected_k,
      emb_effective_pool_size = r$effective_pool_size,
      emb_prefit_rmse_cv = r$prefit_rmse_cv,
      emb_ess = r$ess,
      emb_median_smd = r$median_smd,
      emb_max_smd = r$max_smd,
      emb_top10_share = r$top10_share,
      full_fit_path = chosen_fit,
      full_prefit_rmse_cv = full_prefit,
      full_ess = full_ess,
      full_median_smd = full_median_smd,
      full_max_smd = full_max_smd,
      full_top10_share = full_top10
    )
  }

  out_df <- bind_rows(rows) %>% arrange(year)
  write_csv(out_df, out_csv)
  message('Wrote comparison CSV to ', out_csv)

  # Print a small preview
  print(out_df %>% select(year, selected_k, emb_effective_pool_size, emb_prefit_rmse_cv, emb_ess, full_prefit_rmse_cv, full_ess))

  invisible(out_df)
}

# If run non-interactively, parse args and execute
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  full_pool_dir <- if (length(args) >= 1 && nzchar(args[1])) args[1] else NULL
  tryCatch(
    {
      run_extract_embedding_vs_full_comparison(full_pool_dir = full_pool_dir)
    },
    error = function(e) {
      message("[ERROR] extract_embedding_vs_full_comparison failed: ", conditionMessage(e))
      tb <- utils::capture.output(traceback())
      if (length(tb) > 0) for (ln in tb) message(ln)
      quit(save = "no", status = 1, runLast = FALSE)
    }
  )
}
