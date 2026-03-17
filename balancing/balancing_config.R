# Shared diagnostics configuration used by balancing and diagnostics entrypoints.

get_diagnostics_config <- function(overrides = list()) {
  cfg <- list(
    preprocessing = list(
      default_winsor_p = 0.995
    ),
    overlap_thresholds = list(
      smd_warn = 0.20,
      smd_fail = 0.50,
      pct_outside_warn = 0.01,
      pct_outside_fail = 0.05,
      ks_warn = 0.20,
      ks_fail = 0.30,
      max_fail_fraction = 0.02
    ),
    selection_thresholds = list(
      tiers = list(
        list(name = "strict", max_smd = 0.10, top10 = 0.75, max_weight = 0.10),
        list(name = "moderate", max_smd = 0.10, top10 = 0.80, max_weight = 0.15),
        list(name = "relaxed", max_smd = 0.12, top10 = 0.85, max_weight = 0.20)
      )
    ),
    outputs = list(
      validate_before_write = TRUE,
      keep_legacy_duplicates = TRUE
    )
  )

  if (length(overrides) > 0) {
    for (nm in names(overrides)) {
      if (!nm %in% names(cfg) || !is.list(cfg[[nm]]) || !is.list(overrides[[nm]])) {
        cfg[[nm]] <- overrides[[nm]]
      } else {
        cfg[[nm]][names(overrides[[nm]])] <- overrides[[nm]]
      }
    }
  }

  cfg
}
