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
    lambda_selection = list(
      hard_gates = list(
        max_smd = 0.10,
        median_smd = 0.05,
        top10_share = 0.70,
        max_weight = 0.10,
        ess_frac = 0.02,
        ess_mult_treated = 1.5
      ),
      # Explicit, pre-specified fallback gates for difficult years.
      fallback_gates = list(
        list(name = "relax_ess", max_smd = 0.12, median_smd = 0.05, top10_share = 0.70, max_weight = 0.10, ess_frac = 0.01, ess_mult_treated = 1.2),
        list(name = "relax_concentration_and_ess", max_smd = 0.15, median_smd = 0.05, top10_share = 0.75, max_weight = 0.10, ess_frac = 0.01, ess_mult_treated = 1.2)
      ),
      emergency_selection = list(
        enabled = TRUE,
        # If > 0, enforce a minimal ESS fraction even in emergency mode.
        ess_frac_floor = 0.00,
        # If provided, emergency mode can also enforce absolute ESS floors.
        ess_abs_floor = NULL,
        ess_mult_treated = 1.0,
        # Balance-first emergency ranking avoids over-prioritizing ESS.
        prioritize_balance = TRUE
      ),
      ess_plateau_frac = 0.90,
      stability_filter = list(
        enabled = FALSE,
        tolerances = list(
          max_smd = 0.01,
          top10_share = 0.02
        )
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
