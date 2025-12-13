# Results Tables (Cohort 2012)

This document consolidates the key results tables produced by the analysis. Figures in the manuscript should be rendered directly from these tables to ensure defensible variation and correct confidence intervals.

## Pre-Fit (RMSPE)
- Source: [figures/pre_fit_tables_2012.csv](figures/pre_fit_tables_2012.csv)
- Columns: strategy, n_units, pre_rmspe_mean, pre_rmspe_se, pre_rmspe_norm_mean, pre_rmspe_norm_se
- Use for plots:
  - Pre RMSPE (mean ±95% CI)
  - Normalized Pre RMSPE (mean ±95% CI)

## Effects (ATT & Percent ATT)
- Source: [figures/att_tables_2012.csv](figures/att_tables_2012.csv)
- Columns: strategy, n_units, att_post, att_se, ci_l, ci_u, att_percent, att_percent_se, att_percent_ci_l, att_percent_ci_u
- Use for plots:
  - ATT (mean ±95% CI)
  - Percent ATT (mean ±95% CI)

### Alternate ATT summary (from panel-derived fallback)
- Source: [figures/att_post_summary_2012.csv](figures/att_post_summary_2012.csv)
- Contents:

| donor_strategy | ATT | SE | CI_l | CI_u | n_units |
|---|---:|---:|---:|---:|---:|
| embeddings | -0.9116 | 0.0355 | -0.9812 | -0.8421 | 400 |
| manual     | -0.6894 | 0.0433 | -0.7742 | -0.6046 | 400 |
| random     | -0.5273 | 0.0607 | -0.6463 | -0.4084 | 400 |

(Values rounded for display; use the CSV for exact numbers.)

## Post-Gap Distribution Summary
- Source: [figures/post_gap_distribution_2012.csv](figures/post_gap_distribution_2012.csv)
- Columns: strategy, n_units, post_gap_mean, post_gap_sd, post_gap_q25, post_gap_q50, post_gap_q75
- Use for:
  - Distributional descriptions and figure captions (violin/box plots).

## Placebo Reference
- Source: [data/processed_data/placebos_2012.csv](data/processed_data/placebos_2012.csv)
- Use for:
  - Placebo histograms with overlaid treated effects (use ATT means per strategy from the Effects table).

## Reproducibility
Generate or refresh tables:

```bash
Rscript analysis/make_results_tables.R \
  --cohort 2012 \
  --sc_results_fst data/processed_data/sc_results_2012.fst \
  --placebo_csv data/processed_data/placebos_2012.csv \
  --out_dir figures
```

These tables are designed to be the sole data sources for plotting and manuscript tables for the cohort year.
