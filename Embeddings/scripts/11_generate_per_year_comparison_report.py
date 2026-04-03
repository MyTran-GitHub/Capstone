#!/usr/bin/env python3
"""Generate consolidated per-year comparison report for embedding, random, and full pool methods."""

import argparse
import json
import logging
from pathlib import Path
from typing import Dict, List, Optional

import numpy as np
import pandas as pd


BASE_DIR = Path(__file__).resolve().parent.parent
K_SELECTION_DIR = BASE_DIR / "data" / "k_selection"

logging.basicConfig(level=logging.INFO, format="[%(levelname)s] %(asctime)s - %(message)s")
logger = logging.getLogger(__name__)

LONG_REPORT_COLUMNS = [
    "year",
    "method",
    "K",
    "pool_size",
    "pool_prop_full",
    "median_RMSE",
    "p90_RMSE",
    "ESS_control",
    "max_abs_SMD",
    "top10_share",
    "max_weight_share",
    "selection_mode",
    "temporal_placebo_n_years",
    "temporal_placebo_success_rate",
    "temporal_placebo_false_positive_rate_05",
    "temporal_placebo_false_positive_rate_10",
    "temporal_placebo_median_pval",
    "temporal_placebo_mean_valid_share",
    "temporal_placebo_mean_gate_prefit_pass_rate",
]

WIDE_REPORT_COLUMNS = [
    "year",
    "embedding_K",
    "embedding_pool_size",
    "embedding_median_RMSE",
    "embedding_p90_RMSE",
    "embedding_ESS_control",
    "embedding_max_abs_SMD",
    "random_K",
    "random_pool_size",
    "random_median_RMSE",
    "random_p90_RMSE",
    "random_ESS_control",
    "random_max_abs_SMD",
    "full_pool_K",
    "full_pool_size",
    "full_pool_median_RMSE",
    "full_pool_p90_RMSE",
    "full_pool_ESS_control",
    "full_pool_max_abs_SMD",
    "embedding_vs_random_median_rmse_delta",
    "embedding_vs_full_median_rmse_delta",
    "embedding_vs_random_median_rmse_pct",
    "embedding_vs_full_median_rmse_pct",
    "temporal_placebo_n_years",
    "temporal_placebo_success_rate",
    "temporal_placebo_false_positive_rate_05",
    "temporal_placebo_false_positive_rate_10",
    "temporal_placebo_median_pval",
    "temporal_placebo_mean_valid_share",
    "temporal_placebo_mean_gate_prefit_pass_rate",
]


def _safe_float(value) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return np.nan


def _safe_int(value) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return -1


def _status_is_success(value) -> Optional[bool]:
    if value is None or (isinstance(value, float) and np.isnan(value)):
        return None
    numeric = _safe_float(value)
    if np.isfinite(numeric):
        return numeric == 0.0
    text = str(value).strip().lower()
    if text in {"ok", "success", "succeeded", "pass", "passed"}:
        return True
    if text in {"fail", "failed", "error", "errors"}:
        return False
    return None


def _coerce_k_column(df: pd.DataFrame) -> pd.DataFrame:
    """Coerce K to integer while dropping invalid values safely."""
    work = df.copy()
    if "K" not in work.columns:
        return work
    work["K"] = pd.to_numeric(work["K"], errors="coerce")
    work = work[work["K"].notna()].copy()
    if work.empty:
        return work
    work["K"] = work["K"].astype(int)
    return work


def _format_md_number(value, fmt: str) -> str:
    """Render markdown numeric cells while preserving missingness as 'NA'."""
    if value is None or pd.isna(value):
        return "NA"
    try:
        return format(float(value), fmt)
    except (TypeError, ValueError):
        return "NA"


def _pick_embedding_row(rmse_df: pd.DataFrame, optimal_k: int) -> Optional[pd.Series]:
    rmse_work = _coerce_k_column(rmse_df)
    exact = rmse_work[rmse_work["K"] == int(optimal_k)]
    if not exact.empty:
        return exact.iloc[0]
    if rmse_work.empty:
        return None
    rmse_work["k_dist"] = (rmse_work["K"] - int(optimal_k)).abs()
    return rmse_work.sort_values(["k_dist", "K"]).iloc[0]


def _pick_full_pool_row(rmse_df: pd.DataFrame) -> Optional[pd.Series]:
    if rmse_df.empty:
        return None
    work = rmse_df.copy()
    if "pool_size" not in work.columns:
        return work.sort_values(["K"]).iloc[-1]
    max_pool = work["pool_size"].max()
    full = work[work["pool_size"] == max_pool].copy()
    if "median_RMSE" in full.columns:
        full = full.sort_values(["median_RMSE", "K"], na_position="last")
    else:
        full = full.sort_values(["rmse", "K"], na_position="last")
    return full.iloc[0]


def _pick_random_row(random_summary: pd.DataFrame, optimal_k: int) -> Optional[pd.Series]:
    if random_summary.empty:
        return None
    work = _coerce_k_column(random_summary)
    if "K" not in work.columns:
        return None
    if work.empty:
        return None
    exact = work[work["K"] == int(optimal_k)]
    if not exact.empty:
        return exact.iloc[0]
    work["k_dist"] = (work["K"] - int(optimal_k)).abs()
    return work.sort_values(["k_dist", "K"]).iloc[0]


def _row_from_series(year: int, method: str, row: pd.Series, selection_mode: str) -> Dict:
    return {
        "year": int(year),
        "method": method,
        "K": _safe_int(row.get("K", np.nan)),
        "pool_size": _safe_float(row.get("pool_size", np.nan)),
        "pool_prop_full": _safe_float(row.get("pool_prop_full", np.nan)),
        "median_RMSE": _safe_float(row.get("median_RMSE", row.get("rmse", np.nan))),
        "p90_RMSE": _safe_float(row.get("p90_RMSE", np.nan)),
        "ESS_control": _safe_float(row.get("ess_control", row.get("ESS_control", row.get("median_ess_control", np.nan)))),
        "max_abs_SMD": _safe_float(row.get("max_balance_std", row.get("max_abs_SMD", row.get("median_max_smd", np.nan)))),
        "top10_share": _safe_float(row.get("top10_share", row.get("median_top10_share", np.nan))),
        "max_weight_share": _safe_float(row.get("max_weight_share", row.get("median_max_weight_share", np.nan))),
        "selection_mode": selection_mode,
    }


def _load_temporal_placebo_metrics(year: int, summary_payload: Dict) -> Dict:
    placebo_output_dir = summary_payload.get("placebo_output_dir")
    if not placebo_output_dir:
        return {}

    placebo_base = Path(placebo_output_dir)
    if not placebo_base.is_absolute():
        placebo_base = BASE_DIR.parent / placebo_base

    temporal_path = placebo_base / "temporal" / f"temporal_placebo_summary_{int(year)}.csv"
    if not temporal_path.exists():
        return {}

    try:
        temporal_df = pd.read_csv(temporal_path)
    except Exception as exc:
        logger.warning("Failed reading temporal placebo summary for year %s: %s", year, exc)
        return {}

    if temporal_df.empty:
        return {
            "temporal_placebo_n_years": 0,
            "temporal_placebo_success_rate": np.nan,
            "temporal_placebo_false_positive_rate_05": np.nan,
            "temporal_placebo_false_positive_rate_10": np.nan,
            "temporal_placebo_median_pval": np.nan,
            "temporal_placebo_mean_valid_share": np.nan,
            "temporal_placebo_mean_gate_prefit_pass_rate": np.nan,
        }

    metrics: Dict = {
        "temporal_placebo_n_years": int(len(temporal_df)),
        "temporal_placebo_success_rate": np.nan,
        "temporal_placebo_false_positive_rate_05": np.nan,
        "temporal_placebo_false_positive_rate_10": np.nan,
        "temporal_placebo_median_pval": np.nan,
        "temporal_placebo_mean_valid_share": np.nan,
        "temporal_placebo_mean_gate_prefit_pass_rate": np.nan,
    }

    if "status" in temporal_df.columns:
        status_ok = temporal_df["status"].apply(_status_is_success)
        status_ok = status_ok[status_ok.notna()]
        if len(status_ok) > 0:
            metrics["temporal_placebo_success_rate"] = float(status_ok.mean())

    if "pval_rank" in temporal_df.columns:
        pvals = temporal_df["pval_rank"].apply(_safe_float)
        pvals = pvals[np.isfinite(pvals)]
        if len(pvals) > 0:
            metrics["temporal_placebo_false_positive_rate_05"] = float((pvals < 0.05).mean())
            metrics["temporal_placebo_false_positive_rate_10"] = float((pvals < 0.10).mean())
            metrics["temporal_placebo_median_pval"] = float(np.median(pvals))

    if "valid_share" in temporal_df.columns:
        valid_share = temporal_df["valid_share"].apply(_safe_float)
        valid_share = valid_share[np.isfinite(valid_share)]
        if len(valid_share) > 0:
            metrics["temporal_placebo_mean_valid_share"] = float(valid_share.mean())

    if "gate_prefit_pass_rate" in temporal_df.columns:
        prefit_pass = temporal_df["gate_prefit_pass_rate"].apply(_safe_float)
        prefit_pass = prefit_pass[np.isfinite(prefit_pass)]
        if len(prefit_pass) > 0:
            metrics["temporal_placebo_mean_gate_prefit_pass_rate"] = float(prefit_pass.mean())

    return metrics


def build_year_rows(year_dir: Path, output_tag: str) -> List[Dict]:
    tag_suffix = f"_{output_tag}" if output_tag else ""
    year = int(year_dir.name)

    summary_path = year_dir / f"k_selection_summary{tag_suffix}.json"
    rmse_path = year_dir / f"k_selection_rmse{tag_suffix}.csv"
    random_path = year_dir / f"k_selection_random_summary{tag_suffix}.csv"

    if not summary_path.exists() or not rmse_path.exists():
        logger.warning("Skipping year %s due to missing summary or rmse file", year)
        return []

    try:
        summary_payload = json.loads(summary_path.read_text(encoding="utf-8"))
    except Exception as exc:
        logger.warning("Skipping year %s because summary file could not be parsed: %s", year, exc)
        return []

    optimal_k_raw = summary_payload.get("optimal_K")
    optimal_k = _safe_int(optimal_k_raw)
    if optimal_k < 0:
        logger.warning("Skipping year %s because optimal_K is missing or invalid (%s)", year, optimal_k_raw)
        return []

    selection_mode = str(summary_payload.get("selection_mode", "unknown"))
    temporal_metrics = _load_temporal_placebo_metrics(year, summary_payload)

    try:
        rmse_df = pd.read_csv(rmse_path)
    except Exception as exc:
        logger.warning("Skipping year %s because rmse file could not be read: %s", year, exc)
        return []

    if rmse_df.empty or "K" not in rmse_df.columns:
        logger.warning("Skipping year %s because rmse file has no usable rows", year)
        return []

    embedding_row = _pick_embedding_row(rmse_df, optimal_k)
    full_row = _pick_full_pool_row(rmse_df)
    if random_path.exists():
        try:
            random_summary = pd.read_csv(random_path)
        except Exception as exc:
            logger.warning("Year %s random summary unreadable (%s); random row will be skipped", year, exc)
            random_summary = pd.DataFrame()
    else:
        random_summary = pd.DataFrame()
    random_row = _pick_random_row(random_summary, optimal_k)

    rows = []
    if embedding_row is not None:
        emb = _row_from_series(year, "embedding", embedding_row, selection_mode)
        emb.update(temporal_metrics)
        rows.append(emb)
    if full_row is not None:
        rows.append(_row_from_series(year, "full_pool", full_row, "full_pool_reference"))
    if random_row is not None:
        rows.append(_row_from_series(year, "random", random_row, "random_reference"))

    return rows


def build_wide_report(long_df: pd.DataFrame) -> pd.DataFrame:
    if long_df.empty:
        return long_df

    records = []
    for year in sorted(long_df["year"].unique()):
        block = long_df[long_df["year"] == year].copy()

        def get_metric(method: str, metric: str) -> float:
            sub = block[block["method"] == method]
            if sub.empty or metric not in sub.columns:
                return np.nan
            return _safe_float(sub.iloc[0][metric])

        emb_med = get_metric("embedding", "median_RMSE")
        rnd_med = get_metric("random", "median_RMSE")
        full_med = get_metric("full_pool", "median_RMSE")

        record = {
            "year": int(year),
            "embedding_K": get_metric("embedding", "K"),
            "embedding_pool_size": get_metric("embedding", "pool_size"),
            "embedding_median_RMSE": emb_med,
            "embedding_p90_RMSE": get_metric("embedding", "p90_RMSE"),
            "embedding_ESS_control": get_metric("embedding", "ESS_control"),
            "embedding_max_abs_SMD": get_metric("embedding", "max_abs_SMD"),
            "random_K": get_metric("random", "K"),
            "random_pool_size": get_metric("random", "pool_size"),
            "random_median_RMSE": rnd_med,
            "random_p90_RMSE": get_metric("random", "p90_RMSE"),
            "random_ESS_control": get_metric("random", "ESS_control"),
            "random_max_abs_SMD": get_metric("random", "max_abs_SMD"),
            "full_pool_K": get_metric("full_pool", "K"),
            "full_pool_size": get_metric("full_pool", "pool_size"),
            "full_pool_median_RMSE": full_med,
            "full_pool_p90_RMSE": get_metric("full_pool", "p90_RMSE"),
            "full_pool_ESS_control": get_metric("full_pool", "ESS_control"),
            "full_pool_max_abs_SMD": get_metric("full_pool", "max_abs_SMD"),
            "embedding_vs_random_median_rmse_delta": emb_med - rnd_med if pd.notna(emb_med) and pd.notna(rnd_med) else np.nan,
            "embedding_vs_full_median_rmse_delta": emb_med - full_med if pd.notna(emb_med) and pd.notna(full_med) else np.nan,
            "embedding_vs_random_median_rmse_pct": ((emb_med / rnd_med) - 1.0) * 100.0 if pd.notna(emb_med) and pd.notna(rnd_med) and rnd_med != 0 else np.nan,
            "embedding_vs_full_median_rmse_pct": ((emb_med / full_med) - 1.0) * 100.0 if pd.notna(emb_med) and pd.notna(full_med) and full_med != 0 else np.nan,
            "temporal_placebo_n_years": get_metric("embedding", "temporal_placebo_n_years"),
            "temporal_placebo_success_rate": get_metric("embedding", "temporal_placebo_success_rate"),
            "temporal_placebo_false_positive_rate_05": get_metric("embedding", "temporal_placebo_false_positive_rate_05"),
            "temporal_placebo_false_positive_rate_10": get_metric("embedding", "temporal_placebo_false_positive_rate_10"),
            "temporal_placebo_median_pval": get_metric("embedding", "temporal_placebo_median_pval"),
            "temporal_placebo_mean_valid_share": get_metric("embedding", "temporal_placebo_mean_valid_share"),
            "temporal_placebo_mean_gate_prefit_pass_rate": get_metric("embedding", "temporal_placebo_mean_gate_prefit_pass_rate"),
        }
        records.append(record)

    return pd.DataFrame(records).sort_values("year").reset_index(drop=True)


def write_markdown_summary(wide_df: pd.DataFrame, output_path: Path) -> None:
    lines = [
        "# Per-Year Method Comparison",
        "",
        "Methods included: embedding (selected K), random donor pool baseline (matched pool size), and full pool reference.",
        "",
    ]

    if wide_df.empty:
        lines.append("No years with complete input files were found.")
    else:
        avg_emb_vs_full = wide_df["embedding_vs_full_median_rmse_pct"].mean(skipna=True)
        avg_emb_vs_rand = wide_df["embedding_vs_random_median_rmse_pct"].mean(skipna=True)
        avg_temporal_fp05 = wide_df["temporal_placebo_false_positive_rate_05"].mean(skipna=True) if "temporal_placebo_false_positive_rate_05" in wide_df.columns else np.nan
        lines.append(f"Years included: {len(wide_df)}")
        lines.append(f"Average embedding vs full median RMSE delta (%): {avg_emb_vs_full:.3f}")
        lines.append(f"Average embedding vs random median RMSE delta (%): {avg_emb_vs_rand:.3f}")
        if pd.notna(avg_temporal_fp05):
            lines.append(f"Average temporal placebo false-positive rate (p<0.05): {avg_temporal_fp05:.3f}")
        lines.append("")
        lines.append("| year | embedding_K | embedding_median_RMSE | random_median_RMSE | full_pool_median_RMSE | emb_vs_random_pct | emb_vs_full_pct | temporal_fp05 | temporal_median_p |")
        lines.append("|---:|---:|---:|---:|---:|---:|---:|---:|---:|")
        for _, row in wide_df.iterrows():
            lines.append("| " + " | ".join([
                _format_md_number(row.get("year"), ".0f"),
                _format_md_number(row.get("embedding_K"), ".0f"),
                _format_md_number(row.get("embedding_median_RMSE"), ".6f"),
                _format_md_number(row.get("random_median_RMSE"), ".6f"),
                _format_md_number(row.get("full_pool_median_RMSE"), ".6f"),
                _format_md_number(row.get("embedding_vs_random_median_rmse_pct"), ".3f"),
                _format_md_number(row.get("embedding_vs_full_median_rmse_pct"), ".3f"),
                _format_md_number(row.get("temporal_placebo_false_positive_rate_05"), ".3f"),
                _format_md_number(row.get("temporal_placebo_median_pval"), ".3f"),
            ]) + " |")

    output_path.write_text("\n".join(lines), encoding="utf-8")


def _resolve_output_base(base: Path, years: List[int]) -> Path:
    """Write single-year reports under k_selection/<year>; otherwise use k_selection root."""
    if len(years) == 1:
        out_base = base / str(int(years[0]))
    else:
        out_base = base
    out_base.mkdir(parents=True, exist_ok=True)
    return out_base


def main() -> int:
    parser = argparse.ArgumentParser(description="Generate consolidated per-year comparison report")
    parser.add_argument("--experiment-name", type=str, default=None, help="Deprecated and ignored; reports now always use Embeddings/data/k_selection")
    parser.add_argument("--output-tag", type=str, default="", help="Optional output-tag suffix used in k_selection files")
    parser.add_argument("--year-start", type=int, default=None, help="Optional first year to include")
    parser.add_argument("--year-end", type=int, default=None, help="Optional last year to include")
    args = parser.parse_args()

    base = K_SELECTION_DIR
    if args.experiment_name:
        logger.warning("--experiment-name is deprecated and ignored; using %s", K_SELECTION_DIR)

    if not base.exists():
        logger.warning("Base directory does not exist: %s. Writing empty schema-only outputs.", base)
        years = []
    else:
        year_dirs = [p for p in base.iterdir() if p.is_dir() and p.name.isdigit()]
        years = sorted(int(p.name) for p in year_dirs)

    if args.year_start is not None:
        years = [y for y in years if y >= args.year_start]
    if args.year_end is not None:
        years = [y for y in years if y <= args.year_end]

    rows: List[Dict] = []
    for year in years:
        rows.extend(build_year_rows(base / str(year), output_tag=args.output_tag))

    long_df = pd.DataFrame(rows)
    if long_df.empty:
        long_df = pd.DataFrame(columns=LONG_REPORT_COLUMNS)
    else:
        long_df = long_df.reindex(columns=LONG_REPORT_COLUMNS)

    wide_df = build_wide_report(long_df)
    if wide_df.empty:
        wide_df = pd.DataFrame(columns=WIDE_REPORT_COLUMNS)
    else:
        wide_df = wide_df.reindex(columns=WIDE_REPORT_COLUMNS)

    output_base = _resolve_output_base(base, years)

    tag_suffix = f"_{args.output_tag}" if args.output_tag else ""
    long_out = output_base / f"per_year_method_metrics{tag_suffix}.csv"
    wide_out = output_base / f"per_year_comparison_report{tag_suffix}.csv"
    md_out = output_base / f"per_year_comparison_report{tag_suffix}.md"

    long_df.to_csv(long_out, index=False)
    wide_df.to_csv(wide_out, index=False)
    write_markdown_summary(wide_df, md_out)

    logger.info("Saved method-level long report: %s", long_out)
    logger.info("Saved consolidated per-year report: %s", wide_out)
    logger.info("Saved markdown summary: %s", md_out)
    logger.info("Years included: %s", sorted(long_df['year'].unique().tolist()) if not long_df.empty else [])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
