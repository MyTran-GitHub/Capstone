#!/usr/bin/env python3
"""
Aggregate K-selection diagnostics across cohorts and recommend a default K policy.

Reads per-year diagnostics from:
  Embeddings/data/k_selection/<year>/k_selection_rmse.csv

Outputs:
  - cohort_k_policy_summary.csv
  - default_k_policy.json
  - default_k_policy.md
"""

from __future__ import annotations

import argparse
import json
import math
import logging
import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pandas as pd

sys.path.insert(0, str(Path(__file__).parent.parent))
BASE_DIR = Path(__file__).resolve().parent.parent
DATA_DIR = BASE_DIR / "data"
K_SELECTION_DIR = DATA_DIR / "k_selection"
OUTPUT_EMBEDDINGS_DIR = DATA_DIR / "embeddings"

logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(asctime)s - %(name)s - %(message)s')
logger = logging.getLogger(__name__)


def infer_n_treated(year: int) -> Optional[int]:
    emb_file = OUTPUT_EMBEDDINGS_DIR / f"embeddings_{year}.csv"
    if not emb_file.exists():
        return None
    try:
        df = pd.read_csv(emb_file, usecols=["treated"])
        return int((df["treated"] == 1).sum())
    except Exception:
        return None


def load_lambda_hard_gates(config_path: str = "balancing/balancing_config.R") -> Dict[str, float]:
    default = {
        "max_smd": 0.10,
        "top10_share": 0.70,
        "max_weight": 0.10,
        "ess_frac": 0.02,
        "ess_mult_treated": 1.5,
    }
    cmd = [
        "Rscript",
        "-e",
        (
            f"source('{config_path}'); "
            "cfg <- get_diagnostics_config()$lambda_selection$hard_gates; "
            "cat(paste(c(cfg$max_smd,cfg$top10_share,cfg$max_weight,cfg$ess_frac,cfg$ess_mult_treated), collapse=','))"
        ),
    ]
    try:
        res = subprocess.run(cmd, capture_output=True, text=True, check=True)
        vals = [float(x) for x in res.stdout.strip().split(',')]
        if len(vals) == 5:
            return {
                "max_smd": vals[0],
                "top10_share": vals[1],
                "max_weight": vals[2],
                "ess_frac": vals[3],
                "ess_mult_treated": vals[4],
            }
    except Exception as exc:
        logger.warning("Failed to load hard gates from %s (%s); using defaults", config_path, exc)
    return default


def apply_feasible_and_plateau_rules(
    df: pd.DataFrame,
    n_treated: int,
    gates: Optional[Dict[str, float]] = None,
    rmse_plateau_mult: float = 1.05,
    ess_plateau_frac: float = 0.90,
) -> Tuple[pd.DataFrame, Dict]:
    d = df.copy()
    gates = gates or {}
    gate_max_smd = float(gates.get("max_smd", 0.10))
    gate_top10_share = float(gates.get("top10_share", 0.70))
    gate_max_weight_share = float(gates.get("max_weight", 0.10))
    gate_ess_frac_floor = float(gates.get("ess_frac", 0.02))
    gate_ess_mult_treated = float(gates.get("ess_mult_treated", 1.5))

    if "pool_size" in d.columns:
        full_pool = float(d["pool_size"].max()) if len(d) > 0 else float("nan")
        d["pool_prop_full"] = d["pool_size"] / full_pool if math.isfinite(full_pool) and full_pool > 0 else float("nan")
        d["coverage_ratio"] = d["pool_size"] / max(1, n_treated)
    else:
        d["pool_prop_full"] = float("nan")
        d["coverage_ratio"] = float("nan")

    has_required = all(c in d.columns for c in ["max_balance_std", "ess_control", "top10_share", "pool_size"])
    if has_required:
        d["required_ess_floor"] = pd.concat(
            [
                pd.Series(gate_ess_mult_treated * n_treated, index=d.index),
                gate_ess_frac_floor * d["pool_size"],
            ],
            axis=1,
        ).max(axis=1)
        feasible = (
            (d["max_balance_std"] <= gate_max_smd)
            & (d["ess_control"] >= d["required_ess_floor"])
            & (d["top10_share"] <= gate_top10_share)
        )
        if "max_weight_share" in d.columns:
            feasible = feasible & (d["max_weight_share"] <= gate_max_weight_share)
        d["feasible"] = feasible
    else:
        d["required_ess_floor"] = float("nan")
        # Conservative fallback for old files.
        if "max_balance_std" in d.columns:
            d["feasible"] = d["max_balance_std"] <= gate_max_smd
        else:
            d["feasible"] = True

    feasible_df = d[d["feasible"]].copy()
    if feasible_df.empty:
        pick = d.sort_values(["rmse", "pool_size", "K"], na_position="last").iloc[0]
        return d, {
            "chosen_K": int(pick["K"]),
            "selection_mode": "fallback_min_rmse",
            "rmse_best": float(pick["rmse"]) if "rmse" in pick else float("nan"),
            "plateau_K": [],
        }

    rmse_best = float(feasible_df["rmse"].min())
    if "ess_control" in feasible_df.columns and feasible_df["ess_control"].notna().any():
        ess_best = float(feasible_df["ess_control"].max())
        plateau_df = feasible_df[
            (feasible_df["rmse"] <= rmse_plateau_mult * rmse_best)
            & (feasible_df["ess_control"] >= ess_plateau_frac * ess_best)
        ].copy()
    else:
        plateau_df = feasible_df[feasible_df["rmse"] <= rmse_plateau_mult * rmse_best].copy()

    if plateau_df.empty:
        plateau_df = feasible_df.nsmallest(1, "rmse").copy()

    for c in ["max_weight_share", "top10_share", "ess_control", "pool_size", "K"]:
        if c not in plateau_df.columns:
            plateau_df[c] = float("nan")

    pick = plateau_df.sort_values(
        ["ess_control", "top10_share", "max_weight_share", "pool_size", "K"],
        ascending=[False, True, True, True, True],
        na_position="last",
    ).iloc[0]

    return d, {
        "chosen_K": int(pick["K"]),
        "selection_mode": "feasible_plateau_smallest_pool",
        "rmse_best": rmse_best,
        "plateau_K": sorted(plateau_df["K"].dropna().astype(int).unique().tolist()),
    }


def aggregate_policy(years: List[int], out_dir: Path, experiment_name: str, input_tag: str, gates: Dict[str, float]) -> int:
    out_dir.mkdir(parents=True, exist_ok=True)

    cohort_rows = []
    plateau_sets = []

    for year in years:
        suffix = f"_{input_tag}" if input_tag else ""
        fp = K_SELECTION_DIR / experiment_name / str(year) / f"k_selection_rmse{suffix}.csv"
        if not fp.exists():
            logger.warning("Skipping %s: missing %s", year, fp)
            continue

        df = pd.read_csv(fp)
        if df.empty:
            logger.warning("Skipping %s: empty diagnostics file", year)
            continue

        # Normalize column names from existing outputs.
        rename_map = {
            "rmse_test": "rmse",
            "n_control": "pool_size",
        }
        for old, new in rename_map.items():
            if old in df.columns and new not in df.columns:
                df[new] = df[old]

        required = ["K", "rmse", "pool_size"]
        missing = [c for c in required if c not in df.columns]
        if missing:
            logger.warning("Skipping %s: missing required columns %s", year, missing)
            continue

        n_treated = infer_n_treated(year)
        if n_treated is None or n_treated <= 0:
            # Fallback to historical fixed cohort size when embedding file unavailable.
            n_treated = 414
            logger.warning("Year %s: treated count not found, using fallback n_treated=%s", year, n_treated)

        enriched, pick = apply_feasible_and_plateau_rules(df, n_treated=n_treated, gates=gates)
        enriched_out = out_dir / f"k_selection_enriched_{year}.csv"
        enriched.to_csv(enriched_out, index=False)

        chosen_k = pick["chosen_K"]
        chosen_row = enriched[enriched["K"] == chosen_k].iloc[0]

        plateau_k = pick.get("plateau_K", [])
        if plateau_k:
            plateau_sets.append(set(plateau_k))

        cohort_rows.append(
            {
                "year": year,
                "chosen_K": chosen_k,
                "selection_mode": pick["selection_mode"],
                "rmse": float(chosen_row.get("rmse", float("nan"))),
                "max_balance_std": float(chosen_row.get("max_balance_std", float("nan"))),
                "ess_control": float(chosen_row.get("ess_control", float("nan"))),
                "top10_share": float(chosen_row.get("top10_share", float("nan"))),
                "pool_size": float(chosen_row.get("pool_size", float("nan"))),
                "pool_prop_full": float(chosen_row.get("pool_prop_full", float("nan"))),
                "runtime_seconds": float(chosen_row.get("runtime_seconds", float("nan"))),
                "plateau_K": ",".join(str(x) for x in plateau_k),
            }
        )

    if not cohort_rows:
        logger.error("No cohort diagnostics could be aggregated.")
        return 2

    cohort_df = pd.DataFrame(cohort_rows).sort_values("year")
    cohort_csv = out_dir / "cohort_k_policy_summary.csv"
    cohort_df.to_csv(cohort_csv, index=False)

    chosen_list = cohort_df["chosen_K"].astype(int).tolist()
    count_series = pd.Series(chosen_list).value_counts()
    mode_candidates = count_series[count_series == count_series.max()].index.tolist()
    mode_k = int(min(mode_candidates))

    plateau_intersection = set.intersection(*plateau_sets) if plateau_sets else set()
    default_k = int(min(plateau_intersection)) if plateau_intersection else mode_k

    robustness = {
        "chosen_k_values": chosen_list,
        "chosen_k_min": int(min(chosen_list)),
        "chosen_k_max": int(max(chosen_list)),
        "chosen_k_std": float(pd.Series(chosen_list).std(ddof=0)),
        "mode_k": mode_k,
        "intersection_plateau_k": sorted(int(x) for x in plateau_intersection),
        "stability_flag": "stable" if float(pd.Series(chosen_list).std(ddof=0)) <= 10 else "variable",
    }

    policy = {
        "years": years,
        "default_k": default_k,
        "policy_rule": "smallest K in intersection of cohort plateaus; fallback to mode(chosen_K)",
        "experiment_name": experiment_name,
        "input_tag": input_tag,
        "hard_gates": gates,
        "robustness": robustness,
        "cohort_summary_file": str(cohort_csv),
    }

    policy_json = out_dir / "default_k_policy.json"
    policy_json.write_text(json.dumps(policy, indent=2))

    policy_md = out_dir / "default_k_policy.md"
    policy_md.write_text(
        "\n".join(
            [
                "# Default K Policy",
                "",
                f"Default K: **{default_k}**",
                "",
                "Rule:",
                "- Use the smallest K present in all cohort plateaus.",
                "- If no common plateau K exists, use mode of cohort chosen K values (smallest mode in ties).",
                "",
                "Robustness:",
                f"- Chosen K by cohort: {chosen_list}",
                f"- Range: [{min(chosen_list)}, {max(chosen_list)}]",
                f"- Std dev: {float(pd.Series(chosen_list).std(ddof=0)):.2f}",
                f"- Stability flag: {robustness['stability_flag']}",
                "",
                f"Cohort summary: {cohort_csv}",
                f"Policy JSON: {policy_json}",
            ]
        )
    )

    logger.info("Saved cohort summary: %s", cohort_csv)
    logger.info("Saved policy JSON: %s", policy_json)
    logger.info("Saved policy markdown: %s", policy_md)
    logger.info("Recommended default K: %s", default_k)
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Aggregate K diagnostics across cohorts and recommend default K.")
    parser.add_argument(
        "--years",
        type=int,
        nargs="+",
        default=[2012, 2015, 2019],
        help="Cohort years to aggregate (default: 2012 2015 2019)",
    )
    parser.add_argument(
        "--out-dir",
        type=str,
        default=None,
        help="Output directory for aggregated policy artifacts",
    )
    parser.add_argument("--experiment-name", type=str, default="full_pool")
    parser.add_argument("--input-tag", type=str, default="", help="Optional tag suffix used in k_selection_rmse_<tag>.csv")
    parser.add_argument("--config-path", type=str, default="balancing/balancing_config.R")
    args = parser.parse_args()

    out_dir = Path(args.out_dir) if args.out_dir else (K_SELECTION_DIR / args.experiment_name / "policy")
    gates = load_lambda_hard_gates(args.config_path)
    return aggregate_policy(args.years, out_dir, args.experiment_name, args.input_tag, gates)


if __name__ == "__main__":
    raise SystemExit(main())
