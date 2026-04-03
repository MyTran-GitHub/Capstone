#!/usr/bin/env python3
"""Run robustness sweeps for K recommendation via treated-unit subsampling."""

from __future__ import annotations

import argparse
import csv
import json
import logging
import subprocess
import sys
from collections import Counter, defaultdict
from pathlib import Path
from statistics import mean, pstdev
from typing import Dict, List, Optional

sys.path.insert(0, str(Path(__file__).parent.parent))
BASE_DIR = Path(__file__).resolve().parent.parent
K_SELECTION_DIR = BASE_DIR / "data" / "k_selection"

logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(asctime)s - %(name)s - %(message)s')
logger = logging.getLogger(__name__)


def run_one(
    year: int,
    run_id: int,
    frac: float,
    seed: int,
    k_values: List[int],
    max_workers: int,
    experiment_name: str,
    analysis_base_dir: str,
    target_pool_proportions: Optional[List[float]] = None,
    include_full_pool: bool = True,
    config_path: str = "balancing/balancing_config.R",
    timeout_seconds: int = 7200,
    force_recompute: bool = False,
) -> Dict:
    tag = f"rob_y{year}_f{int(round(frac * 1000)):03d}_r{run_id:03d}"
    cmd = [
        "python3",
        "Embeddings/scripts/03_select_optimal_k.py",
        str(year),
        "--treated-subsample-frac",
        str(frac),
        "--random-seed",
        str(seed),
        "--output-tag",
        tag,
        "--max-workers",
        str(max_workers),
        "--experiment-name",
        experiment_name,
        "--analysis-base-dir",
        analysis_base_dir,
        "--config-path",
        config_path,
        "--k-values",
    ] + [str(k) for k in k_values]

    if force_recompute:
        cmd += ["--force-recompute"]

    if target_pool_proportions:
        cmd += ["--target-pool-proportions"] + [str(x) for x in target_pool_proportions]
    if not include_full_pool:
        cmd += ["--no-full-pool"]

    logger.info("Running robustness selection: %s", " ".join(cmd))
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout_seconds)
    except subprocess.TimeoutExpired as exc:
        return {
            "year": year,
            "run_id": run_id,
            "seed": seed,
            "frac": frac,
            "success": False,
            "error": f"Timed out after {timeout_seconds}s: {exc}",
            "output_tag": tag,
        }

    if result.returncode != 0:
        return {
            "year": year,
            "run_id": run_id,
            "seed": seed,
            "frac": frac,
            "success": False,
            "error": result.stderr or result.stdout,
            "output_tag": tag,
        }

    if experiment_name:
        summary_path = K_SELECTION_DIR / experiment_name / str(year) / f"k_selection_summary_{tag}.json"
    else:
        summary_path = K_SELECTION_DIR / str(year) / f"k_selection_summary_{tag}.json"
    if not summary_path.exists() and experiment_name:
        fallback_summary_path = K_SELECTION_DIR / str(year) / f"k_selection_summary_{tag}.json"
        if fallback_summary_path.exists():
            summary_path = fallback_summary_path
    if not summary_path.exists():
        return {
            "year": year,
            "run_id": run_id,
            "seed": seed,
            "frac": frac,
            "success": False,
            "error": f"Missing summary file: {summary_path}",
            "output_tag": tag,
        }

    payload = json.loads(summary_path.read_text())
    return {
        "year": year,
        "run_id": run_id,
        "seed": seed,
        "frac": frac,
        "success": True,
        "output_tag": tag,
        "optimal_K": int(payload["optimal_K"]),
        "optimal_rmse": float(payload["optimal_rmse"]),
        "selection_mode": payload.get("selection_mode", "unknown"),
    }


def summarize(records: List[Dict]) -> Dict:
    ok = [r for r in records if r.get("success")]
    by_year = defaultdict(list)
    for r in ok:
        by_year[int(r["year"])].append(int(r["optimal_K"]))

    summary = {
        "n_total_runs": len(records),
        "n_success": len(ok),
        "n_failed": len(records) - len(ok),
        "years": {},
    }

    all_k = []
    for year, ks in sorted(by_year.items()):
        freq = Counter(ks)
        mode_count = max(freq.values())
        mode_candidates = sorted([k for k, c in freq.items() if c == mode_count])
        mode_k = mode_candidates[0]
        all_k.extend(ks)
        summary["years"][str(year)] = {
            "n_success": len(ks),
            "k_frequency": dict(sorted(freq.items())),
            "mode_k": mode_k,
            "k_min": min(ks),
            "k_max": max(ks),
            "k_std": pstdev(ks) if len(ks) > 1 else 0.0,
        }

    if all_k:
        freq_all = Counter(all_k)
        mode_count = max(freq_all.values())
        mode_candidates = sorted([k for k, c in freq_all.items() if c == mode_count])
        summary["overall"] = {
            "k_frequency": dict(sorted(freq_all.items())),
            "mode_k": mode_candidates[0],
            "k_min": min(all_k),
            "k_max": max(all_k),
            "k_std": pstdev(all_k) if len(all_k) > 1 else 0.0,
            "k_mean": mean(all_k),
            "stability_flag": "stable" if (pstdev(all_k) if len(all_k) > 1 else 0.0) <= 10 else "variable",
        }
    else:
        summary["overall"] = {}

    return summary


def write_outputs(out_dir: Path, records: List[Dict], summary: Dict) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)

    records_csv = out_dir / "k_robustness_runs.csv"
    fields = [
        "year",
        "run_id",
        "seed",
        "frac",
        "success",
        "output_tag",
        "optimal_K",
        "optimal_rmse",
        "selection_mode",
        "error",
    ]
    with records_csv.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        for r in records:
            w.writerow({k: r.get(k) for k in fields})

    summary_json = out_dir / "k_robustness_summary.json"
    summary_json.write_text(json.dumps(summary, indent=2))

    md = out_dir / "k_robustness_summary.md"
    overall = summary.get("overall", {})
    lines = [
        "# K Recommendation Robustness",
        "",
        f"Total runs: {summary.get('n_total_runs', 0)}",
        f"Successful: {summary.get('n_success', 0)}",
        f"Failed: {summary.get('n_failed', 0)}",
        "",
    ]
    if overall:
        lines.extend(
            [
                "## Overall",
                f"- Mode K: {overall.get('mode_k')}",
                f"- Range: [{overall.get('k_min')}, {overall.get('k_max')}]",
                f"- Std dev: {overall.get('k_std', 0.0):.2f}",
                f"- Stability: {overall.get('stability_flag', 'unknown')}",
                "",
            ]
        )
    lines.append("## By Year")
    for y, ys in sorted(summary.get("years", {}).items()):
        lines.extend(
            [
                f"- {y}: mode={ys.get('mode_k')}, range=[{ys.get('k_min')}, {ys.get('k_max')}], std={ys.get('k_std', 0.0):.2f}",
            ]
        )
    md.write_text("\n".join(lines))

    logger.info("Saved robustness runs: %s", records_csv)
    logger.info("Saved robustness summary: %s", summary_json)
    logger.info("Saved robustness markdown: %s", md)


def main() -> int:
    parser = argparse.ArgumentParser(description="Robustness sweep for K recommendation via treated-unit subsampling")
    parser.add_argument("--years", type=int, nargs="+", default=[2012, 2015, 2019])
    parser.add_argument("--repeats", type=int, default=10, help="Number of subsample runs per year")
    parser.add_argument("--treated-frac", type=float, default=0.8, help="Treated subsample fraction per run")
    parser.add_argument("--seed", type=int, default=1234, help="Base random seed")
    parser.add_argument("--max-workers", type=int, default=6)
    parser.add_argument("--k-values", type=int, nargs="+", default=[5, 10, 20, 30, 50, 100])
    parser.add_argument(
        "--target-pool-proportions",
        type=float,
        nargs="+",
        default=[0.005, 0.01, 0.02, 0.05, 0.10, 0.20, 1.0],
        help="Target donor-pool proportions passed to selector",
    )
    parser.add_argument("--no-full-pool", action="store_true", help="Disable auto full-pool candidate in selector")
    parser.add_argument("--experiment-name", type=str, default="", help="Optional legacy experiment namespace")
    parser.add_argument("--analysis-base-dir", type=str, default="data/processed_data/rev_analysis_low")
    parser.add_argument("--config-path", type=str, default="balancing/balancing_config.R")
    parser.add_argument("--timeout-seconds", type=int, default=7200, help="Per-run timeout for selector subprocess")
    parser.add_argument("--force-recompute", action="store_true", help="Force recomputation in selector (disable cache reuse)")
    parser.add_argument("--out-dir", type=str, default=None)
    args = parser.parse_args()

    out_dir = Path(args.out_dir) if args.out_dir else ((K_SELECTION_DIR / args.experiment_name / "robustness") if args.experiment_name else (K_SELECTION_DIR / "robustness"))

    records = []
    run_counter = 0
    for year in args.years:
        for i in range(args.repeats):
            run_counter += 1
            seed = args.seed + (10000 * year) + i
            rec = run_one(
                year=year,
                run_id=run_counter,
                frac=args.treated_frac,
                seed=seed,
                k_values=args.k_values,
                max_workers=args.max_workers,
                experiment_name=args.experiment_name,
                analysis_base_dir=args.analysis_base_dir,
                target_pool_proportions=args.target_pool_proportions,
                include_full_pool=not args.no_full_pool,
                config_path=args.config_path,
                timeout_seconds=max(1, int(args.timeout_seconds)),
                force_recompute=bool(args.force_recompute),
            )
            records.append(rec)
            if rec.get("success"):
                logger.info("✓ year=%s run=%s K=%s rmse=%.4f", year, run_counter, rec.get("optimal_K"), rec.get("optimal_rmse"))
            else:
                logger.error("✗ year=%s run=%s error=%s", year, run_counter, rec.get("error"))

    summary = summarize(records)
    write_outputs(out_dir, records, summary)

    return 0 if summary.get("n_success", 0) > 0 else 2


if __name__ == "__main__":
    raise SystemExit(main())
