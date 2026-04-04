#!/usr/bin/env python3
"""Smoke test harness for the embedding K-selection pipeline.

Runs a single-year selection pass and verifies required output artifacts exist.
"""

import argparse
import json
import subprocess
import sys
from pathlib import Path
from typing import List


REQUIRED_BASENAMES = [
    "pool_discovery_scan.csv",
    "pool_size_grid.csv",
    "embedding_k_raw_results.csv",
    "embedding_pool_frontier.csv",
    "random_pool_summary.csv",
    "pool_overlap_diagnostics.csv",
    "similarity_support.csv",
    "selection_decision.json",
    "pipeline_commands",
    "run_pipeline_commands",
]


def _suffix(output_tag: str) -> str:
    return f"_{output_tag}" if output_tag else ""


def _expected_files(base_dir: Path, year: int, output_tag: str) -> List[Path]:
    suffix = _suffix(output_tag)
    year_dir = base_dir / str(year)
    return [
        year_dir / "pool_discovery_scan.csv",
        year_dir / "pool_size_grid.csv",
        year_dir / "embedding_k_raw_results.csv",
        year_dir / "embedding_pool_frontier.csv",
        year_dir / "random_pool_summary.csv",
        year_dir / "pool_overlap_diagnostics.csv",
        year_dir / "similarity_support.csv",
        year_dir / "selection_decision.json",
        year_dir / f"pipeline_commands{suffix}.csv",
        year_dir / f"run_pipeline_commands{suffix}.sh",
    ]


def main() -> int:
    parser = argparse.ArgumentParser(description="Run smoke test for K-selection pipeline outputs")
    parser.add_argument("--year", type=int, default=2019, help="Treatment year to run (default: 2019)")
    parser.add_argument("--output-tag", type=str, default="smoke", help="Output tag suffix (default: smoke)")
    parser.add_argument("--max-workers", type=int, default=2, help="Max CBPS workers (default: 2)")
    parser.add_argument("--min-ratio", type=int, default=2, help="Minimum control:treated ratio for smoke test (default: 2)")
    parser.add_argument(
        "--k-values",
        type=int,
        nargs="+",
        default=[10, 30, 60],
        help="Fallback K seed values if target proportions are omitted",
    )
    parser.add_argument("--force-recompute", action="store_true", help="Ignore cache and recompute")
    args = parser.parse_args()

    repo_root = Path(__file__).resolve().parents[2]
    script = repo_root / "Embeddings" / "scripts" / "03_select_optimal_k.py"
    k_selection_dir = repo_root / "Embeddings" / "data" / "k_selection"

    cmd = [
        sys.executable,
        str(script),
        str(args.year),
        "--stage",
        "select",
        "--output-tag",
        args.output_tag,
        "--max-workers",
        str(max(1, int(args.max_workers))),
        "--min-ratio",
        str(max(1, int(args.min_ratio))),
        "--no-adaptive-refine",
        "--placebo-draws",
        "10",
        "--temporal-placebo-draws",
        "10",
    ]

    if args.k_values:
        cmd.extend(["--k-values", *[str(int(k)) for k in args.k_values]])
    if args.force_recompute:
        cmd.append("--force-recompute")

    print("[smoke] running:", " ".join(cmd))
    run = subprocess.run(cmd, cwd=repo_root)
    if run.returncode != 0:
        print(f"[smoke] FAILED: selection run exited with code {run.returncode}")
        return run.returncode

    expected = _expected_files(k_selection_dir, args.year, args.output_tag)
    missing = [str(p) for p in expected if not p.exists()]
    if missing:
        print("[smoke] FAILED: missing expected outputs:")
        for path in missing:
            print("  -", path)
        return 1

    summary_path = next((p for p in expected if p.name == "selection_decision.json"), expected[5])
    summary = json.loads(summary_path.read_text(encoding="utf-8"))
    selected_pool_size = int(summary.get("selected_pool_size", -1))
    if selected_pool_size <= 0:
        print(f"[smoke] FAILED: invalid selected_pool_size in {summary_path}: {selected_pool_size}")
        return 1

    print("[smoke] PASS")
    print(f"[smoke] summary: {summary_path}")
    print(f"[smoke] optimal_K={summary.get('optimal_K')} selected_pool_size={selected_pool_size}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
