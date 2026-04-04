#!/usr/bin/env python3
"""Preflight validator for multi-year embedding donor-pool pipeline runs."""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd


def _resolve_analysis_candidates(analysis_base_dir: Path, experiment_name: str, year: int) -> tuple[Path, Path, Path, Path]:
    exp_dir = analysis_base_dir / experiment_name if analysis_base_dir.name != experiment_name else analysis_base_dir
    base_csv = analysis_base_dir / f"analysis_treated{int(year)}_conifer.csv"
    base_rds = analysis_base_dir / f"analysis_treated{int(year)}_conifer.RDS"
    exp_csv = exp_dir / f"analysis_treated{int(year)}_conifer.csv"
    exp_rds = exp_dir / f"analysis_treated{int(year)}_conifer.RDS"
    return exp_csv, exp_rds, base_csv, base_rds


def _validate_embeddings(path: Path) -> list[str]:
    errors: list[str] = []
    if not path.exists():
        return [f"missing embeddings file: {path}"]
    df = pd.read_csv(path)
    required = {"unit", "treated"}
    missing = required - set(df.columns)
    if missing:
        errors.append(f"missing columns in {path.name}: {sorted(missing)}")
        return errors

    if df['unit'].isna().any() or (df['unit'].astype(str).str.len() == 0).any():
        errors.append(f"empty unit values in {path.name}")
    if df['unit'].duplicated().any():
        errors.append(f"duplicate unit values in {path.name}")

    treated_vals = set(pd.to_numeric(df['treated'], errors='coerce').dropna().astype(int).unique().tolist())
    if not treated_vals.issubset({0, 1}):
        errors.append(f"treated has non-binary values in {path.name}: {sorted(treated_vals)}")

    n_treated = int((pd.to_numeric(df['treated'], errors='coerce') == 1).sum())
    n_control = int((pd.to_numeric(df['treated'], errors='coerce') == 0).sum())
    if n_treated <= 0:
        errors.append(f"no treated units in {path.name}")
    if n_control <= 0:
        errors.append(f"no control units in {path.name}")

    band_cols = [c for c in df.columns if c.startswith('band_')]
    if len(band_cols) == 0:
        errors.append(f"no embedding band columns in {path.name}")
    else:
        frac_nan = float(df[band_cols].isna().values.mean())
        if frac_nan > 0.0:
            errors.append(f"NaN embeddings present in {path.name} (fraction={frac_nan:.6f})")

    return errors


def _validate_analysis(path: Path) -> list[str]:
    errors: list[str] = []
    if not path.exists():
        return [f"missing analysis file: {path}"]
    if path.suffix.lower() == '.csv':
        df = pd.read_csv(path)
    elif path.suffix.lower() == '.rds':
        # Keep preflight lightweight without requiring extra Python RDS deps.
        return errors
    else:
        errors.append(f"unsupported analysis format for preflight: {path.name}")
        return errors

    required = {"unit", "treated"}
    missing = required - set(df.columns)
    if missing:
        errors.append(f"missing columns in {path.name}: {sorted(missing)}")
        return errors

    if df['unit'].isna().any() or (df['unit'].astype(str).str.len() == 0).any():
        errors.append(f"empty unit values in {path.name}")
    if df['unit'].duplicated().any():
        errors.append(f"duplicate unit values in {path.name}")

    treated_vals = set(pd.to_numeric(df['treated'], errors='coerce').dropna().astype(int).unique().tolist())
    if not treated_vals.issubset({0, 1}):
        errors.append(f"treated has non-binary values in {path.name}: {sorted(treated_vals)}")

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description="Preflight validation for batch years")
    parser.add_argument("--years", type=int, nargs='+', required=True)
    parser.add_argument("--analysis-base-dir", type=str, default="data/processed_data/rev_analysis_low")
    parser.add_argument("--experiment-name", type=str, default="full_pool")
    parser.add_argument("--embeddings-dir", type=str, default="Embeddings/data/embeddings")
    args = parser.parse_args()

    repo = Path(__file__).resolve().parents[2]
    analysis_base = repo / args.analysis_base_dir
    embeddings_base = repo / args.embeddings_dir

    all_errors: list[str] = []
    for year in args.years:
        emb = embeddings_base / f"embeddings_{int(year)}.csv"
        exp_csv, exp_rds, base_csv, base_rds = _resolve_analysis_candidates(
            analysis_base,
            str(args.experiment_name),
            int(year),
        )
        analysis_candidates = [exp_csv, exp_rds, base_csv, base_rds]
        ana_path = next((p for p in analysis_candidates if p.exists()), exp_csv)

        for msg in _validate_embeddings(emb):
            all_errors.append(f"[year {year}] {msg}")
        for msg in _validate_analysis(ana_path):
            all_errors.append(f"[year {year}] {msg}")

    if all_errors:
        print("Preflight FAILED")
        for msg in all_errors:
            print(" -", msg)
        return 1

    print("Preflight PASS")
    print("Validated years:", ",".join(str(int(y)) for y in args.years))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
