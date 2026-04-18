#!/usr/bin/env python3
"""Plot median pre-fit RMSE vs control-pool proportion: embedding vs random trajectories."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd


def _pool_col(df: pd.DataFrame) -> str:
    if "effective_pool_size" in df.columns:
        return "effective_pool_size"
    if "pool_size" in df.columns:
        return "pool_size"
    raise ValueError("Expected effective_pool_size (or legacy pool_size) column")


def main() -> int:
    parser = argparse.ArgumentParser(description="RMSE vs pool proportion: embedding vs random")
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--emb-dir", type=str, default="Embeddings/data/k_selection")
    parser.add_argument("--out-file", type=str, default=None)
    args = parser.parse_args()

    year_dir = Path(args.emb_dir) / str(args.year)
    emb_fp = year_dir / "embedding_pool_frontier.csv"
    rnd_fp = year_dir / "random_pool_summary.csv"

    if not emb_fp.exists():
        raise FileNotFoundError(f"Missing embedding frontier: {emb_fp}")
    emb = pd.read_csv(emb_fp)
    pool_col = _pool_col(emb)

    # load random if present
    rnd = pd.DataFrame()
    if rnd_fp.exists():
        rnd = pd.read_csv(rnd_fp)

    # determine denominator for pool proportion
    max_pool = 0
    try:
        max_pool = max(emb[pool_col].max(), rnd[pool_col].max() if not rnd.empty and pool_col in rnd.columns else 0)
    except Exception:
        max_pool = emb[pool_col].max()

    if max_pool <= 0:
        raise ValueError("Invalid max pool size")

    emb = emb.sort_values(pool_col)
    emb_prop = emb[pool_col] / float(max_pool)
    emb_rmse = emb["prefit_rmse_cv"] if "prefit_rmse_cv" in emb.columns else emb.get("rmse", emb.get("median_RMSE", emb.get("rmse_test")))

    plt.figure(figsize=(8.6, 5.0), dpi=180)
    plt.plot(emb_prop, emb_rmse, marker="o", linewidth=2.0, label="embedding frontier", color="tab:blue")

    if not rnd.empty:
        rnd_pool_col = _pool_col(rnd)
        rnd = rnd.sort_values(rnd_pool_col)
        rnd_prop = rnd[rnd_pool_col] / float(max_pool)
        rnd_rmse = rnd["prefit_rmse_cv"] if "prefit_rmse_cv" in rnd.columns else rnd.get("rmse", rnd.get("median_RMSE"))
        plt.plot(rnd_prop, rnd_rmse, marker="s", linestyle="--", linewidth=1.6, label="random pools", color="tab:gray")

    plt.xlabel("Control pool proportion of full")
    plt.ylabel("Prefit RMSE (cv)")
    plt.title(f"Prefit RMSE vs Pool Proportion ({args.year})")
    plt.grid(alpha=0.25)
    plt.legend(frameon=False)

    out_path = Path(args.out_file) if args.out_file else year_dir / f"figure_rmse_vs_poolprop_{args.year}.png"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    plt.tight_layout()
    plt.savefig(out_path)
    plt.close()
    print(f"Saved: {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
