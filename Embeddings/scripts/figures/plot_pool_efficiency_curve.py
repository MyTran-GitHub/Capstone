#!/usr/bin/env python3
"""Figure 1: Donor Pool Efficiency Curve (embedding vs random vs full pool baseline)."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd


def _load_csv(path: Path) -> pd.DataFrame:
    if not path.exists():
        raise FileNotFoundError(f"Missing input file: {path}")
    df = pd.read_csv(path)
    if df.empty:
        raise ValueError(f"Input file is empty: {path}")
    return df


def _pool_col(df: pd.DataFrame) -> str:
    if "effective_pool_size" in df.columns:
        return "effective_pool_size"
    if "pool_size" in df.columns:
        return "pool_size"
    raise ValueError("Expected effective_pool_size (or legacy pool_size) column")


def main() -> int:
    parser = argparse.ArgumentParser(description="Plot donor pool efficiency curve from stored K-selection outputs")
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--base-dir", type=str, default="Embeddings/data/k_selection")
    parser.add_argument("--out-file", type=str, default=None)
    parser.add_argument("--rmse-col", type=str, default="median_RMSE", choices=["median_RMSE", "rmse"])
    args = parser.parse_args()

    year_dir = Path(args.base_dir) / str(args.year)
    emb_fp = year_dir / "embedding_pool_frontier.csv"
    rnd_fp = year_dir / "random_pool_summary.csv"

    emb = _load_csv(emb_fp)
    emb_x = _pool_col(emb)
    emb = emb.sort_values(emb_x)
    rmse_col = args.rmse_col if args.rmse_col in emb.columns and emb[args.rmse_col].notna().any() else "rmse"

    fig, ax = plt.subplots(figsize=(8.8, 5.2), dpi=180)
    ax.plot(emb[emb_x], emb[rmse_col], marker="o", linewidth=2.1, label="embedding frontier")

    if rnd_fp.exists():
        rnd = _load_csv(rnd_fp)
        rnd_x = _pool_col(rnd)
        rnd = rnd.sort_values(rnd_x)
        if "median_RMSE" in rnd.columns and rnd["median_RMSE"].notna().any():
            ax.plot(rnd[rnd_x], rnd["median_RMSE"], marker="s", linestyle="--", linewidth=1.8, label="random pool baseline")
            # Optional uncertainty band: [median, p90].
            if "p90_RMSE" in rnd.columns and rnd["p90_RMSE"].notna().any():
                ax.fill_between(
                    rnd[rnd_x],
                    rnd["median_RMSE"],
                    rnd["p90_RMSE"],
                    alpha=0.16,
                    label="random p90 band",
                )

    full_row = emb.loc[emb[emb_x].idxmax()]
    ax.scatter([full_row[emb_x]], [full_row[rmse_col]], marker="D", s=72, label="full control pool baseline")

    ax.set_xlabel("effective_pool_size")
    ax.set_ylabel(rmse_col)
    ax.set_title(f"Donor Pool Efficiency Curve ({args.year})")
    ax.grid(True, alpha=0.25)
    ax.legend(frameon=False)

    out_path = Path(args.out_file) if args.out_file else year_dir / f"figure1_pool_efficiency_{args.year}.png"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(out_path)
    plt.close(fig)
    print(f"Saved: {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
