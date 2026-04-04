#!/usr/bin/env python3
"""Supplemental figure: ESS vs effective pool size (embedding frontier and random baseline)."""

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
    parser = argparse.ArgumentParser(description="Plot ESS vs pool size from stored K-selection outputs")
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--base-dir", type=str, default="Embeddings/data/k_selection")
    parser.add_argument("--out-file", type=str, default=None)
    args = parser.parse_args()

    year_dir = Path(args.base_dir) / str(args.year)
    emb_fp = year_dir / "embedding_pool_frontier.csv"
    rnd_fp = year_dir / "random_pool_summary.csv"

    emb = pd.read_csv(emb_fp)
    emb_x = _pool_col(emb)
    emb = emb.sort_values(emb_x)

    fig, ax = plt.subplots(figsize=(8.4, 5.0), dpi=180)
    ax.plot(emb[emb_x], emb["ess_control"], marker="o", linewidth=2.0, label="embedding frontier")

    if rnd_fp.exists():
        rnd = pd.read_csv(rnd_fp)
        rnd_x = _pool_col(rnd)
        rnd = rnd.sort_values(rnd_x)
        if "median_ess_control" in rnd.columns and rnd["median_ess_control"].notna().any():
            ax.plot(rnd[rnd_x], rnd["median_ess_control"], marker="s", linestyle="--", linewidth=1.7, label="random pools")

    ax.set_xlabel("effective_pool_size")
    ax.set_ylabel("ESS_control")
    ax.set_title(f"ESS vs Pool Size ({args.year})")
    ax.grid(True, alpha=0.25)
    ax.legend(frameon=False)

    out_path = Path(args.out_file) if args.out_file else year_dir / f"figure_supp_ess_vs_pool_size_{args.year}.png"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(out_path)
    plt.close(fig)
    print(f"Saved: {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
