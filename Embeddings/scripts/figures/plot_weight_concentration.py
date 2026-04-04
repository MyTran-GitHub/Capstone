#!/usr/bin/env python3
"""Figure 2: Weight concentration diagnostic (top10_share and max_weight_share)."""

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
    parser = argparse.ArgumentParser(description="Plot weight concentration diagnostics by pool size")
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--base-dir", type=str, default="Embeddings/data/k_selection")
    parser.add_argument("--out-file", type=str, default=None)
    args = parser.parse_args()

    year_dir = Path(args.base_dir) / str(args.year)
    emb_fp = year_dir / "embedding_pool_frontier.csv"
    emb = pd.read_csv(emb_fp)
    xcol = _pool_col(emb)
    emb = emb.sort_values(xcol)

    fig, axes = plt.subplots(1, 2, figsize=(11.2, 4.6), dpi=180, sharex=True)

    axes[0].plot(emb[xcol], emb["top10_share"], marker="o", linewidth=2.0, color="tab:blue")
    axes[0].set_title("Panel A: top10_share")
    axes[0].set_xlabel("effective_pool_size")
    axes[0].set_ylabel("top10_share")
    axes[0].grid(True, alpha=0.25)

    axes[1].plot(emb[xcol], emb["max_weight_share"], marker="s", linewidth=2.0, color="tab:orange")
    axes[1].set_title("Panel B: max_weight_share")
    axes[1].set_xlabel("effective_pool_size")
    axes[1].set_ylabel("max_weight_share")
    axes[1].grid(True, alpha=0.25)

    fig.suptitle(f"Weight Concentration Diagnostics ({args.year})")

    out_path = Path(args.out_file) if args.out_file else year_dir / f"figure2_weight_concentration_{args.year}.png"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(out_path)
    plt.close(fig)
    print(f"Saved: {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
