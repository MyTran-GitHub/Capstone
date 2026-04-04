#!/usr/bin/env python3
"""Figure 3: Balance stability vs effective pool size (max_balance_std)."""

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
    parser = argparse.ArgumentParser(description="Plot balance stability by effective donor pool size")
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--base-dir", type=str, default="Embeddings/data/k_selection")
    parser.add_argument("--out-file", type=str, default=None)
    parser.add_argument("--threshold", type=float, default=0.10, help="Reference max |SMD| threshold")
    args = parser.parse_args()

    year_dir = Path(args.base_dir) / str(args.year)
    emb_fp = year_dir / "embedding_pool_frontier.csv"
    emb = pd.read_csv(emb_fp)
    xcol = _pool_col(emb)
    emb = emb.sort_values(xcol)

    fig, ax = plt.subplots(figsize=(8.6, 5.1), dpi=180)
    ax.plot(emb[xcol], emb["max_balance_std"], marker="o", linewidth=2.0, label="max_balance_std")
    ax.axhline(float(args.threshold), linestyle="--", linewidth=1.1, color="red", label=f"threshold={args.threshold:.2f}")

    ax.set_xlabel("effective_pool_size")
    ax.set_ylabel("max_balance_std")
    ax.set_title(f"Balance Stability ({args.year})")
    ax.grid(True, alpha=0.25)
    ax.legend(frameon=False)

    out_path = Path(args.out_file) if args.out_file else year_dir / f"figure3_balance_stability_{args.year}.png"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(out_path)
    plt.close(fig)
    print(f"Saved: {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
