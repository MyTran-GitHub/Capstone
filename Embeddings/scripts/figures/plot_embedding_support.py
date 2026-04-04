#!/usr/bin/env python3
"""Figure 4: Embedding support quality vs realized pool size."""

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
    parser = argparse.ArgumentParser(description="Plot embedding support quality by pool size")
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--base-dir", type=str, default="Embeddings/data/k_selection")
    parser.add_argument("--out-file", type=str, default=None)
    args = parser.parse_args()

    year_dir = Path(args.base_dir) / str(args.year)
    support_fp = year_dir / "similarity_support.csv"
    support = pd.read_csv(support_fp)
    xcol = _pool_col(support)
    support = support.sort_values([xcol, "K"])

    # One point per realized pool size (smallest representative K).
    support_frontier = support.groupby(xcol, as_index=False).first()

    fig, ax = plt.subplots(figsize=(8.4, 5.0), dpi=180)
    ax.plot(
        support_frontier[xcol],
        support_frontier["p10_similarity"],
        marker="o",
        linewidth=2.0,
        label="support_similarity_p10",
    )
    ax.set_xlabel("effective_pool_size")
    ax.set_ylabel("support_similarity_p10")
    ax.set_title(f"Embedding Support Quality ({args.year})")
    ax.grid(True, alpha=0.25)
    ax.legend(frameon=False)

    out_path = Path(args.out_file) if args.out_file else year_dir / f"figure4_embedding_support_{args.year}.png"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(out_path)
    plt.close(fig)
    print(f"Saved: {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
