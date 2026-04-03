#!/usr/bin/env python3
"""Plot placebo ATT histogram from placebo_draws_<year>.csv."""

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd


def pick_first_existing_column(df: pd.DataFrame, candidates: list[str], label: str) -> str:
    for col in candidates:
        if col in df.columns:
            return col
    raise ValueError(f"Missing required {label} column. Tried: {', '.join(candidates)}")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Plot placebo histogram from placebo draw CSV")
    parser.add_argument("--year", type=int, required=True, help="Treatment year")
    parser.add_argument(
        "--base-dir",
        type=str,
        default=None,
        help="Base directory containing placebo CSVs/plots (default: Embeddings/results/placebo)",
    )
    parser.add_argument(
        "--input-csv",
        type=str,
        default=None,
        help="Optional custom placebo draw CSV path (default: Embeddings/results/placebo/placebo_draws_<year>.csv)",
    )
    parser.add_argument(
        "--summary-csv",
        type=str,
        default=None,
        help="Optional placebo summary CSV path (default: Embeddings/results/placebo/placebo_summary_<year>.csv)",
    )
    parser.add_argument(
        "--output-png",
        type=str,
        default=None,
        help="Optional output PNG path (default: Embeddings/results/placebo/placebo_histogram_<year>.png)",
    )
    parser.add_argument("--bins", type=int, default=30, help="Histogram bin count (default: 30)")
    return parser


def main() -> int:
    args = build_parser().parse_args()

    default_dir = Path(args.base_dir) if args.base_dir else (Path("Embeddings") / "results" / "placebo")
    input_csv = Path(args.input_csv) if args.input_csv else default_dir / f"placebo_draws_{args.year}.csv"
    summary_csv = Path(args.summary_csv) if args.summary_csv else default_dir / f"placebo_summary_{args.year}.csv"
    output_png = Path(args.output_png) if args.output_png else default_dir / f"placebo_histogram_{args.year}.png"

    if not input_csv.exists():
        raise FileNotFoundError(f"Placebo draw CSV not found: {input_csv}")

    draws = pd.read_csv(input_csv)
    placebo_att_col = pick_first_existing_column(
        draws,
        ["placebo_att_post", "placebo_att"],
        "placebo ATT",
    )

    if "valid" in draws.columns:
        draws_valid = draws[draws["valid"].fillna(False).astype(bool)].copy()
    else:
        draws_valid = draws.dropna(subset=[placebo_att_col]).copy()

    if draws_valid.empty:
        raise ValueError("No valid placebo draws available for plotting.")

    obs_att = None
    pval_rank = None
    if summary_csv.exists():
        summary = pd.read_csv(summary_csv)
        if not summary.empty:
            if "obs_att_post" in summary.columns:
                obs_att = summary.loc[0, "obs_att_post"]
            elif "obs_att" in summary.columns:
                obs_att = summary.loc[0, "obs_att"]
            pval_rank = summary.loc[0, "pval_rank"] if "pval_rank" in summary.columns else None

    if obs_att is None:
        obs_att_col = None
        if "obs_att_post" in draws_valid.columns:
            obs_att_col = "obs_att_post"
        elif "obs_att" in draws_valid.columns:
            obs_att_col = "obs_att"
        if obs_att_col is not None and draws_valid[obs_att_col].notna().any():
            obs_att = float(draws_valid[obs_att_col].dropna().iloc[0])

    output_png.parent.mkdir(parents=True, exist_ok=True)

    fig, ax = plt.subplots(figsize=(8, 5))
    ax.hist(draws_valid[placebo_att_col], bins=max(5, args.bins), color="#7395AE", edgecolor="black", alpha=0.9)

    title = f"Placebo ATT histogram ({args.year})"
    if pval_rank is not None and pd.notna(pval_rank):
        title += f"\nrank p-value={float(pval_rank):.3f}"
    ax.set_title(title)
    ax.set_xlabel("Placebo ATT")
    ax.set_ylabel("Count")

    if obs_att is not None and pd.notna(obs_att):
        ax.axvline(float(obs_att), color="#C70039", linestyle="--", linewidth=2, label=f"Observed ATT={float(obs_att):.4f}")
        ax.legend(loc="best")

    fig.tight_layout()
    fig.savefig(output_png, dpi=200)
    plt.close(fig)

    print(f"Saved placebo histogram to {output_png}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
