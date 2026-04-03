#!/usr/bin/env python3
"""Plot temporal placebo falsification diagnostics from aggregated CSV."""

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Plot temporal placebo falsification summary")
    parser.add_argument("--treated-year", type=int, required=True, help="Observed treatment year")
    parser.add_argument(
        "--base-dir",
        type=str,
        required=True,
        help="Directory containing temporal_placebo_summary_<treated-year>.csv",
    )
    parser.add_argument(
        "--input-csv",
        type=str,
        default=None,
        help="Optional explicit path to temporal_placebo_summary CSV",
    )
    parser.add_argument(
        "--output-png",
        type=str,
        default=None,
        help="Optional output PNG path (default: temporal_placebo_diagnostics_<treated-year>.png in base dir)",
    )
    return parser


def main() -> int:
    args = build_parser().parse_args()
    base_dir = Path(args.base_dir)
    input_csv = (
        Path(args.input_csv)
        if args.input_csv
        else base_dir / f"temporal_placebo_summary_{args.treated_year}.csv"
    )
    output_png = (
        Path(args.output_png)
        if args.output_png
        else base_dir / f"temporal_placebo_diagnostics_{args.treated_year}.png"
    )

    if not input_csv.exists():
        raise FileNotFoundError(f"Temporal placebo summary not found: {input_csv}")

    df = pd.read_csv(input_csv)
    if df.empty:
        raise ValueError(f"Temporal placebo summary is empty: {input_csv}")
    if "placebo_year" not in df.columns:
        raise ValueError(f"Missing required column 'placebo_year' in {input_csv}")

    has_pvals = "pval_rank" in df.columns
    if has_pvals and "false_positive_05" not in df.columns:
        df["false_positive_05"] = df["pval_rank"] < 0.05

    df = df.sort_values("placebo_year").reset_index(drop=True)

    fig, axes = plt.subplots(2, 1, figsize=(10, 8), sharex=True)

    # Panel 1: p-values by fake treatment year
    ax1 = axes[0]
    if has_pvals:
        ax1.plot(df["placebo_year"], df["pval_rank"], marker="o", color="#1f77b4", linewidth=1.8)
        sig = df[df["false_positive_05"].fillna(False).astype(bool)]
        if not sig.empty:
            ax1.scatter(sig["placebo_year"], sig["pval_rank"], color="#d62728", s=45, label="p < 0.05")
            ax1.legend(loc="upper right")
        ax1.axhline(0.05, color="#d62728", linestyle="--", linewidth=1)
        ax1.axhline(0.10, color="#ff7f0e", linestyle=":", linewidth=1)
        ax1.set_ylabel("Rank p-value")
        ax1.set_title(f"Temporal placebo falsification diagnostics ({args.treated_year})")
    else:
        status_col = "status" if "status" in df.columns else None
        if status_col is not None:
            ax1.plot(df["placebo_year"], df[status_col], marker="o", color="#9467bd", linewidth=1.8)
            ax1.set_ylabel("Run status (0=ok)")
            ax1.set_title(f"Temporal placebo run status ({args.treated_year})")
        else:
            ax1.text(
                0.5,
                0.5,
                "No p-value or status columns in summary CSV",
                ha="center",
                va="center",
                transform=ax1.transAxes,
            )
            ax1.set_ylabel("Diagnostics")
            ax1.set_title(f"Temporal placebo diagnostics ({args.treated_year})")

    # Panel 2: gate pass-rate trends by fake year (if columns available)
    ax2 = axes[1]
    gate_cols = [
        ("gate_balance_pass_rate", "balance"),
        ("gate_weight_pass_rate", "weight"),
        ("gate_concentration_pass_rate", "concentration"),
        ("gate_prefit_pass_rate", "prefit"),
        ("gate_ratio_pass_rate", "ratio"),
    ]
    plotted = 0
    for col, label in gate_cols:
        if col in df.columns:
            ax2.plot(df["placebo_year"], df[col], marker="o", linewidth=1.2, label=label)
            plotted += 1

    if plotted > 0:
        ax2.set_ylim(-0.02, 1.02)
        ax2.set_ylabel("Pass rate")
        ax2.legend(loc="lower left", ncol=min(3, plotted), frameon=False)
    else:
        ax2.text(
            0.5,
            0.5,
            "No gate pass-rate columns in summary CSV",
            ha="center",
            va="center",
            transform=ax2.transAxes,
        )
        ax2.set_ylabel("Pass rate")

    ax2.set_xlabel("Fake treatment year")
    ax2.set_xticks(df["placebo_year"].tolist())

    output_png.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(output_png, dpi=220)
    plt.close(fig)

    print(f"Saved temporal placebo diagnostics plot to {output_png}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
