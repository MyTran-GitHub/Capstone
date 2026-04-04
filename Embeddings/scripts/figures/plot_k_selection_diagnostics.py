#!/usr/bin/env python3
"""Plot multi-panel K-selection diagnostics from k_selection_rmse.csv."""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
BASE_DIR = Path(__file__).resolve().parent.parent.parent
DATA_DIR = BASE_DIR / "data"
K_SELECTION_DIR = DATA_DIR / "k_selection"
FIGURES_DIR = DATA_DIR / "figures"

logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(asctime)s - %(name)s - %(message)s')
logger = logging.getLogger(__name__)


def load_diagnostics(year: int, experiment_name: str = "", output_tag: str = "") -> pd.DataFrame:
    tag_suffix = f"_{output_tag}" if output_tag else ""
    if experiment_name:
        fp = K_SELECTION_DIR / experiment_name / str(year) / f"k_selection_rmse{tag_suffix}.csv"
    else:
        fp = K_SELECTION_DIR / str(year) / f"k_selection_rmse{tag_suffix}.csv"
    if not fp.exists():
        # Backward compatibility with legacy non-namespaced outputs.
        legacy_fp = K_SELECTION_DIR / str(year) / f"k_selection_rmse{tag_suffix}.csv"
        if legacy_fp.exists():
            fp = legacy_fp
        else:
            raise FileNotFoundError(f"Missing diagnostics CSV: {fp}")

    df = pd.read_csv(fp)
    if df.empty:
        raise ValueError(f"Empty diagnostics CSV: {fp}")

    if "rmse_test" in df.columns and "rmse" not in df.columns:
        df["rmse"] = df["rmse_test"]
    if "n_control" in df.columns and "pool_size" not in df.columns:
        df["pool_size"] = df["n_control"]

    if "pool_size" not in df.columns:
        raise ValueError("Expected pool size column (pool_size or n_control) in diagnostics CSV")

    full_pool = float(df["pool_size"].max())
    df["pool_prop_full"] = df["pool_size"] / full_pool if full_pool > 0 else float("nan")

    if "runtime_seconds" not in df.columns:
        df["runtime_seconds"] = float("nan")

    return df.sort_values("pool_prop_full")


def make_plot(df: pd.DataFrame, year: int, out_file: Path) -> None:
    fig, axes = plt.subplots(2, 2, figsize=(12, 8), dpi=180, constrained_layout=True)
    ax1, ax2, ax3, ax4 = axes.ravel()

    x = df["pool_prop_full"]

    # Panel A: RMSE
    ax1.plot(x, df["rmse"], marker="o", linewidth=1.8, label="median RMSE")
    if "rmse_train" in df.columns:
        ax1.plot(x, df["rmse_train"], marker="s", linewidth=1.2, alpha=0.7, label="train RMSE")
    ax1.set_title("RMSE vs Pool Proportion")
    ax1.set_xlabel("Control pool proportion of full")
    ax1.set_ylabel("RMSE")
    ax1.grid(alpha=0.25)
    ax1.legend(frameon=False)

    # Panel B: ESS
    if "ess_control" in df.columns and df["ess_control"].notna().any():
        ax2.plot(x, df["ess_control"], marker="o", linewidth=1.8, label="ESS control")
    if "ess_ratio" in df.columns and df["ess_ratio"].notna().any():
        ax2_t = ax2.twinx()
        ax2_t.plot(x, df["ess_ratio"], marker="^", linewidth=1.2, color="tab:orange", alpha=0.8, label="ESS ratio")
        ax2_t.set_ylabel("ESS / N_treated")
    ax2.set_title("ESS vs Pool Proportion")
    ax2.set_xlabel("Control pool proportion of full")
    ax2.set_ylabel("ESS control")
    ax2.grid(alpha=0.25)

    # Panel C: max |SMD|
    if "max_balance_std" in df.columns:
        ax3.plot(x, df["max_balance_std"], marker="o", linewidth=1.8, label="max |SMD|")
    ax3.axhline(0.10, linestyle="--", linewidth=1.0, color="red", label="0.10 threshold")
    ax3.set_title("Balance vs Pool Proportion")
    ax3.set_xlabel("Control pool proportion of full")
    ax3.set_ylabel("max |SMD|")
    ax3.grid(alpha=0.25)
    ax3.legend(frameon=False)

    # Panel D: runtime
    if df["runtime_seconds"].notna().any():
        ax4.plot(x, df["runtime_seconds"], marker="o", linewidth=1.8, label="runtime")
    else:
        ax4.text(0.5, 0.5, "runtime_seconds not available", ha="center", va="center", transform=ax4.transAxes)
    ax4.set_title("Runtime vs Pool Proportion")
    ax4.set_xlabel("Control pool proportion of full")
    ax4.set_ylabel("runtime (sec)")
    ax4.grid(alpha=0.25)

    fig.suptitle(f"K-selection diagnostics: {year}", fontsize=13, fontweight="bold")

    out_file.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_file)
    plt.close(fig)
    logger.info("Saved diagnostics plot: %s", out_file)


def main() -> int:
    parser = argparse.ArgumentParser(description="Plot K-selection diagnostics for one cohort year")
    parser.add_argument("year", type=int, help="Cohort year (e.g., 2019)")
    parser.add_argument(
        "--experiment-name",
        type=str,
        default="",
        help="Optional legacy experiment namespace under Embeddings/data/k_selection",
    )
    parser.add_argument(
        "--output-tag",
        type=str,
        default="",
        help="Optional tag suffix used in k_selection_rmse_<tag>.csv",
    )
    parser.add_argument(
        "--out-file",
        type=str,
        default=None,
        help="Output plot path (default: Embeddings/data/figures/k_selection_diagnostics_<year>.png)",
    )
    args = parser.parse_args()

    name_prefix = f"{args.experiment_name}_" if args.experiment_name else ""
    default_name = f"k_selection_diagnostics_{name_prefix}{args.year}.png"
    out_file = Path(args.out_file) if args.out_file else FIGURES_DIR / default_name

    df = load_diagnostics(args.year, experiment_name=args.experiment_name, output_tag=args.output_tag)
    make_plot(df, args.year, out_file)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
