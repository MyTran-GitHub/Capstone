#!/usr/bin/env python3
"""Plot multi-panel K-selection diagnostics from k_selection_rmse.csv."""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

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
            # Current pipeline may store equivalent diagnostics in embedding_k_raw_results.
            fallback_fp = K_SELECTION_DIR / str(year) / "embedding_k_raw_results.csv"
            if fallback_fp.exists():
                fp = fallback_fp
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

    # Additionally, attempt to plot the embedding frontier trajectory from Embeddings/data/k_selection/<year>/embedding_pool_frontier.csv
    try:
        repo_root = Path(__file__).resolve().parent.parent.parent.parent
        emb_fp = repo_root / "Embeddings" / "data" / "k_selection" / str(year) / "embedding_pool_frontier.csv"
        if emb_fp.exists():
            emb_df = pd.read_csv(emb_fp)
            emb_pool_col = "effective_pool_size" if "effective_pool_size" in emb_df.columns else ("pool_size" if "pool_size" in emb_df.columns else None)
            if emb_pool_col is not None:
                emb_df = emb_df.sort_values(emb_pool_col)
                # compute pool proportion relative to the diagnostics full_pool if available, else use emb max
                try:
                    full_pool_val = float(df['pool_size'].max())
                except Exception:
                    full_pool_val = float(emb_df[emb_pool_col].max())
                if full_pool_val > 0:
                    emb_prop_vals = emb_df[emb_pool_col] / full_pool_val
                else:
                    emb_prop_vals = emb_df[emb_pool_col]
                # RMSE
                if "prefit_rmse_cv" in emb_df.columns:
                    ax1.plot(emb_prop_vals, emb_df["prefit_rmse_cv"], marker="o", linestyle="-", color="tab:blue", linewidth=1.8, label="embedding frontier (trajectory)")
                # ESS
                if "ess" in emb_df.columns or "ess_control" in emb_df.columns:
                    ess_col_name = "ess" if "ess" in emb_df.columns else "ess_control"
                    ax2.plot(emb_prop_vals, emb_df[ess_col_name], marker="o", linestyle="-", color="tab:blue", linewidth=1.8, label="embedding frontier (trajectory)")
                # max |SMD|
                if "max_balance_std" in emb_df.columns or "max_smd" in emb_df.columns:
                    max_col = "max_balance_std" if "max_balance_std" in emb_df.columns else "max_smd"
                    ax3.plot(emb_prop_vals, emb_df[max_col], marker="o", linestyle="-", color="tab:blue", linewidth=1.8, label="embedding frontier (trajectory)")
    except Exception:
        pass

    # Try to overlay embedding-selected and random-selected single points (from diagnostics CSV)
    repo_root = Path(__file__).resolve().parent.parent.parent.parent
    diag_fp = repo_root / "diagnostics" / "k_selection_synthesis" / "emb_vs_full_comparison.csv"
    try:
        if diag_fp.exists():
            diag = pd.read_csv(diag_fp)
            row = diag[diag["year"] == year]
            if not row.empty:
                row = row.iloc[0]
                emb_eff = row.get("emb_effective_pool_size", None)
                if emb_eff is not None and not pd.isna(emb_eff) and full_pool > 0:
                    emb_prop = float(emb_eff) / float(full_pool)
                    # Panel A: RMSE
                    if "emb_prefit_rmse_cv" in row.index and pd.notna(row["emb_prefit_rmse_cv"]):
                        ax1.scatter([emb_prop], [row["emb_prefit_rmse_cv"]], marker="X", color="tab:green", s=90, zorder=6, label="embedding selected (K)")
                    # Panel B: ESS
                    if "emb_ess" in row.index and pd.notna(row["emb_ess"]):
                        ax2.scatter([emb_prop], [row["emb_ess"]], marker="X", color="tab:green", s=90, zorder=6, label="embedding selected (K)")
                    # Panel C: max |SMD|
                    if "emb_max_smd" in row.index and pd.notna(row["emb_max_smd"]):
                        ax3.scatter([emb_prop], [row["emb_max_smd"]], marker="X", color="tab:green", s=90, zorder=6, label="embedding selected (K)")

                # Random-selected K overlay (single point from emb_vs_full_comparison)
                rnd_eff = row.get("emb_effective_pool_size", None)
                if rnd_eff is not None and not pd.isna(rnd_eff) and full_pool > 0:
                    rnd_prop = float(rnd_eff) / float(full_pool)
                    if "random_prefit_rmse_cv" in row.index and pd.notna(row["random_prefit_rmse_cv"]):
                        ax1.scatter([rnd_prop], [row["random_prefit_rmse_cv"]], marker="X", color="tab:gray", s=90, zorder=6, label="random selected (K)")
                    if "random_ess" in row.index and pd.notna(row["random_ess"]):
                        ax2.scatter([rnd_prop], [row["random_ess"]], marker="X", color="tab:gray", s=90, zorder=6, label="random selected (K)")
                    if "random_max_smd" in row.index and pd.notna(row["random_max_smd"]):
                        ax3.scatter([rnd_prop], [row["random_max_smd"]], marker="X", color="tab:gray", s=90, zorder=6, label="random selected (K)")
    except Exception:
        pass

    # Plot full random trajectory if available under Embeddings/data/k_selection/<year>/random_pool_summary.csv
    try:
        repo_root = Path(__file__).resolve().parent.parent.parent.parent
        rnd_fp = repo_root / "Embeddings" / "data" / "k_selection" / str(year) / "random_pool_summary.csv"
        if rnd_fp.exists():
            rnd_df = pd.read_csv(rnd_fp)
            rnd_pool_col = "effective_pool_size" if "effective_pool_size" in rnd_df.columns else ("pool_size" if "pool_size" in rnd_df.columns else None)
            if rnd_pool_col is not None:
                rnd_df = rnd_df.sort_values(rnd_pool_col)
                if full_pool > 0:
                    rnd_prop_vals = rnd_df[rnd_pool_col] / float(full_pool)
                else:
                    rnd_prop_vals = rnd_df[rnd_pool_col]
                if "prefit_rmse_cv" in rnd_df.columns:
                    ax1.plot(rnd_prop_vals, rnd_df["prefit_rmse_cv"], marker="s", linestyle="--", color="tab:gray", alpha=0.9, label="random pools (trajectory)")
                if "median_ess_control" in rnd_df.columns:
                    ax2.plot(rnd_prop_vals, rnd_df["median_ess_control"], marker="s", linestyle="--", color="tab:gray", alpha=0.9, label="random pools (trajectory)")
                if "max_balance_std" in rnd_df.columns:
                    ax3.plot(rnd_prop_vals, rnd_df["max_balance_std"], marker="s", linestyle="--", color="tab:gray", alpha=0.9, label="random pools (trajectory)")
    except Exception:
        pass

    # Deduplicate legends on panels that have overlays
    def _dedupe_legend(ax):
        handles, labels = ax.get_legend_handles_labels()
        if not labels:
            return
        by_label = dict(zip(labels, handles))
        ax.legend(by_label.values(), by_label.keys(), frameon=False)

    _dedupe_legend(ax1)
    _dedupe_legend(ax2)
    _dedupe_legend(ax3)

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
