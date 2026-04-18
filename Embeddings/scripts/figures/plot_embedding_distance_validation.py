#!/usr/bin/env python3
"""Two-panel diagnostic: embedding distance vs pre-fit and trajectory distance proxies."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


def _safe_spearman(df: pd.DataFrame, x: str, y: str) -> float:
    valid = df[[x, y]].replace([np.inf, -np.inf], np.nan).dropna()
    if len(valid) < 3:
        return float("nan")
    return float(valid[x].corr(valid[y], method="spearman"))


def _build_bins(df: pd.DataFrame, x: str, y: str, n_bins: int) -> pd.DataFrame:
    valid = df[[x, y]].replace([np.inf, -np.inf], np.nan).dropna().copy()
    if len(valid) < 5:
        return pd.DataFrame(columns=["x_median", "y_median", "y_lo", "y_hi", "n"])

    q = min(max(3, int(n_bins)), max(3, len(valid) // 3))
    try:
        valid["bin"] = pd.qcut(valid[x], q=q, duplicates="drop")
    except ValueError:
        return pd.DataFrame(columns=["x_median", "y_median", "y_lo", "y_hi", "n"])

    rows = []
    for _, grp in valid.groupby("bin"):
        yy = grp[y].to_numpy(dtype=float)
        rows.append(
            {
                "x_median": float(np.nanmedian(grp[x])),
                "y_median": float(np.nanmedian(yy)),
                "y_lo": float(np.nanpercentile(yy, 10)),
                "y_hi": float(np.nanpercentile(yy, 90)),
                "n": int(len(grp)),
            }
        )
    return pd.DataFrame(rows).sort_values("x_median").reset_index(drop=True)


def _load_distance_table(year_dir: Path) -> pd.DataFrame:
    diagnostics_fp = year_dir / "embedding_distance_fit_diagnostics.csv"
    if diagnostics_fp.exists():
        return pd.read_csv(diagnostics_fp)

    frontier_fp = year_dir / "embedding_pool_frontier.csv"
    if not frontier_fp.exists():
        raise FileNotFoundError(
            f"Missing both {diagnostics_fp.name} and {frontier_fp.name} under {year_dir}"
        )

    frontier = pd.read_csv(frontier_fp)
    rmse_col = "median_RMSE" if ("median_RMSE" in frontier.columns and frontier["median_RMSE"].notna().any()) else "rmse"
    for col in ["support_similarity_median", "support_similarity_p10", "support_similarity_min", "rmse_train"]:
        if col not in frontier.columns:
            frontier[col] = np.nan

    return pd.DataFrame(
        {
            "year": year_dir.name,
            "effective_pool_size": pd.to_numeric(frontier.get("effective_pool_size", frontier.get("pool_size", np.nan)), errors="coerce"),
            "representative_K": pd.to_numeric(frontier.get("representative_K", np.nan), errors="coerce"),
            "embedding_similarity_median": pd.to_numeric(frontier["support_similarity_median"], errors="coerce"),
            "embedding_similarity_p10": pd.to_numeric(frontier["support_similarity_p10"], errors="coerce"),
            "embedding_similarity_min": pd.to_numeric(frontier["support_similarity_min"], errors="coerce"),
            "embedding_distance_median": 1.0 - pd.to_numeric(frontier["support_similarity_median"], errors="coerce"),
            "embedding_distance_p10": 1.0 - pd.to_numeric(frontier["support_similarity_p10"], errors="coerce"),
            "embedding_distance_min": 1.0 - pd.to_numeric(frontier["support_similarity_min"], errors="coerce"),
            "prefit_rmse": pd.to_numeric(frontier[rmse_col], errors="coerce"),
            "prefit_rmse_metric": rmse_col,
            "trajectory_distance_proxy": pd.to_numeric(frontier["rmse_train"], errors="coerce"),
            "trajectory_distance_proxy_metric": "rmse_train",
        }
    )


def _plot_panel(ax, df: pd.DataFrame, x: str, y: str, title: str, n_bins: int) -> None:
    valid = df[[x, y]].replace([np.inf, -np.inf], np.nan).dropna()
    if valid.empty:
        ax.text(0.5, 0.5, "No usable data", ha="center", va="center", transform=ax.transAxes)
        ax.set_title(title)
        ax.set_xlabel(x)
        ax.set_ylabel(y)
        ax.grid(True, alpha=0.2)
        return

    ax.scatter(valid[x], valid[y], alpha=0.38, s=24, linewidths=0, color="#2B6CB0")
    bins = _build_bins(valid, x=x, y=y, n_bins=n_bins)
    if not bins.empty:
        ax.plot(bins["x_median"], bins["y_median"], color="#C53030", linewidth=2.2, marker="o", markersize=4)
        ax.fill_between(bins["x_median"], bins["y_lo"], bins["y_hi"], color="#C53030", alpha=0.18)

    rho = _safe_spearman(valid, x=x, y=y)
    rho_txt = f"Spearman rho={rho:.3f}" if np.isfinite(rho) else "Spearman rho=NA"
    ax.set_title(f"{title}\n{rho_txt}")
    ax.set_xlabel(x)
    ax.set_ylabel(y)
    ax.grid(True, alpha=0.25)


def main() -> int:
    parser = argparse.ArgumentParser(description="Plot embedding distance validation diagnostics")
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--base-dir", type=str, default="Embeddings/data/k_selection")
    parser.add_argument("--distance-col", type=str, default="embedding_distance_median", choices=["embedding_distance_median", "embedding_distance_p10", "embedding_distance_min"])
    parser.add_argument("--bins", type=int, default=10)
    parser.add_argument("--out-file", type=str, default=None)
    args = parser.parse_args()

    year_dir = Path(args.base_dir) / str(args.year)
    diag = _load_distance_table(year_dir)

    fig, axes = plt.subplots(1, 2, figsize=(12.6, 5.2), dpi=180)
    _plot_panel(
        axes[0],
        diag,
        x=args.distance_col,
        y="prefit_rmse",
        title="Panel A: Embedding Distance vs Pre-fit RMSE",
        n_bins=args.bins,
    )
    _plot_panel(
        axes[1],
        diag,
        x=args.distance_col,
        y="trajectory_distance_proxy",
        title="Panel B: Embedding Distance vs Trajectory Distance Proxy",
        n_bins=args.bins,
    )

    out_path = Path(args.out_file) if args.out_file else year_dir / f"figure_embedding_distance_validation_{args.year}.png"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(out_path)
    plt.close(fig)
    print(f"Saved: {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
