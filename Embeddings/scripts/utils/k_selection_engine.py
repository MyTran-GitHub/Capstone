"""Core K-selection evaluation engine extracted from CLI orchestration script."""

import json
import hashlib
import logging
import re
import shlex
import shutil
import subprocess
import tempfile
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed
from functools import partial
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import numpy as np
import pandas as pd

BASE_DIR = Path(__file__).resolve().parent.parent.parent
DATA_DIR = BASE_DIR / "data"
CBPS_INTEGRATION_DIR = DATA_DIR / "cbps_integration"

logger = logging.getLogger(__name__)


def _canonicalize_rolling_windows(rolling_windows: Optional[List[Dict[str, int]]]) -> List[Dict[str, int]]:
    if not rolling_windows:
        return []
    canonical = []
    for row in rolling_windows:
        canonical.append({
            "window_id": str(row.get("window_id", "")),
            "train_start": int(row.get("train_start")),
            "train_end": int(row.get("train_end")),
            "test_start": int(row.get("test_start")),
            "test_end": int(row.get("test_end")),
        })
    return canonical


def _build_eval_context(train_years: List[int], test_years: List[int], rolling_windows: Optional[List[Dict[str, int]]]) -> Dict:
    payload = {
        "train_years": [int(y) for y in train_years],
        "test_years": [int(y) for y in test_years],
        "rolling_windows": _canonicalize_rolling_windows(rolling_windows),
    }
    signature = hashlib.sha1(json.dumps(payload, sort_keys=True).encode("utf-8")).hexdigest()[:12]
    payload["eval_signature"] = signature
    return payload


def _cache_meta_path(year: int, output_prefix: str) -> Path:
    return CBPS_INTEGRATION_DIR / str(year) / f"cbps_metrics_meta_{output_prefix}_{year}.json"


def _write_cache_meta(year: int, output_prefix: str, train_years: List[int], test_years: List[int], rolling_windows: Optional[List[Dict[str, int]]]) -> None:
    meta_path = _cache_meta_path(year, output_prefix)
    meta_path.parent.mkdir(parents=True, exist_ok=True)
    payload = _build_eval_context(train_years, test_years, rolling_windows)
    meta_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


def _cache_meta_matches(year: int, output_prefix: str, train_years: List[int], test_years: List[int], rolling_windows: Optional[List[Dict[str, int]]]) -> bool:
    meta_path = _cache_meta_path(year, output_prefix)
    if not meta_path.exists():
        return False
    try:
        current = _build_eval_context(train_years, test_years, rolling_windows)
        cached = json.loads(meta_path.read_text(encoding="utf-8"))
        return (
            cached.get("eval_signature") == current.get("eval_signature") and
            cached.get("train_years") == current.get("train_years") and
            cached.get("test_years") == current.get("test_years") and
            cached.get("rolling_windows") == current.get("rolling_windows")
        )
    except Exception:
        return False


def _cbps_artifact_paths(year: int, output_prefix: str) -> List[Path]:
    base = CBPS_INTEGRATION_DIR / str(year)
    suffix = f"_{output_prefix}_{year}.csv"
    return [
        base / f"cbps_metrics{suffix}",
        base / f"cbps_rmse_windows{suffix}",
        base / f"cbps_weights{suffix}",
        base / f"cbps_weights_full{suffix}",
        base / f"selected_controls{suffix}",
        _cache_meta_path(year, output_prefix),
    ]


def _cleanup_cbps_artifacts(year: int, output_prefix: str) -> None:
    for p in _cbps_artifact_paths(year, output_prefix):
        try:
            p.unlink(missing_ok=True)
        except Exception:
            continue

def get_k_nearest_union(similarities: Dict[int, np.ndarray], K: int) -> Set[int]:
    """
    Get union of K nearest controls for all treated pixels
    Returns:
        Set of unique control indices
    """
    selected_controls = set()
    for t_idx, sims in similarities.items():
        top_k = sims[:K, 0].astype(int)
        selected_controls.update(top_k)
    return selected_controls


def get_k_pool_diagnostics(
    similarities: Dict[int, np.ndarray],
    K: int,
    n_treated: int,
    n_controls_full: int,
) -> Dict[str, float]:
    """Compute realized donor-pool and embedding support diagnostics for a given K."""
    selected_controls = set()
    all_topk_sim = []
    per_treated_min = []
    per_treated_median = []

    for _, sims in similarities.items():
        top_k = sims[:K, :]
        selected_controls.update(top_k[:, 0].astype(int))
        sim_vals = top_k[:, 1].astype(float)
        if sim_vals.size > 0:
            all_topk_sim.extend(sim_vals.tolist())
            per_treated_min.append(float(np.min(sim_vals)))
            per_treated_median.append(float(np.median(sim_vals)))

    pool_size = len(selected_controls)
    pool_prop_full = (pool_size / max(1, n_controls_full))
    coverage_ratio = (pool_size / max(1, n_treated))

    all_topk_arr = np.array(all_topk_sim, dtype=float) if all_topk_sim else np.array([np.nan])
    min_arr = np.array(per_treated_min, dtype=float) if per_treated_min else np.array([np.nan])
    med_arr = np.array(per_treated_median, dtype=float) if per_treated_median else np.array([np.nan])

    return {
        "K": int(K),
        "pool_size": int(pool_size),
        "pool_prop_full": float(pool_prop_full),
        "coverage_ratio": float(coverage_ratio),
        "support_similarity_mean": float(np.nanmean(all_topk_arr)),
        "support_similarity_min": float(np.nanmin(all_topk_arr)),
        "support_similarity_p10": float(np.nanpercentile(all_topk_arr, 10)),
        "support_similarity_median": float(np.nanmedian(all_topk_arr)),
        "support_per_treated_min_p10": float(np.nanpercentile(min_arr, 10)),
        "support_per_treated_median": float(np.nanmedian(med_arr)),
    }


def build_pool_diagnostics_table(
    similarities: Dict[int, np.ndarray],
    K_candidates: List[int],
    n_treated: int,
    n_controls_full: int,
) -> pd.DataFrame:
    rows = [
        get_k_pool_diagnostics(similarities, K, n_treated=n_treated, n_controls_full=n_controls_full)
        for K in sorted(set(K_candidates))
    ]
    return pd.DataFrame(rows).sort_values("K").reset_index(drop=True)


def build_discovery_k_grid(max_k_possible: int) -> List[int]:
    """Step 1: dense K scan to discover reachable effective pool sizes."""
    if max_k_possible <= 300:
        return list(range(1, int(max_k_possible) + 1))

    linear_grid = np.linspace(1, max_k_possible, num=150, dtype=int).tolist()
    log_grid = np.geomspace(1, max_k_possible, num=150, dtype=int).astype(int).tolist()
    return sorted(set(linear_grid + log_grid + [1, int(max_k_possible)]))


def compress_effective_pool_grid(
    unique_pool_df: pd.DataFrame,
    max_points: int = 25,
    target_points: int = 16,
) -> pd.DataFrame:
    """Step 2: keep an evenly spaced subset of effective pool sizes when grid is too large."""
    if unique_pool_df.empty:
        return unique_pool_df
    if len(unique_pool_df) <= max_points:
        return unique_pool_df.reset_index(drop=True)

    target_points = max(12, min(int(target_points), int(max_points)))
    idx = np.linspace(0, len(unique_pool_df) - 1, num=target_points)
    idx = sorted(set(int(round(i)) for i in idx))
    idx[0] = 0
    idx[-1] = len(unique_pool_df) - 1
    return unique_pool_df.iloc[idx].sort_values('pool_size').reset_index(drop=True)


def compute_donor_hash_for_k(similarities: Dict[int, np.ndarray], embeddings_df: pd.DataFrame, K: int) -> str:
    """Hash sorted donor unit IDs for a specific K to detect identical donor pools."""
    selected_controls = get_k_nearest_union(similarities, int(K))
    selected_units = embeddings_df.loc[list(selected_controls), 'unit'].astype(str).tolist()
    selected_units_sorted = sorted(selected_units)
    payload = "\n".join(selected_units_sorted).encode("utf-8")
    return hashlib.sha256(payload).hexdigest()


def build_unique_pool_frontier(pool_df: pd.DataFrame) -> pd.DataFrame:
    """Return the unique-pool frontier using the smallest K for each realized pool size."""
    if pool_df.empty:
        return pd.DataFrame()

    d = pool_df.copy()
    d['K'] = pd.to_numeric(d['K'], errors='coerce')
    d['pool_size'] = pd.to_numeric(d['pool_size'], errors='coerce')
    d = d.dropna(subset=['K', 'pool_size']).copy()
    if d.empty:
        return pd.DataFrame()

    d['K'] = d['K'].astype(int)
    d['pool_size'] = d['pool_size'].astype(int)
    d = d.sort_values(['pool_size', 'K']).drop_duplicates(subset=['pool_size'], keep='first')
    return d.reset_index(drop=True)


def _warn_if_pool_frontier_saturated(frontier_df: pd.DataFrame, n_controls_full: int) -> None:
    """Stage 3 guard: warn (without stopping) when frontier design variation is too limited."""
    if frontier_df.empty:
        logger.warning("Embedding donor pools saturate too quickly - limited design variation.")
        return

    unique_count = int(frontier_df['pool_size'].nunique())
    pool_range = int(frontier_df['pool_size'].max() - frontier_df['pool_size'].min())
    range_frac = float(pool_range / max(1, int(n_controls_full)))
    if unique_count < 5 or range_frac < 0.05:
        logger.warning("Embedding donor pools saturate too quickly - limited design variation.")


def select_phase2_pool(
    effective_pool_df: pd.DataFrame,
    full_control_pool: int,
    phase2_policy: str = "pool_then_k",
) -> Dict[str, int]:
    """Step 7: choose phase-2 pool by RMSE-elbow rule.

    Rule:
    1) Find the minimum pre-treatment RMSE on the effective frontier.
    2) Keep K values with RMSE <= 1.05 * min_RMSE.
     3) Apply policy tie-break inside the near-optimal plateau:
         - pool_then_k (default): smallest realized pool_size, then smallest representative_K.
         - k_then_pool: smallest representative_K, then smallest pool_size.
    """
    if effective_pool_df.empty:
        raise ValueError("Cannot select phase-2 pool from empty frontier")

    d = effective_pool_df.copy()
    d['pool_size'] = pd.to_numeric(d['pool_size'], errors='coerce')
    d['representative_K'] = pd.to_numeric(d.get('representative_K', d.get('K', np.nan)), errors='coerce')
    d['rmse_objective'] = pd.to_numeric(
        d['median_RMSE'] if ('median_RMSE' in d.columns and d['median_RMSE'].notna().any()) else d['rmse'],
        errors='coerce',
    )
    d = d.dropna(subset=['pool_size', 'representative_K', 'rmse_objective']).copy()
    if d.empty:
        raise ValueError("Cannot select phase-2 pool: no rows with valid pool_size/K/RMSE")

    d['pool_size'] = d['pool_size'].astype(int)
    d['representative_K'] = d['representative_K'].astype(int)

    eligible = d[d['pool_size'] < int(full_control_pool)].copy()
    if eligible.empty:
        eligible = d.copy()

    rmse_min = float(eligible['rmse_objective'].min())
    rmse_threshold = rmse_min * 1.05
    near_opt = eligible[eligible['rmse_objective'] <= (rmse_threshold + 1e-12)].copy()
    if near_opt.empty:
        near_opt = eligible.copy()

    policy = str(phase2_policy or "pool_then_k").strip().lower()
    if policy == 'k_then_pool':
        sort_cols = ['representative_K', 'pool_size']
        selection_rule = 'rmse_plateau_smallest_k_then_pool'
    else:
        sort_cols = ['pool_size', 'representative_K']
        selection_rule = 'rmse_plateau_smallest_pool_then_k'

    pick = near_opt.sort_values(sort_cols).iloc[0]
    return {
        'selected_pool_size': int(pick['pool_size']),
        'selected_K': int(pick['representative_K']),
        'selection_rule': selection_rule,
        'phase2_policy': policy,
        'purpose': 'phase2_comparison',
    }


def _validate_k_candidates(K_candidates: List[int], max_k_possible: int) -> None:
    bad = [int(k) for k in K_candidates if int(k) <= 0 or int(k) > int(max_k_possible)]
    if bad:
        raise ValueError(
            f"Invalid K candidates detected: {sorted(set(bad))}. Allowed range is [1, {int(max_k_possible)}]."
        )


def _validate_pool_diagnostics(pool_df: pd.DataFrame, n_controls_full: int) -> None:
    if pool_df.empty:
        raise ValueError("Pool diagnostics table is empty")
    if (pool_df['pool_size'] > int(n_controls_full)).any():
        bad = pool_df.loc[pool_df['pool_size'] > int(n_controls_full), ['K', 'pool_size']]
        raise ValueError(f"pool_size exceeds full controls for rows: {bad.to_dict(orient='records')[:5]}")

    d = pool_df.sort_values('K').copy()
    diffs = d['pool_size'].diff().fillna(0)
    if (diffs < 0).any():
        bad_rows = d.loc[diffs < 0, ['K', 'pool_size']]
        raise ValueError(f"pool_size is not monotone non-decreasing in K: {bad_rows.to_dict(orient='records')[:5]}")


def build_rolling_windows(start_year: int,
                          end_year: int,
                          train_length: int = 9,
                          test_length: int = 3) -> List[Dict[str, int]]:
    """Build rolling train/test windows over pre-treatment years."""
    windows: List[Dict[str, int]] = []
    if end_year < start_year:
        return windows
    first_test_start = start_year + train_length
    last_test_start = end_year - test_length + 1
    if last_test_start < first_test_start:
        return windows
    for i, test_start in enumerate(range(first_test_start, last_test_start + 1), start=1):
        train_start = test_start - train_length
        train_end = test_start - 1
        test_end = test_start + test_length - 1
        windows.append({
            "window_id": f"w{i}",
            "train_start": int(train_start),
            "train_end": int(train_end),
            "test_start": int(test_start),
            "test_end": int(test_end),
        })
    return windows


def write_pipeline_commands(
    output_dir: Path,
    year: int,
    optimal_k: int,
    selected_controls_csv: Path,
    train_start_year: int,
    train_end_year: int,
    test_start_year: int,
    test_end_year: int,
    analysis_base_dir: str,
    save_full_weights: bool,
    experiment_name: str,
    output_tag: str,
    placebo_draws: int,
    placebo_post_years: str,
    placebo_output_dir: Path,
    placebo_assignment_mode: str = "control_only",
    placebo_workers: int = 1,
    placebo_seed_base: int = 1,
    placebo_checkpoint_every: int = 100,
    placebo_resume: bool = True,
    placebo_gate_prefit_mult: float = 5.0,
    placebo_enforce_ratio_gate: bool = False,
    placebo_gate_ratio_max: float = 20.0,
    placebo_donor_size: int = 1,
    include_temporal_placebo: bool = True,
    temporal_placebo_years: str = "",
    temporal_placebo_draws: int = 300,
    temporal_placebo_pre_start: int = 2008,
    temporal_placebo_pre_end: int = 2017,
    temporal_placebo_post_lag: int = 1,
    temporal_placebo_post_year_count: int = 1,
) -> Tuple[Path, Path]:
    """Write reusable pipeline commands as CSV + shell script for the selected year."""
    tag_suffix = f"_{output_tag}" if output_tag else ""
    command_rows = [
        {
            "step": 1,
            "name": "run_cbps_optimal_k",
            "command": (
                f"Rscript Embeddings/scripts/04_run_cbps_with_selected_controls.R {year} "
                f"'{selected_controls_csv.as_posix()}' k{optimal_k}{tag_suffix} "
                f"{int(train_start_year)} {int(train_end_year)} {int(test_start_year)} {int(test_end_year)} "
                f"--experiment-name {experiment_name} --analysis-base-dir {analysis_base_dir} "
                f"--save-full-weights {'true' if save_full_weights else 'false'}"
            ),
            "description": "Run CBPS with the selected embedding donor pool.",
        },
        {
            "step": 2,
            "name": "run_placebo_simulator",
            "command": (
                "Rscript Embeddings/scripts/figures/placebo_att_simulator.R "
                f"year={year} B={int(placebo_draws)} post_years={placebo_post_years} "
                f"assignment_mode={placebo_assignment_mode} n_workers={int(placebo_workers)} "
                f"seed_base={int(placebo_seed_base)} checkpoint_every={int(placebo_checkpoint_every)} "
                f"resume={'true' if placebo_resume else 'false'} gate_prefit_mult={float(placebo_gate_prefit_mult)} "
                f"enforce_ratio_gate={'true' if placebo_enforce_ratio_gate else 'false'} gate_ratio_max={float(placebo_gate_ratio_max)} "
                f"donor_placebo_size={int(placebo_donor_size)} "
                f"experiment_name={experiment_name} output_tag={output_tag or 'na'} "
                f"out_dir='{placebo_output_dir.as_posix()}'"
            ),
            "description": "Generate placebo ATT draws and summary CSV.",
        },
        {
            "step": 3,
            "name": "plot_placebo_histogram",
            "command": (
                "python Embeddings/scripts/figures/plot_placebo_histogram.py "
                f"--year {year} --base-dir '{placebo_output_dir.as_posix()}'"
            ),
            "description": "Create placebo histogram PNG from placebo draw CSV.",
        },
        {
            "step": 4,
            "name": "build_per_year_comparison_report",
            "command": (
                "python Embeddings/scripts/11_generate_per_year_comparison_report.py "
                f"--year-start {int(year)} --year-end {int(year)} "
                + (f"--output-tag {output_tag}" if output_tag else "")
            ),
            "description": "Build consolidated per-year Full vs Random vs Embedding report.",
        },
    ]

    if include_temporal_placebo:
        temporal_dir = placebo_output_dir / "temporal"
        temporal_command = (
            "Rscript Embeddings/scripts/figures/temporal_placebo_runner.R "
            f"treated_year={year} B={int(temporal_placebo_draws)} "
            f"pre_start={int(temporal_placebo_pre_start)} pre_end={int(temporal_placebo_pre_end)} "
            f"post_lag={int(temporal_placebo_post_lag)} post_year_count={int(temporal_placebo_post_year_count)} "
            f"assignment_mode={placebo_assignment_mode} n_workers={int(placebo_workers)} "
            f"seed_base={int(placebo_seed_base)} checkpoint_every={int(placebo_checkpoint_every)} "
            f"resume={'true' if placebo_resume else 'false'} gate_prefit_mult={float(placebo_gate_prefit_mult)} "
            f"enforce_ratio_gate={'true' if placebo_enforce_ratio_gate else 'false'} gate_ratio_max={float(placebo_gate_ratio_max)} "
            f"donor_placebo_size={int(placebo_donor_size)} "
            + (f"placebo_years={temporal_placebo_years} " if temporal_placebo_years else "")
            + f"out_dir='{temporal_dir.as_posix()}'"
        )
        temporal_plot_command = (
            "python Embeddings/scripts/figures/plot_temporal_placebo_summary.py "
            f"--treated-year {year} --base-dir '{temporal_dir.as_posix()}'"
        )

        command_rows.insert(
            3,
            {
                "step": 4,
                "name": "run_temporal_placebo_falsification",
                "command": temporal_command,
                "description": "Run fake treatment year falsification test and aggregate p-values.",
            },
        )
        command_rows.insert(
            4,
            {
                "step": 5,
                "name": "plot_temporal_placebo_summary",
                "command": temporal_plot_command,
                "description": "Plot temporal placebo p-values and gate pass-rate trends.",
            },
        )

    for idx, row in enumerate(command_rows, start=1):
        row["step"] = idx

    command_rows = [
        {
            "step": row["step"],
            "name": row["name"],
            "command": row["command"],
            "description": row["description"],
        }
        for row in command_rows
    ]

    commands_df = pd.DataFrame(command_rows)
    commands_csv = output_dir / f"pipeline_commands{tag_suffix}.csv"
    commands_sh = output_dir / f"run_pipeline_commands{tag_suffix}.sh"
    commands_df.to_csv(commands_csv, index=False)

    shell_lines = ["#!/usr/bin/env bash", "set -euo pipefail", ""]
    for row in command_rows:
        shell_lines.append(f"echo '[STEP {row['step']}] {row['name']}'")
        shell_lines.append(row["command"])
        shell_lines.append("")
    commands_sh.write_text("\n".join(shell_lines), encoding="utf-8")
    try:
        commands_sh.chmod(0o755)
    except OSError:
        logger.warning("Could not set executable bit on %s", commands_sh)

    return commands_csv, commands_sh


def run_random_pool_experiment(
    similarities: Dict[int, np.ndarray],
    embeddings_df: pd.DataFrame,
    effective_pool_table: pd.DataFrame,
    year: int,
    random_reps: int = 20,
    output_tag: str = "",
    experiment_name: str = "full_pool",
    analysis_base_dir: str = "data/processed_data/rev_analysis_low",
    save_full_weights: bool = False,
    rolling_windows: Optional[List[Dict[str, int]]] = None,
    random_seed: Optional[int] = None,
    train_years: Optional[List[int]] = None,
    test_years: Optional[List[int]] = None,
    random_mode: str = "pool",
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Run null benchmark by random control sampling matched to each realized pool frontier size."""
    rng = np.random.default_rng(random_seed)
    n_treated = len(similarities)
    control_idx = embeddings_df.index[embeddings_df["treated"] == 0].to_numpy()

    train_years = train_years or list(range(2000, 2011))
    test_years = test_years or list(range(2011, 2016))

    rows = []
    logger.info("\n[RANDOM] Running random donor-pool benchmark: reps=%s", random_reps)
    if effective_pool_table.empty:
        return pd.DataFrame(), pd.DataFrame()

    for _, frontier_row in effective_pool_table.sort_values('pool_size').iterrows():
        rep_k = int(frontier_row.get('representative_K', frontier_row.get('K', np.nan)))
        target_pool = int(frontier_row.get('pool_size', 0))
        if target_pool <= 0:
            continue
        target_pool = min(target_pool, len(control_idx))
        logger.info("[RANDOM] representative_K=%s target pool size=%s", rep_k, target_pool)
        for rep in range(1, random_reps + 1):
            if str(random_mode).lower() in ("per_treated", "per-treated", "per-treated-k"):
                # Sample K random controls per treated unit, then take the union — mirrors embedding K-nearest union behavior.
                per_treated_samples = []
                for t_idx in range(n_treated):
                    # allow overlap across treated samples; sample without replacement within each treated draw
                    draw_size = min(rep_k, len(control_idx))
                    per_treated_samples.append(rng.choice(control_idx, size=draw_size, replace=False))
                sampled = np.unique(np.concatenate(per_treated_samples)).astype(int)
                # If union exceeds target_pool, randomly downsample to target_pool for parity with embedding pool_size
                if len(sampled) > target_pool:
                    sampled = rng.choice(sampled, size=target_pool, replace=False)
            else:
                # Default: uniform random pool of size equal to realized embedding pool size
                sampled = rng.choice(control_idx, size=target_pool, replace=False)
            output_prefix = f"random_pool{target_pool}_k{rep_k}_rep{rep}" + (f"_{output_tag}" if output_tag else "")
            if str(random_mode).lower() != "pool":
                output_prefix = output_prefix + f"_mode{str(random_mode)}"
            result = run_cbps_crossval(
                embeddings_df=embeddings_df,
                selected_controls=set(int(x) for x in sampled.tolist()),
                year=year,
                output_prefix=output_prefix,
                train_years=train_years,
                test_years=test_years,
                experiment_name=experiment_name,
                analysis_base_dir=analysis_base_dir,
                save_full_weights=save_full_weights,
                rolling_windows=rolling_windows,
            )
            rows.append({
                "method": "random",
                "K": int(rep_k),
                "representative_K": int(rep_k),
                "rep": int(rep),
                "pool_size": int(target_pool),
                "pool_prop_full": float(target_pool / max(1, len(control_idx))),
                "coverage_ratio": float(target_pool / max(1, n_treated)),
                "rmse": float(result.get("rmse", np.nan)),
                "rmse_train": float(result.get("rmse_train", np.nan)),
                "median_RMSE": float(result.get("median_RMSE", np.nan)),
                "p90_RMSE": float(result.get("p90_RMSE", np.nan)),
                "max_RMSE": float(result.get("max_RMSE", np.nan)),
                "ess_control": float(result.get("ess_control", np.nan)),
                "max_balance_std": float(result.get("max_balance_std", np.nan)),
                "top10_share": float(result.get("top10_share", np.nan)),
                "max_weight_share": float(result.get("max_weight_share", np.nan)),
            })

    rep_df = pd.DataFrame(rows)
    if rep_df.empty:
        return rep_df, rep_df

    summary = (
        rep_df.groupby(["pool_size", "representative_K"], as_index=False)
        .agg(
            pool_prop_full=("pool_prop_full", "median"),
            coverage_ratio=("coverage_ratio", "median"),
            median_RMSE=("median_RMSE", "median"),
            p90_RMSE=("p90_RMSE", "median"),
            median_ess_control=("ess_control", "median"),
            median_max_smd=("max_balance_std", "median"),
            median_top10_share=("top10_share", "median"),
            median_max_weight_share=("max_weight_share", "median"),
            reps=("rep", "count"),
        )
        .sort_values(["pool_size", "representative_K"])
        .reset_index(drop=True)
    )
    summary["method"] = "random"
    return rep_df, summary


def compute_elbow_metrics(similarities: Dict[int, np.ndarray], 
                          K_candidates: List[int]) -> pd.DataFrame:
    """
    Compute mean similarity for each K (elbow plot data)
    Returns:
        DataFrame with K, mean_similarity, std_similarity, min_similarity
        Used to filter K candidates by similarity drop-off (elbow method)
    """
    logger.info("\nStep 1: Computing elbow metrics...")
    results = []
    for K in K_candidates:
        mean_sims = []
        min_sims = []
        for t_idx, sims in similarities.items():
            top_k_sims = sims[:K, 1]
            mean_sims.append(np.mean(top_k_sims))
            min_sims.append(np.min(top_k_sims))
        results.append({
            'K': K,
            'mean_similarity': np.mean(mean_sims),
            'std_similarity': np.std(mean_sims),
            'min_similarity': np.min(min_sims)
        })
        logger.info(f"  K={K}: mean_sim={np.mean(mean_sims):.4f}, "
                   f"min_sim={np.min(min_sims):.4f}")
    
    return pd.DataFrame(results)


def filter_by_elbow(elbow_df: pd.DataFrame, 
                    drop_threshold: float = 0.02) -> List[int]:
    """
    Filter K candidates by elbow method - drop K where similarity drops sharply
    
    Args:
        elbow_df: DataFrame from compute_elbow_metrics with K, mean_similarity
        drop_threshold: Trigger elbow if similarity drops by more than this (default 0.02)
    
    Returns:
        List of K values before the elbow (where similarity drop is gradual)
    
    Logic:
        - As K increases, mean similarity DECREASES (adding less similar controls)
        - Compute marginal change: Δsim = sim[K+1] - sim[K] (will be negative)
        - If Δsim < -threshold (sharp drop), trigger elbow BEFORE this K
        - Example: K=[10,20,30,50], sim=[0.92,0.915,0.911,0.85]
          → Δsim=[-0.005,-0.004,-0.061] → Elbow at K=50 (Δsim=-0.061 < -0.02)
          → Keep K=[10,20,30] (gradual decrease), drop K=50+ (sharp drop)
    """
    logger.info(f"\nStep 1b: Knee detection (replacing fragile elbow heuristic)...")
    elbow_df = elbow_df.sort_values('K')
    similarities = elbow_df['mean_similarity'].values.astype(float)
    K_values = elbow_df['K'].values
    # If too few points, keep all
    if len(K_values) < 3:
        logger.info("  Too few K candidates for knee detection - keeping all")
        return K_values.tolist()
    # Normalize similarities to [0,1]
    sim_min, sim_max = similarities.min(), similarities.max()
    if sim_max - sim_min == 0:
        logger.info("  Similarities constant - keeping all K candidates")
        return K_values.tolist()
    sims_norm = (similarities - sim_min) / (sim_max - sim_min)
    # Line from first to last point
    x = np.arange(len(sims_norm)).astype(float)
    x0, y0 = x[0], sims_norm[0]
    x1, y1 = x[-1], sims_norm[-1]
    # Perpendicular distance from each point to the line
    denom = np.hypot(x1 - x0, y1 - y0)
    if denom == 0:
        logger.info("  Degenerate end points for knee detection - keeping all")
        return K_values.tolist()
    distances = np.abs((y1 - y0) * x - (x1 - x0) * sims_norm + x1 * y0 - y1 * x0) / denom
    knee_idx = int(np.argmax(distances))
    logger.info(f"  Knee detected at index {knee_idx} (K={K_values[knee_idx]})")
    # Keep K values up to and including knee index
    filtered_K = K_values[: (knee_idx + 1)].tolist()
    logger.info(f"  Kept K values: {filtered_K}")
    return filtered_K


def build_effective_pool_table(
    rmse_df: pd.DataFrame,
    full_control_pool: int,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Collapse K-level results into a frontier keyed by realized pool_size."""
    if rmse_df.empty:
        return pd.DataFrame(), pd.DataFrame()

    d = rmse_df.copy()
    d = d[d['pool_size'].notna()].copy()
    if d.empty:
        return pd.DataFrame(), pd.DataFrame()

    d['pool_size'] = pd.to_numeric(d['pool_size'], errors='coerce')
    d = d[d['pool_size'].notna()].copy()
    d['pool_size'] = d['pool_size'].astype(int)
    d['K'] = pd.to_numeric(d['K'], errors='coerce').astype(int)

    metric_cols = [
        'rmse', 'rmse_train', 'median_RMSE', 'p90_RMSE', 'max_RMSE',
        'max_balance_std', 'mean_balance_std',
        'ess_control', 'ess_ratio',
        'top10_share', 'max_weight_share',
        'support_similarity_min', 'support_similarity_p10', 'support_similarity_median',
    ]
    metric_cols = [c for c in metric_cols if c in d.columns]

    group_rows = []
    sanity_rows = []
    for pool_size, grp in d.groupby('pool_size', sort=True):
        grp_sorted = grp.sort_values('K').copy()
        rep = grp_sorted.iloc[0].copy()
        rep['representative_K'] = int(rep['K'])
        rep['k_values'] = ','.join(str(int(x)) for x in grp_sorted['K'].tolist())
        rep['k_equivalent_count'] = int(len(grp_sorted))
        rep['compression_ratio'] = float(full_control_pool / max(1, int(pool_size)))
        group_rows.append(rep)

        sanity = {
            'pool_size': int(pool_size),
            'representative_K': int(rep['K']),
            'k_equivalent_count': int(len(grp_sorted)),
            'k_values': rep['k_values'],
        }
        for col in metric_cols:
            vals = pd.to_numeric(grp_sorted[col], errors='coerce').dropna()
            spread = float(vals.max() - vals.min()) if not vals.empty else np.nan
            sanity[f'{col}_spread'] = spread
        sanity_rows.append(sanity)

    effective_pool_table = pd.DataFrame(group_rows)
    effective_pool_table = effective_pool_table.sort_values(['pool_size', 'representative_K']).reset_index(drop=True)

    ordered_cols = [
        'pool_size',
        'pool_prop_full',
        'coverage_ratio',
        'compression_ratio',
        'representative_K',
        'k_values',
        'k_equivalent_count',
        'rmse',
        'rmse_train',
        'median_RMSE',
        'p90_RMSE',
        'max_RMSE',
        'max_balance_std',
        'mean_balance_std',
        'ess_control',
        'ess_ratio',
        'top10_share',
        'max_weight_share',
        'support_similarity_min',
        'support_similarity_p10',
        'support_similarity_median',
    ]
    existing_ordered = [c for c in ordered_cols if c in effective_pool_table.columns]
    remaining = [c for c in effective_pool_table.columns if c not in existing_ordered]
    effective_pool_table = effective_pool_table[existing_ordered + remaining]

    sanity_df = pd.DataFrame(sanity_rows).sort_values('pool_size').reset_index(drop=True)
    return effective_pool_table, sanity_df


def build_pool_overlap_diagnostics(
    similarities: Dict[int, np.ndarray],
    effective_pool_table: pd.DataFrame,
    year: int,
) -> pd.DataFrame:
    """Stage 5: adjacent frontier Jaccard overlap diagnostics."""
    if effective_pool_table.empty:
        return pd.DataFrame(columns=['year', 'pool_size', 'next_pool_size', 'jaccard_overlap'])

    frontier = effective_pool_table.sort_values('pool_size').copy()
    if 'representative_K' not in frontier.columns:
        return pd.DataFrame(columns=['year', 'pool_size', 'next_pool_size', 'jaccard_overlap'])

    rep_k = pd.to_numeric(frontier['representative_K'], errors='coerce').dropna().astype(int).tolist()
    if len(rep_k) < 2:
        return pd.DataFrame(columns=['year', 'pool_size', 'next_pool_size', 'jaccard_overlap'])

    pools = []
    sizes = []
    for _, row in frontier.iterrows():
        k_val = int(row['representative_K'])
        pools.append(get_k_nearest_union(similarities, k_val))
        sizes.append(int(row['pool_size']))

    rows = []
    for i in range(len(pools) - 1):
        a = pools[i]
        b = pools[i + 1]
        union_size = len(a.union(b))
        jaccard = float(len(a.intersection(b)) / union_size) if union_size > 0 else np.nan
        rows.append({
            'year': int(year),
            'pool_size': int(sizes[i]),
            'next_pool_size': int(sizes[i + 1]),
            'jaccard_overlap': jaccard,
        })

    overlap_df = pd.DataFrame(rows)
    if not overlap_df.empty and (overlap_df['jaccard_overlap'] > 0.98).all():
        logger.warning("Donor pools nearly identical across frontier - metrics may appear flat.")
    return overlap_df


def _warn_if_metrics_invariant(effective_pool_df: pd.DataFrame) -> None:
    """Stage 6: emit warning when key metrics are invariant across the frontier."""
    if effective_pool_df.empty:
        return

    rmse_col = 'median_RMSE' if ('median_RMSE' in effective_pool_df.columns and effective_pool_df['median_RMSE'].notna().any()) else 'rmse'
    std_rmse = float(pd.to_numeric(effective_pool_df.get(rmse_col, np.nan), errors='coerce').std(ddof=0))
    std_ess = float(pd.to_numeric(effective_pool_df.get('ess_control', np.nan), errors='coerce').std(ddof=0))
    _ = float(pd.to_numeric(effective_pool_df.get('top10_share', np.nan), errors='coerce').std(ddof=0))
    _ = float(pd.to_numeric(effective_pool_df.get('max_weight_share', np.nan), errors='coerce').std(ddof=0))

    if np.isfinite(std_rmse) and np.isfinite(std_ess) and std_rmse < 1e-4 and std_ess < 1.0:
        logger.warning(
            "Evaluation metrics invariant across donor pools. Possible saturation or low outcome signal."
        )


# REMOVED: split_pretreatment_data - unused placeholder function
# The R script (run_cbps_with_selected_controls.R) handles train/test split internally


def run_cbps_crossval(embeddings_df: pd.DataFrame,
                      selected_controls: Set[int],
                      year: int,
                      output_prefix: str,
                      train_years: List[int],
                      test_years: List[int],
                      experiment_name: str = "full_pool",
                      analysis_base_dir: str = "data/processed_data/rev_analysis_low",
                      save_full_weights: bool = False,
                      rolling_windows: Optional[List[Dict[str, int]]] = None,
                      persist_artifacts: bool = False) -> Dict:
    """
    Run CBPS with cross-validation to compute RMSPE
    
    === INTEGRATION MECHANISM ===
    This function bridges Python (embedding selection) with R (CBPS estimation):
    
    1. Extract unit IDs for selected control pixels (via embeddings_df indices)
    2. Write unit IDs to temporary CSV
    3. Call run_cbps_with_selected_controls.R with arguments:
       - year: Treatment year (e.g., 2019)
       - selected_units_csv: Path to temp CSV with control unit IDs
       - output_prefix: File prefix (e.g., "k10", "k50")
       - train_start, train_end: Training period (e.g., 2000-2010)
       - test_start, test_end: Test period (e.g., 2011-2015)
    
    4. R script execution:
       a. Loads analysis_treated{year}_conifer.RDS (full covariate data)
       b. Filters to treated=1 OR unit IN selected_units (embedding-filtered pool)
       c. Applies transformations (two-part SWE, log+winsorize, drop sparse)
       d. Runs CBPS with regularization grid (same as baseline)
       e. Computes weighted fire frequency for train/test periods
       f. Calculates RMSE: sqrt(mean((treated_freq - control_freq)^2))
       g. Saves metrics to cbps_integration/{year}/cbps_metrics_{prefix}_{year}.csv
    
    5. Python reads metrics CSV and returns RMSE + balance diagnostics
    
    === CRITICAL CONSISTENCY ===
    run_cbps_with_selected_controls.R MUST replicate implement_cbps.R logic exactly,
    except for control pool filtering. Any transformation discrepancy invalidates
    the baseline vs embedding comparison.
    
    Args:
        embeddings_df: DataFrame with columns [unit, treated, band_0, ..., band_11]
        selected_controls: Set of control pixel row indices (after filtering by K)
        year: Treatment year (e.g., 2019)
        output_prefix: Prefix for output files (e.g., "k10" for K=10 nearest)
        train_years: Years for training CBPS weights (e.g., [2000,...,2010])
        test_years: Years for testing pre-treatment fit (e.g., [2011,...,2015])
    
    Returns:
        Dictionary with:
          - rmse: Test period RMSE (cross-validation error)
          - rmse_train: Training period RMSE (in-sample fit)
          - max_balance_std: Max standardized mean difference across covariates
          - mean_balance_std: Mean standardized mean difference
          - convergence: CBPS convergence status (0=success)
          - n_controls_used: Actual control pool size after filtering
    
    Raises:
        RuntimeError: If R script fails (non-zero exit code)
        FileNotFoundError: If metrics CSV not created by R
    """
    
    # Validate selected control indices before any IO/CBPS calls.
    if not selected_controls:
        raise ValueError("selected_controls is empty")
    idx_arr = np.array(list(selected_controls), dtype=int)
    if (idx_arr < 0).any() or (idx_arr >= len(embeddings_df)).any():
        raise ValueError("selected_controls contains out-of-range dataframe indices")
    treated_flags = embeddings_df.loc[idx_arr, 'treated'].astype(int)
    if (treated_flags == 1).any():
        raise ValueError("selected_controls contains treated units")

    # Get unit IDs for selected controls
    # Note: selected_controls contains DataFrame indices (which are 0-based after reset_index)
    selected_units = embeddings_df.loc[list(selected_controls), 'unit'].tolist()
    
    # Ensure output directory exists for R outputs.
    output_dir = CBPS_INTEGRATION_DIR / str(year)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Create temporary CSV with selected units
    with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False) as f:
        temp_csv = f.name
        pd.DataFrame({'unit': selected_units}).to_csv(temp_csv, index=False)
    windows_json_path = None
    try:
        # Call R script
        r_script = "Embeddings/scripts/04_run_cbps_with_selected_controls.R"
        # This module lives under Embeddings/scripts/utils, so Capstone root is four levels up.
        capstone_root = Path(__file__).resolve().parent.parent.parent.parent
        abs_r_script = capstone_root / r_script
        logger.info(f"    [DEBUG] Current working directory for R call: {capstone_root.resolve()}")
        logger.info(f"    [DEBUG] R script relative path: {r_script}")
        logger.info(f"    [DEBUG] R script absolute path: {abs_r_script.resolve()}")
        if not abs_r_script.exists():
            logger.error(f"    [ERROR] R script does NOT exist at: {abs_r_script.resolve()}")
        else:
            logger.info(f"    [DEBUG] R script found at: {abs_r_script.resolve()}")
        cmd = [
            "Rscript",
            r_script,
            str(year),
            temp_csv,
            output_prefix,
            str(train_years[0]),
            str(train_years[-1]),
            str(test_years[0]),
            str(test_years[-1]),
            "--experiment-name", experiment_name,
            "--output-experiment-name", "",
            "--analysis-base-dir", analysis_base_dir,
            "--save-full-weights", "true" if save_full_weights else "false",
        ]
        if rolling_windows:
            with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as wf:
                windows_json_path = wf.name
                json.dump(rolling_windows, wf)
            cmd.extend(["--rolling-windows-json", windows_json_path])
        match_k = re.match(r"^k(\d+)", output_prefix)
        if match_k:
            cmd.extend(["--embedding-k", match_k.group(1)])
        # R runner enforces diagnostics and will raise on degenerate weights
        logger.info(f"    Calling R CBPS: {' '.join(cmd)}")
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=capstone_root  # Run from Capstone root
        )
        if result.returncode != 0:
            debug_dir = CBPS_INTEGRATION_DIR / str(year) / "debug_failures"
            debug_dir.mkdir(parents=True, exist_ok=True)
            stamp = datetime.utcnow().strftime("%Y%m%dT%H%M%SZ")
            debug_prefix = f"{output_prefix}_{stamp}"

            debug_selected_csv = debug_dir / f"selected_units_{debug_prefix}.csv"
            debug_windows_json = debug_dir / f"rolling_windows_{debug_prefix}.json"
            debug_stdout = debug_dir / f"stdout_{debug_prefix}.log"
            debug_stderr = debug_dir / f"stderr_{debug_prefix}.log"
            debug_cmd = debug_dir / f"rerun_command_{debug_prefix}.txt"

            try:
                shutil.copyfile(temp_csv, debug_selected_csv)
                if windows_json_path is not None and Path(windows_json_path).exists():
                    shutil.copyfile(windows_json_path, debug_windows_json)

                debug_stdout.write_text(result.stdout or "", encoding="utf-8")
                debug_stderr.write_text(result.stderr or "", encoding="utf-8")

                rerun_cmd = [
                    "Rscript",
                    r_script,
                    str(year),
                    str(debug_selected_csv),
                    output_prefix,
                    str(train_years[0]),
                    str(train_years[-1]),
                    str(test_years[0]),
                    str(test_years[-1]),
                    "--experiment-name", experiment_name,
                    "--output-experiment-name", "",
                    "--analysis-base-dir", analysis_base_dir,
                    "--save-full-weights", "true" if save_full_weights else "false",
                ]
                if windows_json_path is not None and debug_windows_json.exists():
                    rerun_cmd.extend(["--rolling-windows-json", str(debug_windows_json)])
                match_k = re.match(r"^k(\d+)", output_prefix)
                if match_k:
                    rerun_cmd.extend(["--embedding-k", match_k.group(1)])

                debug_cmd.write_text(shlex.join(rerun_cmd), encoding="utf-8")
            except Exception as debug_err:
                logger.warning("Failed to persist CBPS debug artifacts for K run %s: %s", output_prefix, debug_err)

            logger.error(f"R script failed with return code {result.returncode}")
            logger.error(f"STDOUT: {result.stdout}")
            logger.error(f"STDERR: {result.stderr}")
            raise RuntimeError(
                "R CBPS script failed. "
                f"Persistent debug artifacts written to: {debug_dir}. "
                f"Rerun command file: {debug_cmd}. "
                f"Original error: {result.stderr or result.stdout}"
            )
        # Parse R output
        # Look in year-specific subdirectory
        metrics_file = CBPS_INTEGRATION_DIR / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
        if not metrics_file.exists():
            raise FileNotFoundError(f"CBPS metrics file not created: {metrics_file}")
        metrics = pd.read_csv(metrics_file)
        # Validate required columns
        required_cols = ['rmse_test', 'rmse_train', 'max_balance_std', 
                        'mean_balance_std', 'converged', 'n_control']
        missing_cols = [col for col in required_cols if col not in metrics.columns]
        if missing_cols:
            raise ValueError(f"Metrics file missing required columns: {missing_cols}")
        _write_cache_meta(
            year=year,
            output_prefix=output_prefix,
            train_years=train_years,
            test_years=test_years,
            rolling_windows=rolling_windows,
        )
        out = {
            'rmse': float(metrics['rmse_test'].iloc[0]),
            'rmse_train': float(metrics['rmse_train'].iloc[0]),
            'median_RMSE': float(metrics['median_rmse_test'].iloc[0]) if 'median_rmse_test' in metrics.columns else np.nan,
            'p90_RMSE': float(metrics['p90_rmse_test'].iloc[0]) if 'p90_rmse_test' in metrics.columns else np.nan,
            'max_RMSE': float(metrics['max_rmse_test'].iloc[0]) if 'max_rmse_test' in metrics.columns else np.nan,
            'max_balance_std': float(metrics['max_balance_std'].iloc[0]),
            'mean_balance_std': float(metrics['mean_balance_std'].iloc[0]),
            'ess_control': float(metrics['ess_control'].iloc[0]) if 'ess_control' in metrics.columns else np.nan,
            'ess_ratio': float(metrics['ess_ratio'].iloc[0]) if 'ess_ratio' in metrics.columns else np.nan,
            'top10_share': float(metrics['top10_share'].iloc[0]) if 'top10_share' in metrics.columns else np.nan,
            'max_weight_share': float(metrics['max_weight_share'].iloc[0]) if 'max_weight_share' in metrics.columns else np.nan,
            'runtime_seconds': float(metrics['runtime_seconds'].iloc[0]) if 'runtime_seconds' in metrics.columns else np.nan,
            'convergence': int(metrics['converged'].iloc[0]),
            'n_controls_used': int(metrics['n_control'].iloc[0])
        }
        if not persist_artifacts:
            _cleanup_cbps_artifacts(year=year, output_prefix=output_prefix)
        return out
    finally:
        # Clean up temporary file
        Path(temp_csv).unlink(missing_ok=True)
        if windows_json_path is not None:
            Path(windows_json_path).unlink(missing_ok=True)


def compute_k_value(K: int, similarities: Dict[int, np.ndarray], 
                    embeddings_df: pd.DataFrame, year: int,
                    train_years: List[int], test_years: List[int], output_tag: str = "",
                    experiment_name: str = "full_pool",
                    analysis_base_dir: str = "data/processed_data/rev_analysis_low",
                    save_full_weights: bool = False,
                    rolling_windows: Optional[List[Dict[str, int]]] = None,
                    n_controls_full: Optional[int] = None) -> Dict:
    """
    Worker function to compute CBPS metrics for a specific K value
    Designed for parallel execution via ThreadPoolExecutor
    
    Args:
        K: Number of nearest neighbors
        similarities: Pre-computed similarity matrix
        embeddings_df: DataFrame with embeddings and treatment labels
        year: Treatment year
        train_years: Training period years
        test_years: Test period years
    
    Returns:
        Dictionary with results and success status
    """
    try:
        # Get K-nearest controls
        selected_controls = get_k_nearest_union(similarities, K)
        n_controls_full = int(n_controls_full if n_controls_full is not None else (embeddings_df['treated'] == 0).sum())
        pool_info = get_k_pool_diagnostics(
            similarities,
            K,
            n_treated=len(similarities),
            n_controls_full=n_controls_full,
        )
        output_prefix = f"k{K}" + (f"_{output_tag}" if output_tag else "")
        
        # Run CBPS cross-validation
        result = run_cbps_crossval(
            embeddings_df,
            selected_controls,
            year=year,
            output_prefix=output_prefix,
            train_years=train_years,
            test_years=test_years,
            experiment_name=experiment_name,
            analysis_base_dir=analysis_base_dir,
            save_full_weights=save_full_weights,
            rolling_windows=rolling_windows,
        )
        
        
        # Return success result
        donor_hash = compute_donor_hash_for_k(similarities, embeddings_df, K)
        return {
            'K': K,
            'pool_size': int(pool_info['pool_size']),
            'pool_prop_full': float(pool_info['pool_prop_full']),
            'coverage_ratio': float(pool_info['coverage_ratio']),
            'support_similarity_mean': float(pool_info['support_similarity_mean']),
            'support_similarity_min': float(pool_info['support_similarity_min']),
            'support_similarity_p10': float(pool_info['support_similarity_p10']),
            'support_similarity_median': float(pool_info['support_similarity_median']),
            'rmse': result['rmse'],
            'rmse_train': result['rmse_train'],
            'median_RMSE': result.get('median_RMSE', np.nan),
            'p90_RMSE': result.get('p90_RMSE', np.nan),
            'max_RMSE': result.get('max_RMSE', np.nan),
            'max_balance_std': result['max_balance_std'],
            'mean_balance_std': result['mean_balance_std'],
            'ess_control': result.get('ess_control', np.nan),
            'ess_ratio': result.get('ess_ratio', np.nan),
            'top10_share': result.get('top10_share', np.nan),
            'max_weight_share': result.get('max_weight_share', np.nan),
            'donor_hash': donor_hash,
            'runtime_seconds': result.get('runtime_seconds', np.nan),
            'convergence': result['convergence'],
            'n_controls_used': result['n_controls_used'],
            'success': True,
            'error': None
        }
    except Exception as e:
        # Return failure result with error message
        return {
            'K': K,
            'success': False,
            'error': str(e)
        }


def select_optimal_k(similarities: Dict[int, np.ndarray],
                    embeddings_df: pd.DataFrame,
                    K_candidates: List[int],
                    year: int,
                    min_ratio: int = 10,
                    max_control_ratio: Optional[float] = 20.0,
                    phase2_policy: str = "pool_then_k",
                    force_recompute: bool = False,
                    max_workers: int = 6,
                    output_tag: str = "",
                    experiment_name: str = "full_pool",
                    analysis_base_dir: str = "data/processed_data/rev_analysis_low",
                    save_full_weights: bool = False,
                    gates: Optional[Dict[str, float]] = None,
                    rolling_windows: Optional[List[Dict[str, int]]] = None,
                    random_baseline_reps: int = 100,
                    random_seed: Optional[int] = None,
                    random_mode: str = "pool",
                    train_years: Optional[List[int]] = None,
                    test_years: Optional[List[int]] = None,
                    keep_per_k_artifacts: bool = False) -> Dict:
    """
    Complete K selection pipeline
    Args:
        similarities: Pre-computed similarity matrix
        embeddings_df: DataFrame with embeddings and treatment labels
        K_candidates: List of K values to test
        year: Treatment year (e.g., 2019)
        min_ratio: Minimum control pool size as multiple of treated count
        force_recompute: Force recomputation of CBPS (ignore cache)
        max_workers: Maximum number of parallel workers (default: 6)
    Returns:
        Dictionary with optimal K and all diagnostics
    """
    n_treated = len(similarities)
    n_controls = (embeddings_df['treated'] == 0).sum()
    logger.info(f"\n{'='*80}")
    logger.info("OPTIMAL K SELECTION PIPELINE")
    logger.info(f"{'='*80}")
    logger.info(f"Treated pixels: {n_treated}")
    logger.info(f"Control pool: {n_controls}")
    logger.info(f"K candidates: {K_candidates}")
    logger.info(f"Min control ratio: {min_ratio}× treated")
    if max_control_ratio is not None and float(max_control_ratio) > 0:
        logger.info(f"Max control ratio: {float(max_control_ratio):.2f}× treated")
    logger.info(f"Parallelization: {max_workers} workers")

    train_years = train_years or list(range(2000, 2011))
    test_years = test_years or list(range(2011, 2016))
    logger.info("Train years: %s-%s | Test years: %s-%s", train_years[0], train_years[-1], test_years[0], test_years[-1])
    if rolling_windows:
        logger.info("Evaluation mode: rolling windows (%s windows)", len(rolling_windows))
    else:
        logger.info("Evaluation mode: single fixed split")

    max_k_possible = min([arr.shape[0] for arr in similarities.values()])

    # Step 1: discover all reachable effective pool sizes via dense K scan.
    discovery_k = build_discovery_k_grid(max_k_possible)
    logger.info("Pool discovery scan size: %s K values", len(discovery_k))

    pool_discovery_df = build_pool_diagnostics_table(
        similarities,
        discovery_k,
        n_treated=n_treated,
        n_controls_full=n_controls,
    )
    _validate_k_candidates(discovery_k, max_k_possible=max_k_possible)
    _validate_pool_diagnostics(pool_discovery_df, n_controls_full=n_controls)

    elbow_df = compute_elbow_metrics(similarities, discovery_k)

    # Step 2: build experiment grid directly on unique effective pool sizes.
    min_controls_required = int(min_ratio * n_treated)
    max_controls_allowed = None
    if max_control_ratio is not None and float(max_control_ratio) > 0:
        max_controls_allowed = int(float(max_control_ratio) * n_treated)
    unique_pool_df = build_unique_pool_frontier(pool_discovery_df)
    unique_pool_df = unique_pool_df[unique_pool_df['pool_size'] >= min_controls_required].copy().reset_index(drop=True)
    if max_controls_allowed is not None:
        unique_pool_df = unique_pool_df[unique_pool_df['pool_size'] <= max_controls_allowed].copy().reset_index(drop=True)
    if unique_pool_df.empty:
        if max_controls_allowed is None:
            logger.error("No effective donor pools satisfy minimum size requirement: %s", min_controls_required)
        else:
            logger.error(
                "No effective donor pools satisfy size requirements: min=%s, max=%s",
                min_controls_required,
                max_controls_allowed,
            )
        return None
    pool_grid_df = compress_effective_pool_grid(unique_pool_df, max_points=25, target_points=16)
    valid_K = sorted(pool_grid_df['K'].astype(int).tolist())
    pool_df = pool_discovery_df[pool_discovery_df['K'].isin(valid_K)].copy().reset_index(drop=True)
    mapping_df = pool_grid_df.rename(columns={
        'pool_size': 'effective_pool_size',
        'K': 'representative_K',
    })[['effective_pool_size', 'representative_K', 'pool_prop_full', 'coverage_ratio']].copy()

    # Stage 2 + 3 warning guard on selected grid.
    initial_frontier = build_unique_pool_frontier(pool_df)
    _warn_if_pool_frontier_saturated(initial_frontier, n_controls_full=n_controls)

    pool_lookup = pool_df.set_index('K').to_dict(orient='index')
    logger.info("\nEffective-pool experiment grid contains %s points", len(valid_K))
    logger.info("Representative K values: %s", valid_K)

    if force_recompute:
        for K in valid_K:
            output_prefix = f"k{K}" + (f"_{output_tag}" if output_tag else "")
            metrics_file = CBPS_INTEGRATION_DIR / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
            if metrics_file.exists():
                logger.info(f"🗑️  Deleting cached CBPS metrics for K={K} (--force-recompute)")
                metrics_file.unlink()
            meta_file = _cache_meta_path(year, output_prefix)
            if meta_file.exists():
                meta_file.unlink()

    rmse_results: Dict[int, Dict] = {}

    def load_cached_metrics(K: int) -> Optional[Dict]:
        output_prefix = f"k{K}" + (f"_{output_tag}" if output_tag else "")
        metrics_file = CBPS_INTEGRATION_DIR / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
        if not keep_per_k_artifacts:
            return None
        if not metrics_file.exists() or force_recompute:
            return None
        if not _cache_meta_matches(
            year=year,
            output_prefix=output_prefix,
            train_years=train_years,
            test_years=test_years,
            rolling_windows=rolling_windows,
        ):
            logger.info(
                "Ignoring stale cache for K=%s due to evaluation-split mismatch (or missing metadata).",
                K,
            )
            return None
        metrics = pd.read_csv(metrics_file)
        pool_info = pool_lookup.get(K, {})
        return {
            'K': K,
            'pool_size': int(pool_info.get('pool_size', np.nan)),
            'pool_prop_full': float(pool_info.get('pool_prop_full', np.nan)),
            'coverage_ratio': float(pool_info.get('coverage_ratio', np.nan)),
            'support_similarity_mean': float(pool_info.get('support_similarity_mean', np.nan)),
            'support_similarity_min': float(pool_info.get('support_similarity_min', np.nan)),
            'support_similarity_p10': float(pool_info.get('support_similarity_p10', np.nan)),
            'support_similarity_median': float(pool_info.get('support_similarity_median', np.nan)),
            'rmse': float(metrics['rmse_test'].iloc[0]),
            'rmse_train': float(metrics['rmse_train'].iloc[0]),
            'median_RMSE': float(metrics['median_rmse_test'].iloc[0]) if 'median_rmse_test' in metrics.columns else np.nan,
            'p90_RMSE': float(metrics['p90_rmse_test'].iloc[0]) if 'p90_rmse_test' in metrics.columns else np.nan,
            'max_RMSE': float(metrics['max_rmse_test'].iloc[0]) if 'max_rmse_test' in metrics.columns else np.nan,
            'max_balance_std': float(metrics['max_balance_std'].iloc[0]),
            'mean_balance_std': float(metrics['mean_balance_std'].iloc[0]),
            'ess_control': float(metrics['ess_control'].iloc[0]) if 'ess_control' in metrics.columns else np.nan,
            'ess_ratio': float(metrics['ess_ratio'].iloc[0]) if 'ess_ratio' in metrics.columns else np.nan,
            'top10_share': float(metrics['top10_share'].iloc[0]) if 'top10_share' in metrics.columns else np.nan,
            'max_weight_share': float(metrics['max_weight_share'].iloc[0]) if 'max_weight_share' in metrics.columns else np.nan,
            'donor_hash': compute_donor_hash_for_k(similarities, embeddings_df, K),
            'runtime_seconds': float(metrics['runtime_seconds'].iloc[0]) if 'runtime_seconds' in metrics.columns else np.nan,
            'convergence': int(metrics['converged'].iloc[0]),
            'n_controls_used': int(metrics['n_control'].iloc[0])
        }

    def evaluate_k_list(k_list: List[int], stage_label: str) -> None:
        if not k_list:
            return
        K_cached = []
        K_to_compute = []
        for K in k_list:
            cached = None
            try:
                cached = load_cached_metrics(K)
            except Exception as e:
                logger.warning("[%s] Failed reading cache for K=%s: %s", stage_label, K, e)
            if cached is not None:
                K_cached.append(K)
                rmse_results[K] = cached
            else:
                K_to_compute.append(K)

        logger.info("[%s] Cache status: %s cached, %s to compute", stage_label, len(K_cached), len(K_to_compute))
        if K_cached:
            logger.info("[%s] Loaded cached: %s", stage_label, K_cached)
        if not K_to_compute:
            return

        n_workers = min(len(K_to_compute), max_workers)
        logger.info("[%s] Computing %s K values with %s workers", stage_label, len(K_to_compute), n_workers)
        compute_func = partial(
            compute_k_value,
            similarities=similarities,
            embeddings_df=embeddings_df,
            year=year,
            train_years=train_years,
            test_years=test_years,
            output_tag=output_tag,
            experiment_name=experiment_name,
            analysis_base_dir=analysis_base_dir,
            save_full_weights=save_full_weights,
            rolling_windows=rolling_windows,
            n_controls_full=n_controls,
        )
        with ThreadPoolExecutor(max_workers=n_workers) as executor:
            future_to_k = {executor.submit(compute_func, K): K for K in K_to_compute}
            completed = 0
            for future in as_completed(future_to_k):
                K = future_to_k[future]
                completed += 1
                try:
                    result = future.result()
                    if result['success']:
                        rmse_results[K] = {k: v for k, v in result.items() if k not in ['success', 'error']}
                        logger.info("[%s] K=%s: ✓ RMSE=%.4f balance=%.3f [%s/%s]",
                                    stage_label, K, result['rmse'], result['max_balance_std'], completed, len(K_to_compute))
                    else:
                        logger.error("[%s] K=%s: ✗ Failed: %s [%s/%s]",
                                     stage_label, K, result['error'], completed, len(K_to_compute))
                except Exception as e:
                    logger.error("[%s] K=%s: ✗ Exception: %s [%s/%s]",
                                 stage_label, K, e, completed, len(K_to_compute))

    evaluate_k_list(valid_K, "coarse")

    if not rmse_results:
        logger.error("\n" + "="*80)
        logger.error("ERROR: All K values failed CBPS cross-validation!")
        logger.error("="*80)
        logger.error("Possible causes:")
        logger.error("  1. R script not found or not executable")
        logger.error("  2. Missing R dependencies (CBPS, dplyr, tidyr)")
        logger.error("  3. Data quality issues in analysis_treated{year}_conifer.RDS")
        logger.error("  4. FIRMS.RDS missing or corrupted")
        logger.error("\nCheck R script output above for details.")
        return None

    rmse_df = pd.DataFrame(list(rmse_results.values()))

    # Standardized reporting aliases (pre-treatment diagnostics only).
    rmse_df['N_control_K'] = rmse_df.get('pool_size', np.nan)
    if 'median_RMSE' not in rmse_df.columns:
        rmse_df['median_RMSE'] = rmse_df.get('rmse', np.nan)
    if 'p90_RMSE' not in rmse_df.columns:
        rmse_df['p90_RMSE'] = np.nan
    if 'max_RMSE' not in rmse_df.columns:
        rmse_df['max_RMSE'] = np.nan
    rmse_df['max_abs_SMD'] = rmse_df.get('max_balance_std', np.nan)
    rmse_df['ESS_control'] = rmse_df.get('ess_control', np.nan)
    all_tested = sorted(set(valid_K + [int(k) for k in rmse_results.keys()]))
    logger.info(f"\n{'='*80}")
    logger.info(f"CBPS CROSS-VALIDATION SUMMARY")
    logger.info(f"{'='*80}")
    logger.info(f"Tested: {len(all_tested)} K values")
    logger.info(f"Succeeded: {len(rmse_results)} K values")
    logger.info(f"Failed: {len(all_tested) - len(rmse_results)} K values")
    if len(rmse_results) < len(all_tested):
        failed_K = set(all_tested) - set(rmse_df['K'].values)
        logger.warning(f"  Failed K values: {sorted(failed_K)}")
        logger.warning(f"  (Check logs above for error details)")
    logger.info("")    
    # Collapse repeated K rows that map to identical realized donor pools.
    effective_pool_df, duplicate_sanity_df = build_effective_pool_table(
        rmse_df=rmse_df,
        full_control_pool=n_controls,
    )
    if effective_pool_df.empty:
        effective_pool_df = rmse_df.copy()
        effective_pool_df['representative_K'] = effective_pool_df['K']
        duplicate_sanity_df = pd.DataFrame()

    # Step 4 + Step 7: donor-pool hash guard and phase-2 selection rule.
    hash_dupes = int(effective_pool_df['donor_hash'].duplicated().sum()) if 'donor_hash' in effective_pool_df.columns else 0
    if hash_dupes > 0:
        logger.warning("Multiple grid points correspond to identical donor pools")

    phase2 = select_phase2_pool(
        effective_pool_df,
        full_control_pool=n_controls,
        phase2_policy=phase2_policy,
    )
    optimal_K = int(phase2['selected_K'])
    optimal_pool = int(phase2['selected_pool_size'])
    selection_mode = str(phase2['selection_rule'])
    phase2_policy = str(phase2.get('phase2_policy', phase2_policy))

    selection_table = effective_pool_df.sort_values('pool_size').reset_index(drop=True)
    objective_col = 'median_RMSE' if ('median_RMSE' in selection_table.columns and selection_table['median_RMSE'].notna().any()) else 'rmse'
    rmse_series = pd.to_numeric(selection_table.get(objective_col, np.nan), errors='coerce')
    degenerate_rmse_frontier = bool(
        rmse_series.notna().any() and
        float(rmse_series.max(skipna=True) - rmse_series.min(skipna=True)) <= 1e-4
    )
    if degenerate_rmse_frontier:
        logger.warning(
            "Degenerate pre-treatment RMSE frontier detected (spread <= 1e-4 across effective pools). "
            "Selection will rely on RMSE-plateau tie-break policy plus ESS/balance guardrails."
        )
    overlap_df = build_pool_overlap_diagnostics(
        similarities=similarities,
        effective_pool_table=selection_table,
        year=year,
    )
    _warn_if_metrics_invariant(selection_table)
    optimal_rmse = float(
        effective_pool_df.loc[
            effective_pool_df['representative_K'] == optimal_K,
            objective_col,
        ].iloc[0]
    )
    logger.info(f"\n{'='*80}")
    logger.info(f"OPTIMAL POOL SELECTED: pool_size={optimal_pool} (representative K={optimal_K})")
    logger.info(f"Selection mode: {selection_mode}")
    logger.info(f"Phase-2 policy: {phase2_policy}")
    logger.info(f"Pre-treatment RMSE: {optimal_rmse:.4f}")
    logger.info(f"Control pool size: {optimal_pool}")
    logger.info(f"{'='*80}\n")    

    # Persist one canonical selected-controls file for downstream phase-2 command wiring.
    optimal_prefix = f"k{optimal_K}" + (f"_{output_tag}" if output_tag else "")
    optimal_selected_controls = get_k_nearest_union(similarities, optimal_K)
    optimal_units = embeddings_df.loc[list(optimal_selected_controls), 'unit'].astype(str).tolist()
    optimal_controls_path = CBPS_INTEGRATION_DIR / str(year) / f"selected_controls_{optimal_prefix}_{year}.csv"
    optimal_controls_path.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame({'unit': sorted(set(optimal_units))}).to_csv(optimal_controls_path, index=False)
    logger.info("Saved canonical selected-controls file: %s", optimal_controls_path)
    random_rep_df = pd.DataFrame()
    random_summary_df = pd.DataFrame()
    if random_baseline_reps > 0:
        try:
            random_rep_df, random_summary_df = run_random_pool_experiment(
                similarities=similarities,
                embeddings_df=embeddings_df,
                effective_pool_table=effective_pool_df,
                year=year,
                random_reps=random_baseline_reps,
                output_tag=output_tag,
                experiment_name=experiment_name,
                analysis_base_dir=analysis_base_dir,
                save_full_weights=save_full_weights,
                rolling_windows=rolling_windows,
                random_seed=random_seed,
                random_mode=random_mode,
                train_years=train_years,
                test_years=test_years,
            )
        except Exception as e:
            logger.warning("Random baseline experiment failed: %s", e)

    return {
        'optimal_K': int(optimal_K),
        'selected_pool_size': int(optimal_pool),
        'optimal_rmse': float(optimal_rmse),
        'selection_mode': selection_mode,
        'selection_rule': selection_mode,
        'phase2_policy': phase2_policy,
        'optimal_selected_controls_path': str(optimal_controls_path),
        'elbow_metrics': elbow_df,
        'pool_discovery_scan': pool_discovery_df.sort_values('K').reset_index(drop=True),
        'pool_size_grid': mapping_df.sort_values('effective_pool_size').reset_index(drop=True),
        'rmse_results': rmse_df.sort_values('K').reset_index(drop=True),
        'effective_pool_table': selection_table,
        'effective_pool_duplicates': duplicate_sanity_df,
        'valid_K_values': all_tested,
        'all_similarities': similarities,
        'pool_diagnostics': pool_df,
        'pool_target_mapping': mapping_df,
        'pool_overlap_diagnostics': overlap_df,
        'random_pool_replicates': random_rep_df,
        'random_pool_summary': random_summary_df,
        'diagnostics': {
            'n_treated': int(n_treated),
            'full_control_pool': int(n_controls),
            'n_k_evaluated': int(len(rmse_df)),
            'n_effective_pool_rows': int(len(selection_table)),
            'n_duplicate_pool_rows_collapsed': int(max(0, len(rmse_df) - len(selection_table))),
            'selection_purpose': 'phase2_comparison',
            'phase2_policy': phase2_policy,
            'duplicate_donor_hash_rows': int(hash_dupes),
            'min_controls_required': int(min_controls_required),
            'max_controls_allowed': int(max_controls_allowed) if max_controls_allowed is not None else None,
            'degenerate_rmse_frontier': bool(degenerate_rmse_frontier),
        },
        'plots': {},
    }
