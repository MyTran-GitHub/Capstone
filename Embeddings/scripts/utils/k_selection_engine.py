"""Core K-selection evaluation engine extracted from CLI orchestration script."""

import json
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


def map_candidates_to_target_proportions(
    pool_df: pd.DataFrame,
    target_pool_proportions: List[float],
) -> Tuple[List[int], pd.DataFrame]:
    """Map target donor-pool proportions to nearest realized K points."""
    if pool_df.empty:
        return [], pd.DataFrame()

    unique_targets = sorted(set(float(x) for x in target_pool_proportions if x > 0))
    mapped_rows = []
    for target in unique_targets:
        idx = (pool_df["pool_prop_full"] - target).abs().idxmin()
        row = pool_df.loc[idx].copy()
        row["target_pool_prop_full"] = target
        row["target_abs_error"] = abs(float(row["pool_prop_full"]) - target)
        mapped_rows.append(row)

    mapping_df = pd.DataFrame(mapped_rows)
    if mapping_df.empty:
        return [], mapping_df

    mapping_df = mapping_df.sort_values(["target_pool_prop_full", "K"]).reset_index(drop=True)
    mapped_k = sorted(set(mapping_df["K"].astype(int).tolist()))
    return mapped_k, mapping_df


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


def adaptive_k_search(best_k: int, max_k_possible: int) -> List[int]:
    """Generate local refinement K candidates around a preliminary best K."""
    seeds = [0.67, 0.80, 0.90, 1.10, 1.25, 1.50]
    additive = [-80, -40, -20, 20, 40, 80]
    candidates = set()
    for mult in seeds:
        k = int(round(best_k * mult))
        if 1 <= k <= max_k_possible:
            candidates.add(k)
    for delta in additive:
        k = int(best_k + delta)
        if 1 <= k <= max_k_possible:
            candidates.add(k)
    candidates.discard(int(best_k))
    return sorted(candidates)


def write_pipeline_commands(
    output_dir: Path,
    year: int,
    optimal_k: int,
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
                f"k{optimal_k} --experiment-name {experiment_name}"
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
    K_values: List[int],
    year: int,
    pool_lookup: Dict[int, Dict[str, float]],
    random_reps: int = 20,
    output_tag: str = "",
    experiment_name: str = "full_pool",
    analysis_base_dir: str = "data/processed_data/rev_analysis_low",
    save_full_weights: bool = False,
    rolling_windows: Optional[List[Dict[str, int]]] = None,
    random_seed: Optional[int] = None,
    train_years: Optional[List[int]] = None,
    test_years: Optional[List[int]] = None,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Run null benchmark by random control pool sampling matched to realized embedding pool size."""
    rng = np.random.default_rng(random_seed)
    n_treated = len(similarities)
    control_idx = embeddings_df.index[embeddings_df["treated"] == 0].to_numpy()

    train_years = train_years or list(range(2000, 2011))
    test_years = test_years or list(range(2011, 2016))

    rows = []
    logger.info("\n[RANDOM] Running random donor-pool benchmark: reps=%s", random_reps)
    for K in K_values:
        target_pool = int(pool_lookup.get(K, {}).get("pool_size", 0))
        if target_pool <= 0:
            continue
        target_pool = min(target_pool, len(control_idx))
        logger.info("[RANDOM] K=%s target pool size=%s", K, target_pool)
        for rep in range(1, random_reps + 1):
            sampled = rng.choice(control_idx, size=target_pool, replace=False)
            output_prefix = f"random_k{K}_rep{rep}" + (f"_{output_tag}" if output_tag else "")
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
                "K": int(K),
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
        rep_df.groupby("K", as_index=False)
        .agg(
            pool_size=("pool_size", "median"),
            pool_prop_full=("pool_prop_full", "median"),
            median_RMSE=("median_RMSE", "median"),
            p90_RMSE=("p90_RMSE", "median"),
            median_ess_control=("ess_control", "median"),
            median_max_smd=("max_balance_std", "median"),
            median_top10_share=("top10_share", "median"),
            median_max_weight_share=("max_weight_share", "median"),
            reps=("rep", "count"),
        )
        .sort_values("K")
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


def check_pool_sizes(pool_df: pd.DataFrame,
                     n_treated: int,
                     min_ratio: int = 10) -> List[int]:
    """
    Check which K values produce large enough control pools
    Args:
        min_ratio: minimum controls = min_ratio × n_treated
    Returns:
        List of valid K values
    """
    logger.info(f"\nStep 2: Checking pool sizes (min required: {min_ratio} × {n_treated} = {min_ratio * n_treated})...")
    min_controls_required = min_ratio * n_treated
    valid_K = []
    for _, row in pool_df.sort_values("K").iterrows():
        K = int(row["K"])
        pool_size = int(row["pool_size"])
        reduction_pct = 100 * (1 - float(row["pool_prop_full"]))
        is_valid = pool_size >= min_controls_required
        status = "✓ VALID" if is_valid else "✗ TOO SMALL"
        logger.info(f"  K={K}: {pool_size} unique controls ({reduction_pct:.1f}% reduction) {status}")
        if is_valid:
            valid_K.append(K)
    return valid_K


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
                      rolling_windows: Optional[List[Dict[str, int]]] = None) -> Dict:
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
    
    # Get unit IDs for selected controls
    # Note: selected_controls contains DataFrame indices (which are 0-based after reset_index)
    selected_units = embeddings_df.loc[list(selected_controls), 'unit'].tolist()
    
    # Save selected controls to permanent file for diagnostics
    output_dir = CBPS_INTEGRATION_DIR / str(year)
    output_dir.mkdir(parents=True, exist_ok=True)
    selected_units_file = output_dir / f"selected_controls_{output_prefix}_{year}.csv"
    pd.DataFrame({'unit': selected_units}).to_csv(selected_units_file, index=False)
    logger.info(f"    Selected controls saved to: {selected_units_file}")
    
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
        return {
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
        return {
            'K': K,
            'pool_size': int(pool_info['pool_size']),
            'pool_prop_full': float(pool_info['pool_prop_full']),
            'coverage_ratio': float(pool_info['coverage_ratio']),
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


def select_k_with_plateau(rmse_df: pd.DataFrame,
                          n_treated: int,
                          full_control_pool: int,
                          rmse_plateau_mult: float = 1.05,
                          ess_plateau_frac: float = 0.80,
                          gates: Optional[Dict[str, float]] = None) -> Dict:
    """
    Select K by feasibility gates, RMSE plateau, ESS plateau, then parsimony.
    """
    d = rmse_df.copy()
    rmse_col = 'median_RMSE' if ('median_RMSE' in d.columns and d['median_RMSE'].notna().any()) else 'rmse'
    p90_col = 'p90_RMSE' if ('p90_RMSE' in d.columns and d['p90_RMSE'].notna().any()) else None
    gates = gates or {}
    gate_max_smd = float(gates.get("max_smd", 0.10))
    gate_top10_share = float(gates.get("top10_share", 0.70))
    gate_max_weight_share = float(gates.get("max_weight", 0.10))
    gate_ess_frac_floor = float(gates.get("ess_frac", 0.02))
    gate_ess_mult_treated = float(gates.get("ess_mult_treated", 1.5))

    d['pool_prop_full'] = d['pool_size'] / max(1, full_control_pool)
    d['coverage_ratio'] = d['pool_size'] / max(1, n_treated)
    d['required_ess_floor'] = np.maximum(gate_ess_mult_treated * n_treated,
                                         gate_ess_frac_floor * d['pool_size'])
    d['feasibility_reasons'] = ''

    has_required = all(col in d.columns for col in ['max_balance_std', 'ess_control', 'top10_share'])
    if not has_required:
        logger.warning("Feasibility columns missing (max_balance_std/ess_control/top10_share); using convergence-only fallback.")
        d['feasible'] = (d.get('convergence', 0) == 1)
        d['feasibility_reasons'] = np.where(d['feasible'], '', 'missing_required_columns')
        feasible_df = d[d['feasible']].copy()
        if feasible_df.empty:
            pick = d.sort_values([rmse_col, 'pool_size', 'K']).iloc[0]
            return {'chosen_K': int(pick['K']), 'selection_mode': 'fallback_min_rmse', 'table': d}
    else:
        def apply_gates(max_smd_thr: float, top10_thr: float, ess_mult_thr: float) -> pd.Series:
            required_ess = np.maximum(ess_mult_thr * n_treated, gate_ess_frac_floor * d['pool_size'])
            feas = (
                (d['max_balance_std'] <= max_smd_thr) &
                (d['ess_control'] >= required_ess) &
                (d['top10_share'] <= top10_thr)
            )
            if 'max_weight_share' in d.columns:
                feas = feas & (d['max_weight_share'] <= gate_max_weight_share)
            return feas

        feasible_hard = apply_gates(gate_max_smd, gate_top10_share, gate_ess_mult_treated)
        feasible_relaxed = apply_gates(0.15, 0.75, 1.2)

        d['feasible_hard'] = feasible_hard
        d['feasible_relaxed'] = feasible_relaxed
        d['feasible'] = feasible_hard | feasible_relaxed

        reasons = []
        reasons.append(np.where(d['max_balance_std'] > gate_max_smd, 'max_smd', ''))
        reasons.append(np.where(d['ess_control'] < d['required_ess_floor'], 'ess_floor', ''))
        reasons.append(np.where(d['top10_share'] > gate_top10_share, 'top10_share', ''))
        if 'max_weight_share' in d.columns:
            reasons.append(np.where(d['max_weight_share'] > gate_max_weight_share, 'max_weight', ''))
        reason_df = pd.DataFrame(reasons).T
        d['feasibility_reasons'] = reason_df.apply(
            lambda x: ';'.join([v for v in x.tolist() if isinstance(v, str) and v]), axis=1
        )

        feasible_df = d[d['feasible_hard']].copy()
        mode_prefix = 'feasible_hard'
        if feasible_df.empty:
            feasible_df = d[d['feasible_relaxed']].copy()
            mode_prefix = 'feasible_relaxed'
        if feasible_df.empty:
            logger.warning("No feasible K under hard/relaxed gates; falling back to minimum RMSE overall.")
            pick = d.sort_values([rmse_col, 'pool_size', 'K']).iloc[0]
            return {'chosen_K': int(pick['K']), 'selection_mode': 'fallback_min_rmse_overall', 'table': d}

    if 'mode_prefix' not in locals():
        mode_prefix = 'feasible'

    rmse_best = feasible_df[rmse_col].min()
    plateau_mask = feasible_df[rmse_col] <= rmse_plateau_mult * rmse_best
    if p90_col is not None:
        p90_best = feasible_df[p90_col].min()
        plateau_mask = plateau_mask & (feasible_df[p90_col] <= rmse_plateau_mult * p90_best)
    plateau_df = feasible_df[plateau_mask].copy()

    if (
        'ess_control' in plateau_df.columns and
        plateau_df['ess_control'].notna().any() and
        0 < ess_plateau_frac <= 1
    ):
        ess_best = plateau_df['ess_control'].max()
        ess_cutoff = ess_plateau_frac * ess_best
        plateau_ess = plateau_df[plateau_df['ess_control'] >= ess_cutoff].copy()
        if not plateau_ess.empty:
            plateau_df = plateau_ess

    if plateau_df.empty:
        plateau_df = feasible_df.nsmallest(1, rmse_col).copy()

    # Parsimonious ranking: smallest realized donor pool first.
    sort_cols = ['pool_size', 'top10_share', 'max_weight_share', 'ess_control', 'K']
    ascending = [True, True, True, False, True]
    for col in ['max_weight_share']:
        if col not in plateau_df.columns:
            plateau_df[col] = np.nan
    pick = plateau_df.sort_values(sort_cols, ascending=ascending, na_position='last').iloc[0]
    return {
        'chosen_K': int(pick['K']),
        'selection_mode': f'{mode_prefix}_plateau_smallest_pool',
        'table': d,
        'rmse_best': float(rmse_best)
    }


def select_optimal_k(similarities: Dict[int, np.ndarray],
                    embeddings_df: pd.DataFrame,
                    K_candidates: List[int],
                    year: int,
                    min_ratio: int = 10,
                    force_recompute: bool = False,
                    max_workers: int = 6,
                    output_tag: str = "",
                    experiment_name: str = "full_pool",
                    analysis_base_dir: str = "data/processed_data/rev_analysis_low",
                    save_full_weights: bool = False,
                    target_pool_proportions: Optional[List[float]] = None,
                    include_full_pool: bool = True,
                    gates: Optional[Dict[str, float]] = None,
                    rolling_windows: Optional[List[Dict[str, int]]] = None,
                    adaptive_refine: bool = True,
                    random_baseline_reps: int = 0,
                    random_seed: Optional[int] = None,
                    train_years: Optional[List[int]] = None,
                    test_years: Optional[List[int]] = None) -> Dict:
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
    logger.info(f"Parallelization: {max_workers} workers")

    train_years = train_years or list(range(2000, 2011))
    test_years = test_years or list(range(2011, 2016))
    logger.info("Train years: %s-%s | Test years: %s-%s", train_years[0], train_years[-1], test_years[0], test_years[-1])
    if rolling_windows:
        logger.info("Evaluation mode: rolling windows (%s windows)", len(rolling_windows))
    else:
        logger.info("Evaluation mode: single fixed split")

    max_k_possible = min([arr.shape[0] for arr in similarities.values()])

    if target_pool_proportions:
        # Proportion-driven mode: build a broad scan over K, then map requested proportions
        # to realized donor-pool sizes after similarity filtering.
        if max_k_possible <= 250:
            K_candidates_eff = list(range(1, max_k_possible + 1))
        else:
            linear_grid = np.linspace(1, max_k_possible, num=140, dtype=int).tolist()
            log_grid = np.geomspace(1, max_k_possible, num=140, dtype=int).astype(int).tolist()
            K_candidates_eff = sorted(set(linear_grid + log_grid + [1, int(max_k_possible)]))
        logger.info(
            "Primary driver: target donor-pool proportions %s (scan grid size=%s)",
            [float(x) for x in target_pool_proportions],
            len(K_candidates_eff),
        )
    else:
        candidate_set = set(int(k) for k in K_candidates if int(k) > 0)
        if include_full_pool:
            candidate_set.add(int(max_k_possible))
        K_candidates_eff = sorted(candidate_set)
        logger.info("Primary driver: explicit K grid %s", K_candidates_eff)

    pool_df = build_pool_diagnostics_table(
        similarities,
        K_candidates_eff,
        n_treated=n_treated,
        n_controls_full=n_controls,
    )

    logger.info("Evaluating realized donor-pool targets at K values: %s", K_candidates_eff)
    elbow_df = compute_elbow_metrics(similarities, K_candidates_eff)

    # Stage 1 filter is now active: prune candidate K values by elbow/knee before CBPS runs.
    elbow_kept = filter_by_elbow(elbow_df)
    if elbow_kept:
        K_candidates_eff = sorted(set(int(k) for k in elbow_kept))
        logger.info("After similarity knee filter, keeping K values: %s", K_candidates_eff)
    else:
        logger.warning("Knee filter returned empty; retaining original K candidates.")

    pool_df = pool_df[pool_df['K'].isin(K_candidates_eff)].copy().reset_index(drop=True)
    mapping_df = pd.DataFrame()
    if target_pool_proportions:
        mapped_k, mapping_df = map_candidates_to_target_proportions(pool_df, target_pool_proportions)
        if mapped_k:
            K_candidates_eff = mapped_k
            pool_df = pool_df[pool_df['K'].isin(K_candidates_eff)].copy().reset_index(drop=True)
            logger.info("Mapped target proportions to realized K values: %s", K_candidates_eff)
        else:
            logger.warning("Could not map target proportions to K values; using current filtered K set.")

    pool_lookup = pool_df.set_index('K').to_dict(orient='index')

    valid_K = check_pool_sizes(pool_df, n_treated, min_ratio)
    if not valid_K:
        logger.error("No K values produce large enough control pools!")
        logger.error(f"Try smaller min_ratio (current: {min_ratio}) or larger K values")
        return None
    logger.info(f"\nValid K values for RMSPE testing: {valid_K}")

    if force_recompute:
        for K in valid_K:
            output_prefix = f"k{K}" + (f"_{output_tag}" if output_tag else "")
            metrics_file = CBPS_INTEGRATION_DIR / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
            if metrics_file.exists():
                logger.info(f"🗑️  Deleting cached CBPS metrics for K={K} (--force-recompute)")
                metrics_file.unlink()

    rmse_results: Dict[int, Dict] = {}

    def load_cached_metrics(K: int) -> Optional[Dict]:
        output_prefix = f"k{K}" + (f"_{output_tag}" if output_tag else "")
        metrics_file = CBPS_INTEGRATION_DIR / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
        if not metrics_file.exists() or force_recompute:
            return None
        metrics = pd.read_csv(metrics_file)
        pool_info = pool_lookup.get(K, {})
        return {
            'K': K,
            'pool_size': int(pool_info.get('pool_size', np.nan)),
            'pool_prop_full': float(pool_info.get('pool_prop_full', np.nan)),
            'coverage_ratio': float(pool_info.get('coverage_ratio', np.nan)),
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

    if adaptive_refine and not rmse_df.empty:
        coarse_pick = select_k_with_plateau(
            rmse_df=rmse_df,
            n_treated=n_treated,
            full_control_pool=n_controls,
            gates=gates,
        )
        refine_candidates = adaptive_k_search(int(coarse_pick['chosen_K']), max_k_possible)
        refine_candidates = [k for k in refine_candidates if k not in set(rmse_results.keys())]
        if refine_candidates:
            logger.info("\nAdaptive refinement candidates around K=%s: %s", coarse_pick['chosen_K'], refine_candidates)
            refine_pool_df = build_pool_diagnostics_table(
                similarities,
                refine_candidates,
                n_treated=n_treated,
                n_controls_full=n_controls,
            )
            for _, row in refine_pool_df.iterrows():
                pool_lookup[int(row['K'])] = row.to_dict()
            pool_df = pd.concat([pool_df, refine_pool_df], ignore_index=True).drop_duplicates(subset=['K']).sort_values('K').reset_index(drop=True)
            refine_valid = check_pool_sizes(refine_pool_df, n_treated, min_ratio)
            evaluate_k_list(refine_valid, "refine")
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
    # Step 4: Select optimal K (feasible gates + plateau + smallest donor pool)
    selection = select_k_with_plateau(
        rmse_df=rmse_df,
        n_treated=n_treated,
        full_control_pool=n_controls,
        gates=gates,
    )
    optimal_K = selection['chosen_K']
    objective_col = 'median_RMSE' if ('median_RMSE' in rmse_df.columns and rmse_df['median_RMSE'].notna().any()) else 'rmse'
    optimal_rmse = rmse_df.loc[rmse_df['K'] == optimal_K, objective_col].iloc[0]
    optimal_pool = rmse_df.loc[rmse_df['K'] == optimal_K, 'pool_size'].iloc[0]
    logger.info(f"\n{'='*80}")
    logger.info(f"OPTIMAL K SELECTED: {optimal_K}")
    logger.info(f"Selection mode: {selection['selection_mode']}")
    logger.info(f"Pre-treatment RMSE: {optimal_rmse:.4f}")
    logger.info(f"Control pool size: {optimal_pool}")
    logger.info(f"{'='*80}\n")    
    random_rep_df = pd.DataFrame()
    random_summary_df = pd.DataFrame()
    if random_baseline_reps > 0:
        try:
            random_rep_df, random_summary_df = run_random_pool_experiment(
                similarities=similarities,
                embeddings_df=embeddings_df,
                K_values=sorted(set(rmse_df['K'].astype(int).tolist())),
                year=year,
                pool_lookup=pool_lookup,
                random_reps=random_baseline_reps,
                output_tag=output_tag,
                experiment_name=experiment_name,
                analysis_base_dir=analysis_base_dir,
                save_full_weights=save_full_weights,
                rolling_windows=rolling_windows,
                random_seed=random_seed,
                train_years=train_years,
                test_years=test_years,
            )
        except Exception as e:
            logger.warning("Random baseline experiment failed: %s", e)

    return {
        'optimal_K': int(optimal_K),
        'optimal_rmse': float(optimal_rmse),
        'selection_mode': selection['selection_mode'],
        'elbow_metrics': elbow_df,
        'rmse_results': selection['table'],
        'valid_K_values': all_tested,
        'all_similarities': similarities,
        'pool_diagnostics': pool_df,
        'pool_target_mapping': mapping_df,
        'random_pool_replicates': random_rep_df,
        'random_pool_summary': random_summary_df,
    }
