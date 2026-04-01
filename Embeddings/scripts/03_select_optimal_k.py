"""
Optimal K Selection for Embedding-Based Control Selection
Uses pre-treatment RMSPE cross-validation to select K

===== MECHANISM OVERVIEW =====

This script optimizes K (number of nearest neighbors per treated pixel) using a 
multi-stage filtering approach:

STAGE 1: Elbow Analysis (Similarity-Based Filtering)
  - Compute mean cosine similarity for each K candidate
  - Identify "elbow" where similarity drops sharply (>2% decrease)
  - Drop K after elbow (poor quality controls, sharp degradation)
  - Example: K=[10,20,30,50,75,100], sim=[0.92,0.915,0.911,0.906,0.903,0.90]
    → Δsim=[-0.005,-0.004,-0.005,-0.003,-0.003] (gradual)
    → No sharp drop → Keep all K values

STAGE 2: Pool Size Check (Statistical Power Filtering)  
  - For each K, compute union of K-nearest controls across all treated pixels
  - Require pool size ≥ min_ratio × n_treated (default: 10×)
  - Drop K producing insufficient control pool
  - Example: n_treated=100, K=[10,20,30,50] → pools=[500,800,1200,1500]
    → min_ratio=10 requires ≥1000 controls → Keep K=[30,50]

STAGE 3: Pre-treatment RMSPE Cross-Validation (Predictive Validity)
  - Train: years 2000-2010 (fit CBPS weights)
  - Test: years 2011-2015 (compute fire frequency RMSE)
  - Select K with lowest test RMSE
  - Example: K=30 (RMSE=0.12), K=50 (RMSE=0.09) → Optimal K=50

===== R INTEGRATION MECHANISM =====

run_cbps_crossval() calls run_cbps_with_selected_controls.R:

1. Python writes selected control units to temp CSV
2. R script loads analysis_treated{year}_conifer.RDS (full data)
3. R filters to treated + selected controls ONLY
4. R applies identical transformations as implement_cbps.R:
   - Two-part SWE (presence + log-intensity winsorized)
   - Log+winsorize max_FRP, prcp, avg_BRIGHTNESS
   - Drop sparse fire_* columns (<0.5% ones)
5. R runs CBPS with same regularization grid as baseline
6. R computes pre-treatment RMSE via calculate_fire_outcomes.R
7. R saves metrics CSV → Python reads and extracts RMSE

CRITICAL: run_cbps_with_selected_controls.R MUST apply IDENTICAL transformations 
as implement_cbps.R (baseline) to ensure fair comparison. Any discrepancy 
invalidates the embedding vs baseline comparison.

===== COMPARISON TO BASELINE =====

- Baseline (implement_cbps.R): Full control pool (~50k pixels)
- Embedding (run_cbps_with_selected_controls.R): Filtered pool (K-nearest, ~1-5k)
- Both use: Same CBPS algorithm, same transformations, same train/test split
- Difference: Control pool composition only
===== LITERATURE CONTEXT =====
K-NN selection via cross-validation is standard in ML (e.g., Hastie et al. 2009).
Synthetic control hyperparameter tuning via RMSPE follows Abadie et al. (2015),
Ben-Michael et al. (2021). The embedding approach combines both traditions.
References:
- Abadie et al. (2015): "Comparative Politics and the Synthetic Control Method"
- Ben-Michael et al. (2021): "The Augmented Synthetic Control Method"  
- Hastie et al. (2009): "The Elements of Statistical Learning"
"""

import sys
from pathlib import Path

# Add parent directory to path for config import
sys.path.insert(0, str(Path(__file__).parent.parent))

import numpy as np
import pandas as pd
import logging
import subprocess
import tempfile
import json
from typing import Dict, List, Tuple, Set, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
from functools import partial

BASE_DIR = Path(__file__).resolve().parent.parent
DATA_DIR = BASE_DIR / "data"
K_SELECTION_DIR = DATA_DIR / "k_selection"
CBPS_INTEGRATION_DIR = DATA_DIR / "cbps_integration"

# Setup logging
logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(asctime)s - %(name)s - %(message)s')
logger = logging.getLogger(__name__)


from Embeddings._similarity_utils import compute_all_similarities


def load_lambda_hard_gates(config_path: str = "balancing/balancing_config.R") -> Dict[str, float]:
    """Load canonical hard-gate thresholds from R config to avoid drift."""
    default = {
        "max_smd": 0.10,
        "top10_share": 0.70,
        "max_weight": 0.10,
        "ess_frac": 0.02,
        "ess_mult_treated": 1.5,
    }
    cmd = [
        "Rscript",
        "-e",
        (
            f"source('{config_path}'); "
            "cfg <- get_diagnostics_config()$lambda_selection$hard_gates; "
            "cat(paste(c(cfg$max_smd,cfg$top10_share,cfg$max_weight,cfg$ess_frac,cfg$ess_mult_treated), collapse=','))"
        ),
    ]
    try:
        res = subprocess.run(cmd, capture_output=True, text=True, check=True)
        vals = [float(x) for x in res.stdout.strip().split(',')]
        if len(vals) == 5:
            return {
                "max_smd": vals[0],
                "top10_share": vals[1],
                "max_weight": vals[2],
                "ess_frac": vals[3],
                "ess_mult_treated": vals[4],
            }
    except Exception as exc:
        logger.warning("Failed to load hard gates from %s (%s); using defaults", config_path, exc)
    return default


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
                      save_full_weights: bool = False) -> Dict:
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
    output_dir = CBPS_INTEGRATION_DIR / experiment_name / str(year)
    output_dir.mkdir(parents=True, exist_ok=True)
    selected_units_file = output_dir / f"selected_controls_{output_prefix}_{year}.csv"
    pd.DataFrame({'unit': selected_units}).to_csv(selected_units_file, index=False)
    logger.info(f"    Selected controls saved to: {selected_units_file}")
    
    # Create temporary CSV with selected units
    with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False) as f:
        temp_csv = f.name
        pd.DataFrame({'unit': selected_units}).to_csv(temp_csv, index=False)
    try:
        # Call R script
        r_script = "Embeddings/scripts/04_run_cbps_with_selected_controls.R"
        # Capstone root is three levels up from this script (Embeddings/scripts/ -> Capstone)
        capstone_root = Path(__file__).parent.parent.parent
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
            "--analysis-base-dir", analysis_base_dir,
            "--save-full-weights", "true" if save_full_weights else "false",
        ]
        # R runner enforces diagnostics and will raise on degenerate weights
        logger.info(f"    Calling R CBPS: {' '.join(cmd)}")
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=capstone_root  # Run from Capstone root
        )
        if result.returncode != 0:
            logger.error(f"R script failed with return code {result.returncode}")
            logger.error(f"STDOUT: {result.stdout}")
            logger.error(f"STDERR: {result.stderr}")
            raise RuntimeError(f"R CBPS script failed: {result.stderr or result.stdout}")
        # Parse R output
        # Look in year-specific subdirectory
        metrics_file = CBPS_INTEGRATION_DIR / experiment_name / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
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


def compute_k_value(K: int, similarities: Dict[int, np.ndarray], 
                    embeddings_df: pd.DataFrame, year: int,
                    train_years: List[int], test_years: List[int], output_tag: str = "",
                    experiment_name: str = "full_pool",
                    analysis_base_dir: str = "data/processed_data/rev_analysis_low",
                    save_full_weights: bool = False,
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
                          ess_plateau_frac: float = 0.90,
                          gates: Optional[Dict[str, float]] = None) -> Dict:
    """
    Select K by feasibility gates (aligned with hard-gate intent), plateau region,
    then lexicographic ranking that prioritizes precision/stability before pool size.
    """
    d = rmse_df.copy()
    rmse_col = 'median_RMSE' if ('median_RMSE' in d.columns and d['median_RMSE'].notna().any()) else 'rmse'
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
    if has_required:
        feasible = (
            (d['max_balance_std'] <= gate_max_smd) &
            (d['ess_control'] >= d['required_ess_floor']) &
            (d['top10_share'] <= gate_top10_share)
        )
        if 'max_weight_share' in d.columns:
            feasible = feasible & (d['max_weight_share'] <= gate_max_weight_share)

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
        d['feasible'] = feasible
    else:
        logger.warning("Feasibility columns missing (max_balance_std/ess_control/top10_share); using convergence-only fallback.")
        d['feasible'] = (d.get('convergence', 0) == 1)
        d['feasibility_reasons'] = np.where(d['feasible'], '', 'missing_required_columns')

    feasible_df = d[d['feasible']].copy()
    if feasible_df.empty:
        logger.warning("No feasible K found by hard gates; falling back to minimum RMSE.")
        pick = d.sort_values([rmse_col, 'pool_size', 'K']).iloc[0]
        return {'chosen_K': int(pick['K']), 'selection_mode': 'fallback_min_rmse', 'table': d}

    rmse_best = feasible_df[rmse_col].min()
    if 'ess_control' in feasible_df.columns and feasible_df['ess_control'].notna().any():
        ess_best = feasible_df['ess_control'].max()
        plateau_df = feasible_df[
            (feasible_df[rmse_col] <= rmse_plateau_mult * rmse_best) &
            (feasible_df['ess_control'] >= ess_plateau_frac * ess_best)
        ].copy()
    else:
        plateau_df = feasible_df[feasible_df[rmse_col] <= rmse_plateau_mult * rmse_best].copy()

    if plateau_df.empty:
        plateau_df = feasible_df.nsmallest(1, rmse_col).copy()

    # Constrained lexicographic ranking inside plateau:
    # 1) maximize ESS, 2) minimize concentration, 3) choose smallest donor pool.
    sort_cols = ['ess_control', 'top10_share', 'max_weight_share', 'pool_size', 'K']
    ascending = [False, True, True, True, True]
    for col in ['max_weight_share']:
        if col not in plateau_df.columns:
            plateau_df[col] = np.nan
    pick = plateau_df.sort_values(sort_cols, ascending=ascending, na_position='last').iloc[0]
    return {
        'chosen_K': int(pick['K']),
        'selection_mode': 'feasible_plateau_smallest_pool',
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
                    gates: Optional[Dict[str, float]] = None) -> Dict:
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

    max_k_possible = min([arr.shape[0] for arr in similarities.values()])
    candidate_set = set(int(k) for k in K_candidates if int(k) > 0)
    if include_full_pool:
        candidate_set.add(int(max_k_possible))
    K_candidates_eff = sorted(candidate_set)

    pool_df = build_pool_diagnostics_table(
        similarities,
        K_candidates_eff,
        n_treated=n_treated,
        n_controls_full=n_controls,
    )

    mapping_df = pd.DataFrame()
    if target_pool_proportions:
        mapped_k, mapping_df = map_candidates_to_target_proportions(pool_df, target_pool_proportions)
        if mapped_k:
            K_candidates_eff = mapped_k

    pool_df = pool_df[pool_df['K'].isin(K_candidates_eff)].copy().reset_index(drop=True)
    pool_lookup = pool_df.set_index('K').to_dict(orient='index')

    logger.info("Evaluating realized donor-pool targets at K values: %s", K_candidates_eff)
    # Step 1: Compute similarity metrics for each K (for diagnostics only)
    elbow_df = compute_elbow_metrics(similarities, K_candidates_eff)
    # Step 2: Check pool sizes (only filter by pool size)
    valid_K = check_pool_sizes(pool_df, n_treated, min_ratio)
    if not valid_K:
        logger.error("No K values produce large enough control pools!")
        logger.error(f"Try smaller min_ratio (current: {min_ratio}) or larger K values")
        return None
    logger.info(f"\nValid K values for RMSPE testing: {valid_K}")

    # Step 3: Run CBPS + RMSPE cross-validation
    # If force_recompute, delete all cached CBPS metrics files for this year/K
    if force_recompute:
        for K in K_candidates_eff:
            output_prefix = f"k{K}" + (f"_{output_tag}" if output_tag else "")
            metrics_file = CBPS_INTEGRATION_DIR / experiment_name / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
            if metrics_file.exists():
                logger.info(f"🗑️  Deleting cached CBPS metrics for K={K} (--force-recompute)")
                metrics_file.unlink()
    K_to_compute = []
    K_cached = []
    for K in valid_K:
        output_prefix = f"k{K}" + (f"_{output_tag}" if output_tag else "")
        metrics_file = CBPS_INTEGRATION_DIR / experiment_name / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
        if metrics_file.exists() and not force_recompute:
            K_cached.append(K)
        else:
            K_to_compute.append(K)
    # Log cache status
    if not force_recompute:
        logger.info(f"  Cache status: {len(K_cached)} cached, {len(K_to_compute)} need computation")
        if K_cached:
            logger.info(f"  Will load cached: {K_cached}")
        if K_to_compute:
            logger.info(f"  Will compute: {K_to_compute}")
    else:
        logger.info(f"  --force-recompute: Computing all {len(K_to_compute)} K values")
    # Step 3b: Load cached results
    rmse_results = []
    if K_cached:
        logger.info(f"\n  Loading cached results...")
        for K in K_cached:
            output_prefix = f"k{K}" + (f"_{output_tag}" if output_tag else "")
            metrics_file = CBPS_INTEGRATION_DIR / experiment_name / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
            try:
                metrics = pd.read_csv(metrics_file)
                pool_info = pool_lookup.get(K, {})
                rmse_results.append({
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
                })
                logger.info(f"    K={K}: ✓ Loaded (RMSE={rmse_results[-1]['rmse']:.4f})")
            except Exception as e:
                logger.warning(f"    K={K}: ⚠ Failed to load cache: {e}")
                logger.warning(f"             Adding back to computation queue...")
                K_to_compute.append(K)  # Re-add to computation list
    # Step 3c: Parallelize computation of remaining K values
    if K_to_compute:
        logger.info(f"\n  Computing CBPS for {len(K_to_compute)} K values in parallel...")
        n_workers = min(len(K_to_compute), max_workers)
        logger.info(f"  Using {n_workers} parallel workers")
        compute_func = partial(
            compute_k_value,
            similarities=similarities,
            embeddings_df=embeddings_df,
            year=year,
            train_years=list(range(2000, 2011)),
            test_years=list(range(2011, 2016)),
            output_tag=output_tag,
            experiment_name=experiment_name,
            analysis_base_dir=analysis_base_dir,
            save_full_weights=save_full_weights,
            n_controls_full=n_controls,
        )
        # (strict removed) R runner will always fail on degenerate weights
        with ThreadPoolExecutor(max_workers=n_workers) as executor:
            future_to_k = {executor.submit(compute_func, K): K for K in K_to_compute}
            completed = 0
            for future in as_completed(future_to_k):
                K = future_to_k[future]
                completed += 1
                try:
                    result = future.result()
                    if result['success']:
                        metrics = {k: v for k, v in result.items() if k not in ['success', 'error']}
                        rmse_results.append(metrics)
                        logger.info(f"    K={K}: ✓ RMSE={result['rmse']:.4f}, "
                                   f"balance={result['max_balance_std']:.3f} "
                                   f"[{completed}/{len(K_to_compute)}]")
                    else:
                        logger.error(f"    K={K}: ✗ Failed: {result['error']} "
                                    f"[{completed}/{len(K_to_compute)}]")
                except Exception as e:
                    logger.error(f"    K={K}: ✗ Exception: {e} "
                                f"[{completed}/{len(K_to_compute)}]")
        logger.info(f"  ✓ Parallel computation complete")
    # Step 3d: Check if we have any results
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
    rmse_df = pd.DataFrame(rmse_results)
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
    logger.info(f"\n{'='*80}")
    logger.info(f"CBPS CROSS-VALIDATION SUMMARY")
    logger.info(f"{'='*80}")
    logger.info(f"Tested: {len(valid_K)} K values")
    logger.info(f"Succeeded: {len(rmse_results)} K values")
    logger.info(f"Failed: {len(valid_K) - len(rmse_results)} K values")
    if len(rmse_results) < len(valid_K):
        failed_K = set(valid_K) - set(rmse_df['K'].values)
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
    optimal_rmse = rmse_df.loc[rmse_df['K'] == optimal_K, 'rmse'].iloc[0]
    optimal_pool = rmse_df.loc[rmse_df['K'] == optimal_K, 'pool_size'].iloc[0]
    logger.info(f"\n{'='*80}")
    logger.info(f"OPTIMAL K SELECTED: {optimal_K}")
    logger.info(f"Selection mode: {selection['selection_mode']}")
    logger.info(f"Pre-treatment RMSE: {optimal_rmse:.4f}")
    logger.info(f"Control pool size: {optimal_pool}")
    logger.info(f"{'='*80}\n")    
    return {
        'optimal_K': int(optimal_K),
        'optimal_rmse': float(optimal_rmse),
        'selection_mode': selection['selection_mode'],
        'elbow_metrics': elbow_df,
        'rmse_results': selection['table'],
        'valid_K_values': valid_K,
        'all_similarities': similarities,
        'pool_diagnostics': pool_df,
        'pool_target_mapping': mapping_df,
    }
def main():
    """Run optimal K selection on embeddings data
    Usage:
        python select_optimal_k.py [year]
    Args:
        year (optional): Treatment year to process (default: 2019)    
    Examples:
        python select_optimal_k.py          # Uses 2019
        python select_optimal_k.py 2015     # Uses 2015
    """    
    # Parse command-line arguments
    import argparse
    parser = argparse.ArgumentParser(
        description='Select optimal K for embedding-based control selection',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python select_optimal_k.py          # Process 2019 (default)
  python select_optimal_k.py 2015     # Process 2015
  python select_optimal_k.py 2008     # Process 2008
        """
    )
    parser.add_argument(
        'year',
        type=int,
        nargs='?',
        default=2019,
        help='Treatment year to process (default: 2019)'
    )
    parser.add_argument(
        '--k-values',
        type=int,
        nargs='+',
        default=[5, 10, 20, 30, 50, 100],
        help='Raw K candidates used for realized donor-pool mapping (default: 5 10 20 30 50 100)'
    )
    parser.add_argument(
        '--target-pool-proportions',
        type=float,
        nargs='+',
        default=[0.005, 0.01, 0.02, 0.05, 0.10, 0.20, 1.0],
        help='Target donor-pool proportions of full controls to evaluate (default: 0.005 0.01 0.02 0.05 0.10 0.20 1.0)'
    )
    parser.add_argument(
        '--no-full-pool',
        action='store_true',
        help='Do not automatically include full-pool candidate in proportion mapping'
    )
    parser.add_argument(
        '--min-ratio',
        type=int,
        default=10,
        help='Minimum control:treated ratio (default: 10)'
    )
    parser.add_argument(
        '--force-recompute',
        action='store_true',
        help='Force recomputation of similarities and CBPS (ignore cache)'
    )
    parser.add_argument(
        '--max-workers',
        type=int,
        default=6,
        help='Maximum number of parallel workers for CBPS computation (default: 6)'
    )
    parser.add_argument(
        '--treated-subsample-frac',
        type=float,
        default=1.0,
        help='Fraction of treated units to keep for robustness runs (default: 1.0)'
    )
    parser.add_argument(
        '--random-seed',
        type=int,
        default=None,
        help='Random seed used when treated-subsample-frac < 1.0'
    )
    parser.add_argument(
        '--output-tag',
        type=str,
        default='',
        help='Optional suffix tag for output files (e.g., boot1)'
    )
    parser.add_argument(
        '--experiment-name',
        type=str,
        default='full_pool',
        help='Experiment namespace for input/output isolation (default: full_pool)'
    )
    parser.add_argument(
        '--analysis-base-dir',
        type=str,
        default='data/processed_data/rev_analysis_low',
        help='Base directory for analysis_treated inputs (default: data/processed_data/rev_analysis_low)'
    )
    parser.add_argument(
        '--save-full-weights',
        action='store_true',
        help='Also save full unit-level weights CSV for each K (default: off to limit files)'
    )
    parser.add_argument(
        '--config-path',
        type=str,
        default='balancing/balancing_config.R',
        help='Path to canonical balancing config used to load hard-gate thresholds'
    )
    # NOTE: strict mode removed — R runner now always fails on degenerate weights
    args = parser.parse_args()
    year = args.year    
    logger.info(f"Processing year: {year}")    
    # Load embeddings with treatment labels
    # For production, use Embeddings/data/embeddings/ output directory
    embeddings_file = Path(f"Embeddings/data/embeddings/embeddings_{year}.csv")    
    # Fallback to test data if production data not found   
    if not embeddings_file.exists():
        logger.error(f"Embeddings file not found for year {year}: {embeddings_file}")
        logger.error(f"Expected locations:")
        logger.error(f"  - Embeddings/data/embeddings/embeddings_{year}.csv")
        logger.error(f"  - tests/data/11SLA_embeddings_{year}_with_treatment.csv")
        return 1    
    logger.info(f"Loading embeddings from {embeddings_file}...")
    embeddings_df = pd.read_csv(embeddings_file)    
    # Validate required columns (72D embeddings after quantization: 12 months × 6 channels)
    required_cols = ['unit', 'treated'] + [f'band_{i}' for i in range(72)]
    missing_cols = [col for col in required_cols if col not in embeddings_df.columns]
    if missing_cols:
        logger.error(f"Embeddings file missing required columns: {missing_cols}")
        logger.error(f"Available columns: {list(embeddings_df.columns)}")
        return 1    
    logger.info(f"✓ Embeddings validated: {len(required_cols)} required columns present")
    
    # Verify all 72 dimensions are present (12 months × 6 channels after quantization)
    embedding_cols = [col for col in embeddings_df.columns if col.startswith('band_')]
    if len(embedding_cols) != 72:
        logger.warning(f"Expected 72 embedding dimensions (quantized), found {len(embedding_cols)}")
        logger.warning(f"Embedding columns: {embedding_cols[:12]}... (showing first 12)")
    else:
        logger.info(f"✓ All 72 embedding dimensions present (12 months × 6 channels)")
    
    logger.info(f"Loaded {len(embeddings_df)} pixels")
    logger.info(f"  Treated: {(embeddings_df['treated'] == 1).sum()}")
    logger.info(f"  Control: {(embeddings_df['treated'] == 0).sum()}")
    
    # CRITICAL: Filter out rows with NaN in embeddings
    nan_rows = embeddings_df[embedding_cols].isna().any(axis=1).sum()
    
    if nan_rows > 0:
        logger.warning(f"⚠ Found {nan_rows} rows ({100*nan_rows/len(embeddings_df):.1f}%) with NaN embeddings")
        logger.warning(f"  Filtering out rows with any NaN in embeddings...")
        embeddings_df = embeddings_df[~embeddings_df[embedding_cols].isna().any(axis=1)].reset_index(drop=True)
        logger.info(f"  After filtering: {len(embeddings_df)} pixels remain")
        logger.info(f"    Treated: {(embeddings_df['treated'] == 1).sum()}")
        logger.info(f"    Control: {(embeddings_df['treated'] == 0).sum()}")
        
        if len(embeddings_df) == 0:
            logger.error("ERROR: All embeddings have NaN values - cannot proceed!")
            return 1
    else:
        logger.info(f"  ✓ No NaN values in embeddings")
    
    # CRITICAL: Always reset index to ensure sequential 0-based indexing
    # This ensures DataFrame indices match positional indices
    embeddings_df = embeddings_df.reset_index(drop=True)
    logger.info(f"  ✓ Reset index to ensure sequential indexing")

    # Optional robustness mode: subsample treated units while keeping full controls.
    subsample_frac = float(args.treated_subsample_frac)
    if subsample_frac <= 0 or subsample_frac > 1:
        logger.error("--treated-subsample-frac must be in (0, 1].")
        return 1
    if subsample_frac < 1.0:
        treated_df = embeddings_df[embeddings_df['treated'] == 1].copy()
        control_df = embeddings_df[embeddings_df['treated'] == 0].copy()
        n_treated = len(treated_df)
        if n_treated == 0:
            logger.error("No treated units available for subsampling.")
            return 1
        n_keep = max(1, int(np.ceil(subsample_frac * n_treated)))
        treated_sub = treated_df.sample(n=n_keep, random_state=args.random_seed, replace=False)
        embeddings_df = pd.concat([treated_sub, control_df], axis=0, ignore_index=True)
        embeddings_df = embeddings_df.reset_index(drop=True)
        logger.info(
            "  ✓ Treated subsample enabled: kept %s/%s treated units (frac=%.3f, seed=%s)",
            n_keep,
            n_treated,
            subsample_frac,
            args.random_seed,
        )
    
    # Step 1: Compute all similarities (one-time computation with caching)
    tag_suffix = f"_{args.output_tag}" if args.output_tag else ""
    robust_suffix = ""
    if subsample_frac < 1.0:
        seed_label = "na" if args.random_seed is None else str(args.random_seed)
        robust_suffix = f"_sub{int(round(subsample_frac * 1000)):03d}_seed{seed_label}"
    similarities_cache = K_SELECTION_DIR / args.experiment_name / str(year) / f"similarities_cache_{year}{robust_suffix}.npy"
    similarities_cache.parent.mkdir(parents=True, exist_ok=True)
    
    # Delete cache if force recompute requested
    if args.force_recompute and similarities_cache.exists():
        logger.info(f"🗑️  Deleting cached similarities (--force-recompute)")
        similarities_cache.unlink()
    
    if similarities_cache.exists():
        logger.info(f"Loading cached similarities from {similarities_cache}...")
        similarities_array = np.load(similarities_cache, allow_pickle=True).item()
        # Convert back to proper format
        similarities = {}
        for k, v in similarities_array.items():
            similarities[int(k)] = v
        logger.info(f"  ✓ Loaded {len(similarities)} treated pixels from cache")
    else:
        logger.info("Computing similarities (will be cached for future runs)...")
        similarities = compute_all_similarities(embeddings_df)
        
        # Save to cache for future runs
        np.save(similarities_cache, similarities)
        logger.info(f"  ✓ Cached similarities to {similarities_cache}")
        logger.info(f"     (Delete this file to force recomputation)")    
    # Step 2-4: Select optimal K
    # K range justification:
    #   - Lower bound (20): Avoid degenerate weights and heavy regularization
    #   - Upper bound (200): Allows for improved balance and ESS
    #   - Spacing: Dense at low K (20→30→40→50), sparser at high K (75→100→150→200)
    # Union effect: K per treated × ~200 treated → ~5-10k unique controls after overlap
    # Expected result: K ∈ [30, 100] after pool size filtering and balance diagnostics
    K_candidates = args.k_values
    min_ratio = args.min_ratio    
    logger.info(f"K candidates: {K_candidates}")
    logger.info(f"Min control ratio: {min_ratio}× treated (CBPS stability requirement)")

    gates = load_lambda_hard_gates(args.config_path)
    logger.info(
        "Loaded hard gates from config: max_smd=%.3f top10_share=%.3f max_weight=%.3f ess_frac=%.3f ess_mult_treated=%.3f",
        gates['max_smd'], gates['top10_share'], gates['max_weight'], gates['ess_frac'], gates['ess_mult_treated']
    )
    
    results = select_optimal_k(
        similarities,
        embeddings_df,
        K_candidates,
        year=year,
        min_ratio=min_ratio,
        force_recompute=args.force_recompute,
        max_workers=args.max_workers,
        output_tag=args.output_tag,
        experiment_name=args.experiment_name,
        analysis_base_dir=args.analysis_base_dir,
        save_full_weights=args.save_full_weights,
        target_pool_proportions=args.target_pool_proportions,
        include_full_pool=not args.no_full_pool,
        gates=gates,
    )    
    if results is None:
        return 1   
    # Save results
    output_dir = K_SELECTION_DIR / args.experiment_name / str(year)
    output_dir.mkdir(parents=True, exist_ok=True)
    elbow_name = f"k_selection_elbow{tag_suffix}.csv"
    rmse_name = f"k_selection_rmse{tag_suffix}.csv"
    pool_name = f"k_selection_pool_diagnostics{tag_suffix}.csv"
    mapping_name = f"k_selection_pool_target_mapping{tag_suffix}.csv"
    summary_name = f"k_selection_summary{tag_suffix}.json"
    results['elbow_metrics'].to_csv(output_dir / elbow_name, index=False)
    results['rmse_results'].to_csv(output_dir / rmse_name, index=False)
    if 'pool_diagnostics' in results and isinstance(results['pool_diagnostics'], pd.DataFrame):
        results['pool_diagnostics'].to_csv(output_dir / pool_name, index=False)
    if 'pool_target_mapping' in results and isinstance(results['pool_target_mapping'], pd.DataFrame):
        results['pool_target_mapping'].to_csv(output_dir / mapping_name, index=False)

    summary_payload = {
        'year': int(year),
        'optimal_K': int(results['optimal_K']),
        'optimal_rmse': float(results['optimal_rmse']),
        'selection_mode': results.get('selection_mode', 'unknown'),
        'treated_subsample_frac': subsample_frac,
        'random_seed': args.random_seed,
        'output_tag': args.output_tag,
        'k_values': [int(k) for k in K_candidates],
        'target_pool_proportions': [float(x) for x in args.target_pool_proportions],
        'include_full_pool': not args.no_full_pool,
        'hard_gates': gates,
    }
    with open(output_dir / summary_name, 'w', encoding='utf-8') as f:
        json.dump(summary_payload, f, indent=2)

    logger.info(f"\nResults saved to {output_dir}/")
    logger.info(f"  - {elbow_name} (similarity by K)")
    logger.info(f"  - {rmse_name} (RMSPE by K)")
    logger.info(f"  - {pool_name} (realized donor pool and support diagnostics)")
    logger.info(f"  - {mapping_name} (target proportion to realized K mapping)")
    logger.info(f"  - {summary_name} (selection summary)")
    
    logger.info("\n" + "="*80)
    logger.info("NEXT STEPS:")
    logger.info("="*80)
    logger.info(f"1. Run CBPS with optimal K={results['optimal_K']} for {year}")
    logger.info(f"   → Rscript run_cbps_with_selected_controls.R {year} k{results['optimal_K']}")
    logger.info(f"2. Create trajectory plots (validate parallel trends)")
    logger.info(f"   → Rscript figures/plot_trajectory.R --year {year} --k {results['optimal_K']}")
    logger.info(f"3. If validation passes, scale to all years (2005-2020)")
    logger.info("="*80)
    
    return 0
if __name__ == "__main__":
    sys.exit(main())
