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
from typing import Dict, List, Tuple, Set
from concurrent.futures import ThreadPoolExecutor, as_completed
from functools import partial

# Setup logging
logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(asctime)s - %(name)s - %(message)s')
logger = logging.getLogger(__name__)


def compute_all_similarities(embeddings_df: pd.DataFrame) -> Dict[int, np.ndarray]:
    """
    Compute similarity from each treated pixel to all control pixels (VECTORIZED)
    Returns:
        Dictionary: treated_idx -> array of (control_idx, similarity) sorted by similarity descending
    """
    logger.info("Computing similarities for all treated-control pairs (vectorized)...")
    
    # Import sklearn for vectorized cosine similarity
    try:
        from sklearn.metrics.pairwise import cosine_similarity as sklearn_cosine_similarity
    except ImportError:
        logger.error("sklearn (scikit-learn) is required for vectorized similarity computation")
        logger.error("Install with: pip install scikit-learn")
        raise ImportError("sklearn not found. Install with: pip install scikit-learn")
    
    # Get masks and indices
    treated_mask = embeddings_df['treated'] == 1
    control_mask = embeddings_df['treated'] == 0
    treated_indices = embeddings_df[treated_mask].index.tolist()
    control_indices = embeddings_df[control_mask].index.tolist()
    
    # Extract embedding columns
    embedding_cols = [col for col in embeddings_df.columns if col.startswith('band_')]
    
    # Extract treated and control embeddings
    treated_embeddings = embeddings_df.loc[treated_mask, embedding_cols].values  # (n_treated, n_dims)
    control_embeddings = embeddings_df.loc[control_mask, embedding_cols].values  # (n_control, n_dims)
    
    # DEBUG: Check for inf/nan in matrices
    nan_in_treated = np.isnan(treated_embeddings).sum()
    inf_in_treated = np.isinf(treated_embeddings).sum()
    nan_in_control = np.isnan(control_embeddings).sum()
    inf_in_control = np.isinf(control_embeddings).sum()
    
    if nan_in_treated > 0 or inf_in_treated > 0:
        logger.warning(f"  Treated embeddings contain {nan_in_treated} NaN and {inf_in_treated} inf!")
    if nan_in_control > 0 or inf_in_control > 0:
        logger.warning(f"  Control embeddings contain {nan_in_control} NaN and {inf_in_control} inf!")
    
    # VECTORIZED: Compute all similarities at once
    # Output shape: (n_treated, n_control)
    logger.info(f"  Computing {len(treated_indices)} × {len(control_indices)} = {len(treated_indices) * len(control_indices):,} similarities...")
    similarity_matrix = sklearn_cosine_similarity(treated_embeddings, control_embeddings)
    
    # Verify matrix dimensions match expectations
    expected_shape = (len(treated_indices), len(control_indices))
    if similarity_matrix.shape != expected_shape:
        raise ValueError(f"Similarity matrix shape {similarity_matrix.shape} doesn't match expected {expected_shape}")
    
    # Check for NaN in results
    nan_count = np.isnan(similarity_matrix).sum()
    if nan_count > 0:
        logger.warning(f"  {nan_count} NaN similarities detected, replacing with 0")
        similarity_matrix = np.nan_to_num(similarity_matrix, nan=0.0)
    
    # Convert to same output format as original function
    # Dictionary: treated_idx -> array of (control_idx, similarity) sorted descending
    similarities = {}
    for i, t_idx in enumerate(treated_indices):
        # Get similarities for this treated pixel (row i)
        sims = [(control_indices[j], similarity_matrix[i, j]) 
                for j in range(len(control_indices))]
        # Sort by similarity descending
        sims.sort(key=lambda x: x[1], reverse=True)
        similarities[t_idx] = np.array(sims)
    
    logger.info(f"  ✓ Computed similarities for {len(treated_indices)} treated pixels (vectorized)")
    return similarities


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
    logger.info(f"\nStep 1b: Filtering by elbow (drop threshold: {drop_threshold})...")
    elbow_df = elbow_df.sort_values('K')
    similarities = elbow_df['mean_similarity'].values
    K_values = elbow_df['K'].values
    # Compute marginal changes (similarity decreases as K increases)
    marginal_gains = np.diff(similarities)  # Will be negative (similarity decreases)
    # Find first K where similarity drops sharply (magnitude > threshold)
    elbow_idx = None
    for i, gain in enumerate(marginal_gains):
        logger.info(f"  K={K_values[i]} → K={K_values[i+1]}: Δsim={gain:.4f}")
        # Check if drop is LARGE (more negative than -threshold)
        if gain < -drop_threshold:
            elbow_idx = i + 1  # Stop BEFORE the sharp drop
            logger.info(f"  ✂ Elbow detected at K={K_values[i+1]} (Δsim < -{drop_threshold})")
            break
    # If no elbow found, keep all K (no sharp drop detected)
    if elbow_idx is None:
        logger.info(f"  ℹ No sharp drop detected - keeping all K candidates")
        filtered_K = K_values.tolist()
    else:
        filtered_K = K_values[:elbow_idx].tolist()
    logger.info(f"  Kept K values: {filtered_K}")
    return filtered_K


def check_pool_sizes(similarities: Dict[int, np.ndarray],
                     K_candidates: List[int],
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
    for K in K_candidates:
        selected_controls = get_k_nearest_union(similarities, K)
        pool_size = len(selected_controls)
        reduction_pct = 100 * (1 - pool_size / 50000)  # assuming ~50k baseline 
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
                      test_years: List[int]) -> Dict:
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
    from config import CBPS_INTEGRATION_DIR
    output_dir = CBPS_INTEGRATION_DIR / str(year)
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
        cmd = [
            "Rscript",
            r_script,
            str(year),
            temp_csv,
            output_prefix,
            str(train_years[0]),
            str(train_years[-1]),
            str(test_years[0]),
            str(test_years[-1])
        ]
        logger.info(f"    Calling R CBPS: {' '.join(cmd)}")
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent  # Run from Capstone/ root (Embeddings/../)
        )
        if result.returncode != 0:
            logger.error(f"R script failed with return code {result.returncode}")
            logger.error(f"STDOUT: {result.stdout}")
            logger.error(f"STDERR: {result.stderr}")
            raise RuntimeError(f"R CBPS script failed: {result.stderr or result.stdout}")
        # Parse R output
        from config import CBPS_INTEGRATION_DIR
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
            'max_balance_std': float(metrics['max_balance_std'].iloc[0]),
            'mean_balance_std': float(metrics['mean_balance_std'].iloc[0]),
            'convergence': int(metrics['converged'].iloc[0]),
            'n_controls_used': int(metrics['n_control'].iloc[0])
        }
    finally:
        # Clean up temporary file
        Path(temp_csv).unlink(missing_ok=True)


def compute_k_value(K: int, similarities: Dict[int, np.ndarray], 
                    embeddings_df: pd.DataFrame, year: int,
                    train_years: List[int], test_years: List[int]) -> Dict:
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
        output_prefix = f"k{K}"
        
        # Run CBPS cross-validation
        result = run_cbps_crossval(
            embeddings_df,
            selected_controls,
            year=year,
            output_prefix=output_prefix,
            train_years=train_years,
            test_years=test_years
        )
        
        # Return success result
        return {
            'K': K,
            'pool_size': len(selected_controls),
            'rmse': result['rmse'],
            'rmse_train': result['rmse_train'],
            'max_balance_std': result['max_balance_std'],
            'mean_balance_std': result['mean_balance_std'],
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
                    force_recompute: bool = False,
                    max_workers: int = 6) -> Dict:
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
    # Step 1: Elbow analysis (compute similarity for each K)
    elbow_df = compute_elbow_metrics(similarities, K_candidates)
    # Step 1b: Filter by elbow (drop K beyond similarity plateau)
    K_after_elbow = filter_by_elbow(elbow_df, drop_threshold=0.02)
    if not K_after_elbow:
        logger.error("Elbow filtering removed all K candidates!")
        logger.error("Try smaller drop_threshold or check similarity computation")
        return None
    # Step 2: Check pool sizes (only on K passing elbow filter)
    valid_K = check_pool_sizes(similarities, K_after_elbow, n_treated, min_ratio)
    if not valid_K:
        logger.error("No K values produce large enough control pools!")
        logger.error(f"Try smaller min_ratio (current: {min_ratio}) or larger K values")
        return None
    logger.info(f"\nValid K values for RMSPE testing: {valid_K}")
    
    # Step 3a: Determine which K values need computation (respect cache)
    logger.info(f"\nStep 3: Running CBPS + RMSPE cross-validation...")
    from config import CBPS_INTEGRATION_DIR
    
    K_to_compute = []
    K_cached = []
    
    for K in valid_K:
        output_prefix = f"k{K}"
        metrics_file = CBPS_INTEGRATION_DIR / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
        
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
            output_prefix = f"k{K}"
            metrics_file = CBPS_INTEGRATION_DIR / str(year) / f"cbps_metrics_{output_prefix}_{year}.csv"
            try:
                metrics = pd.read_csv(metrics_file)
                selected_controls = get_k_nearest_union(similarities, K)
                rmse_results.append({
                    'K': K,
                    'pool_size': len(selected_controls),
                    'rmse': float(metrics['rmse_test'].iloc[0]),
                    'rmse_train': float(metrics['rmse_train'].iloc[0]),
                    'max_balance_std': float(metrics['max_balance_std'].iloc[0]),
                    'mean_balance_std': float(metrics['mean_balance_std'].iloc[0]),
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
        
        # Determine number of parallel workers (limit to avoid overwhelming system)
        n_workers = min(len(K_to_compute), max_workers)  # Don't spawn more workers than K values
        logger.info(f"  Using {n_workers} parallel workers")
        
        # Create partial function with common arguments bound
        compute_func = partial(
            compute_k_value,
            similarities=similarities,
            embeddings_df=embeddings_df,
            year=year,
            train_years=list(range(2000, 2011)),
            test_years=list(range(2011, 2016))
        )
        
        # Execute in parallel using ThreadPoolExecutor
        # (Threads work well here since bottleneck is I/O-bound R subprocess calls)
        with ThreadPoolExecutor(max_workers=n_workers) as executor:
            # Submit all K values for computation
            future_to_k = {executor.submit(compute_func, K): K for K in K_to_compute}
            
            # Collect results as they complete
            completed = 0
            for future in as_completed(future_to_k):
                K = future_to_k[future]
                completed += 1
                
                try:
                    result = future.result()
                    
                    if result['success']:
                        # Extract metrics (exclude success/error flags)
                        metrics = {k: v for k, v in result.items() 
                                  if k not in ['success', 'error']}
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
    
    # Summary of what succeeded/failed
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
    # Step 4: Select optimal K
    optimal_idx = rmse_df['rmse'].idxmin()
    optimal_K = rmse_df.loc[optimal_idx, 'K']
    optimal_rmse = rmse_df.loc[optimal_idx, 'rmse']   
    logger.info(f"\n{'='*80}")
    logger.info(f"OPTIMAL K SELECTED: {optimal_K}")
    logger.info(f"Pre-treatment RMSE: {optimal_rmse:.4f}")
    logger.info(f"Control pool size: {rmse_df.loc[optimal_idx, 'pool_size']}")
    logger.info(f"{'='*80}\n")    
    return {
        'optimal_K': int(optimal_K),
        'optimal_rmse': float(optimal_rmse),
        'elbow_metrics': elbow_df,
        'rmse_results': rmse_df,
        'valid_K_values': valid_K,
        'all_similarities': similarities
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
        default=[10, 20, 30, 50, 75, 100],
        help='K candidates to test (default: 10 20 30 50 75 100)'
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
    
    # Step 1: Compute all similarities (one-time computation with caching)
    from config import K_SELECTION_DIR
    similarities_cache = K_SELECTION_DIR / str(year) / f"similarities_cache_{year}.npy"
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
    #   - Lower bound (10): Minimum for statistical stability (K-NN standard)
    #   - Upper bound (100): Practical limit before approaching full pool (due to union effect)
    #   - Spacing: Dense at low K (10→20→30) where differences matter most
    #              Sparse at high K (50→75→100) for diminishing returns
    # Union effect: K per treated × ~200 treated → ~5-10k unique controls after overlap
    # Expected result: K ∈ [20, 50] after elbow + pool size filtering
    K_candidates = args.k_values
    min_ratio = args.min_ratio    
    logger.info(f"K candidates: {K_candidates}")
    logger.info(f"Min control ratio: {min_ratio}× treated (CBPS stability requirement)")
    
    results = select_optimal_k(
        similarities,
        embeddings_df,
        K_candidates,
        year=year,
        min_ratio=min_ratio,
        force_recompute=args.force_recompute,
        max_workers=args.max_workers
    )    
    if results is None:
        return 1   
    # Save results
    from config import K_SELECTION_DIR, CBPS_INTEGRATION_DIR
    output_dir = K_SELECTION_DIR / str(year)
    output_dir.mkdir(parents=True, exist_ok=True)    
    results['elbow_metrics'].to_csv(output_dir / "k_selection_elbow.csv", index=False)
    results['rmse_results'].to_csv(output_dir / "k_selection_rmse.csv", index=False)    
    logger.info(f"\nResults saved to {output_dir}/")
    logger.info(f"  - k_selection_elbow.csv (similarity by K)")
    logger.info(f"  - k_selection_rmse.csv (RMSPE by K)")
    
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
