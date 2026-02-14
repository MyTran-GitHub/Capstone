"""
Optimal K Selection for Embedding-Based Control Selection
Uses pre-treatment RMSPE cross-validation to select K
"""

import sys
from pathlib import Path


import numpy as np
import pandas as pd
import logging
import subprocess
import tempfile
from typing import Dict, List, Tuple, Set

# Setup logging
logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(asctime)s - %(name)s - %(message)s')
logger = logging.getLogger(__name__)


def cosine_similarity(x: np.ndarray, y: np.ndarray) -> float:
    """Compute cosine similarity between two vectors"""
    return np.dot(x, y) / (np.linalg.norm(x) * np.linalg.norm(y))


def compute_all_similarities(embeddings_df: pd.DataFrame) -> Dict[int, np.ndarray]:
    """
    Compute similarity from each treated pixel to all control pixels
    
    Returns:
        Dictionary: treated_idx -> array of (control_idx, similarity) sorted by similarity descending
    """
    logger.info("Computing similarities for all treated-control pairs...")
    
    treated_indices = embeddings_df[embeddings_df['treated'] == 1].index.tolist()
    control_indices = embeddings_df[embeddings_df['treated'] == 0].index.tolist()
    
    embedding_cols = [col for col in embeddings_df.columns if col.startswith('band_')]
    embeddings_matrix = embeddings_df[embedding_cols].values
    
    similarities = {}
    
    for t_idx in treated_indices:
        treated_emb = embeddings_matrix[t_idx]
        sims = []
        
        for c_idx in control_indices:
            control_emb = embeddings_matrix[c_idx]
            sim = cosine_similarity(treated_emb, control_emb)
            sims.append((c_idx, sim))
        
        # Sort by similarity descending
        sims.sort(key=lambda x: x[1], reverse=True)
        similarities[t_idx] = np.array(sims)
    
    logger.info(f"Computed similarities for {len(treated_indices)} treated pixels")
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


def split_pretreatment_data(df: pd.DataFrame, 
                            train_years: List[int],
                            test_years: List[int]) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Split covariates by pre-treatment years (for cross-validation)
    This is a placeholder - actual implementation depends on your data structure
    """
    # TODO: Implement based on your actual covariate structure
    # Should return X_train, X_test split by years
    pass


def run_cbps_crossval(embeddings_df: pd.DataFrame,
                      selected_controls: Set[int],
                      year: int,
                      output_prefix: str,
                      train_years: List[int],
                      test_years: List[int]) -> Dict:
    """
    Run CBPS with cross-validation to compute RMSPE
    
    Calls R script (run_cbps_with_selected_controls.R) that:
    1. Loads analysis_treated{year}_conifer.RDS
    2. Filters to treated + selected controls
    3. Runs CBPS
    4. Returns balance metrics and RMSE
    
    Args:
        embeddings_df: DataFrame with embeddings and treated labels
        selected_controls: Set of control pixel indices to use
        year: Treatment year (e.g., 2019)
        output_prefix: Prefix for output files (e.g., "k10")
        train_years: List of years for training period
        test_years: List of years for test period
    
    Returns:
        Dictionary with RMSE, balance metrics, convergence status
    """
    
    # Get unit IDs for selected controls
    selected_units = embeddings_df.iloc[list(selected_controls)]['unit'].tolist()
    
    # Create temporary CSV with selected units
    with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False) as f:
        temp_csv = f.name
        pd.DataFrame({'unit': selected_units}).to_csv(temp_csv, index=False)
    
    try:
        # Call R script
        r_script = "run_cbps_with_selected_controls.R"
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
            cwd=Path(__file__).parent.parent.parent  # Run from Capstone/ root
        )
        
        if result.returncode != 0:
            logger.error(f"R script failed with return code {result.returncode}")
            logger.error(f"STDERR: {result.stderr}")
            raise RuntimeError(f"R CBPS script failed: {result.stderr}")
        
        # Parse R output
        metrics_file = Path(f"tests/results/cbps_integration/cbps_metrics_{output_prefix}_{year}.csv")
        
        if not metrics_file.exists():
            raise FileNotFoundError(f"CBPS metrics file not created: {metrics_file}")
        
        metrics = pd.read_csv(metrics_file)
        
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


def select_optimal_k(similarities: Dict[int, np.ndarray],
                    embeddings_df: pd.DataFrame,
                    K_candidates: List[int],
                    year: int,
                    min_ratio: int = 10) -> Dict:
    """
    Complete K selection pipeline
    
    Args:
        similarities: Pre-computed similarity matrix
        embeddings_df: DataFrame with embeddings and treatment labels
        K_candidates: List of K values to test
        year: Treatment year (e.g., 2019)
        min_ratio: Minimum control pool size as multiple of treated count
    
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
    
    # Step 1: Elbow analysis
    elbow_df = compute_elbow_metrics(similarities, K_candidates)
    
    # Step 2: Check pool sizes
    valid_K = check_pool_sizes(similarities, K_candidates, n_treated, min_ratio)
    
    if not valid_K:
        logger.error("No K values produce large enough control pools!")
        logger.error(f"Try smaller min_ratio (current: {min_ratio}) or larger K values")
        return None
    
    logger.info(f"\nValid K values for RMSPE testing: {valid_K}")
    
    # Step 3: RMSPE cross-validation
    logger.info(f"\nStep 3: Running CBPS + RMSPE cross-validation...")
    
    rmse_results = []
    for K in valid_K:
        selected_controls = get_k_nearest_union(similarities, K)
        output_prefix = f"k{K}"
        
        logger.info(f"  Testing K={K} ({len(selected_controls)} controls)...")
        
        try:
            result = run_cbps_crossval(
                embeddings_df,
                selected_controls,
                year=year,
                output_prefix=output_prefix,
                train_years=list(range(2000, 2011)),
                test_years=list(range(2011, 2016))
            )
            
            rmse_results.append({
                'K': K,
                'pool_size': len(selected_controls),
                'rmse': result['rmse'],
                'rmse_train': result['rmse_train'],
                'max_balance_std': result['max_balance_std'],
                'mean_balance_std': result['mean_balance_std'],
                'convergence': result['convergence'],
                'n_controls_used': result['n_controls_used']
            })
            
            logger.info(f"    ✓ RMSE={result['rmse']:.4f}, "
                       f"balance={result['max_balance_std']:.3f}, "
                       f"converged={result['convergence'] == 1}")
        
        except Exception as e:
            logger.error(f"    ✗ Failed for K={K}: {e}")
            continue
    
    if not rmse_results:
        logger.error("All K values failed CBPS cross-validation!")
        return None
    
    rmse_df = pd.DataFrame(rmse_results)
    
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
    """Run optimal K selection on test data"""
    
    # Load embeddings with treatment labels
    # For production, use embeddings/ output directory
    embeddings_file = Path("embeddings/embeddings_2019.csv")
    year = 2019  # Treatment year
    
    # Fallback to test data if production data not found
    if not embeddings_file.exists():
        embeddings_file = Path("tests/data/11SLA_embeddings_2019_with_treatment.csv")
    
    if not embeddings_file.exists():
        logger.error(f"Embeddings file not found: {embeddings_file}")
        return 1
    
    logger.info(f"Loading embeddings from {embeddings_file}...")
    embeddings_df = pd.read_csv(embeddings_file)
    
    logger.info(f"Loaded {len(embeddings_df)} pixels")
    logger.info(f"  Treated: {(embeddings_df['treated'] == 1).sum()}")
    logger.info(f"  Control: {(embeddings_df['treated'] == 0).sum()}")
    
    # Step 1: Compute all similarities (one-time computation)
    similarities = compute_all_similarities(embeddings_df)
    
    # Step 2-4: Select optimal K
    K_candidates = [10, 20, 30, 50, 75, 100]
    min_ratio = 10  # controls ≥ 10 × treated
    
    results = select_optimal_k(
        similarities,
        embeddings_df,
        K_candidates,
        year=year,
        min_ratio=min_ratio
    )
    
    if results is None:
        return 1
    
    # Save results
    output_dir = Path("tests/results/k_selection")
    output_dir.mkdir(parents=True, exist_ok=True)
    
    results['elbow_metrics'].to_csv(output_dir / "k_selection_elbow.csv", index=False)
    results['rmse_results'].to_csv(output_dir / "k_selection_rmse.csv", index=False)
    
    logger.info(f"\nResults saved to {output_dir}/")
    logger.info(f"  - k_selection_elbow.csv (similarity by K)")
    logger.info(f"  - k_selection_rmse.csv (RMSPE by K)")
    
    logger.info("\n" + "="*80)
    logger.info("NEXT STEPS:")
    logger.info("="*80)
    logger.info(f"1. Run full comparison: embedding K={results['optimal_K']} vs full pool vs random")
    logger.info(f"2. Compare ATT estimates, CI width, and covariate balance")
    logger.info(f"3. Implement for all years (2005-2020)")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
