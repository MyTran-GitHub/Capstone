"""
Compare Embedding-Based Control Selection vs Baseline (Full Pool CBPS)

Tests three approaches:
1. Baseline: Full control pool + CBPS (Wu 2023 method)
2. Embedding K-nearest: Reduced pool + CBPS (our method)
3. Random K: Random subset + CBPS (placebo test)

Compares:
- Pre-treatment RMSPE (lower = better fit)
- Covariate balance (SMD on actual covariates)
- Computational time
"""

import sys
from pathlib import Path


import numpy as np
import pandas as pd
import logging
import subprocess
import tempfile
import time
from typing import Dict

# Setup logging
logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(asctime)s - %(message)s')
logger = logging.getLogger(__name__)


def run_cbps_r(year: int, 
               selected_units: list,
               output_prefix: str,
               train_years: list,
               test_years: list) -> Dict:
    """
    Call R CBPS script with selected control units
    
    Returns:
        Dictionary with RMSE, balance metrics, timing
    """
    
    # Create temporary CSV with selected units
    with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False) as f:
        temp_csv = f.name
        pd.DataFrame({'unit': selected_units}).to_csv(temp_csv, index=False)
    
    try:
        start_time = time.time()
        
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
        
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent.parent
        )
        
        elapsed_time = time.time() - start_time
        
        if result.returncode != 0:
            logger.error(f"R script failed: {result.stderr}")
            raise RuntimeError(f"R CBPS failed: {result.stderr}")
        
        # Parse results
        metrics_file = Path(f"tests/results/cbps_integration/cbps_metrics_{output_prefix}_{year}.csv")
        
        if not metrics_file.exists():
            raise FileNotFoundError(f"Metrics file not created: {metrics_file}")
        
        metrics = pd.read_csv(metrics_file)
        
        return {
            'rmse': float(metrics['rmse_test'].iloc[0]),
            'rmse_train': float(metrics['rmse_train'].iloc[0]),
            'max_balance_std': float(metrics['max_balance_std'].iloc[0]),
            'mean_balance_std': float(metrics['mean_balance_std'].iloc[0]),
            'n_treated': int(metrics['n_treated'].iloc[0]),
            'n_control': int(metrics['n_control'].iloc[0]),
            'converged': bool(metrics['converged'].iloc[0]),
            'elapsed_time': elapsed_time
        }
        
    finally:
        Path(temp_csv).unlink(missing_ok=True)


def get_k_nearest_controls(embeddings_df: pd.DataFrame, K: int) -> list:
    """
    Select K nearest controls for each treated pixel using cosine similarity
    Returns list of unique control unit IDs
    """
    from select_optimal_k import compute_all_similarities, get_k_nearest_union
    
    logger.info(f"Computing K={K} nearest neighbors...")
    
    similarities = compute_all_similarities(embeddings_df)
    selected_indices = get_k_nearest_union(similarities, K)
    selected_units = embeddings_df.iloc[list(selected_indices)]['unit'].tolist()
    
    logger.info(f"  Selected {len(selected_units)} unique control pixels")
    
    return selected_units


def get_random_controls(embeddings_df: pd.DataFrame, K: int, n_treated: int) -> list:
    """
    Randomly select K controls per treated pixel (placebo test)
    """
    logger.info(f"Randomly selecting K={K} controls per treated pixel...")
    
    control_df = embeddings_df[embeddings_df['treated'] == 0]
    
    # Sample K×n_treated with replacement (to match embedding strategy)
    n_samples = K * n_treated
    random_controls = control_df.sample(n=min(n_samples, len(control_df)), 
                                       replace=True,
                                       random_state=42)
    
    selected_units = random_controls['unit'].unique().tolist()
    logger.info(f"  Selected {len(selected_units)} unique control pixels")
    
    return selected_units


def main():
    """Run comparison: baseline vs embedding vs random"""
    
    # Configuration
    year = 2019
    K = 30  # Can be changed based on optimal K selection results
    embeddings_file = Path("embeddings/embeddings_2019.csv")
    
    # Fallback to test data if production data not found
    if not embeddings_file.exists():
        embeddings_file = Path("tests/data/11SLA_embeddings_2019_with_treatment.csv")
    
    # Cross-validation split
    train_years = list(range(2000, 2011))
    test_years = list(range(2011, 2016))
    
    logger.info("="*80)
    logger.info(f"COMPARISON: EMBEDDING-BASED vs BASELINE (Year {year})")
    logger.info("="*80)
    logger.info(f"K = {K}")
    logger.info(f"Train: {train_years[0]}-{train_years[-1]}")
    logger.info(f"Test: {test_years[0]}-{test_years[-1]}")
    logger.info("")
    
    # Load embeddings
    if not embeddings_file.exists():
        logger.error(f"Embeddings file not found: {embeddings_file}")
        return 1
    
    embeddings_df = pd.read_csv(embeddings_file)
    n_treated = (embeddings_df['treated'] == 1).sum()
    n_total_controls = (embeddings_df['treated'] == 0).sum()
    
    logger.info(f"Data loaded: {len(embeddings_df)} pixels")
    logger.info(f"  Treated: {n_treated}")
    logger.info(f"  Total controls: {n_total_controls}")
    logger.info("")
    
    results = []
    
    # 1. BASELINE: Full control pool + CBPS
    logger.info("="*80)
    logger.info("1. BASELINE (Full Pool + CBPS)")
    logger.info("="*80)
    
    try:
        all_controls = embeddings_df[embeddings_df['treated'] == 0]['unit'].tolist()
        baseline_result = run_cbps_r(year, all_controls, "baseline", train_years, test_years)
        
        results.append({
            'method': 'baseline_full_pool',
            'K': n_total_controls,
            'n_controls_selected': len(all_controls),
            'pool_reduction_pct': 0.0,
            **baseline_result
        })
        
        logger.info(f"✓ Baseline completed")
        logger.info(f"  RMSE (test): {baseline_result['rmse']:.4f}")
        logger.info(f"  Balance (max): {baseline_result['max_balance_std']:.3f}")
        logger.info(f"  Time: {baseline_result['elapsed_time']:.1f}s")
        
    except Exception as e:
        logger.error(f"✗ Baseline failed: {e}")
    
    logger.info("")
    
    # 2. EMBEDDING K-NEAREST: Reduced pool + CBPS
    logger.info("="*80)
    logger.info(f"2. EMBEDDING K-NEAREST (K={K})")
    logger.info("="*80)
    
    try:
        embedding_controls = get_k_nearest_controls(embeddings_df, K)
        embedding_result = run_cbps_r(year, embedding_controls, f"embedding_k{K}", train_years, test_years)
        
        pool_reduction = 100 * (1 - len(embedding_controls) / n_total_controls)
        
        results.append({
            'method': f'embedding_k{K}',
            'K': K,
            'n_controls_selected': len(embedding_controls),
            'pool_reduction_pct': pool_reduction,
            **embedding_result
        })
        
        logger.info(f"✓ Embedding K={K} completed")
        logger.info(f"  RMSE (test): {embedding_result['rmse']:.4f}")
        logger.info(f"  Balance (max): {embedding_result['max_balance_std']:.3f}")
        logger.info(f"  Pool reduction: {pool_reduction:.1f}%")
        logger.info(f"  Time: {embedding_result['elapsed_time']:.1f}s")
        
    except Exception as e:
        logger.error(f"✗ Embedding method failed: {e}")
    
    logger.info("")
    
    # 3. RANDOM K: Random subset + CBPS (placebo)
    logger.info("="*80)
    logger.info(f"3. RANDOM K={K} (Placebo)")
    logger.info("="*80)
    
    try:
        random_controls = get_random_controls(embeddings_df, K, n_treated)
        random_result = run_cbps_r(year, random_controls, f"random_k{K}", train_years, test_years)
        
        pool_reduction = 100 * (1 - len(random_controls) / n_total_controls)
        
        results.append({
            'method': f'random_k{K}',
            'K': K,
            'n_controls_selected': len(random_controls),
            'pool_reduction_pct': pool_reduction,
            **random_result
        })
        
        logger.info(f"✓ Random K={K} completed")
        logger.info(f"  RMSE (test): {random_result['rmse']:.4f}")
        logger.info(f"  Balance (max): {random_result['max_balance_std']:.3f}")
        logger.info(f"  Pool reduction: {pool_reduction:.1f}%")
        logger.info(f"  Time: {random_result['elapsed_time']:.1f}s")
        
    except Exception as e:
        logger.error(f"✗ Random method failed: {e}")
    
    # Save comparison results
    if results:
        results_df = pd.DataFrame(results)
        output_file = Path("tests/results/cbps_integration/comparison_results.csv")
        results_df.to_csv(output_file, index=False)
        
        logger.info("")
        logger.info("="*80)
        logger.info("COMPARISON SUMMARY")
        logger.info("="*80)
        logger.info(f"\n{results_df.to_string(index=False)}\n")
        
        # Compute improvements
        if len(results) >= 2:
            baseline_rmse = results_df[results_df['method'] == 'baseline_full_pool']['rmse'].iloc[0]
            
            for _, row in results_df[results_df['method'] != 'baseline_full_pool'].iterrows():
                improvement = 100 * (baseline_rmse - row['rmse']) / baseline_rmse
                logger.info(f"{row['method']}: {improvement:+.1f}% RMSE vs baseline")
        
        logger.info(f"\nResults saved to: {output_file}")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
