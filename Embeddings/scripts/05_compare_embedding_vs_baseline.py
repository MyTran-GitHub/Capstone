"""
Compare Embedding-Based Control Selection vs Baseline (Full Pool CBPS)

EFFICIENT VERSION: Reads pre-computed results instead of re-running CBPS

Compares:
- Pre-treatment RMSE (lower = better pre-treatment fit)
- Covariate balance (standardized mean difference)
- Pool size reduction
- Computational efficiency

Data Sources:
1. Baseline: Results from implement_cbps.R + weighted_outcome_analysis.R (already run)
2. Embedding: Results from select_optimal_k.py (already computed for all K values)

This script just reads, compares, and visualizes - no expensive re-computation!
"""

import sys
from pathlib import Path

# Add parent directory to path for config import
sys.path.insert(0, str(Path(__file__).parent.parent))

import argparse

import numpy as np
import pandas as pd
import logging
import subprocess
from typing import Dict, Optional

# Setup logging
logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(asctime)s - %(message)s')
logger = logging.getLogger(__name__)


def load_baseline_metrics(year: int, 
                           train_start: int, 
                           train_end: int, 
                           test_start: int, 
                           test_end: int) -> Optional[Dict]:
    """
    Load or compute baseline metrics from existing outputs
    
    First tries to load pre-computed metrics, otherwise computes from
    existing weighted_outcome_analysis.R outputs
    
    Returns:
        Dictionary with baseline metrics or None if failed
    """
    from config import CBPS_INTEGRATION_DIR
    
    # Try to load pre-computed metrics
    metrics_file = CBPS_INTEGRATION_DIR / str(year) / f"baseline_metrics_{year}.csv"
    
    if metrics_file.exists():
        logger.info(f"  Loading pre-computed baseline metrics from {metrics_file}")
        metrics = pd.read_csv(metrics_file)
        return {
            'method': 'baseline_full_pool',
            'rmse_train': float(metrics['rmse_train'].iloc[0]),
            'rmse_test': float(metrics['rmse_test'].iloc[0]),
            'n_treated': int(metrics['n_treated'].iloc[0]) if not pd.isna(metrics['n_treated'].iloc[0]) else None,
            'n_control': int(metrics['n_control'].iloc[0]) if not pd.isna(metrics['n_control'].iloc[0]) else None,
        }
    
    # Otherwise compute from existing outputs
    logger.info(f"  Computing baseline RMSE from existing weighted_outcome_analysis.R outputs...")
    
    r_script = "scripts/utils/compute_baseline_rmse_from_existing.R"
    cmd = [
        "Rscript",
        r_script,
        str(year),
        str(train_start),
        str(train_end),
        str(test_start),
        str(test_end)
    ]
    
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )
        
        if result.returncode != 0:
            logger.error(f"  Baseline RMSE computation failed: {result.stderr}")
            return None
        
        # Load the newly created metrics file
        if metrics_file.exists():
            metrics = pd.read_csv(metrics_file)
            return {
                'method': 'baseline_full_pool',
                'rmse_train': float(metrics['rmse_train'].iloc[0]),
                'rmse_test': float(metrics['rmse_test'].iloc[0]),
                'n_treated': int(metrics['n_treated'].iloc[0]) if not pd.isna(metrics['n_treated'].iloc[0]) else None,
                'n_control': int(metrics['n_control'].iloc[0]) if not pd.isna(metrics['n_control'].iloc[0]) else None,
            }
        else:
            logger.error(f"  Metrics file not created: {metrics_file}")
            return None
            
    except Exception as e:
        logger.error(f"  Failed to compute baseline metrics: {e}")
        return None


def load_embedding_metrics(year: int, K: int) -> Optional[Dict]:
    """
    Load embedding method metrics (already computed by select_optimal_k.py)
    
    Returns:
        Dictionary with embedding metrics or None if not found
    """
    from config import CBPS_INTEGRATION_DIR
    
    metrics_file = CBPS_INTEGRATION_DIR / str(year) / f"cbps_metrics_k{K}_{year}.csv"
    
    if not metrics_file.exists():
        logger.error(f"  Embedding metrics not found: {metrics_file}")
        logger.error(f"  Run select_optimal_k.py first to generate these metrics")
        return None
    
    logger.info(f"  Loading embedding metrics from {metrics_file}")
    metrics = pd.read_csv(metrics_file)
    
    return {
        'method': f'embedding_k{K}',
        'K': K,
        'rmse_train': float(metrics['rmse_train'].iloc[0]),
        'rmse_test': float(metrics['rmse_test'].iloc[0]),
        'max_balance_std': float(metrics['max_balance_std'].iloc[0]),
        'mean_balance_std': float(metrics['mean_balance_std'].iloc[0]),
        'n_treated': int(metrics['n_treated'].iloc[0]),
        'n_control': int(metrics['n_control'].iloc[0]),
        'n_covariates': int(metrics['n_covariates'].iloc[0]),
        'rho': float(metrics['rho'].iloc[0]),
        'converged': bool(metrics['converged'].iloc[0])
    }


def find_optimal_k(year: int) -> Optional[int]:
    """
    Find optimal K from select_optimal_k.py results
    
    Returns:
        Optimal K value or None if not found
    """
    from config import K_SELECTION_DIR
    
    rmse_file = K_SELECTION_DIR / str(year) / "k_selection_rmse.csv"
    
    if not rmse_file.exists():
        logger.error(f"  K selection results not found: {rmse_file}")
        logger.error(f"  Run select_optimal_k.py first")
        return None
    
    rmse_df = pd.read_csv(rmse_file)
    optimal_idx = rmse_df['rmse'].idxmin()
    optimal_K = int(rmse_df.loc[optimal_idx, 'K'])
    
    logger.info(f"  Optimal K from selection: {optimal_K}")
    logger.info(f"  (RMSE = {rmse_df.loc[optimal_idx, 'rmse']:.4f})")
    
    return optimal_K


def load_all_k_metrics(year: int) -> pd.DataFrame:
    """
    Load metrics for all K values tested
    
    Returns:
        DataFrame with metrics for each K
    """
    from config import K_SELECTION_DIR
    
    rmse_file = K_SELECTION_DIR / str(year) / "k_selection_rmse.csv"
    
    if not rmse_file.exists():
        logger.warning(f"  K selection results not found: {rmse_file}")
        return pd.DataFrame()
    
    return pd.read_csv(rmse_file)


def main():
    """
    EFFICIENT comparison workflow:
    1. Find optimal K from select_optimal_k.py results
    2. Load embedding metrics from select_optimal_k.py outputs
    3. Load baseline metrics from existing analysis
    4. Compare and visualize results
    
    This script just READS and COMPARES - no expensive re-computation!
    """
    
    parser = argparse.ArgumentParser(description='Compare embedding vs baseline CBPS')
    parser.add_argument('--year', type=int, default=2019, help='Treatment year')
    parser.add_argument('--K', type=int, default=None, help='Specific K to compare (default: use optimal from selection)')
    parser.add_argument('--train-start', type=int, default=2000)
    parser.add_argument('--train-end', type=int, default=2010)
    parser.add_argument('--test-start', type=int, default=2011)
    parser.add_argument('--test-end', type=int, default=2015)
    parser.add_argument('--show-all-k', action='store_true', help='Show results for all K values tested')
    args = parser.parse_args()
    
    logger.info("="*80)
    logger.info("EMBEDDINGS VS BASELINE COMPARISON")
    logger.info("="*80)
    logger.info(f"Year: {args.year}")
    logger.info(f"Train period: {args.train_start}-{args.train_end}")
    logger.info(f"Test period: {args.test_start}-{args.test_end}")
    logger.info("")
    
    # 1. Find optimal K or use specified K
    if args.K is None:
        logger.info("Finding optimal K from select_optimal_k.py results...")
        optimal_K = find_optimal_k(args.year)
        if optimal_K is None:
            logger.error("Failed to find optimal K. Run select_optimal_k.py first!")
            return 1
    else:
        optimal_K = args.K
        logger.info(f"Using specified K={optimal_K}")
    
    logger.info("")
    
    # 2. Load baseline metrics
    logger.pinfo("="*80)
    logger.info("LOADING BASELINE METRICS")
    logger.info("="*80)
    
    baseline_metrics = load_baseline_metrics(
        year=args.year,
        train_start=args.train_start,
        train_end=args.train_end,
        test_start=args.test_start,
        test_end=args.test_end
    )
    
    if baseline_metrics is None:
        logger.error("Failed to load baseline metrics!")
        logger.error("Make sure you've run implement_cbps.R and weighted_outcome_analysis.R")
        return 1
    
    logger.info(f"✓ Baseline loaded successfully")
    logger.info(f"  RMSE (train): {baseline_metrics['rmse_train']:.4f}")
    logger.info(f"  RMSE (test): {baseline_metrics['rmse_test']:.4f}")
    if baseline_metrics['n_control']:
        logger.info(f"  N control: {baseline_metrics['n_control']:,}")
    logger.info("")
    
    # 3. Load embedding metrics
    logger.info("="*80)
    logger.info(f"LOADING EMBEDDING METRICS (K={optimal_K})")
    logger.info("="*80)
    
    embedding_metrics = load_embedding_metrics(args.year, optimal_K)
    
    if embedding_metrics is None:
        logger.error(f"Failed to load embedding metrics for K={optimal_K}!")
        logger.error("Run select_optimal_k.py first to generate these results")
        return 1
    
    logger.info(f"✓ Embedding metrics loaded successfully")
    logger.info(f"  RMSE (train): {embedding_metrics['rmse_train']:.4f}")
    logger.info(f"  RMSE (test): {embedding_metrics['rmse_test']:.4f}")
    logger.info(f"  Max balance std: {embedding_metrics['max_balance_std']:.4f}")
    logger.info(f"  Mean balance std: {embedding_metrics['mean_balance_std']:.4f}")
    logger.info(f"  N control: {embedding_metrics['n_control']:,}")
    logger.info(f"  Converged: {embedding_metrics['converged']}")
    logger.info("")
    
    # 4. Compare results
    logger.info("="*80)
    logger.info("COMPARISON SUMMARY")
    logger.info("="*80)
    
    comparison = pd.DataFrame({
        'Method': ['Baseline (all controls)', f'Embedding (K={optimal_K})'],
        'N_Control': [
            baseline_metrics['n_control'] if baseline_metrics['n_control'] else 'N/A',
            embedding_metrics['n_control']
        ],
        'RMSE_Train': [baseline_metrics['rmse_train'], embedding_metrics['rmse_train']],
        'RMSE_Test': [baseline_metrics['rmse_test'], embedding_metrics['rmse_test']],
        'Max_Balance': ['N/A', f"{embedding_metrics['max_balance_std']:.4f}"],
        'Mean_Balance': ['N/A', f"{embedding_metrics['mean_balance_std']:.4f}"]
    })
    
    print("\n" + comparison.to_string(index=False))
    
    # Calculate improvements
    rmse_train_improvement = 100 * (baseline_metrics['rmse_train'] - embedding_metrics['rmse_train']) / baseline_metrics['rmse_train']
    rmse_test_improvement = 100 * (baseline_metrics['rmse_test'] - embedding_metrics['rmse_test']) / baseline_metrics['rmse_test']
    
    logger.info("")
    logger.info(f"RMSE Train Improvement: {rmse_train_improvement:+.2f}%")
    logger.info(f"RMSE Test Improvement: {rmse_test_improvement:+.2f}%")
    
    if baseline_metrics['n_control']:
        pool_reduction = 100 * (baseline_metrics['n_control'] - embedding_metrics['n_control']) / baseline_metrics['n_control']
        logger.info(f"Control Pool Reduction: {pool_reduction:.1f}%")
    
    logger.info("")
    
    # 5. Show all K results if requested
    if args.show_all_k:
        logger.info("="*80)
        logger.info("ALL K VALUES TESTED")
        logger.info("="*80)
        
        all_k_df = load_all_k_metrics(args.year)
        if not all_k_df.empty:
            print("\n" + all_k_df.to_string(index=False))
            logger.info("")
    
    # 6. Save comparison results
    from config import CBPS_INTEGRATION_DIR
    output_dir = CBPS_INTEGRATION_DIR / str(args.year)
    output_dir.mkdir(parents=True, exist_ok=True)
    output_file = output_dir / f"comparison_results_{args.year}.csv"
    
    # Create detailed comparison with improvements
    detailed_comparison = pd.DataFrame({
        'year': [args.year, args.year],
        'method': ['baseline', f'embedding_k{optimal_K}'],
        'n_treated': [baseline_metrics.get('n_treated', 'N/A'), embedding_metrics['n_treated']],
        'n_control': [baseline_metrics.get('n_control', 'N/A'), embedding_metrics['n_control']],
        'rmse_train': [baseline_metrics['rmse_train'], embedding_metrics['rmse_train']],
        'rmse_test': [baseline_metrics['rmse_test'], embedding_metrics['rmse_test']],
        'rmse_train_improvement_pct': ['', f"{rmse_train_improvement:+.2f}"],
        'rmse_test_improvement_pct': ['', f"{rmse_test_improvement:+.2f}"],
        'max_balance_std': ['N/A', embedding_metrics['max_balance_std']],
        'mean_balance_std': ['N/A', embedding_metrics['mean_balance_std']],
    })
    
    detailed_comparison.to_csv(output_file, index=False)
    logger.info(f"✓ Saved comparison to: {output_file}")
    
    logger.info("")
    logger.info("="*80)
    logger.info("DONE - Comparison complete!")
    logger.info("="*80)
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
