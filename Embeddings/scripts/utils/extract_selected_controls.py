#!/usr/bin/env python3
"""
Extract selected control units from cached similarity matrix
This allows diagnostic analysis without re-running the full CBPS pipeline
"""

import numpy as np
import pandas as pd
from pathlib import Path
import logging

logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')
logger = logging.getLogger(__name__)

def get_k_nearest_union(similarities, K):
    """Get union of K nearest controls for all treated pixels"""
    selected_controls = set()
    for t_idx, sims in similarities.items():
        top_k = sims[:K, 0].astype(int)
        selected_controls.update(top_k)
    return selected_controls

def main():
    year = 2019
    K_values = [10, 20, 30, 50, 75, 100]
    
    logger.info(f"Extracting selected controls for year {year}")
    
    # Load cached similarities
    from config import OUTPUT_EMBEDDINGS_DIR, CBPS_INTEGRATION_DIR
    similarities_cache = OUTPUT_EMBEDDINGS_DIR / f"similarities_cache_{year}.npy"
    
    if not similarities_cache.exists():
        logger.error(f"Similarity cache not found: {similarities_cache}")
        logger.error("Run select_optimal_k.py first to generate similarity cache")
        return 1
    
    logger.info(f"Loading similarities from {similarities_cache}...")
    similarities_array = np.load(similarities_cache, allow_pickle=True).item()
    similarities = {int(k): v for k, v in similarities_array.items()}
    logger.info(f"  ✓ Loaded similarities for {len(similarities)} treated pixels")
    
    # Load embeddings to get unit IDs
    embeddings_file = Path(f"Embeddings/data/embeddings/embeddings_{year}.csv")
    if not embeddings_file.exists():
        logger.error(f"Embeddings file not found: {embeddings_file}")
        return 1
    
    logger.info(f"Loading embeddings from {embeddings_file}...")
    embeddings_df = pd.read_csv(embeddings_file)
    embeddings_df = embeddings_df.reset_index(drop=True)
    logger.info(f"  ✓ Loaded {len(embeddings_df)} pixels")
    
    # Create output directory
    output_dir = CBPS_INTEGRATION_DIR / str(year)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Extract and save selected controls for each K
    logger.info(f"\nExtracting selected controls for K = {K_values}...")
    
    for K in K_values:
        # Get DataFrame indices of selected controls
        selected_control_indices = get_k_nearest_union(similarities, K)
        
        # Map indices to unit IDs
        selected_units = embeddings_df.loc[list(selected_control_indices), 'unit'].tolist()
        
        # Save to CSV
        output_file = output_dir / f"selected_controls_k{K}_{year}.csv"
        pd.DataFrame({'unit': selected_units}).to_csv(output_file, index=False)
        
        logger.info(f"  K={K}: {len(selected_units)} controls → {output_file.name}")
    
    logger.info(f"\n✓ All selected_controls files saved to {output_dir}/")
    logger.info(f"\nNow you can run diagnostics:")
    logger.info(f"  Rscript balancing/diagnose_cbps_weights.R {year} {output_dir}/selected_controls_k20_{year}.csv")
    
    return 0

if __name__ == "__main__":
    import sys
    from pathlib import Path
    
    # Add parent directory to path for config import
    sys.path.insert(0, str(Path(__file__).parent.parent.parent))
    
    sys.exit(main())
