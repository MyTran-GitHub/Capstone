#!/usr/bin/env python3
"""One-time similarity precompute CLI.

Computes and caches treated→(control,similarity) lists for a given year.

Usage:
  python Embeddings/scripts/compute_similarities.py --year 2019

Outputs:
  Embeddings/data/k_selection/{year}/similarities_cache_{year}.npy
"""
import argparse
from pathlib import Path
import logging
import sys
import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from Embeddings._similarity_utils import compute_all_similarities
from config import K_SELECTION_DIR

logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')
logger = logging.getLogger(__name__)


def main(argv=None):
    parser = argparse.ArgumentParser(description='Compute and cache similarities for a given year')
    parser.add_argument('--year', type=int, required=True)
    parser.add_argument('--embeddings-file', type=str, default=None,
                        help='Optional path to embeddings CSV (overrides default)')
    parser.add_argument('--force', action='store_true', help='Force recompute even if cache exists')
    args = parser.parse_args(argv)

    year = args.year
    base = Path('Embeddings')
    default_embeddings = base / 'data' / 'embeddings' / f'embeddings_{year}.csv'
    embeddings_path = Path(args.embeddings_file) if args.embeddings_file else default_embeddings

    if not embeddings_path.exists():
        logger.error('Embeddings file not found: %s', embeddings_path)
        return 2

    logger.info('Loading embeddings from %s', embeddings_path)
    embeddings_df = pd.read_csv(embeddings_path)

    # Basic validation (expect 'treated' and band_ columns)
    if 'treated' not in embeddings_df.columns:
        logger.error("Embeddings CSV missing 'treated' column")
        return 2

    # Compute similarities
    out_dir = Path(K_SELECTION_DIR) / str(year)
    out_dir.mkdir(parents=True, exist_ok=True)
    out_file = out_dir / f'similarities_cache_{year}.npy'
    if out_file.exists() and not args.force:
        logger.info('Cache already exists at %s (use --force to recompute)', out_file)
        return 0

    logger.info('Computing similarities (this may take a while)')
    similarities = compute_all_similarities(embeddings_df)

    # Save
    np.save(out_file, similarities)
    logger.info('Saved similarities cache to %s', out_file)
    return 0


if __name__ == '__main__':
    rc = main()
    sys.exit(rc)
