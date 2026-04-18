#!/usr/bin/env python3
"""
Select control pools by K from embedding similarity cache and optionally run CBPS R runner.

Usage:
  python Embeddings/scripts/select_controls.py --year 2019 --k-values 20 50 100 --run-cbps

This script expects a similarity cache at Embeddings/data/embeddings/similarities_cache_{year}.npy
and an embeddings CSV at Embeddings/data/embeddings/embeddings_{year}.csv
"""

from pathlib import Path
import argparse
import logging
import numpy as np
import pandas as pd
import subprocess
import sys

logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')
logger = logging.getLogger(__name__)


def load_similarities(cache_path: Path):
    if not cache_path.exists():
        raise FileNotFoundError(f"Similarity cache not found: {cache_path}")
    arr = np.load(cache_path, allow_pickle=True).item()
    # ensure integer keys
    return {int(k): v for k, v in arr.items()}


def get_k_nearest_union(similarities: dict, K: int):
    selected = set()
    for t_idx, sims in similarities.items():
        # sims expected shape: (n_controls, 2) where col0 = control_idx, col1 = similarity
        try:
            topk = sims[:K, 0].astype(int)
        except Exception:
            # fallback if sims is list/1D
            topk = np.array([r[0] for r in sims[:K]], dtype=int)
        selected.update(topk.tolist())
    return selected


def write_selected_units(output_dir: Path, year: int, K: int, unit_ids: list):
    output_dir.mkdir(parents=True, exist_ok=True)
    out_file = output_dir / f"selected_controls_k{K}_{year}.csv"
    pd.DataFrame({'unit': unit_ids}).to_csv(out_file, index=False)
    return out_file


def run_r_runner(year: int, selected_csv: Path, K: int, train_start: int, train_end: int, test_start: int, test_end: int, write_rds: bool):
    rscript = Path('Embeddings/scripts/04_run_cbps_with_selected_controls.R')
    if not rscript.exists():
        raise FileNotFoundError(f"R runner not found: {rscript}")
    output_prefix = f"k{K}"
    cmd = [
        'Rscript', str(rscript), str(year), str(selected_csv), output_prefix,
        str(train_start), str(train_end), str(test_start), str(test_end)
    ]
    if write_rds:
        cmd.append('--write_rds')
    logger.info('Running R runner: %s', ' '.join(cmd))
    proc = subprocess.run(cmd, capture_output=True, text=True)
    logger.info('R stdout:\n%s', proc.stdout)
    if proc.returncode != 0:
        logger.error('R stderr:\n%s', proc.stderr)
        raise RuntimeError(f'R runner failed (exit {proc.returncode})')
    return proc.returncode


def main(argv=None):
    parser = argparse.ArgumentParser(description='Select controls by K and optionally run CBPS R runner')
    parser.add_argument('--year', type=int, required=True)
    parser.add_argument('--k-values', type=int, nargs='+', required=True)
    parser.add_argument('--similarity-cache', type=str, default=None,
                        help='Path to similarities_cache_{year}.npy (optional)')
    parser.add_argument('--embeddings-file', type=str, default=None,
                        help='Path to embeddings CSV (optional)')
    parser.add_argument('--out-dir', type=str, default=None,
                        help='Output directory for selected control CSVs (optional)')
    parser.add_argument('--run-cbps', action='store_true', help='Invoke R runner for each K')
    parser.add_argument('--write-rds', action='store_true', help='Pass --write_rds to R runner')
    parser.add_argument('--train-start', type=int, default=2000)
    parser.add_argument('--train-end', type=int, default=2010)
    parser.add_argument('--test-start', type=int, default=2011)
    parser.add_argument('--test-end', type=int, default=2015)
    args = parser.parse_args(argv)

    year = args.year
    # determine default paths
    base_dir = Path('Embeddings')
    default_cache = base_dir / 'data' / 'embeddings' / f'similarities_cache_{year}.npy'
    cache_path = Path(args.similarity_cache) if args.similarity_cache else default_cache
    default_embeddings = base_dir / 'data' / 'embeddings' / f'embeddings_{year}.csv'
    embeddings_path = Path(args.embeddings_file) if args.embeddings_file else default_embeddings
    default_out = base_dir / 'data' / 'cbps_integration' / str(year)
    out_dir = Path(args.out_dir) if args.out_dir else default_out

    logger.info('Year: %s', year)
    logger.info('Loading similarity cache from: %s', cache_path)
    sims = load_similarities(cache_path)
    logger.info('Loaded similarities for %d treated pixels', len(sims))
    # compute total unique control candidates from similarity cache
    all_control_idxs = set()
    for v in sims.values():
        try:
            idxs = v[:, 0].astype(int)
        except Exception:
            idxs = [r[0] for r in v]
        all_control_idxs.update(int(i) for i in idxs)
    logger.info('Similarity cache covers %d unique control candidates', len(all_control_idxs))

    if not embeddings_path.exists():
        raise FileNotFoundError(f'Embeddings file not found: {embeddings_path}')
    emb_df = pd.read_csv(embeddings_path)
    emb_df = emb_df.reset_index(drop=True)
    logger.info('Loaded embeddings with %d rows', len(emb_df))

    for K in sorted(set(args.k_values)):
        logger.info('Processing K=%d', K)
        selected_idx = get_k_nearest_union(sims, K)
        # map indices to unit ids; guard for out-of-range indices
        selected_idx = [int(i) for i in selected_idx if 0 <= int(i) < len(emb_df)]
        unit_ids = emb_df.loc[selected_idx, 'unit'].tolist()
        out_file = write_selected_units(out_dir, year, K, unit_ids)
        logger.info('Wrote %d selected units to %s', len(unit_ids), out_file)

        # Append summary row for K experiment tracking
        summary_file = out_dir / f'k_selection_summary_{year}.csv'
        summary_cols = ['K', 'n_selected_controls', 'selected_csv', 'sample_units_preview']
        preview = ';'.join([str(u) for u in unit_ids[:10]])
        row = { 'K': K, 'n_selected_controls': len(unit_ids), 'selected_csv': str(out_file), 'sample_units_preview': preview }
        # write header if missing
        if not summary_file.exists():
            pd.DataFrame(columns=summary_cols).to_csv(summary_file, index=False)
        pd.DataFrame([row])[summary_cols].to_csv(summary_file, mode='a', header=False, index=False)
        logger.info('Appended summary row to %s', summary_file)

        if args.run_cbps:
            try:
                run_r_runner(year, out_file, K, args.train_start, args.train_end, args.test_start, args.test_end, args.write_rds)
                logger.info('R runner completed for K=%d', K)
            except Exception as e:
                logger.error('R runner failed for K=%d: %s', K, e)

    logger.info('All K values processed. Selected CSVs in: %s', out_dir)


if __name__ == '__main__':
    try:
        sys.exit(main())
    except Exception as exc:
        logger.exception('Fatal error: %s', exc)
        sys.exit(2)
