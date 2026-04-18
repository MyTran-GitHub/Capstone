#!/usr/bin/env python3
"""Run CBPS R runner for all available selected_controls CSVs under cbps_integration.

Usage examples:
  python Embeddings/scripts/run_batch_cbps.py --input-dir Embeddings/data/cbps_integration \
      --years 2008 2009 2010 --concurrency 2 --experiment-name 2019_k13

This will locate files like:
  Embeddings/data/cbps_integration/2019/selected_controls_k13_2019.csv
and call the R runner:
  Rscript Embeddings/scripts/04_run_cbps_with_selected_controls.R <year> <selected_csv> <output_prefix> <train_start> <train_end> <test_start> <test_end> --experiment-name <exp>

The script skips years with no selected_controls CSV and logs results.
"""

import argparse
import concurrent.futures
import logging
import os
import re
import subprocess
from pathlib import Path
from typing import List, Optional

logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s')
logger = logging.getLogger(__name__)

SEL_RE = re.compile(r"selected_controls_k(?P<K>\d+).*_(?P<yr>\d{4})\.csv$")


def find_selected_csvs(input_dir: Path, years: Optional[List[int]] = None) -> List[Path]:
    out = []
    if years:
        for y in years:
            p = input_dir / str(y)
            if not p.exists():
                continue
            for f in p.glob("selected_controls_*.csv"):
                out.append(f)
    else:
        # scan all year subdirs
        for p in sorted(input_dir.iterdir()):
            if not p.is_dir():
                continue
            for f in p.glob("selected_controls_*.csv"):
                out.append(f)
    return sorted(out)


def make_command(r_script: Path, csv_path: Path, train_start: int, train_end: int, test_start: int, test_end: int, output_experiment_name: str, analysis_experiment: str, analysis_base_dir: str, save_full_weights: bool) -> List[str]:
    m = SEL_RE.search(csv_path.name)
    if m:
        K = m.group('K')
        yr = m.group('yr')
        output_prefix = f"k{K}"
    else:
        # fallback: use parent dir name as year and derive prefix from filename
        yr = csv_path.parent.name
        output_prefix = Path(csv_path.name).stem
    cmd = [
        'Rscript',
        str(r_script),
        str(int(yr)),
        str(csv_path),
        output_prefix,
        str(int(train_start)),
        str(int(train_end)),
        str(int(test_start)),
        str(int(test_end)),
        '--experiment-name', analysis_experiment,
        '--output-experiment-name', output_experiment_name,
        '--analysis-base-dir', analysis_base_dir,
    ]
    if save_full_weights:
        cmd.extend(['--save-full-weights', 'true'])
    return cmd


def run_one(cmd: List[str], cwd: Optional[Path] = None) -> int:
    logger.info('Running: %s', ' '.join(cmd))
    p = subprocess.run(cmd, capture_output=True, text=True, cwd=cwd)
    if p.returncode != 0:
        logger.error('FAILED (rc=%s): %s', p.returncode, p.stderr.strip()[:500])
    else:
        logger.info('OK: %s', p.stdout.strip().splitlines()[:3])
    return p.returncode


def main():
    parser = argparse.ArgumentParser(description='Batch-run CBPS for selected control CSVs')
    parser.add_argument('--input-dir', type=str, default='Embeddings/data/cbps_integration')
    parser.add_argument('--years', type=int, nargs='*', default=None, help='Optional list of years to run (default: all subdirs)')
    parser.add_argument('--r-script', type=str, default='Embeddings/scripts/04_run_cbps_with_selected_controls.R')
    parser.add_argument('--train-start', type=int, default=2000)
    parser.add_argument('--train-end', type=int, default=2010)
    parser.add_argument('--test-start', type=int, default=2011)
    parser.add_argument('--test-end', type=int, default=2015)
    parser.add_argument('--experiment-name', type=str, default='full_pool')
    parser.add_argument('--analysis-experiment', type=str, default='full_pool',
                        help='Experiment subdirectory where analysis_treated files live (default: full_pool).')
    parser.add_argument('--analysis-base-dir', type=str, default='data/processed_data/rev_analysis_low')
    parser.add_argument('--save-full-weights', action='store_true')
    parser.add_argument('--concurrency', type=int, default=1, help='Parallel R runs')
    parser.add_argument('--capstone-root', type=str, default='.', help='Working directory to run Rscript from (repo root)')
    args = parser.parse_args()

    input_dir = Path(args.input_dir)
    if not input_dir.exists():
        logger.error('input-dir not found: %s', input_dir)
        return 2
    r_script = Path(args.r_script)
    if not r_script.exists():
        logger.error('R script not found: %s', r_script)
        return 2

    csvs = find_selected_csvs(input_dir, args.years)
    if not csvs:
        logger.warning('No selected_controls CSVs found under %s', input_dir)
        return 0

    logger.info('Found %d selected_controls CSVs (skipping missing years)', len(csvs))
    cmds = []
    for c in csvs:
        cmd = make_command(
            r_script,
            c,
            args.train_start,
            args.train_end,
            args.test_start,
            args.test_end,
            args.experiment_name,  # output_experiment_name
            args.analysis_experiment,
            args.analysis_base_dir,
            args.save_full_weights,
        )
        cmds.append((c, cmd))

    failures = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=max(1, args.concurrency)) as ex:
        futures = {ex.submit(run_one, cmd, Path(args.capstone_root)): c for c, cmd in cmds}
        for fut in concurrent.futures.as_completed(futures):
            csv = futures[fut]
            try:
                rc = fut.result()
                if rc != 0:
                    failures.append((csv, rc))
            except Exception as e:
                logger.exception('Exception running for %s', csv)
                failures.append((csv, -1))

    logger.info('Done. failures=%d', len(failures))
    if failures:
        for f, rc in failures:
            logger.error('Failed: %s (rc=%s)', f, rc)
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
