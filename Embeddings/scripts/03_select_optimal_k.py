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

# Add repository root and Embeddings package directory for robust imports.
SCRIPT_PATH = Path(__file__).resolve()
EMBEDDINGS_DIR = SCRIPT_PATH.parent.parent
REPO_ROOT = EMBEDDINGS_DIR.parent
sys.path.insert(0, str(REPO_ROOT))
sys.path.insert(0, str(EMBEDDINGS_DIR))

import logging
import subprocess
from typing import Dict

BASE_DIR = Path(__file__).resolve().parent.parent
DATA_DIR = BASE_DIR / "data"
K_SELECTION_DIR = DATA_DIR / "k_selection"

# Setup logging
logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(asctime)s - %(name)s - %(message)s')
logger = logging.getLogger(__name__)


try:
    from Embeddings.scripts.utils._similarity_utils import compute_all_similarities
except ModuleNotFoundError:
    from Embeddings.scripts.utils._similarity_utils import compute_all_similarities

try:
    from Embeddings.scripts.utils.k_selection_helpers import (
        build_selection_runtime_config,
        load_and_prepare_embeddings,
        load_or_compute_similarity_cache,
        write_selection_outputs,
    )
except ModuleNotFoundError:
    from Embeddings.scripts.utils.k_selection_helpers import (
        build_selection_runtime_config,
        load_and_prepare_embeddings,
        load_or_compute_similarity_cache,
        write_selection_outputs,
    )

try:
    from Embeddings.scripts.utils.k_selection_engine import (
        build_rolling_windows,
        select_optimal_k,
        write_pipeline_commands,
    )
except ModuleNotFoundError:
    from Embeddings.scripts.utils.k_selection_engine import (
        build_rolling_windows,
        select_optimal_k,
        write_pipeline_commands,
    )


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
        default=None,
        help='Optional fallback K seed values used only when target-pool-proportions is empty'
    )
    parser.add_argument(
        '--target-pool-proportions',
        type=float,
        nargs='+',
        default=[0.10, 0.20, 0.3, 0.40, 0.5, 0.60, 0.7, 0.8, 0.9, 1.0],
        help='Target donor-pool proportions of full controls to evaluate (default: 0.10, 0.20, 0.3, 0.40, 0.5, 0.60, 0.7, 0.8, 0.9, 1.0)'
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
        help='Experiment namespace for processed_data inputs (default: full_pool); Embeddings/data outputs are now flat by year'
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
    parser.add_argument(
        '--use-rolling-windows',
        dest='use_rolling_windows',
        action='store_true',
        help='Use rolling train/test pre-treatment windows and aggregate RMSE across windows (default: on)'
    )
    parser.add_argument(
        '--no-rolling-windows',
        dest='use_rolling_windows',
        action='store_false',
        help='Disable rolling windows and use a single fixed train/test split'
    )
    parser.set_defaults(use_rolling_windows=True)
    parser.add_argument(
        '--rolling-start-year',
        type=int,
        default=None,
        help='First pre-treatment year for rolling windows (default: auto by treated year)'
    )
    parser.add_argument(
        '--rolling-end-year',
        type=int,
        default=None,
        help='Last pre-treatment year for rolling windows (default: min(2015, treated_year-1))'
    )
    parser.add_argument(
        '--rolling-train-length',
        type=int,
        default=9,
        help='Training window length in years for rolling validation (default: 9)'
    )
    parser.add_argument(
        '--rolling-test-length',
        type=int,
        default=3,
        help='Testing window length in years for rolling validation (default: 3)'
    )
    parser.add_argument(
        '--train-start-year',
        type=int,
        default=None,
        help='Train start year for non-rolling evaluation (default: auto by treated year)'
    )
    parser.add_argument(
        '--train-end-year',
        type=int,
        default=None,
        help='Train end year for non-rolling evaluation (default: auto by treated year)'
    )
    parser.add_argument(
        '--test-start-year',
        type=int,
        default=None,
        help='Test start year for non-rolling evaluation (default: auto by treated year)'
    )
    parser.add_argument(
        '--test-end-year',
        type=int,
        default=None,
        help='Test end year for non-rolling evaluation (default: min(2015, treated_year-1))'
    )
    parser.add_argument(
        '--no-adaptive-refine',
        action='store_true',
        help='Disable local adaptive K refinement around preliminary best K'
    )
    parser.add_argument(
        '--random-baseline-reps',
        type=int,
        default=0,
        help='Run random donor-pool benchmark with N replicates per K (default: 0/off)'
    )
    parser.add_argument(
        '--placebo-draws',
        type=int,
        default=1000,
        help='Placebo draw count written into pipeline commands (default: 1000; use 2000 for final paper tables)'
    )
    parser.add_argument(
        '--placebo-post-years',
        type=str,
        default='2020',
        help='Comma-separated post years for placebo command payload (default: 2020)'
    )
    parser.add_argument(
        '--placebo-assignment-mode',
        type=str,
        choices=['control_only', 'full_sample_randomization', 'donor_unit_placebo'],
        default='control_only',
        help='Pseudo-treatment assignment mode for placebo simulator (default: control_only)'
    )
    parser.add_argument(
        '--placebo-workers',
        type=int,
        default=1,
        help='Parallel workers for placebo draws (default: 1)'
    )
    parser.add_argument(
        '--placebo-seed-base',
        type=int,
        default=1,
        help='Base seed; each draw uses seed_base + draw_id for deterministic parallel reproducibility'
    )
    parser.add_argument(
        '--placebo-checkpoint-every',
        type=int,
        default=100,
        help='Checkpoint placebo state every N draws (default: 100)'
    )
    parser.add_argument(
        '--no-placebo-resume',
        action='store_true',
        help='Disable resume-from-checkpoint behavior in placebo simulator'
    )
    parser.add_argument(
        '--placebo-gate-prefit-mult',
        type=float,
        default=5.0,
        help='Gate: placebo pre-RMSPE <= multiplier * observed pre-RMSPE (default: 5.0)'
    )
    parser.add_argument(
        '--placebo-enforce-ratio-gate',
        action='store_true',
        help='Enable stricter gate on post/pre RMSPE ratio'
    )
    parser.add_argument(
        '--placebo-gate-ratio-max',
        type=float,
        default=20.0,
        help='If ratio gate enabled: require post/pre RMSPE ratio <= this threshold (default: 20.0)'
    )
    parser.add_argument(
        '--placebo-donor-size',
        type=int,
        default=1,
        help='When assignment_mode=donor_unit_placebo, number of pseudo-treated donor units per draw (default: 1)'
    )
    parser.add_argument(
        '--no-temporal-placebo-command',
        action='store_true',
        help='Do not include temporal placebo falsification command in generated pipeline commands'
    )
    parser.add_argument(
        '--temporal-placebo-years',
        type=str,
        default='',
        help='Optional comma-separated fake treatment years for temporal placebo command (default: inferred pre-period range)'
    )
    parser.add_argument(
        '--temporal-placebo-draws',
        type=int,
        default=1000,
        help='Draws for generated temporal placebo command (default: 1000; use 2000 for final inference)'
    )
    parser.add_argument(
        '--temporal-placebo-pre-start',
        type=int,
        default=2008,
        help='Default fake-year range start for temporal placebo command when years are not provided'
    )
    parser.add_argument(
        '--temporal-placebo-pre-end',
        type=int,
        default=2017,
        help='Default fake-year range end for temporal placebo command when years are not provided'
    )
    parser.add_argument(
        '--temporal-placebo-post-lag',
        type=int,
        default=1,
        help='Post-period lag used by temporal placebo runner (default: 1)'
    )
    parser.add_argument(
        '--temporal-placebo-post-year-count',
        type=int,
        default=1,
        help='Number of post years per fake treatment year in temporal placebo runner (default: 1)'
    )
    parser.add_argument(
        '--stage',
        type=str,
        choices=['all', 'prepare', 'evaluate', 'select', 'export'],
        default='all',
        help='Pipeline stage to run: prepare/evaluate/select/export/all (default: all)'
    )
    # NOTE: strict mode removed — R runner now always fails on degenerate weights
    args = parser.parse_args()

    if not (0.0 < float(args.treated_subsample_frac) <= 1.0):
        raise ValueError("--treated-subsample-frac must be in (0, 1]")

    year = args.year    
    logger.info(f"Processing year: {year}")    
    logger.info(f"Stage: {args.stage}")
    try:
        embeddings_df, subsample_frac = load_and_prepare_embeddings(year, args, logger)
        similarities, tag_suffix = load_or_compute_similarity_cache(
            embeddings_df=embeddings_df,
            year=year,
            args=args,
            subsample_frac=subsample_frac,
            k_selection_dir=K_SELECTION_DIR,
            compute_all_similarities_fn=compute_all_similarities,
            logger=logger,
        )
        if args.stage == 'prepare':
            logger.info("Stage 'prepare' complete: similarities are cached and ready for downstream stages.")
            return 0

        K_candidates, min_ratio, gates, rolling_windows, train_years, test_years = build_selection_runtime_config(
            args,
            load_lambda_hard_gates,
            build_rolling_windows,
            logger,
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
            rolling_windows=rolling_windows,
            adaptive_refine=not args.no_adaptive_refine,
            random_baseline_reps=max(0, int(args.random_baseline_reps)),
            random_seed=args.random_seed,
            train_years=train_years,
            test_years=test_years,
        )
        if results is None:
            return 1

        if args.stage == 'evaluate':
            logger.info("Stage 'evaluate' complete: K evaluations are cached in cbps_integration outputs.")
            logger.info("Run with --stage select to materialize selection artifacts.")
            return 0
        if args.stage == 'export':
            logger.info("Stage 'export' currently reuses selection flow and writes artifacts from cached metrics when available.")

        write_selection_outputs(
            results=results,
            args=args,
            year=year,
            tag_suffix=tag_suffix,
            subsample_frac=subsample_frac,
            k_candidates=K_candidates,
            gates=gates,
            rolling_windows=rolling_windows,
            train_years=train_years,
            test_years=test_years,
            k_selection_dir=K_SELECTION_DIR,
            write_pipeline_commands_fn=write_pipeline_commands,
            logger=logger,
        )
    except (FileNotFoundError, ValueError) as exc:
        logger.error(str(exc))
        return 1

    return 0
if __name__ == "__main__":
    sys.exit(main())
