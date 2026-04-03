"""Helper functions for organizing K-selection script orchestration."""

from pathlib import Path
from typing import Dict, List, Optional, Tuple
import json

import numpy as np
import pandas as pd


def load_and_prepare_embeddings(year: int, args, logger) -> Tuple[pd.DataFrame, float]:
    """Load embeddings, validate schema, clean NaNs, and apply optional treated subsampling."""
    embeddings_file = Path(f"Embeddings/data/embeddings/embeddings_{year}.csv")
    if not embeddings_file.exists():
        logger.error(f"Embeddings file not found for year {year}: {embeddings_file}")
        logger.error("Expected locations:")
        logger.error(f"  - Embeddings/data/embeddings/embeddings_{year}.csv")
        logger.error(f"  - tests/data/11SLA_embeddings_{year}_with_treatment.csv")
        raise FileNotFoundError(str(embeddings_file))

    logger.info(f"Loading embeddings from {embeddings_file}...")
    embeddings_df = pd.read_csv(embeddings_file)

    required_cols = ['unit', 'treated'] + [f'band_{i}' for i in range(72)]
    missing_cols = [col for col in required_cols if col not in embeddings_df.columns]
    if missing_cols:
        logger.error(f"Embeddings file missing required columns: {missing_cols}")
        logger.error(f"Available columns: {list(embeddings_df.columns)}")
        raise ValueError(f"Missing required columns: {missing_cols}")
    logger.info(f"✓ Embeddings validated: {len(required_cols)} required columns present")

    embedding_cols = [col for col in embeddings_df.columns if col.startswith('band_')]
    if len(embedding_cols) != 72:
        logger.warning(f"Expected 72 embedding dimensions (quantized), found {len(embedding_cols)}")
        logger.warning(f"Embedding columns: {embedding_cols[:12]}... (showing first 12)")
    else:
        logger.info("✓ All 72 embedding dimensions present (12 months × 6 channels)")

    logger.info(f"Loaded {len(embeddings_df)} pixels")
    logger.info(f"  Treated: {(embeddings_df['treated'] == 1).sum()}")
    logger.info(f"  Control: {(embeddings_df['treated'] == 0).sum()}")

    nan_rows = embeddings_df[embedding_cols].isna().any(axis=1).sum()
    if nan_rows > 0:
        logger.warning(f"⚠ Found {nan_rows} rows ({100*nan_rows/len(embeddings_df):.1f}%) with NaN embeddings")
        logger.warning("  Filtering out rows with any NaN in embeddings...")
        embeddings_df = embeddings_df[~embeddings_df[embedding_cols].isna().any(axis=1)].reset_index(drop=True)
        logger.info(f"  After filtering: {len(embeddings_df)} pixels remain")
        logger.info(f"    Treated: {(embeddings_df['treated'] == 1).sum()}")
        logger.info(f"    Control: {(embeddings_df['treated'] == 0).sum()}")
        if len(embeddings_df) == 0:
            raise ValueError("All embeddings have NaN values")
    else:
        logger.info("  ✓ No NaN values in embeddings")

    embeddings_df = embeddings_df.reset_index(drop=True)
    logger.info("  ✓ Reset index to ensure sequential indexing")

    subsample_frac = float(args.treated_subsample_frac)
    if subsample_frac <= 0 or subsample_frac > 1:
        raise ValueError("--treated-subsample-frac must be in (0, 1].")
    if subsample_frac < 1.0:
        treated_df = embeddings_df[embeddings_df['treated'] == 1].copy()
        control_df = embeddings_df[embeddings_df['treated'] == 0].copy()
        n_treated = len(treated_df)
        if n_treated == 0:
            raise ValueError("No treated units available for subsampling.")
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

    return embeddings_df, subsample_frac


def load_or_compute_similarity_cache(
    embeddings_df: pd.DataFrame,
    year: int,
    args,
    subsample_frac: float,
    k_selection_dir: Path,
    compute_all_similarities_fn,
    logger,
) -> Tuple[Dict[int, np.ndarray], str]:
    """Load similarities from cache or compute and persist them."""
    tag_suffix = f"_{args.output_tag}" if args.output_tag else ""
    robust_suffix = ""
    if subsample_frac < 1.0:
        seed_label = "na" if args.random_seed is None else str(args.random_seed)
        robust_suffix = f"_sub{int(round(subsample_frac * 1000)):03d}_seed{seed_label}"

    similarities_cache = k_selection_dir / str(year) / f"similarities_cache_{year}{robust_suffix}.npy"
    similarities_cache.parent.mkdir(parents=True, exist_ok=True)

    if args.force_recompute and similarities_cache.exists():
        logger.info("🗑️  Deleting cached similarities (--force-recompute)")
        similarities_cache.unlink()

    if similarities_cache.exists():
        logger.info(f"Loading cached similarities from {similarities_cache}...")
        similarities_array = np.load(similarities_cache, allow_pickle=True).item()
        similarities = {int(k): v for k, v in similarities_array.items()}
        logger.info(f"  ✓ Loaded {len(similarities)} treated pixels from cache")
    else:
        logger.info("Computing similarities (will be cached for future runs)...")
        similarities = compute_all_similarities_fn(embeddings_df)
        np.save(similarities_cache, similarities)
        logger.info(f"  ✓ Cached similarities to {similarities_cache}")
        logger.info("     (Delete this file to force recomputation)")

    return similarities, tag_suffix


def build_selection_runtime_config(args, load_lambda_hard_gates_fn, build_rolling_windows_fn, logger) -> Tuple[List[int], int, Dict[str, float], Optional[List[Dict[str, int]]], List[int], List[int]]:
    """Assemble selection-time hyperparameters and validate temporal splits."""
    k_candidates = [int(k) for k in (args.k_values or []) if int(k) > 0]
    if not k_candidates:
        k_candidates = [5, 10, 20, 30, 50, 100]
        logger.info("No --k-values provided; using fallback K seed values: %s", k_candidates)
    min_ratio = args.min_ratio
    logger.info(f"K seed values: {k_candidates}")
    if args.target_pool_proportions:
        logger.info(
            "Target pool proportions (primary driver): %s",
            [float(x) for x in args.target_pool_proportions],
        )
    logger.info(f"Min control ratio: {min_ratio}× treated (CBPS stability requirement)")
    if args.no_full_pool and any(float(x) >= 1.0 for x in args.target_pool_proportions):
        logger.warning("--no-full-pool is set but target pool proportions include 1.0; nearest available sub-full pool will be used.")

    gates = load_lambda_hard_gates_fn(args.config_path)
    logger.info(
        "Loaded hard gates from config: max_smd=%.3f top10_share=%.3f max_weight=%.3f ess_frac=%.3f ess_mult_treated=%.3f",
        gates['max_smd'], gates['top10_share'], gates['max_weight'], gates['ess_frac'], gates['ess_mult_treated'],
    )

    rolling_windows = None
    if args.use_rolling_windows:
        rolling_windows = build_rolling_windows_fn(
            start_year=args.rolling_start_year,
            end_year=args.rolling_end_year,
            train_length=args.rolling_train_length,
            test_length=args.rolling_test_length,
        )
        logger.info("Rolling windows enabled: %s windows", len(rolling_windows))
        for window in rolling_windows:
            logger.info(
                "  %s: train=%s-%s test=%s-%s",
                window['window_id'], window['train_start'], window['train_end'], window['test_start'], window['test_end'],
            )

    train_years = list(range(int(args.train_start_year), int(args.train_end_year) + 1))
    test_years = list(range(int(args.test_start_year), int(args.test_end_year) + 1))
    if train_years[-1] >= test_years[0]:
        raise ValueError(
            f"Invalid split: train_end_year ({train_years[-1]}) must be < test_start_year ({test_years[0]})."
        )

    return k_candidates, min_ratio, gates, rolling_windows, train_years, test_years


def write_selection_outputs(
    results: Dict,
    args,
    year: int,
    tag_suffix: str,
    subsample_frac: float,
    k_candidates: List[int],
    gates: Dict[str, float],
    rolling_windows: Optional[List[Dict[str, int]]],
    train_years: List[int],
    test_years: List[int],
    k_selection_dir: Path,
    write_pipeline_commands_fn,
    logger,
) -> Path:
    """Persist selection artifacts and summary manifest."""
    output_dir = k_selection_dir / str(year)
    output_dir.mkdir(parents=True, exist_ok=True)
    placebo_output_dir = output_dir / "placebo"
    placebo_output_dir.mkdir(parents=True, exist_ok=True)

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
    if 'random_pool_replicates' in results and isinstance(results['random_pool_replicates'], pd.DataFrame) and not results['random_pool_replicates'].empty:
        results['random_pool_replicates'].to_csv(output_dir / f"k_selection_random_replicates{tag_suffix}.csv", index=False)
    if 'random_pool_summary' in results and isinstance(results['random_pool_summary'], pd.DataFrame) and not results['random_pool_summary'].empty:
        results['random_pool_summary'].to_csv(output_dir / f"k_selection_random_summary{tag_suffix}.csv", index=False)

    commands_csv_path, commands_sh_path = write_pipeline_commands_fn(
        output_dir=output_dir,
        year=year,
        optimal_k=int(results['optimal_K']),
        experiment_name=args.experiment_name,
        output_tag=args.output_tag,
        placebo_draws=max(1, int(args.placebo_draws)),
        placebo_post_years=args.placebo_post_years,
        placebo_output_dir=placebo_output_dir,
        placebo_assignment_mode=args.placebo_assignment_mode,
        placebo_workers=max(1, int(args.placebo_workers)),
        placebo_seed_base=int(args.placebo_seed_base),
        placebo_checkpoint_every=max(1, int(args.placebo_checkpoint_every)),
        placebo_resume=not bool(args.no_placebo_resume),
        placebo_gate_prefit_mult=float(args.placebo_gate_prefit_mult),
        placebo_enforce_ratio_gate=bool(args.placebo_enforce_ratio_gate),
        placebo_gate_ratio_max=float(args.placebo_gate_ratio_max),
        placebo_donor_size=max(1, int(args.placebo_donor_size)),
    )

    summary_payload = {
        'year': int(year),
        'optimal_K': int(results['optimal_K']),
        'optimal_rmse': float(results['optimal_rmse']),
        'selection_mode': results.get('selection_mode', 'unknown'),
        'treated_subsample_frac': subsample_frac,
        'random_seed': args.random_seed,
        'output_tag': args.output_tag,
        'k_values_requested': [int(k) for k in k_candidates],
        'k_values_evaluated': sorted([int(k) for k in results['rmse_results']['K'].dropna().astype(int).tolist()]) if 'K' in results['rmse_results'].columns else [],
        'target_pool_proportions': [float(x) for x in args.target_pool_proportions],
        'include_full_pool': not args.no_full_pool,
        'rolling_windows': rolling_windows,
        'adaptive_refine': not args.no_adaptive_refine,
        'random_baseline_reps': max(0, int(args.random_baseline_reps)),
        'hard_gates': gates,
        'pipeline_commands_csv': str(commands_csv_path),
        'pipeline_commands_shell': str(commands_sh_path),
        'placebo_draws': max(1, int(args.placebo_draws)),
        'placebo_post_years': args.placebo_post_years,
        'placebo_output_dir': str(placebo_output_dir),
        'placebo_assignment_mode': args.placebo_assignment_mode,
        'placebo_workers': max(1, int(args.placebo_workers)),
        'placebo_seed_base': int(args.placebo_seed_base),
        'placebo_checkpoint_every': max(1, int(args.placebo_checkpoint_every)),
        'placebo_resume': not bool(args.no_placebo_resume),
        'placebo_gate_prefit_mult': float(args.placebo_gate_prefit_mult),
        'placebo_enforce_ratio_gate': bool(args.placebo_enforce_ratio_gate),
        'placebo_gate_ratio_max': float(args.placebo_gate_ratio_max),
        'placebo_donor_size': max(1, int(args.placebo_donor_size)),
        'train_years': train_years,
        'test_years': test_years,
    }
    with open(output_dir / summary_name, 'w', encoding='utf-8') as summary_file:
        json.dump(summary_payload, summary_file, indent=2)

    logger.info(f"\nResults saved to {output_dir}/")
    logger.info(f"  - {elbow_name} (similarity by K)")
    logger.info(f"  - {rmse_name} (RMSPE by K)")
    logger.info(f"  - {pool_name} (realized donor pool and support diagnostics)")
    logger.info(f"  - {mapping_name} (target proportion to realized K mapping)")
    logger.info(f"  - {summary_name} (selection summary)")
    logger.info(f"  - {commands_csv_path.name} (pipeline commands in CSV)")
    logger.info(f"  - {commands_sh_path.name} (pipeline commands as executable shell)")

    logger.info("\n" + "=" * 80)
    logger.info("NEXT STEPS:")
    logger.info("=" * 80)
    logger.info(f"1. Run CBPS with optimal K={results['optimal_K']} for {year}")
    logger.info(f"   → Rscript Embeddings/scripts/04_run_cbps_with_selected_controls.R {year} k{results['optimal_K']} --experiment-name {args.experiment_name}")
    logger.info("2. Run placebo pipeline and export draws CSV")
    logger.info(
        "   → "
        f"Rscript Embeddings/scripts/figures/placebo_att_simulator.R year={year} B={max(1, int(args.placebo_draws))} "
        f"post_years={args.placebo_post_years} assignment_mode={args.placebo_assignment_mode} "
        f"n_workers={max(1, int(args.placebo_workers))} seed_base={int(args.placebo_seed_base)} "
        f"checkpoint_every={max(1, int(args.placebo_checkpoint_every))} "
        f"resume={'true' if (not bool(args.no_placebo_resume)) else 'false'} "
        f"gate_prefit_mult={float(args.placebo_gate_prefit_mult)} "
        f"enforce_ratio_gate={'true' if bool(args.placebo_enforce_ratio_gate) else 'false'} "
        f"gate_ratio_max={float(args.placebo_gate_ratio_max)} donor_placebo_size={max(1, int(args.placebo_donor_size))} "
        f"out_dir='{placebo_output_dir.as_posix()}'"
    )
    logger.info("3. Plot placebo histogram from CSV draws")
    logger.info(f"   → python Embeddings/scripts/figures/plot_placebo_histogram.py --year {year} --base-dir '{placebo_output_dir.as_posix()}'")
    logger.info("4. Build consolidated per-year comparison report")
    logger.info("   → python Embeddings/scripts/11_generate_per_year_comparison_report.py")
    logger.info(f"5. You can also run all steps from: {commands_sh_path}")
    logger.info("=" * 80)

    return output_dir
