"""Helper functions for organizing K-selection script orchestration."""

from pathlib import Path
from typing import Dict, List, Optional, Tuple
import json
import tempfile

import numpy as np
import pandas as pd

BASE_DIR = Path(__file__).resolve().parent.parent.parent
DATA_DIR = BASE_DIR / "data"


def _write_csv_atomic(df: pd.DataFrame, out_path: Path) -> None:
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False, dir=out_path.parent, encoding='utf-8') as tmp:
        tmp_path = Path(tmp.name)
    try:
        df.to_csv(tmp_path, index=False)
        tmp_path.replace(out_path)
    finally:
        if tmp_path.exists():
            tmp_path.unlink(missing_ok=True)


def _write_json_atomic(payload: Dict, out_path: Path) -> None:
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False, dir=out_path.parent, encoding='utf-8') as tmp:
        tmp_path = Path(tmp.name)
    try:
        tmp_path.write_text(json.dumps(payload, indent=2), encoding='utf-8')
        tmp_path.replace(out_path)
    finally:
        if tmp_path.exists():
            tmp_path.unlink(missing_ok=True)


def _write_pool_frontier_plots(output_dir: Path,
                               tag_suffix: str,
                               effective_pool_df: pd.DataFrame,
                               random_summary_df: Optional[pd.DataFrame],
                               full_pool_size: Optional[int]) -> Dict[str, str]:
    """Write frontier diagnostics plots keyed on realized pool size."""
    plots = {}
    if effective_pool_df is None or effective_pool_df.empty:
        return plots

    try:
        import matplotlib.pyplot as plt
    except Exception:
        return plots

    eff = effective_pool_df.copy().sort_values('pool_size')
    rmse_col = 'median_RMSE' if ('median_RMSE' in eff.columns and eff['median_RMSE'].notna().any()) else 'rmse'

    def _save(fig, stem: str) -> None:
        out = output_dir / f"{stem}{tag_suffix}.png"
        fig.tight_layout()
        fig.savefig(out, dpi=150)
        plt.close(fig)
        plots[stem] = str(out)

    # Pool efficiency curve: RMSE vs pool size (embedding + random + full-pool marker).
    fig, ax = plt.subplots(figsize=(8, 4.5))
    ax.plot(eff['pool_size'], eff[rmse_col], marker='o', linewidth=1.8, label='embedding')
    if random_summary_df is not None and not random_summary_df.empty:
        rnd = random_summary_df.copy().sort_values('pool_size')
        ycol = 'median_RMSE' if 'median_RMSE' in rnd.columns else ('rmse' if 'rmse' in rnd.columns else None)
        if ycol is not None:
            ax.plot(rnd['pool_size'], rnd[ycol], marker='s', linestyle='--', linewidth=1.4, label='random baseline')
    if full_pool_size is not None and np.isfinite(full_pool_size):
        full_rows = eff[eff['pool_size'] == int(full_pool_size)]
        if not full_rows.empty:
            ax.scatter([int(full_pool_size)], [float(full_rows[rmse_col].iloc[0])], s=70, marker='D', label='full pool baseline')
    ax.set_xlabel('pool_size')
    ax.set_ylabel(rmse_col)
    ax.set_title('Pool Efficiency Curve')
    ax.grid(True, alpha=0.25)
    ax.legend()
    _save(fig, 'pool_efficiency_curve')

    # ESS vs pool size.
    if 'ess_control' in eff.columns:
        fig, ax = plt.subplots(figsize=(8, 4.5))
        ax.plot(eff['pool_size'], eff['ess_control'], marker='o', linewidth=1.8)
        ax.set_xlabel('pool_size')
        ax.set_ylabel('ess_control')
        ax.set_title('ESS vs Pool Size')
        ax.grid(True, alpha=0.25)
        _save(fig, 'ess_vs_pool_size')

    # Weight concentration diagnostics.
    if 'top10_share' in eff.columns or 'max_weight_share' in eff.columns:
        fig, ax = plt.subplots(figsize=(8, 4.5))
        if 'top10_share' in eff.columns:
            ax.plot(eff['pool_size'], eff['top10_share'], marker='o', linewidth=1.6, label='top10_share')
        if 'max_weight_share' in eff.columns:
            ax.plot(eff['pool_size'], eff['max_weight_share'], marker='s', linewidth=1.6, label='max_weight_share')
        ax.set_xlabel('pool_size')
        ax.set_ylabel('weight concentration')
        ax.set_title('Weight Concentration vs Pool Size')
        ax.grid(True, alpha=0.25)
        ax.legend()
        _save(fig, 'weight_concentration_vs_pool_size')

    return plots


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


def build_selection_runtime_config(args, load_lambda_hard_gates_fn, build_rolling_windows_fn, logger, treated_year: Optional[int] = None) -> Tuple[List[int], int, Dict[str, float], Optional[List[Dict[str, int]]], List[int], List[int]]:
    """Assemble selection-time hyperparameters and validate temporal splits."""
    k_candidates = []
    min_ratio = args.min_ratio
    logger.info(f"Min control ratio: {min_ratio}× treated (CBPS stability requirement)")

    gates = load_lambda_hard_gates_fn(args.config_path)
    logger.info(
        "Loaded hard gates from config: max_smd=%.3f top10_share=%.3f max_weight=%.3f ess_frac=%.3f ess_mult_treated=%.3f",
        gates['max_smd'], gates['top10_share'], gates['max_weight'], gates['ess_frac'], gates['ess_mult_treated'],
    )

    if treated_year is None:
        treated_year = int(getattr(args, 'year', 2019))

    rolling_start = args.rolling_start_year
    rolling_end = args.rolling_end_year
    if rolling_start is None:
        rolling_start = 2000
    if rolling_end is None:
        rolling_end = min(2015, int(treated_year) - 1)

    rolling_windows = None
    if args.use_rolling_windows:
        rolling_windows = build_rolling_windows_fn(
            start_year=int(rolling_start),
            end_year=int(rolling_end),
            train_length=args.rolling_train_length,
            test_length=args.rolling_test_length,
        )
        if not rolling_windows:
            raise ValueError(
                "Rolling windows produced no valid splits. "
                f"start={rolling_start}, end={rolling_end}, train_length={args.rolling_train_length}, test_length={args.rolling_test_length}."
            )
        logger.info("Rolling windows enabled: %s windows", len(rolling_windows))
        for window in rolling_windows:
            logger.info(
                "  %s: train=%s-%s test=%s-%s",
                window['window_id'], window['train_start'], window['train_end'], window['test_start'], window['test_end'],
            )

    # Defaults for non-rolling cross-validation split.
    train_start_year = args.train_start_year
    train_end_year = args.train_end_year
    test_start_year = args.test_start_year
    test_end_year = args.test_end_year

    if all(x is None for x in [train_start_year, train_end_year, test_start_year, test_end_year]):
        if int(treated_year) - 1 >= 2015:
            train_start_year, train_end_year = 2000, 2010
            test_start_year, test_end_year = 2011, 2015
        else:
            test_end_year = int(treated_year) - 1
            test_start_year = max(2001, int(test_end_year) - 4)
            train_end_year = int(test_start_year) - 1
            train_start_year = max(1985, int(train_end_year) - 10)

    if train_start_year is None:
        train_start_year = 2000
    if train_end_year is None:
        train_end_year = 2010
    if test_start_year is None:
        test_start_year = int(train_end_year) + 1
    if test_end_year is None:
        test_end_year = min(2015, int(treated_year) - 1)

    train_years = list(range(int(train_start_year), int(train_end_year) + 1))
    test_years = list(range(int(test_start_year), int(test_end_year) + 1))
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
    effective_pool_name = f"k_selection_effective_pool{tag_suffix}.csv"
    effective_dup_name = f"k_selection_effective_pool_duplicates{tag_suffix}.csv"
    pool_name = f"k_selection_pool_diagnostics{tag_suffix}.csv"
    mapping_name = f"k_selection_pool_target_mapping{tag_suffix}.csv"
    summary_name = f"k_selection_summary{tag_suffix}.json"

    meta = {
        'year': int(year),
        'experiment_name': str(args.experiment_name),
        'output_tag': str(args.output_tag),
    }
    write_legacy_outputs = bool(getattr(args, 'write_legacy_outputs', False))
    write_frontier_plots = bool(getattr(args, 'write_frontier_plots', False))

    def _safe_float(value):
        if value is None:
            return None
        try:
            f = float(value)
        except Exception:
            return None
        return None if not np.isfinite(f) else f

    if write_legacy_outputs:
        results['elbow_metrics'].to_csv(output_dir / elbow_name, index=False)
        results['rmse_results'].to_csv(output_dir / rmse_name, index=False)
        if 'effective_pool_table' in results and isinstance(results['effective_pool_table'], pd.DataFrame):
            results['effective_pool_table'].to_csv(output_dir / effective_pool_name, index=False)
        if 'effective_pool_duplicates' in results and isinstance(results['effective_pool_duplicates'], pd.DataFrame):
            results['effective_pool_duplicates'].to_csv(output_dir / effective_dup_name, index=False)
        if 'pool_diagnostics' in results and isinstance(results['pool_diagnostics'], pd.DataFrame):
            results['pool_diagnostics'].to_csv(output_dir / pool_name, index=False)
        if 'pool_target_mapping' in results and isinstance(results['pool_target_mapping'], pd.DataFrame):
            results['pool_target_mapping'].to_csv(output_dir / mapping_name, index=False)
        if 'random_pool_replicates' in results and isinstance(results['random_pool_replicates'], pd.DataFrame) and not results['random_pool_replicates'].empty:
            results['random_pool_replicates'].to_csv(output_dir / f"k_selection_random_replicates{tag_suffix}.csv", index=False)
        if 'random_pool_summary' in results and isinstance(results['random_pool_summary'], pd.DataFrame) and not results['random_pool_summary'].empty:
            results['random_pool_summary'].to_csv(output_dir / f"k_selection_random_summary{tag_suffix}.csv", index=False)
    if 'pool_discovery_scan' in results and isinstance(results['pool_discovery_scan'], pd.DataFrame):
        scan_df = results['pool_discovery_scan'].copy()
        if 'pool_size' in scan_df.columns:
            scan_df = scan_df.rename(columns={'pool_size': 'effective_pool_size'})
        scan_cols = [
            'K', 'effective_pool_size', 'pool_prop_full', 'coverage_ratio',
            'support_similarity_min', 'support_similarity_p10', 'support_similarity_median',
        ]
        for col in scan_cols:
            if col not in scan_df.columns:
                scan_df[col] = np.nan
        scan_df = scan_df[scan_cols].sort_values('K').reset_index(drop=True)
        scan_df.insert(0, 'year', int(year))
        _write_csv_atomic(scan_df, output_dir / 'pool_discovery_scan.csv')
    if 'pool_size_grid' in results and isinstance(results['pool_size_grid'], pd.DataFrame):
        grid_df = results['pool_size_grid'].copy()
        grid_cols = ['effective_pool_size', 'representative_K', 'pool_prop_full', 'coverage_ratio']
        for col in grid_cols:
            if col not in grid_df.columns:
                grid_df[col] = np.nan
        grid_df = grid_df[grid_cols].sort_values('effective_pool_size').reset_index(drop=True)
        grid_df.insert(0, 'year', int(year))
        _write_csv_atomic(grid_df, output_dir / 'pool_size_grid.csv')

    # Canonical figure-facing datasets (pool-size indexed, no recomputation required).
    raw_cols = [
        'K', 'pool_size', 'pool_prop_full', 'coverage_ratio',
        'rmse', 'median_RMSE', 'p90_RMSE', 'max_RMSE', 'rmse_train',
        'max_balance_std', 'mean_balance_std',
        'ess_control', 'ess_ratio',
        'top10_share', 'max_weight_share',
        'support_similarity_min', 'support_similarity_p10', 'support_similarity_median',
        'runtime_seconds', 'convergence',
    ]
    raw_df = results.get('rmse_results', pd.DataFrame()).copy()
    if not raw_df.empty:
        for col in raw_cols:
            if col not in raw_df.columns:
                raw_df[col] = np.nan
        raw_df = raw_df[raw_cols].sort_values('K').reset_index(drop=True)
        raw_df.insert(0, 'output_tag', meta['output_tag'])
        raw_df.insert(0, 'experiment_name', meta['experiment_name'])
        raw_df.insert(0, 'year', meta['year'])
        _write_csv_atomic(raw_df, output_dir / 'embedding_k_raw_results.csv')

    frontier_cols = [
        'pool_size', 'pool_prop_full', 'coverage_ratio', 'representative_K',
        'rmse', 'median_RMSE', 'p90_RMSE', 'max_RMSE',
        'max_balance_std', 'mean_balance_std',
        'ess_control', 'ess_ratio',
        'top10_share', 'max_weight_share',
        'support_similarity_min', 'support_similarity_p10', 'support_similarity_median', 'donor_hash',
    ]
    frontier_df = results.get('effective_pool_table', pd.DataFrame()).copy()
    if not frontier_df.empty:
        for col in frontier_cols:
            if col not in frontier_df.columns:
                frontier_df[col] = np.nan
        frontier_df = frontier_df[frontier_cols].sort_values('pool_size').reset_index(drop=True)
        frontier_df = frontier_df.rename(columns={'pool_size': 'effective_pool_size'})
        frontier_df.insert(0, 'output_tag', meta['output_tag'])
        frontier_df.insert(0, 'experiment_name', meta['experiment_name'])
        frontier_df.insert(0, 'year', meta['year'])
        _write_csv_atomic(frontier_df, output_dir / 'embedding_pool_frontier.csv')

    random_cols = [
        'pool_size', 'pool_prop_full', 'median_RMSE', 'p90_RMSE',
        'median_ess_control', 'median_max_smd',
        'median_top10_share', 'median_max_weight_share', 'reps',
    ]
    random_df = results.get('random_pool_summary', pd.DataFrame()).copy()
    for col in random_cols:
        if col not in random_df.columns:
            random_df[col] = np.nan
    random_df = random_df[random_cols]
    if not random_df.empty:
        random_df = random_df.sort_values('pool_size').reset_index(drop=True)
        random_df = random_df.rename(columns={'pool_size': 'effective_pool_size'})
    random_df.insert(0, 'output_tag', meta['output_tag'])
    random_df.insert(0, 'experiment_name', meta['experiment_name'])
    random_df.insert(0, 'year', meta['year'])
    _write_csv_atomic(random_df, output_dir / 'random_pool_summary.csv')

    overlap_cols = ['pool_size', 'next_pool_size', 'jaccard_overlap']
    overlap_df = results.get('pool_overlap_diagnostics', pd.DataFrame()).copy()
    for col in overlap_cols:
        if col not in overlap_df.columns:
            overlap_df[col] = np.nan
    overlap_df = overlap_df[overlap_cols]
    if not overlap_df.empty:
        overlap_df = overlap_df.sort_values(['pool_size', 'next_pool_size']).reset_index(drop=True)
        overlap_df = overlap_df.rename(columns={'pool_size': 'effective_pool_size', 'next_pool_size': 'next_effective_pool_size'})
    overlap_df.insert(0, 'output_tag', meta['output_tag'])
    overlap_df.insert(0, 'experiment_name', meta['experiment_name'])
    overlap_df.insert(0, 'year', meta['year'])
    _write_csv_atomic(overlap_df, output_dir / 'pool_overlap_diagnostics.csv')

    support_cols = ['K', 'pool_size', 'support_similarity_mean', 'support_similarity_median', 'support_similarity_p10', 'support_similarity_min']
    support_df = results.get('pool_diagnostics', pd.DataFrame()).copy()
    if not support_df.empty:
        for col in support_cols:
            if col not in support_df.columns:
                support_df[col] = np.nan
        support_df = support_df[support_cols].sort_values(['pool_size', 'K']).reset_index(drop=True)
        support_df = support_df.rename(columns={
            'support_similarity_mean': 'mean_similarity',
            'support_similarity_median': 'median_similarity',
            'support_similarity_p10': 'p10_similarity',
            'support_similarity_min': 'min_similarity',
        })
        support_df.insert(0, 'output_tag', meta['output_tag'])
        support_df.insert(0, 'experiment_name', meta['experiment_name'])
        support_df.insert(0, 'year', meta['year'])
        _write_csv_atomic(support_df, output_dir / 'similarity_support.csv')

    selected_controls_csv = (
        DATA_DIR / "cbps_integration" / str(year) /
        f"selected_controls_k{int(results['optimal_K'])}{tag_suffix}_{year}.csv"
    )

    commands_csv_path, commands_sh_path = write_pipeline_commands_fn(
        output_dir=output_dir,
        year=year,
        optimal_k=int(results['optimal_K']),
        selected_controls_csv=selected_controls_csv,
        train_start_year=int(train_years[0]),
        train_end_year=int(train_years[-1]),
        test_start_year=int(test_years[0]),
        test_end_year=int(test_years[-1]),
        analysis_base_dir=args.analysis_base_dir,
        save_full_weights=bool(args.save_full_weights),
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
        include_temporal_placebo=not bool(args.no_temporal_placebo_command),
        temporal_placebo_years=args.temporal_placebo_years,
        temporal_placebo_draws=max(1, int(args.temporal_placebo_draws)),
        temporal_placebo_pre_start=int(args.temporal_placebo_pre_start),
        temporal_placebo_pre_end=int(args.temporal_placebo_pre_end),
        temporal_placebo_post_lag=int(args.temporal_placebo_post_lag),
        temporal_placebo_post_year_count=int(args.temporal_placebo_post_year_count),
    )

    frontier_plots = {}
    if write_frontier_plots:
        frontier_plots = _write_pool_frontier_plots(
            output_dir=output_dir,
            tag_suffix=tag_suffix,
            effective_pool_df=results.get('effective_pool_table', pd.DataFrame()),
            random_summary_df=results.get('random_pool_summary', pd.DataFrame()),
            full_pool_size=results.get('diagnostics', {}).get('full_control_pool') if isinstance(results.get('diagnostics'), dict) else None,
        )

    summary_payload = {
        'year': int(year),
        'optimal_K': int(results['optimal_K']),
        'selected_pool_size': int(results.get('selected_pool_size', -1)),
        'optimal_rmse': float(results['optimal_rmse']),
        'selection_mode': results.get('selection_mode', 'unknown'),
        'treated_subsample_frac': subsample_frac,
        'random_seed': args.random_seed,
        'output_tag': args.output_tag,
        'k_values_requested': [],
        'k_values_evaluated': sorted([int(k) for k in results['rmse_results']['K'].dropna().astype(int).tolist()]) if 'K' in results['rmse_results'].columns else [],
        'rolling_windows': rolling_windows,
        'random_baseline_reps': max(0, int(args.random_baseline_reps)),
        'hard_gates': gates,
        'pipeline_commands_csv': str(commands_csv_path),
        'pipeline_commands_shell': str(commands_sh_path),
        'diagnostics': results.get('diagnostics', {}),
        'plots': frontier_plots,
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
        'selected_controls_csv': str(selected_controls_csv),
        'include_temporal_placebo': not bool(args.no_temporal_placebo_command),
        'temporal_placebo_years': args.temporal_placebo_years,
        'temporal_placebo_draws': max(1, int(args.temporal_placebo_draws)),
        'temporal_placebo_pre_start': int(args.temporal_placebo_pre_start),
        'temporal_placebo_pre_end': int(args.temporal_placebo_pre_end),
        'temporal_placebo_post_lag': int(args.temporal_placebo_post_lag),
        'temporal_placebo_post_year_count': int(args.temporal_placebo_post_year_count),
        'train_years': train_years,
        'test_years': test_years,
    }
    _write_json_atomic(summary_payload, output_dir / summary_name)

    selected_row = frontier_df[frontier_df['representative_K'] == int(results['optimal_K'])] if not frontier_df.empty else pd.DataFrame()
    selected_row = selected_row.iloc[0] if not selected_row.empty else None
    decision_payload = {
        'year': int(year),
        'experiment_name': str(args.experiment_name),
        'output_tag': str(args.output_tag),
        'selected_pool_size': int(results.get('selected_pool_size', -1)),
        'selected_K': int(results['optimal_K']),
        'selection_rule': results.get('selection_rule', results.get('selection_mode', 'unknown')),
        'purpose': 'phase2_comparison',
        'rmse_best': _safe_float(results.get('optimal_rmse', np.nan)),
        'rmse_selected': _safe_float(selected_row.get('median_RMSE', np.nan) if selected_row is not None else results.get('optimal_rmse', np.nan)),
        'ess_selected': _safe_float(selected_row.get('ess_control', np.nan) if selected_row is not None else np.nan),
    }
    _write_json_atomic(decision_payload, output_dir / 'selection_decision.json')

    rmse_var = float(pd.to_numeric(frontier_df.get('median_RMSE', np.nan), errors='coerce').var(ddof=0)) if isinstance(frontier_df, pd.DataFrame) and not frontier_df.empty else np.nan
    ess_var = float(pd.to_numeric(frontier_df.get('ess_control', np.nan), errors='coerce').var(ddof=0)) if isinstance(frontier_df, pd.DataFrame) and not frontier_df.empty else np.nan
    top10_var = float(pd.to_numeric(frontier_df.get('top10_share', np.nan), errors='coerce').var(ddof=0)) if isinstance(frontier_df, pd.DataFrame) and not frontier_df.empty else np.nan

    invariance_warning = bool(
        np.isfinite(rmse_var) and rmse_var < 1e-5 and
        np.isfinite(ess_var) and ess_var < 1e-5 and
        np.isfinite(top10_var) and top10_var < 1e-5
    )

    audit_payload = {
        'year': int(year),
        'experiment_name': str(args.experiment_name),
        'output_tag': str(args.output_tag),
        'assertions': {
            'selected_pool_size_positive': bool(int(results.get('selected_pool_size', -1)) > 0),
            'optimal_k_positive': bool(int(results.get('optimal_K', -1)) > 0),
            'train_end_before_test_start': bool(int(train_years[-1]) < int(test_years[0])),
            'frontier_nonempty': bool(isinstance(frontier_df, pd.DataFrame) and not frontier_df.empty),
            'raw_results_nonempty': bool(isinstance(raw_df, pd.DataFrame) and not raw_df.empty),
        },
        'counts': {
            'raw_rows': int(len(raw_df)) if isinstance(raw_df, pd.DataFrame) else 0,
            'frontier_rows': int(len(frontier_df)) if isinstance(frontier_df, pd.DataFrame) else 0,
            'random_rows': int(len(random_df)) if isinstance(random_df, pd.DataFrame) else 0,
            'support_rows': int(len(support_df)) if isinstance(support_df, pd.DataFrame) else 0,
            'overlap_rows': int(len(overlap_df)) if isinstance(overlap_df, pd.DataFrame) else 0,
        },
        'variance': {
            'rmse': rmse_var,
            'ess': ess_var,
            'top10_share': top10_var,
        },
        'warnings': {
            'invariant_metrics_warning': invariance_warning,
        },
        'selection_mode': str(results.get('selection_mode', 'unknown')),
    }
    _write_json_atomic(audit_payload, output_dir / 'run_audit.json')

    logger.info(f"\nResults saved to {output_dir}/")
    if write_legacy_outputs:
        logger.info(f"  - {elbow_name} (similarity by K)")
        logger.info(f"  - {rmse_name} (RMSPE by K)")
        logger.info(f"  - {effective_pool_name} (primary effective donor-pool frontier)")
        logger.info(f"  - {effective_dup_name} (duplicate-pool sanity spread diagnostics)")
        logger.info(f"  - {pool_name} (realized donor pool and support diagnostics)")
        logger.info(f"  - {mapping_name} (target proportion to realized K mapping)")
        logger.info(f"  - {summary_name} (selection summary)")
    logger.info("  - embedding_k_raw_results.csv (raw evaluated K records)")
    logger.info("  - embedding_pool_frontier.csv (effective donor-pool frontier)")
    logger.info("  - pool_discovery_scan.csv (dense reachable donor-pool scan)")
    logger.info("  - pool_size_grid.csv (effective donor-pool experiment grid)")
    logger.info("  - random_pool_summary.csv (random benchmark by pool size)")
    logger.info("  - pool_overlap_diagnostics.csv (adjacent frontier overlap diagnostics)")
    logger.info("  - similarity_support.csv (embedding support quality by pool size)")
    logger.info("  - selection_decision.json (transparent final decision record)")
    logger.info("  - run_audit.json (assertion audit for this year/run)")
    logger.info(f"  - {commands_csv_path.name} (pipeline commands in CSV)")
    logger.info(f"  - {commands_sh_path.name} (pipeline commands as executable shell)")
    if frontier_plots:
        logger.info("  - frontier plots: %s", ", ".join(Path(v).name for v in frontier_plots.values()))

    logger.info("\n" + "=" * 80)
    logger.info("NEXT STEPS:")
    logger.info("=" * 80)
    logger.info(f"1. Run CBPS with optimal K={results['optimal_K']} for {year}")
    logger.info(
        "   → Rscript Embeddings/scripts/04_run_cbps_with_selected_controls.R "
        f"{year} '{selected_controls_csv.as_posix()}' "
        f"k{int(results['optimal_K'])}{tag_suffix} {int(train_years[0])} {int(train_years[-1])} {int(test_years[0])} {int(test_years[-1])} "
        f"--experiment-name {args.experiment_name} --analysis-base-dir {args.analysis_base_dir}"
    )
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
