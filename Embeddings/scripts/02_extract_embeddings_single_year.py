"""
Extract 12-dimensional embeddings from a single ESD GeoTIFF year.
Modified version for testing with a single year of data.
Uses GeoTIFF transforms to avoid spatial misalignment.

run this first pip install packaging
Usage: python 02_extract_embeddings_single_year.py --year 2008
"""

import argparse
import logging
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple

# Add parent directory and scripts directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

import numpy as np
import pandas as pd
from tqdm import tqdm

try:
    import rasterio
    from rasterio.transform import rowcol
    from rasterio.warp import transform as rio_transform
    rasterio_available = True
except ImportError:
    rasterio_available = False

from config import (
    setup_logging,
    get_esd_filepath,
    get_embeddings_output_path,
    ensure_dir,
    ESD_BANDS_PER_MONTH,
    ESD_TOTAL_BANDS,
    QA_THRESHOLD,
    QA_BAND_INDEX,
    validate_mgrs_tile,
    LOGS_DIR,
)
from utils.esd_quantizer import Quantizer, dequantize_pixel

logger = setup_logging(__name__, logging.INFO)

BATCH_SIZE = 1000
# Single year embeddings:
# - 12 months of codes → 12 months × 6 channels per month = 72 dimensions
# - Codes are uint16 values that get dequantized to 6D vectors in [-1, 1]
SINGLE_YEAR_EMBEDDING_DIM = 72  # 12 months × 6 channels
ESD_CHANNELS_PER_MONTH = 6  # Dequantized vector dimensions per month

# Initialize quantizer (used globally for all extractions)
GLOBAL_QUANTIZER = Quantizer(use_torch=False)


class ESDTileCache:
    """Cache for loaded ESD GeoTIFF tiles (data + transform + CRS)."""

    def __init__(self, max_cache_size: int = 6):
        self.cache: Dict[str, Dict] = {}
        self.max_size = max_cache_size
        self.access_order: List[str] = []

    def get(self, filepath: Path) -> Optional[Dict]:
        key = str(filepath)
        if key in self.cache:
            self.access_order.remove(key)
            self.access_order.append(key)
            return self.cache[key]
        return None

    def put(self, filepath: Path, tile_data: Dict) -> None:
        key = str(filepath)
        if len(self.cache) >= self.max_size:
            oldest = self.access_order.pop(0)
            del self.cache[oldest]
        self.cache[key] = tile_data
        if key in self.access_order:
            self.access_order.remove(key)
        self.access_order.append(key)


def get_mgrs_tile_from_coords(lat: float, lon: float) -> str:
    """Return 5-char MGRS tile code for a lat/lon."""
    import mgrs

    mgrs_obj = mgrs.MGRS()
    code = mgrs_obj.toMGRS(lat, lon)  # Returns full MGRS string like '11SLA6566240454'
    return code[:5]  # Return just the tile code '11SLA'


def load_esd_tile(filepath: Path, year: int, tile: str) -> Tuple[bool, Optional[Dict], Dict]:
    """Load ESD tile and return data + transform + CRS metadata."""
    report = {"tile": tile, "year": year, "filepath": str(filepath)}

    if not rasterio_available:
        logger.error("[ERROR] rasterio required. Install with: pip install rasterio")
        report["error"] = "rasterio not available"
        return False, None, report

    if not filepath.exists():
        report["error"] = "file not found"
        return False, None, report

    try:
        with rasterio.open(filepath) as src:
            data = src.read()
            tile_data = {
                "data": data,
                "transform": src.transform,
                "crs": src.crs,
                "bounds": src.bounds,
                "height": src.height,
                "width": src.width,
            }
            report["shape"] = data.shape
            return True, tile_data, report
    except Exception as exc:
        report["error"] = str(exc)
        return False, None, report


def latlon_to_rowcol(lat: float, lon: float, tile_data: Dict) -> Tuple[int, int]:
    """Convert WGS84 lat/lon to tile row/col using GeoTIFF transform."""
    crs = tile_data.get("crs")
    transform = tile_data.get("transform")
    bounds = tile_data.get("bounds")
    if crs is None or transform is None:
        raise ValueError("Tile CRS/transform missing")

    if crs.to_epsg() == 4326:
        x, y = lon, lat
    else:
        x_list, y_list = rio_transform("EPSG:4326", crs, [lon], [lat])
        x, y = x_list[0], y_list[0]

    if bounds is not None:
        left, bottom, right, top = bounds
        if not (left <= x <= right and bottom <= y <= top):
            raise ValueError("point outside tile bounds")

    row, col = rowcol(transform, x, y)
    return int(row), int(col)


def extract_pixel_embedding(tile_data: Dict, row: int, col: int, quantizer: Quantizer) -> Tuple[bool, np.ndarray, Dict]:
    """Extract 72-dim embedding (12 months × 6 channels) for one year at a pixel row/col.
    
    Process:
    1. Extract 12 codes (uint16) from the GeoTIFF (bands 0-11)
    2. Check QA band (band 12) for quality threshold
    3. Dequantize codes to 6D vectors per month using the Quantizer
    4. Return flattened 72D embedding
    """
    data = tile_data["data"]
    report = {"row": row, "col": col}

    n_bands, height, width = data.shape
    if not (0 <= row < height and 0 <= col < width):
        report["error"] = "out of bounds"
        return False, np.array([]), report

    if n_bands < ESD_TOTAL_BANDS:
        report["error"] = "insufficient bands"
        return False, np.array([]), report

    if n_bands <= QA_BAND_INDEX:
        report["error"] = "missing QA band"
        return False, np.array([]), report

    qa_value = data[QA_BAND_INDEX, row, col]
    if qa_value < QA_THRESHOLD:
        report["qa_value"] = int(qa_value)
        report["error"] = "qa_below_threshold"
        return False, np.array([]), report

    # Extract codes (uint16 quantized values) for 12 months
    codes = data[:ESD_BANDS_PER_MONTH, row, col].astype(np.int32)
    
    # Check for invalid codes (NaN, negative, etc.)
    invalid_mask = (codes < 0) | np.isnan(codes.astype(np.float32))
    if invalid_mask.any():
        report["invalid_codes"] = int(invalid_mask.sum())
        report["error"] = "invalid_codes"
        return False, np.array([]), report

    # Dequantize codes to vectors (12 months → 12 × 6 = 72D)
    try:
        vectors = dequantize_pixel(codes, quantizer)  # Shape: (12, 6)
        embedding = vectors.flatten()  # Shape: (72,)
    except Exception as exc:
        report["error"] = f"dequantization_failed: {exc}"
        return False, np.array([]), report
    
    # Verify output validity (values should be in [-1, 1] after dequantization)
    if not np.all(np.isfinite(embedding)):
        report["error"] = "non_finite_vectors"
        return False, np.array([]), report

    report["qa_value"] = int(qa_value)
    report["value_range"] = [float(embedding.min()), float(embedding.max())]
    return True, embedding, report


def extract_embeddings_for_pixel(
    pixel_id: str,
    lat: float,
    lon: float,
    treated: int,
    year: int,
    unit: Optional[str] = None,
    tile_cache: Optional[ESDTileCache] = None,
) -> Tuple[bool, Dict]:
    """Extract 12-dim embedding for a pixel from a single year."""
    result = {
        "pixel_id": pixel_id,
        "lat": lat,
        "lon": lon,
        "treated": treated,
        "year": year,
    }
    if unit is not None:
        result["unit"] = unit

    if tile_cache is None:
        tile_cache = ESDTileCache()

    tile = get_mgrs_tile_from_coords(lat, lon)
    result["tile"] = tile
    result["tile_in_ca"] = validate_mgrs_tile(tile)

    esd_filepath = get_esd_filepath(tile, year, ensure_exists=False)
    if not esd_filepath.exists():
        result["error"] = f"missing tile: {esd_filepath}"
        return False, result

    tile_data = tile_cache.get(esd_filepath)
    if tile_data is None:
        success, tile_data, _ = load_esd_tile(esd_filepath, year, tile)
        if not success or tile_data is None:
            result["error"] = f"failed to load tile: {esd_filepath}"
            return False, result
        tile_cache.put(esd_filepath, tile_data)

    try:
        row, col = latlon_to_rowcol(lat, lon, tile_data)
    except Exception as exc:
        result["error"] = f"rowcol transform failed: {exc}"
        return False, result

    result["pixel_row"] = row
    result["pixel_col"] = col

    ok, embedding, report = extract_pixel_embedding(tile_data, row, col, GLOBAL_QUANTIZER)
    if not ok:
        result["error"] = report.get("error", "extraction failed")
        return False, result

    # After dequantization, values are in [-1, 1] range
    # Apply L2-normalization for consistent similarity computation
    embedding_norm = np.linalg.norm(embedding)
    if embedding_norm > 1e-10:  # Avoid division by zero
        embedding = embedding / embedding_norm
    else:
        # This shouldn't happen with valid dequantized vectors
        result["error"] = "zero_vector"
        return False, result
    
    # Store embedding values (72 dimensions: 12 months × 6 channels)
    for i, val in enumerate(embedding):
        result[f"band_{i}"] = float(val)

    result["qa_value"] = report.get("qa_value", 0)
    result["embedding_norm"] = float(np.linalg.norm(embedding))  # Should be ~1.0 after normalization
    result["value_range_pre_norm"] = report.get("value_range", [0, 0])
    result["success"] = True
    return True, result


def extract_embeddings_for_year(
    data_df: pd.DataFrame,
    year: int,
    output_csv: Optional[Path] = None,
) -> Tuple[bool, pd.DataFrame]:
    """Extract embeddings for all pixels in a year."""
    logger.info("[INFO] Extracting embeddings for year %s", year)

    lat_col = "LATITUDE" if "LATITUDE" in data_df.columns else "latitude" if "latitude" in data_df.columns else None
    lon_col = "LONGITUDE" if "LONGITUDE" in data_df.columns else "longitude" if "longitude" in data_df.columns else None
    if lat_col is None or lon_col is None:
        raise ValueError("Input data must include LATITUDE/LONGITUDE columns (case-insensitive).")

    tile_cache = ESDTileCache(max_cache_size=6)
    results: List[Dict] = []
    errors: List[str] = []

    total_pixels = len(data_df)
    n_batches = (total_pixels + BATCH_SIZE - 1) // BATCH_SIZE
    
    # Quick diagnostic: check which tiles are needed
    if len(data_df) > 0:
        logger.info("[INFO] Input columns: %s", list(data_df.columns[:10]))
        logger.info("[INFO] Using lat column: '%s', lon column: '%s'", lat_col, lon_col)
        
        # Check first pixel in detail
        first_row = data_df.iloc[0]
        logger.info("[INFO] First pixel - lat: %s, lon: %s", first_row[lat_col], first_row[lon_col])
        
        sample_tiles = []
        sample_errors = []
        for idx, row in data_df.head(10).iterrows():
            try:
                lat_val = float(row[lat_col])
                lon_val = float(row[lon_col])
                tile = get_mgrs_tile_from_coords(lat_val, lon_val)
                sample_tiles.append(tile)
            except Exception as e:
                sample_errors.append(str(e))
        
        logger.info("[INFO] Sample tiles from first 10 pixels: %s", set(sample_tiles))
        if sample_errors:
            logger.error("[ERROR] MGRS conversion errors: %s", sample_errors[:3])
        
        if sample_tiles:
            sample_path = get_esd_filepath(sample_tiles[0], year)
            logger.info("[INFO] Looking for tiles at: %s", sample_path.parent)
            logger.info("[INFO] Example tile path: %s", sample_path)

    for batch_idx in tqdm(range(n_batches), desc=f"Processing {year}", ncols=100):
        batch_start = batch_idx * BATCH_SIZE
        batch_end = min(batch_start + BATCH_SIZE, total_pixels)
        batch_df = data_df.iloc[batch_start:batch_end]

        for _, row in batch_df.iterrows():
            try:
                lat_val = float(row[lat_col])
                lon_val = float(row[lon_col])
                # Preserve 'unit' column if it exists (from RDS files)
                unit_val = row.get("unit", None)
                success, result = extract_embeddings_for_pixel(
                    pixel_id=row.get("pixel_id", f"{lat_val}_{lon_val}"),
                    lat=lat_val,
                    lon=lon_val,
                    treated=int(row.get("treated", 0)),
                    year=year,
                    unit=unit_val,
                    tile_cache=tile_cache,
                )
                results.append(result)
                if not success:
                    errors.append(result.get("error", "unknown error"))
            except Exception as exc:
                errors.append(str(exc))

    results_df = pd.DataFrame(results)

    logger.info("[INFO] Extracted %s embeddings (72D: 12 months × 6 channels)", len(results_df))
    
    # Check if ANY embeddings were successfully extracted
    embedding_cols = [f"band_{i}" for i in range(SINGLE_YEAR_EMBEDDING_DIM)]
    has_embeddings = all(col in results_df.columns for col in embedding_cols)
    
    if not has_embeddings:
        logger.error("[ERROR] No embeddings extracted - all pixels failed!")
        logger.error("[ERROR] This usually means:")
        logger.error("[ERROR]   1. ESD tiles not found at expected location")
        logger.error("[ERROR]   2. Coordinates outside tile bounds")
        logger.error("[ERROR]   3. QA values below threshold")
        
        # Analyze failure reasons
        if "error" in results_df.columns:
            logger.error("[ERROR] Failure breakdown:")
            error_counts = results_df["error"].value_counts()
            for error_type, count in error_counts.items():
                pct = 100 * count / len(results_df)
                logger.error("[ERROR]   %s: %d pixels (%.1f%%)", error_type, count, pct)
        
        return False, results_df
    
    # Check for NaN values in embedding bands (failed extractions)
    nan_mask = results_df[embedding_cols].isna().any(axis=1)
    nan_count = nan_mask.sum()
    
    if nan_count > 0:
        logger.warning("[WARN] Found %s pixels with NaN embeddings (failed extraction)", nan_count)
        logger.info("[INFO] Breaking down failed pixels by error type:")
        
        failed_df = results_df[nan_mask]
        if "error" in failed_df.columns:
            error_counts = failed_df["error"].value_counts()
            for error_type, count in error_counts.items():
                logger.info("[INFO]   %s: %d pixels", error_type, count)
        
        logger.info("[INFO] FILTERING OUT failed pixels (keeping only valid embeddings)")
        results_df = results_df[~nan_mask].reset_index(drop=True)
        logger.info("[INFO] After filtering: %s pixels remain", len(results_df))
    
    if not results_df.empty and "tile_in_ca" in results_df.columns:
        outside_mask = results_df["tile_in_ca"] == False
        outside_count = int(outside_mask.sum())
        if outside_count:
            outside_pct = 100 * outside_count / len(results_df)
            logger.warning("[WARN] Pixels outside CA tiles: %s/%s (%.2f%%)", outside_count, len(results_df), outside_pct)
    if not results_df.empty and "error" in results_df.columns:
        missing_mask = results_df["error"].astype(str).str.contains("missing tile", na=False)
        missing_count = int(missing_mask.sum())
        if missing_count:
            missing_pct = 100 * missing_count / len(results_df)
            logger.warning("[WARN] Missing tiles: %s/%s pixels (%.2f%%)", missing_count, len(results_df), missing_pct)
    if errors:
        logger.warning("[WARN] %s errors during extraction", len(errors))
        logger.warning("[WARN] Sample errors: %s", errors[:5])

    if output_csv:
        ensure_dir(output_csv.parent)
        results_df.to_csv(output_csv, index=False)
        logger.info("[INFO] Saved embeddings to: %s", output_csv)
    
    # Validate extracted embeddings
    if not results_df.empty:
        embedding_cols = [f"band_{i}" for i in range(SINGLE_YEAR_EMBEDDING_DIM)]
        
        # Check for any remaining NaN (should be none after filtering)
        all_present = results_df[embedding_cols].notna().all().all()
        if all_present:
            logger.info("[INFO] ✓ All embedding columns populated (no NaN)")
        else:
            remaining_nan = results_df[embedding_cols].isna().sum().sum()
            logger.error("[ERROR] ⚠ %d NaN values still present after filtering!", remaining_nan)
        
        # Embedding norm statistics
        embedding_matrix = results_df[embedding_cols].values
        norms = np.linalg.norm(embedding_matrix, axis=1)
        
        # Check for zero vectors (all bands ≈ 0) - indicate failed extraction
        zero_rows = norms < 1e-10
        zero_count = zero_rows.sum()
        if zero_count > 0:
            zero_pct = 100 * zero_count / len(results_df)
            logger.warning("[WARN] ⚠ %d pixels (%.2f%%) are zero vectors (norm≈0)", zero_count, zero_pct)
            logger.warning("[WARN]    These are likely failed extractions that were filled with 0 in earlier processing")
        else:
            logger.info("[INFO] ✓ No zero vectors")
        
        logger.info("[INFO] Embedding norm statistics (n=%d, 72D vectors):", len(results_df))
        logger.info("[INFO]   Min: %.6f", np.min(norms))
        logger.info("[INFO]   Max: %.6f", np.max(norms))
        logger.info("[INFO]   Mean: %.6f (should be near 1.0 if L2-normalized)", np.mean(norms))
        logger.info("[INFO]   Median: %.6f", np.median(norms))
        
        # Check if embeddings are normalized
        mean_norm = np.mean(norms)
        if 0.99 < mean_norm < 1.01:
            logger.info("[INFO] ✓ Embeddings appear L2-normalized (mean norm ≈ 1.0)")
        else:
            logger.warning("[WARN] ⚠ Embeddings may not be normalized (mean norm = %.4f)", mean_norm)
        
        # Check pre-normalization value ranges (should be in [-1, 1] after dequantization)
        if 'value_range_pre_norm' in results_df.columns:
            try:
                ranges = results_df['value_range_pre_norm'].apply(lambda x: eval(x) if isinstance(x, str) else x)
                min_vals = ranges.apply(lambda r: r[0] if isinstance(r, list) and len(r) > 0 else np.nan)
                max_vals = ranges.apply(lambda r: r[1] if isinstance(r, list) and len(r) > 1 else np.nan)
                
                logger.info("[INFO] Pre-normalization value range (after dequantization):")
                logger.info("[INFO]   Overall min: %.6f (expected ≈ -1.0)", min_vals.min())
                logger.info("[INFO]   Overall max: %.6f (expected ≈ +1.0)", max_vals.max())
                
                if min_vals.min() < -1.1 or max_vals.max() > 1.1:
                    logger.warning("[WARN] ⚠ Dequantized values outside expected [-1, 1] range!")
            except Exception as e:
                logger.debug("[DEBUG] Could not parse value ranges: %s", e)

    return True, results_df


def resolve_input_csv(year: int, input_csv: Optional[Path]) -> Path:
    """Resolve the input CSV path for a given year."""
    if input_csv is not None:
        return input_csv

    # Try standard paths (RDS exported to CSV)
    candidates = [
        Path("data/processed_data/rev_analysis_low") / f"analysis_treated{year}_conifer.csv",
        Path("./data/processed_data/rev_analysis_low") / f"analysis_treated{year}_conifer.csv",
        Path(__file__).parent.parent / "data" / "processed_data" / "rev_analysis_low" / f"analysis_treated{year}_conifer.csv",
    ]
    for path in candidates:
        if path.exists():
            return path

    raise FileNotFoundError(
        f"No input CSV found for year {year}. Provide --input-csv or create filtered data."
    )


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Extract 12-dimensional embeddings from single-year ESD tiles"
    )
    parser.add_argument("--year", type=int, required=True, help="Year to extract (e.g., 2019)")
    parser.add_argument("--input-csv", type=Path, help="Input CSV with pixel data")
    parser.add_argument("--output-csv", type=Path, help="Output CSV for embeddings")
    parser.add_argument(
        "--log-level",
        default="INFO",
        choices=["DEBUG", "INFO", "WARNING", "ERROR"],
        help="Logging level",
    )

    args = parser.parse_args()

    logger.setLevel(getattr(logging, args.log_level))
    ensure_dir(LOGS_DIR)
    file_handler = logging.FileHandler(LOGS_DIR / f"03_extract_embeddings_single_{args.year}.log")
    file_handler.setFormatter(logging.Formatter("[%(levelname)s] %(asctime)s - %(message)s"))
    logger.addHandler(file_handler)

    if not rasterio_available:
        logger.error("[ERROR] rasterio not available. Install with: pip install rasterio")
        return 1

    try:
        input_path = resolve_input_csv(args.year, args.input_csv)
        logger.info("[INFO] Loading input data from: %s", input_path)
        data_df = pd.read_csv(input_path)
        logger.info("[INFO] Loaded %s pixels", len(data_df))

        output_csv = args.output_csv or get_embeddings_output_path(args.year)
        success, results_df = extract_embeddings_for_year(data_df, args.year, output_csv)

        if success:
            n_success = results_df.get("success", pd.Series([False])).sum()
            logger.info("[INFO] Extraction complete: %s/%s pixels successful", n_success, len(results_df))
            return 0
        else:
            logger.error("[ERROR] Extraction failed")
            return 1

    except Exception as exc:
        logger.error("[ERROR] Failed: %s", exc)
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
