"""
Extract 12-dimensional embeddings from a single ESD GeoTIFF year.
Modified version for testing with a single year of data.
Uses GeoTIFF transforms to avoid spatial misalignment.
"""

import argparse
import logging
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple

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
    ensure_dir,
    ESD_BANDS_PER_MONTH,
    ESD_TOTAL_BANDS,
    QA_THRESHOLD,
    QA_BAND_INDEX,
    validate_mgrs_tile,
    LOGS_DIR,
)

logger = setup_logging(__name__, logging.INFO)

BATCH_SIZE = 1000
# Single year = 12 dimensions (12 bands)
SINGLE_YEAR_EMBEDDING_DIM = 12


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


def extract_pixel_embedding(tile_data: Dict, row: int, col: int) -> Tuple[bool, np.ndarray, Dict]:
    """Extract 12-dim embedding for one year at a pixel row/col."""
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

    embedding = data[:ESD_BANDS_PER_MONTH, row, col].astype(np.float32)
    invalid_mask = ~np.isfinite(embedding)
    if invalid_mask.any():
        embedding[invalid_mask] = 0
        report["invalid_values"] = int(invalid_mask.sum())

    report["qa_value"] = int(qa_value)
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

    ok, embedding, report = extract_pixel_embedding(tile_data, row, col)
    if not ok:
        result["error"] = report.get("error", "extraction failed")
        return False, result

    # Store embedding values (use 'band_' prefix to match test workflow)
    for i, val in enumerate(embedding):
        result[f"band_{i}"] = float(val)

    result["qa_value"] = report.get("qa_value", 0)
    result["embedding_norm"] = float(np.linalg.norm(embedding))
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

    logger.info("[INFO] Extracted %s embeddings", len(results_df))
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

    return True, results_df


def resolve_input_csv(year: int, input_csv: Optional[Path]) -> Path:
    """Resolve the input CSV path for a given year."""
    if input_csv is not None:
        return input_csv

    # Try standard paths (RDS exported to CSV)
    candidates = [
        Path("data/processed_data/rev_analysis_low") / f"analysis_treated{year}_conifer.csv",
        Path("../data/processed_data/rev_analysis_low") / f"analysis_treated{year}_conifer.csv",
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

        output_csv = args.output_csv or Path(f"embeddings_single_year_{args.year}.csv")
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
