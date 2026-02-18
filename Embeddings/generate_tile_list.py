"""
Generate list of MGRS tiles needed for pixel extraction.
Scans input CSV and outputs unique tiles with pixel counts.
Can process single year or all years 2008-2019.
"""

import argparse
import sys
from pathlib import Path
from collections import Counter, defaultdict

import pandas as pd
import mgrs

from config import setup_logging, FOCAL_YEARS

logger = setup_logging(__name__)


def get_mgrs_tile_from_coords(lat: float, lon: float) -> str:
    """Return 5-char MGRS tile code for a lat/lon."""
    mgrs_obj = mgrs.MGRS()
    code = mgrs_obj.toMGRS(lat, lon)
    return code[:5]


def generate_tile_list(csv_path: Path, output_path: Path = None) -> dict:
    """
    Scan CSV and generate list of needed MGRS tiles.
    
    Args:
        csv_path: Path to input CSV with LATITUDE/LONGITUDE columns
        output_path: Optional path to save tile list
        
    Returns:
        Dictionary mapping tile codes to pixel counts
    """
    logger.info("Reading CSV: %s", csv_path)
    df = pd.read_csv(csv_path)
    
    # Find lat/lon columns (case-insensitive)
    lat_col = None
    lon_col = None
    for col in df.columns:
        if col.upper() == "LATITUDE":
            lat_col = col
        elif col.upper() == "LONGITUDE":
            lon_col = col
    
    if lat_col is None or lon_col is None:
        raise ValueError(f"CSV must contain LATITUDE and LONGITUDE columns. Found: {df.columns.tolist()}")
    
    logger.info("Found %d pixels in CSV", len(df))
    logger.info("Using columns: %s, %s", lat_col, lon_col)
    
    # Convert all pixels to MGRS tiles
    logger.info("Converting coordinates to MGRS tiles...")
    tiles = []
    errors = 0
    
    for idx, row in df.iterrows():
        try:
            lat = float(row[lat_col])
            lon = float(row[lon_col])
            tile = get_mgrs_tile_from_coords(lat, lon)
            tiles.append(tile)
        except Exception as e:
            errors += 1
            if errors <= 5:
                logger.warning("Failed to convert pixel %d: %s", idx, e)
    
    if errors > 0:
        logger.warning("Total conversion errors: %d/%d pixels", errors, len(df))
    
    # Count pixels per tile
    tile_counts = Counter(tiles)
    
    # Sort by pixel count (descending)
    sorted_tiles = sorted(tile_counts.items(), key=lambda x: x[1], reverse=True)
    
    logger.info("\n" + "="*60)
    logger.info("TILE COVERAGE SUMMARY")
    logger.info("="*60)
    logger.info("Total unique tiles needed: %d", len(sorted_tiles))
    logger.info("Total pixels processed: %d", sum(tile_counts.values()))
    logger.info("")
    
    # Display tile breakdown
    logger.info("%-10s %10s %8s %12s", "Tile", "Pixels", "Percent", "Cumulative")
    logger.info("-"*60)
    
    cumulative = 0
    total = sum(tile_counts.values())
    
    for tile, count in sorted_tiles:
        cumulative += count
        pct = 100 * count / total
        cum_pct = 100 * cumulative / total
        logger.info("%-10s %10d %7.2f%% %11.2f%%", tile, count, pct, cum_pct)
    
    # Save to file if requested
    if output_path:
        output_path = Path(output_path)
        with open(output_path, 'w') as f:
            f.write("# MGRS tiles needed for extraction\n")
            f.write(f"# Generated from: {csv_path}\n")
            f.write(f"# Total tiles: {len(sorted_tiles)}\n")
            f.write(f"# Total pixels: {total}\n\n")
            
            for tile, count in sorted_tiles:
                f.write(f"{tile}\t{count}\n")
        
        logger.info("\nTile list saved to: %s", output_path)
    
    return dict(tile_counts)


def process_all_years(data_dir: Path, output_file: Path):
    """
    Process all focal years (2008-2019) and generate single combined tile list.
    
    Args:
        data_dir: Directory containing analysis CSV files
        output_file: Path to save combined tile list
    """
    all_tiles = Counter()  # Combined tiles across all years
    years_processed = []
    
    for year in FOCAL_YEARS:
        logger.info("\n" + "="*60)
        logger.info("PROCESSING YEAR %d", year)
        logger.info("="*60)
        
        # Find CSV file for this year
        csv_candidates = [
            data_dir / f"analysis_treated{year}_conifer.csv",
            data_dir / "rev_analysis_low" / f"analysis_treated{year}_conifer.csv",
        ]
        
        csv_path = None
        for candidate in csv_candidates:
            if candidate.exists():
                csv_path = candidate
                break
        
        if csv_path is None:
            logger.warning("CSV not found for year %d, skipping", year)
            continue
        
        # Process this year (don't save individual file)
        try:
            logger.info("Reading %s", csv_path)
            df = pd.read_csv(csv_path)
            
            # Find lat/lon columns
            lat_col = None
            lon_col = None
            for col in df.columns:
                if col.upper() == "LATITUDE":
                    lat_col = col
                elif col.upper() == "LONGITUDE":
                    lon_col = col
            
            if lat_col is None or lon_col is None:
                logger.warning("Year %d: Missing LATITUDE/LONGITUDE columns, skipping", year)
                continue
            
            logger.info("Found %d pixels", len(df))
            
            # Convert coordinates to tiles
            for idx, row in df.iterrows():
                try:
                    lat = float(row[lat_col])
                    lon = float(row[lon_col])
                    tile = get_mgrs_tile_from_coords(lat, lon)
                    all_tiles[tile] += 1
                except Exception:
                    pass  # Skip failed conversions
            
            years_processed.append(year)
            logger.info("✓ Year %d complete", year)
                
        except Exception as e:
            logger.error("Failed to process year %d: %s", year, e)
            continue
    
    # Generate combined tile list across all years
    if all_tiles:
        logger.info("\n" + "="*60)
        logger.info("COMBINED TILE LIST (ALL YEARS 2008-2019)")
        logger.info("="*60)
        
        sorted_tiles = sorted(all_tiles.items(), key=lambda x: x[1], reverse=True)
        total_pixels = sum(all_tiles.values())
        
        logger.info("Years processed: %s", years_processed)
        logger.info("Total unique tiles: %d", len(sorted_tiles))
        logger.info("Total pixels across all years: %d", total_pixels)
        logger.info("")
        logger.info("%-10s %12s %8s", "Tile", "Total Pixels", "Percent")
        logger.info("-"*60)
        
        for tile, count in sorted_tiles:
            pct = 100 * count / total_pixels
            logger.info("%-10s %12d %7.2f%%", tile, count, pct)
        
        # Save combined list
        output_file.parent.mkdir(parents=True, exist_ok=True)
        with open(output_file, 'w') as f:
            f.write("# MGRS tiles needed for extraction (ALL YEARS 2008-2019)\n")
            f.write(f"# Years processed: {years_processed}\n")
            f.write(f"# Total unique tiles: {len(sorted_tiles)}\n")
            f.write(f"# Total pixels: {total_pixels}\n\n")
            
            for tile, count in sorted_tiles:
                f.write(f"{tile}\t{count}\n")
        
        logger.info("\n✓ Tile list saved to: %s", output_file)
    
    return all_tiles


def main():
    parser = argparse.ArgumentParser(
        description="Generate MGRS tile list from pixel coordinates"
    )
    parser.add_argument(
        "--input-csv",
        type=Path,
        help="Input CSV with LATITUDE/LONGITUDE columns (single file)"
    )
    parser.add_argument(
        "--data-dir",
        type=Path,
        help="Directory containing analysis CSV files for all years"
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=None,
        help="Output file/directory for tile list(s)"
    )
    parser.add_argument(
        "--all-years",
        action="store_true",
        help="Process all focal years 2008-2019"
    )
    
    args = parser.parse_args()
    
    try:
        if args.all_years:
            # Process all years
            if not args.data_dir:
                args.data_dir = Path("../data/processed_data")
            
            if not args.data_dir.exists():
                logger.error("Data directory not found: %s", args.data_dir)
                return 1
            
            output_file = args.output or Path("tiles_needed_ALL_YEARS.txt")
            logger.info("Processing all years 2008-2019")
            logger.info("Data directory: %s", args.data_dir)
            logger.info("Output file: %s", output_file)
            
            all_tiles = process_all_years(args.data_dir, output_file)
            
            logger.info("\n" + "="*60)
            logger.info("✓ COMPLETE")
            logger.info("="*60)
            logger.info("Total unique tiles needed: %d", len(all_tiles))
            logger.info("\nDownload these tiles to get full coverage across all years 2008-2019.")
            
        else:
            # Process single file
            if not args.input_csv:
                logger.error("Must specify --input-csv or use --all-years")
                return 1
            
            if not args.input_csv.exists():
                logger.error("Input CSV not found: %s", args.input_csv)
                return 1
            
            output_path = args.output or Path("tiles_needed.txt")
            
            tile_counts = generate_tile_list(args.input_csv, output_path)
            
            logger.info("\n" + "="*60)
            logger.info("SUMMARY")
            logger.info("="*60)
            logger.info("Tiles to download: %d", len(tile_counts))
            logger.info("Top 5 tiles by pixel count:")
            for tile, count in sorted(tile_counts.items(), key=lambda x: x[1], reverse=True)[:5]:
                logger.info("  %s: %d pixels", tile, count)
        
        return 0
        
    except Exception as e:
        logger.error("Failed: %s", e)
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
