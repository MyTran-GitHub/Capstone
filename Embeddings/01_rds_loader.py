"""
Load, validate, and prepare RDS analysis data for embedding extraction.
Uses rpy2 to interface with R data structures.
"""

import argparse
import logging
import sys
from pathlib import Path
from typing import Dict, Tuple, Optional
import warnings

import pandas as pd
import numpy as np

try:
    from rpy2.robjects import pandas2ri, ro
    from rpy2.robjects.conversion import localconverter
    rpy2_available = True
except ImportError:
    rpy2_available = False

from config import (
    setup_logging, get_rds_filepath, FOCAL_YEARS, RDS_DATA_DIR,
    ensure_dir, validate_year, TREATMENT_PIXELS_PER_YEAR, CONTROL_PIXELS_PER_YEAR,
    PIXELS_PER_YEAR, LOGS_DIR
)

# ============================================================================
# LOGGING
# ============================================================================

logger = setup_logging(__name__)

# ============================================================================
# RDS LOADING FUNCTIONS
# ============================================================================

def load_rds_as_dataframe(filepath: Path) -> pd.DataFrame:
    """
    Load RDS file and convert to pandas DataFrame using rpy2.
    
    Args:
        filepath: Path to RDS file
        
    Returns:
        pandas DataFrame containing RDS data
        
    Raises:
        FileNotFoundError: If RDS file not found
        ImportError: If rpy2 not available
        RuntimeError: If RDS loading fails
    """
    if not rpy2_available:
        raise ImportError("rpy2 required for RDS loading. Install with: pip install rpy2")
    
    filepath = Path(filepath)
    if not filepath.exists():
        raise FileNotFoundError(f"RDS file not found: {filepath}")
    
    try:
        logger.info(f"[INFO] Loading RDS file: {filepath}")
        
        # Use rpy2 to load RDS
        ro.r(f'data <- readRDS("{str(filepath)}")')
        
        # Convert to pandas DataFrame
        with localconverter(ro.default_converter + pandas2ri.converter):
            df = ro.conversion.rpy2py(ro.r('data'))
        
        logger.info(f"[INFO] Successfully loaded RDS. Shape: {df.shape}")
        return df
    
    except Exception as e:
        logger.error(f"[ERROR] Failed to load RDS file: {e}")
        raise RuntimeError(f"RDS loading failed: {e}") from e


def validate_analysis_data(df: pd.DataFrame, year: int) -> Tuple[bool, Dict]:
    """
    Validate RDS analysis data structure and content.
    
    Args:
        df: Input DataFrame
        year: Focal year for record
        
    Returns:
        (is_valid, validation_report) tuple
    """
    report = {"year": year, "checks": {}, "valid": True}
    
    # Check required columns
    required_cols = ['treated', 'LATITUDE', 'LONGITUDE']
    missing_cols = [col for col in required_cols if col not in df.columns]
    
    if missing_cols:
        logger.warning(f"[WARN] Missing columns: {missing_cols}")
        report["checks"]["missing_columns"] = missing_cols
        report["valid"] = False
    else:
        logger.info(f"[INFO] All required columns present")
        report["checks"]["required_columns"] = "OK"
    
    # Check for null values in critical columns
    for col in required_cols:
        if col in df.columns:
            null_count = df[col].isnull().sum()
            if null_count > 0:
                logger.warning(f"[WARN] Column '{col}' has {null_count} null values")
                report["checks"][f"nulls_{col}"] = int(null_count)
            else:
                report["checks"][f"nulls_{col}"] = 0
    
    # Check treatment distribution
    if 'treated' in df.columns:
        treatment_counts = df['treated'].value_counts()
        logger.info(f"[INFO] Treatment distribution:\n{treatment_counts}")
        report["checks"]["treatment_counts"] = treatment_counts.to_dict()
    
    # Check geographic bounds (California approx)
    if 'LATITUDE' in df.columns and 'LONGITUDE' in df.columns:
        lat_valid = ((df['LATITUDE'] >= 32.5) & (df['LATITUDE'] <= 42.0)).all()
        lon_valid = ((df['LONGITUDE'] >= -124.5) & (df['LONGITUDE'] <= -114.0)).all()
        
        if lat_valid and lon_valid:
            logger.info(f"[INFO] Geographic bounds valid (California)")
            report["checks"]["geography"] = "OK"
        else:
            logger.warning(f"[WARN] Some coordinates outside California bounds")
            report["checks"]["geography"] = "OUT_OF_BOUNDS"
            report["valid"] = False
    
    return report["valid"], report


def clean_and_prepare_data(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """
    Clean data: create pixel_id, remove duplicates, add metadata.
    
    Args:
        df: Input DataFrame
        year: Focal year
        
    Returns:
        Cleaned DataFrame ready for embedding extraction
    """
    logger.info(f"[INFO] Cleaning and preparing data for year {year}")
    
    df_clean = df.copy()
    
    # Create pixel IDs from coordinates
    if 'LATITUDE' in df_clean.columns and 'LONGITUDE' in df_clean.columns:
        df_clean['pixel_id'] = (
            df_clean['LATITUDE'].round(6).astype(str) + '_' +
            df_clean['LONGITUDE'].round(6).astype(str)
        )
    
    logger.info(f"[INFO] Created {df_clean['pixel_id'].nunique()} unique pixel IDs")
    
    # Handle duplicate coordinates (keep first occurrence)
    initial_count = len(df_clean)
    df_clean = df_clean.drop_duplicates(subset=['LATITUDE', 'LONGITUDE'], keep='first')
    removed_count = initial_count - len(df_clean)
    if removed_count > 0:
        logger.warning(f"[WARN] Removed {removed_count} duplicate pixel locations")
    
    # Add focal year column
    df_clean['focal_year'] = year
    
    # Ensure numeric types
    df_clean['LATITUDE'] = pd.to_numeric(df_clean['LATITUDE'], errors='coerce')
    df_clean['LONGITUDE'] = pd.to_numeric(df_clean['LONGITUDE'], errors='coerce')
    if 'treated' in df_clean.columns:
        df_clean['treated'] = df_clean['treated'].astype(int)
    
    # Remove rows with NaN coordinates
    na_count = df_clean[['LATITUDE', 'LONGITUDE']].isnull().any(axis=1).sum()
    if na_count > 0:
        logger.warning(f"[WARN] Removing {na_count} rows with null coordinates")
        df_clean = df_clean.dropna(subset=['LATITUDE', 'LONGITUDE'])
    
    logger.info(f"[INFO] Final cleaned data shape: {df_clean.shape}")
    return df_clean


def save_prepared_data(df: pd.DataFrame, year: int, output_dir: Optional[Path] = None) -> Path:
    """
    Save cleaned data to CSV for next pipeline stage.
    """
    if output_dir is None:
        from config import OUTPUT_EMBEDDINGS_DIR
        output_dir = OUTPUT_EMBEDDINGS_DIR.parent / "prepared_analysis_data"
    
    ensure_dir(output_dir)
    
    out_path = output_dir / f"prepared_analysis_{year}.csv"
    df.to_csv(out_path, index=False)
    logger.info(f"[INFO] Saved prepared data to: {out_path}")
    
    return out_path


# ============================================================================
# MAIN PIPELINE
# ============================================================================

def process_single_year(year: int, output_dir: Optional[Path] = None) -> Tuple[bool, pd.DataFrame]:
    """
    Load, validate, and prepare data for single focal year.
    """
    try:
        # Validate year
        if not validate_year(year, context='focal'):
            logger.error(f"[ERROR] Invalid focal year: {year}")
            return False, pd.DataFrame()
        
        # Load RDS
        rds_path = get_rds_filepath(year)
        df = load_rds_as_dataframe(rds_path)
        
        # Validate
        is_valid, validation_report = validate_analysis_data(df, year)
        if not is_valid:
            logger.warning(f"[WARN] Validation failed for year {year}: {validation_report}")
        
        # Clean and prepare
        df_clean = clean_and_prepare_data(df, year)
        
        # Save
        save_prepared_data(df_clean, year, output_dir)
        
        logger.info(f"[INFO] Successfully processed year {year}")
        return True, df_clean
    
    except Exception as e:
        logger.error(f"[ERROR] Processing year {year} failed: {e}")
        return False, pd.DataFrame()


def process_all_years(output_dir: Optional[Path] = None) -> Dict[int, Tuple[bool, pd.DataFrame]]:
    """
    Process all focal years.
    """
    results = {}
    for year in FOCAL_YEARS:
        logger.info(f"[INFO] ============ Processing year {year} ============")
        success, df = process_single_year(year, output_dir)
        results[year] = (success, df)
    
    successful = sum(1 for success, _ in results.values() if success)
    logger.info(f"[INFO] Processed {successful}/{len(FOCAL_YEARS)} years successfully")
    
    return results


# ============================================================================
# CLI INTERFACE
# ============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Load, validate, and prepare RDS analysis data"
    )
    parser.add_argument(
        "--year",
        type=int,
        help="Specific focal year to process (default: all)"
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="Output directory for prepared data"
    )
    parser.add_argument(
        "--log-level",
        default="INFO",
        choices=["DEBUG", "INFO", "WARNING", "ERROR"],
        help="Logging level"
    )
    
    args = parser.parse_args()
    
    # Set logging level
    logger.setLevel(getattr(logging, args.log_level))
    
    ensure_dir(LOGS_DIR)
    file_handler = logging.FileHandler(LOGS_DIR / "01_rds_loader.log")
    file_handler.setFormatter(
        logging.Formatter('[%(levelname)s] %(asctime)s - %(message)s')
    )
    logger.addHandler(file_handler)
    
    logger.info(f"[INFO] Starting RDS loader with args: {args}")
    
    # Check rpy2 availability
    if not rpy2_available:
        logger.error("[ERROR] rpy2 not available. Install with: pip install rpy2")
        return 1
    
    # Process
    if args.year:
        success, df = process_single_year(args.year, args.output_dir)
        if success:
            logger.info(f"[INFO] Year {args.year} processed successfully")
            return 0
        else:
            logger.error(f"[ERROR] Year {args.year} processing failed")
            return 1
    else:
        results = process_all_years(args.output_dir)
        successful = sum(1 for success, _ in results.values() if success)
        if successful == len(FOCAL_YEARS):
            logger.info(f"[INFO] All years processed successfully")
            return 0
        else:
            logger.error(f"[ERROR] Some years failed")
            return 1


if __name__ == "__main__":
    sys.exit(main())
