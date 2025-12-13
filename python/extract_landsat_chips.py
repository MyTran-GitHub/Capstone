#!/usr/bin/env python3
"""
Extract Landsat imagery chips for conifer grid units.

This script uses Google Earth Engine to extract pre-treatment Landsat composites
for each pixel in the conifer analysis dataset. Outputs are saved as GeoTIFF chips
suitable for embedding extraction.

Usage:
    python extract_landsat_chips.py --year 2012 --sample 1000
"""

import argparse
import os
import sys
from pathlib import Path
from typing import List, Tuple

import ee
import pandas as pd
import numpy as np
from tqdm import tqdm

# Initialize Earth Engine (requires authentication first time)
try:
    ee.Initialize()
except Exception:
    print("Authenticating with Google Earth Engine...")
    ee.Authenticate()
    ee.Initialize()


def load_conifer_units(analysis_file: str, sample_size: int = None) -> pd.DataFrame:
    """Load conifer analysis dataset and extract unit coordinates."""
    try:
        import rpy2.robjects as ro
        from rpy2.robjects import pandas2ri
        pandas2ri.activate()
        
        ro.r(f'dat <- readRDS("{analysis_file}")')
        df = ro.r('as.data.frame(dat[, c("unit", "LATITUDE", "LONGITUDE", "treated")])')
        df = pandas2ri.rpy2py(df)
    except ImportError:
        print("Warning: rpy2 not available. Please export units to CSV from R first.")
        print("Run in R: write.csv(dat[, c('unit','LATITUDE','LONGITUDE','treated')], 'data/processed_data/conifer_units.csv')")
        csv_path = 'data/processed_data/conifer_units.csv'
        if not os.path.exists(csv_path):
            raise FileNotFoundError(f"Please create {csv_path} from R first")
        df = pd.read_csv(csv_path)
    
    df = df.drop_duplicates(subset=['unit'])
    
    if sample_size and sample_size < len(df):
        print(f"Sampling {sample_size} units from {len(df)} total")
        df = df.sample(n=sample_size, random_state=42)
    
    return df


def get_landsat_composite(
    lat: float, 
    lon: float, 
    years: List[int],
    buffer_m: int = 500
) -> ee.Image:
    """
    Extract Landsat 8 median composite for a point location.
    
    Args:
        lat: Latitude
        lon: Longitude
        years: List of years for composite (e.g., [2009, 2010, 2011])
        buffer_m: Buffer radius in meters (500m = ~1km² area)
    
    Returns:
        Earth Engine Image object
    """
    point = ee.Geometry.Point([lon, lat])
    region = point.buffer(buffer_m).bounds()
    
    # Landsat 8 Collection 2 Level-2
    start_date = f'{min(years)}-01-01'
    end_date = f'{max(years)}-12-31'
    
    collection = (ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
                  .filterBounds(point)
                  .filterDate(start_date, end_date)
                  .filter(ee.Filter.lt('CLOUD_COVER', 20)))
    
    # Apply scaling factors for Collection 2
    def apply_scale_factors(image):
        optical = image.select('SR_B.').multiply(0.0000275).add(-0.2)
        thermal = image.select('ST_B.*').multiply(0.00341802).add(149.0)
        return image.addBands(optical, None, True).addBands(thermal, None, True)
    
    collection = collection.map(apply_scale_factors)
    
    # Median composite
    composite = collection.median().clip(region)
    
    # Select bands: Blue, Green, Red, NIR, SWIR1, SWIR2
    bands = ['SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7']
    return composite.select(bands)


def export_chip(
    image: ee.Image,
    lat: float,
    lon: float,
    unit_id: str,
    output_dir: Path,
    scale: int = 30
) -> str:
    """
    Export a single chip to GeoTIFF.
    
    Args:
        image: Earth Engine image
        lat, lon: Center coordinates
        unit_id: Unique pixel identifier
        output_dir: Directory to save chips
        scale: Resolution in meters
    
    Returns:
        Path to saved file
    """
    point = ee.Geometry.Point([lon, lat])
    region = point.buffer(500).bounds()
    
    # Create safe filename
    safe_id = unit_id.replace('.', '_').replace('-', 'm')
    filename = f"chip_{safe_id}.tif"
    filepath = output_dir / filename
    
    if filepath.exists():
        return str(filepath)
    
    # Export to Drive or download directly
    # For small batches, direct download:
    try:
        url = image.getDownloadURL({
            'region': region.getInfo()['coordinates'],
            'scale': scale,
            'format': 'GEO_TIFF',
            'filePerBand': False
        })
        
        import urllib.request
        urllib.request.urlretrieve(url, filepath)
        return str(filepath)
        
    except Exception as e:
        print(f"Warning: Could not download {unit_id}: {e}")
        return None


def main():
    parser = argparse.ArgumentParser(description='Extract Landsat chips for conifer units')
    parser.add_argument('--year', type=int, default=2012, help='Focal treatment year')
    parser.add_argument('--sample', type=int, default=None, help='Sample N units (None = all)')
    parser.add_argument('--pre-window', type=int, default=3, help='Years before treatment for composite')
    parser.add_argument('--output-dir', type=str, default='data/imagery', help='Output directory')
    
    args = parser.parse_args()
    
    # Setup paths
    base_dir = Path(__file__).parent.parent
    analysis_file = base_dir / 'data' / 'processed_data' / 'analysis_conifer' / f'analysis_treated{args.year}_conifer.RDS'
    output_dir = base_dir / args.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)
    
    print(f"Loading conifer units from {analysis_file}")
    units = load_conifer_units(str(analysis_file), sample_size=args.sample)
    
    print(f"Processing {len(units)} units")
    print(f"Pre-treatment window: {args.year - args.pre_window} to {args.year - 1}")
    
    pre_years = list(range(args.year - args.pre_window, args.year))
    
    successful = 0
    failed = 0
    
    for idx, row in tqdm(units.iterrows(), total=len(units), desc="Extracting chips"):
        try:
            composite = get_landsat_composite(
                row['LATITUDE'],
                row['LONGITUDE'],
                pre_years
            )
            
            result = export_chip(
                composite,
                row['LATITUDE'],
                row['LONGITUDE'],
                row['unit'],
                output_dir
            )
            
            if result:
                successful += 1
            else:
                failed += 1
                
        except Exception as e:
            print(f"\nError processing {row['unit']}: {e}")
            failed += 1
    
    print(f"\nComplete: {successful} successful, {failed} failed")
    print(f"Chips saved to {output_dir}")
    
    # Save manifest
    manifest = units.copy()
    manifest['chip_path'] = manifest['unit'].apply(
        lambda x: str(output_dir / f"chip_{x.replace('.', '_').replace('-', 'm')}.tif")
    )
    manifest.to_csv(output_dir / 'chip_manifest.csv', index=False)
    print(f"Manifest saved to {output_dir / 'chip_manifest.csv'}")


if __name__ == '__main__':
    main()
