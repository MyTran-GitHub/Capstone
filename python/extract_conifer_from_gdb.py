# Extract conifer grid from CAL FIRE fveg22_1.gdb using geopandas
# This Python script handles .gdb format which R's sf struggles with

import geopandas as gpd
import pandas as pd
import os

# Paths
gdb_path = "data/raw_data/fveg22_1.gdb"
grid_path = "data/raw_data/gpw_grid_ca.RDS"
output_path = "data/processed_data/conifer_polygons.geojson"

print("Reading CAL FIRE vegetation geodatabase...")
# Read the geodatabase - it should have a layer name
# Common layer names: fveg22_1, CALVEG, ExistingVegetation
try:
    veg = gpd.read_file(gdb_path)
except Exception as e:
    print(f"Error reading as single layer: {e}")
    print("\nListing available layers...")
    import fiona
    layers = fiona.listlayers(gdb_path)
    print(f"Available layers: {layers}")
    if len(layers) > 0:
        print(f"\nReading first layer: {layers[0]}")
        veg = gpd.read_file(gdb_path, layer=layers[0])
    else:
        raise Exception("No layers found in geodatabase")

print(f"Vegetation data loaded: {len(veg)} features")
print(f"Columns: {list(veg.columns)}")

# Find the conifer type column
# Common names: WHR13_TYPE, WHRTYPE13, COVERTYPE, WHR_TYPE
type_cols = [c for c in veg.columns if 'WHR' in c.upper() or 'TYPE' in c.upper() or 'COVER' in c.upper()]
print(f"\nPotential type columns: {type_cols}")

if len(type_cols) > 0:
    type_col = type_cols[0]
    print(f"Using column: {type_col}")
    print(f"Unique values (first 20): {veg[type_col].unique()[:20]}")
    
    # Filter to conifer types
    # Codes 31 and 32 are Montane and Subalpine Coniferous
    if veg[type_col].dtype in ['int64', 'float64']:
        conifer = veg[veg[type_col].isin([31, 32])]
    else:
        # String matching for conifer keywords
        conifer = veg[veg[type_col].str.contains('Conifer|conifer|CON', case=False, na=False)]
    
    print(f"\nConifer features: {len(conifer)}")
    
    # Save as GeoJSON for R to read easily
    conifer = conifer.to_crs("EPSG:4326")  # WGS84
    conifer.to_file(output_path, driver='GeoJSON')
    print(f"✓ Saved conifer polygons to {output_path}")
    
else:
    print("WARNING: Could not identify type column. Saving all features.")
    veg.to_crs("EPSG:4326").to_file(output_path, driver='GeoJSON')
