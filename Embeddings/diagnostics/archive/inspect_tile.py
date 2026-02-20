"""Quick inspection of ESD tiles."""
import sys
import rasterio
from pathlib import Path

# Accept tile path as argument or use default
if len(sys.argv) > 1:
    tile_path = sys.argv[1]
else:
    # Default to 11SLA 2019 for testing
    tile_path = "../embedding_images/2019/SDC30_EBD_V001_11SLA_2019.tif"
    print(f"No path provided, using default: {tile_path}\n")

if not Path(tile_path).exists():
    print(f"ERROR: Tile not found: {tile_path}")
    print(f"Usage: python inspect_tile.py [path/to/tile.tif]")
    sys.exit(1)

print(f"Opening: {tile_path}\n")

with rasterio.open(tile_path) as src:
    print("=== TILE METADATA ===")
    print(f"Shape (height x width): {src.shape}")
    print(f"Number of bands: {src.count}")
    print(f"CRS: {src.crs}")
    print(f"Bounds: {src.bounds}")
    print(f"Transform:\n{src.transform}")
    print(f"Pixel size: {src.res}")
    print(f"Data types: {src.dtypes[0]}")
    
    # Get WGS84 bounds
    from rasterio.warp import transform_bounds
    bounds_wgs84 = transform_bounds(src.crs, "EPSG:4326", *src.bounds)
    west, south, east, north = bounds_wgs84
    print(f"\n=== COVERAGE (WGS84) ===")
    print(f"Longitude: [{west:.6f}, {east:.6f}]")
    print(f"Latitude: [{south:.6f}, {north:.6f}]")
    
    # Sample center pixel
    data = src.read()
    center_y, center_x = src.height // 2, src.width // 2
    
    print(f"\n=== SAMPLE PIXEL (center: row={center_y}, col={center_x}) ===")
    for i in range(min(src.count, 13)):
        value = data[i, center_y, center_x]
        label = f"Band {i+1:2d}"
        if i == 12:
            label += " (QA)"
        print(f"{label}: {value:8.2f}")
    
    print(f"\n=== STATUS ===")
    if src.count >= 13:
        print("✓ Tile has 13 bands (12 data + 1 QA)")
    else:
        print(f"⚠ Expected 13 bands, found {src.count}")
