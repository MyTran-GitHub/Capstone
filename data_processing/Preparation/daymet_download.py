#!/usr/bin/env python3
import os
import sys
import ssl
import time
import warnings
from pathlib import Path

import fsspec
import numpy as np
import pandas as pd
import planetary_computer as pc
import xarray as xr
from pyproj import CRS, Transformer
from urllib.parse import urlparse

# ============================================================
# CONFIGURATION
# ============================================================
# Resolve workspace root relative to this script
WORKSPACE_ROOT = Path(__file__).resolve().parents[2]

CONIFER_CSV = WORKSPACE_ROOT / "data" / "processed_data" / "conifer_grid_points.csv"

OUT_DIR = WORKSPACE_ROOT / "data" / "raw_data" / "daymet_data"
OUT_DIR.mkdir(parents=True, exist_ok=True)

DAYMET_ZARR = (
    "https://daymeteuwest.blob.core.windows.net/"
    "daymet-zarr/monthly/na.zarr"
)

VAR_MAP = {
    "prcp": "prcp",    # precipitation
    "tmin": "minat",   # minimum temperature
    "tmax": "maxat",   # maximum temperature
    "vp": "wvp",       # vapor pressure
    "swe": "swe",      # snow water equivalent
}

DEFAULT_YEARS = range(2000, 2021)

MAX_RETRIES = 3
RETRY_DELAY = 30  # seconds

# ============================================================
# SSL / WARNING SUPPRESSION (HPC SAFE)
# ============================================================
warnings.filterwarnings("ignore")
ssl._create_default_https_context = ssl._create_unverified_context
os.environ.update({
    "PYTHONHTTPSVERIFY": "0",
    "SSL_VERIFY": "0",
    "CURL_CA_BUNDLE": "",
    "REQUESTS_CA_BUNDLE": "",
})

# ============================================================
# COMMAND-LINE YEAR PARSING
# ============================================================
if len(sys.argv) == 3:
    START_YEAR = int(sys.argv[1])
    END_YEAR = int(sys.argv[2])
    YEARS = range(START_YEAR, END_YEAR + 1)
else:
    YEARS = DEFAULT_YEARS

print(f"Processing years: {YEARS.start}–{YEARS.stop - 1}")

# ============================================================
# LOAD CONIFER GRID POINTS
# ============================================================
df = pd.read_csv(CONIFER_CSV)
lon = df["LONGITUDE"].to_numpy()
lat = df["LATITUDE"].to_numpy()

# ============================================================
# OPEN DAYMET ZARR (SIGNED, SYNC, SAFE)
# ============================================================
def open_daymet_zarr():
    signed = pc.sign(DAYMET_ZARR)
    sas = urlparse(signed).query

    fs = fsspec.filesystem(
        "abfs",
        account_name="daymeteuwest",
        sas_token=sas,
        asynchronous=False,
    )

    store = fs.get_mapper("daymet-zarr/monthly/na.zarr")

    return xr.open_zarr(
        store,
	consolidated=True,
        chunks=None,  # force synchronous reads
    )

ds = open_daymet_zarr()

# ============================================================
# CRS HANDLING
# ============================================================
daymet_crs = CRS.from_cf(ds["lambert_conformal_conic"].attrs)
transformer = Transformer.from_crs(4326, daymet_crs, always_xy=True)
x_proj, y_proj = transformer.transform(lon, lat)

x_grid = ds["x"].values
y_grid = ds["y"].values

ix = np.searchsorted(x_grid, x_proj)
iy = np.searchsorted(y_grid[::-1], y_proj)
iy = len(y_grid) - iy - 1

valid = (
    (ix >= 0) & (ix < len(x_grid)) &
    (iy >= 0) & (iy < len(y_grid))
)

if not np.any(valid):
    raise RuntimeError("No valid grid points found in Daymet domain for the provided conifer grid.")

ix = ix[valid]
iy = iy[valid]

mask = np.zeros((len(y_grid), len(x_grid)), dtype=bool)
mask[iy, ix] = True

ys, xs = np.where(mask)
y0, y1 = ys.min(), ys.max()
x0, x1 = xs.min(), xs.max()
mask_clip = mask[y0:y1 + 1, x0:x1 + 1]

lat_clip = ds["lat"].isel(y=slice(y0, y1 + 1), x=slice(x0, x1 + 1))
lon_clip = ds["lon"].isel(y=slice(y0, y1 + 1), x=slice(x0, x1 + 1))

# ============================================================
# WRITE YEAR (SAS-SAFE)
# ============================================================
def write_year(daymet_var: str, r_var: str, year: int):
    out_path = OUT_DIR / f"{r_var}_{year}.nc"
    if os.path.exists(out_path):
        print(f"Skipping existing file: {r_var}_{year}.nc")
        return

    print(f"Writing {r_var} {year}")

    # Re-open dataset with fresh SAS
    ds_local = open_daymet_zarr()

    da = ds_local[daymet_var].sel(time=str(year))
    da = da.isel(y=slice(y0, y1 + 1), x=slice(x0, x1 + 1))
    da = da.where(mask_clip)
    da.load()  # FORCE FULL READ BEFORE TOKEN EXPIRY

    out = xr.Dataset(
        {
            f"Band{i+1}": (("y", "x"), da.isel(time=i).values)
            for i in range(12)
        },
	coords={
            "x": ds["x"].isel(x=slice(x0, x1 + 1)).values,
            "y": ds["y"].isel(y=slice(y0, y1 + 1)).values,
            "lat": (("y", "x"), lat_clip.values),
            "lon": (("y", "x"), lon_clip.values),
        },
	attrs={
            "source": "Daymet v4 (Planetary Computer)",
            "variable": r_var,
            "year": year,
            "aggregation": "monthly",
        },
    )

    encoding = {
        f"Band{i+1}": {
            "zlib": True,
            "complevel": 4,
            "chunksizes": (500, 500),
        }
	for i in range(12)
    }

    out.to_netcdf(out_path, encoding=encoding)
    print(f"Saved: {r_var}_{year}.nc")

# ============================================================
# MAIN LOOP (RESUME-SAFE)
# ============================================================
for year in YEARS:
    for dvar, rvar in VAR_MAP.items():
        for attempt in range(1, MAX_RETRIES + 1):
            try:
                write_year(dvar, rvar, year)
                break
            except Exception as e:
                print(f"Error {rvar} {year} (attempt {attempt}): {e}")
                if attempt < MAX_RETRIES:
                    time.sleep(RETRY_DELAY)
                else:
                    print(f"FAILED permanently: {rvar} {year}")