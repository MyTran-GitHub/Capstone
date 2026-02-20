"""
Centralized configuration, paths, coordinate utilities for embedding-based matching pipeline.
Handles MGRS/UTM transformations, file paths, and constants.
"""

import logging
from pathlib import Path
from typing import Tuple, Optional, Dict

import pyproj

# ============================================================================
# LOGGING CONFIGURATION
# ============================================================================

def setup_logging(name: str, level: int = logging.INFO) -> logging.Logger:
    """
    Configure logging with INFO/WARN/ERROR markers.

    Args:
        name: Logger name (typically __name__)
        level: Logging level (default INFO)

    Returns:
        Configured logger instance
    """
    logger = logging.getLogger(name)
    if not logger.handlers:
        handler = logging.StreamHandler()
        formatter = logging.Formatter(
            "[%(levelname)s] %(asctime)s - %(name)s - %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S",
        )
        handler.setFormatter(formatter)
        logger.addHandler(handler)
    logger.setLevel(level)
    return logger


# ============================================================================
# CONSTANTS
# ============================================================================

# Temporal configuration
FOCAL_YEARS = list(range(2008, 2020))  # 2008-2019 inclusive
PRE_FOCAL_WINDOW = list(range(2000, 2008))  # 2000-2007 inclusive (8 years)
ALL_YEARS = PRE_FOCAL_WINDOW + FOCAL_YEARS

# Pixel counts per focal year (approximate)
PIXELS_PER_YEAR = 86_725
TREATMENT_PIXELS_PER_YEAR = 2_652
CONTROL_PIXELS_PER_YEAR = 84_073

# Matching parameters
K_NEIGHBORS = 5  # Controls per treatment
EMBEDDING_DIM = 96  # 12 bands/year x 8 years

# ESD configuration
ESD_BANDS_PER_MONTH = 12  # Data bands per year
ESD_TOTAL_BANDS = 13  # Data bands + QA
ESD_MONTHS = 12  # Jan-Dec
ESD_PRE_FOCAL_YEARS = len(PRE_FOCAL_WINDOW)  # 8 years

# QA/QC configuration
QA_THRESHOLD = 5  # Minimum acceptable QA flag
QA_BAND_INDEX = 12  # Band 13 (0-indexed)

# California MGRS tiles covering Capstone study area
CA_MGRS_TILES = [
    "10SDJ", "10SEG", "10SEH", "10SEJ", "10SFE", "10SFF", "10SFJ", "10SGH", "10SGJ", "10TCK", "10TCM", "10TDK", "10TDL", "10TDM", "10TEK", "10TEL", "10TEM", "10TFK", "10TFL", "10TFM", "10TGL", "10TGM",
    "11SKB", "11SKU", "11SLA", "11SLB", "11SLU", "11SLV", "11SMT", "11SNT",
]

# UTM zone for California (varies by longitude, but mostly Zone 11)
UTM_ZONE_PRIMARY = 11
UTM_ZONES = {
    "10SDJ": 10, "10SEG": 10, "10SEH": 10, "10SEJ": 10, "10SFE": 10, "10SFF": 10, "10SFJ": 10, "10SGH": 10, "10SGJ": 10,
    "10TCK": 10, "10TCM": 10, "10TDK": 10, "10TDL": 10, "10TDM": 10, "10TEK": 10, "10TEL": 10, "10TEM": 10, "10TFK": 10, "10TFL": 10, "10TFM": 10, "10TGL": 10, "10TGM": 10,
    "11SKB": 11, "11SKU": 11, "11SLA": 11, "11SLB": 11, "11SLU": 11, "11SLV": 11, "11SMT": 11, "11SNT": 11,
}

# Pixel size and MGRS tile size
ESD_PIXEL_SIZE_M = 30  # 30-meter pixels
MGRS_TILE_SIZE_PIXELS = 3600  # 3600x3600 pixels per MGRS tile (standard)

# ============================================================================
# DIRECTORY CONFIGURATION
# ============================================================================

# Get base directory (Embeddings folder)
BASE_DIR = Path(__file__).resolve().parent
BASE_DIR.mkdir(parents=True, exist_ok=True)

# Input data directories
RDS_DATA_DIR = BASE_DIR.parent / "data" / "processed_data"
ESD_TILES_DIR = BASE_DIR / "embedding_images"
ESD_TILES_DIR.mkdir(parents=True, exist_ok=True)

# ============================================================================
# OUTPUT DIRECTORY STRUCTURE (Reorganized 2026-02-20)
# ============================================================================
#
# New structure organizes all outputs under data/ by analysis phase:
#   data/embeddings/         - Raw embeddings (Phase 0: preprocessing)
#   data/k_selection/        - K optimization results (Phase 1)
#   data/cbps_integration/   - CBPS with optimal K (Phase 1)
#   data/phase2_efficiency/  - Statistical efficiency tests (Phase 2)
#   data/phase3_robustness/  - Robustness checks (Phase 3)
#   data/figures/            - Visualization outputs (all phases)
#
# Scripts reorganized under scripts/ directory
# Documentation moved to docs/ directory
# HPC/SLURM scripts moved to hpc/ directory

# Data output directories
DATA_DIR = BASE_DIR / "data"
DATA_DIR.mkdir(parents=True, exist_ok=True)

OUTPUT_EMBEDDINGS_DIR = DATA_DIR / "embeddings"
OUTPUT_EMBEDDINGS_DIR.mkdir(parents=True, exist_ok=True)

K_SELECTION_DIR = DATA_DIR / "k_selection"
K_SELECTION_DIR.mkdir(parents=True, exist_ok=True)

CBPS_INTEGRATION_DIR = DATA_DIR / "cbps_integration"
CBPS_INTEGRATION_DIR.mkdir(parents=True, exist_ok=True)

PHASE2_DIR = DATA_DIR / "phase2_efficiency"
PHASE2_DIR.mkdir(parents=True, exist_ok=True)

PHASE3_DIR = DATA_DIR / "phase3_robustness"
PHASE3_DIR.mkdir(parents=True, exist_ok=True)

FIGURES_DIR = DATA_DIR / "figures"
FIGURES_DIR.mkdir(parents=True, exist_ok=True)

# Logs directory
LOGS_DIR = BASE_DIR / "logs"
LOGS_DIR.mkdir(parents=True, exist_ok=True)

# Deprecated directories (kept for backward compatibility, but not created)
# RESULTS_DIR was reorganized into data/k_selection and data/cbps_integration
RESULTS_DIR = DATA_DIR  # For backward compatibility with old scripts

# ============================================================================
# FILE PATH FUNCTIONS
# ============================================================================

def get_rds_filepath(year: int) -> Path:
    """
    Get path to RDS analysis data for focal year.

    Args:
        year: Focal year (2008-2019)

    Returns:
        Path to RDS file

    Raises:
        FileNotFoundError: If RDS file doesn't exist
    """
    potential_names = [
        f"analysis_treated{year}_conifer.RDS",
        f"treatment_analysis_{year}.rds",
        f"analysis_data_{year}.rds",
    ]

    for name in potential_names:
        filepath = RDS_DATA_DIR / name
        if filepath.exists():
            return filepath

    return RDS_DATA_DIR / potential_names[0]


def get_esd_filepath(tile: str, year: int, ensure_exists: bool = False) -> Path:
    """Get path to ESD GeoTIFF for specified tile and year."""
    # First try the standard naming convention
    filepath = ESD_TILES_DIR / str(year) / f"ESD_{tile}_{year}.tif"
    
    # If not found, try the SDC30 naming pattern
    if not filepath.exists():
        filepath = ESD_TILES_DIR / str(year) / f"SDC30_EBD_V001_{tile}_{year}.tif"
    
    if ensure_exists and not filepath.exists():
        raise FileNotFoundError(f"ESD tile not found: {filepath}")

    return filepath


def get_embeddings_output_path(year: int) -> Path:
    """Get output path for embeddings CSV for focal year."""
    return OUTPUT_EMBEDDINGS_DIR / f"embeddings_{year}.csv"


# Unused functions - commented out (matching/enrichment pipeline not implemented)
# def get_matches_output_path(year: int) -> Path:
#     """Get output path for matched pairs CSV for focal year."""
#     return MATCHES_DIR / f"matched_pairs_{year}.csv"


# def get_matches_stats_path(year: int) -> Path:
#     """Get output path for matching statistics JSON for focal year."""
#     return MATCHES_DIR / f"matching_stats_{year}.json"


# def get_enriched_output_path(year: int) -> Path:
#     """Get output path for enriched matched pairs CSV for focal year."""
#     return ENRICHED_DIR / f"enriched_matched_pairs_{year}.csv"


# ============================================================================
# COORDINATE TRANSFORMATION UTILITIES
# ============================================================================

class CoordinateTransformer:
    """Utilities for WGS84 <-> UTM <-> MGRS <-> Pixel coordinate transformations."""

    def __init__(self):
        """Initialize pyproj transformers."""
        self.wgs84 = pyproj.CRS("EPSG:4326")
        self.transformers_utm: Dict[int, pyproj.Transformer] = {}

        for zone in set(UTM_ZONES.values()):
            utm_crs = pyproj.CRS(f"EPSG:{32600 + zone if zone <= 30 else 32700 - zone}")
            self.transformers_utm[zone] = pyproj.Transformer.from_crs(
                self.wgs84, utm_crs, always_xy=True
            )

    def latlon_to_utm(self, lat: float, lon: float, zone: Optional[int] = None) -> Tuple[float, float, int]:
        """Convert WGS84 (lat, lon) to UTM (easting, northing)."""
        if zone is None:
            zone = self._determine_utm_zone(lon)

        if zone not in self.transformers_utm:
            utm_crs = pyproj.CRS(f"EPSG:{32600 + zone}")
            self.transformers_utm[zone] = pyproj.Transformer.from_crs(
                self.wgs84, utm_crs, always_xy=True
            )

        easting, northing = self.transformers_utm[zone].transform(lon, lat)
        return easting, northing, zone

    def utm_to_pixel_row_col(
        self,
        easting: float,
        northing: float,
        mgrs_tile: str,
        pixel_size_m: int = ESD_PIXEL_SIZE_M,
        tile_size_pixels: int = MGRS_TILE_SIZE_PIXELS,
    ) -> Tuple[int, int]:
        """Convert UTM coordinates to pixel row, col within MGRS tile."""
        tile_size_m = tile_size_pixels * pixel_size_m

        tile_row = int(mgrs_tile[4]) if len(mgrs_tile) > 4 else 0
        tile_col = ord(mgrs_tile[3]) - ord("A") if len(mgrs_tile) > 3 else 0

        origin_easting = tile_col * tile_size_m
        origin_northing = tile_row * tile_size_m

        pixel_col = int((easting - origin_easting) / pixel_size_m)
        pixel_row = int((northing - origin_northing) / pixel_size_m)

        pixel_row = max(0, min(pixel_row, tile_size_pixels - 1))
        pixel_col = max(0, min(pixel_col, tile_size_pixels - 1))

        return pixel_row, pixel_col

    @staticmethod
    def _determine_utm_zone(lon: float) -> int:
        """Determine UTM zone from longitude."""
        return int((lon + 180) / 6) + 1


# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

def ensure_dir(directory: Path) -> Path:
    """Create directory if it doesn't exist."""
    directory.mkdir(parents=True, exist_ok=True)
    return directory


def validate_year(year: int, context: str = "analysis") -> bool:
    """Validate year is in valid range."""
    if context == "focal":
        return year in FOCAL_YEARS
    if context == "prefocal":
        return year in PRE_FOCAL_WINDOW
    if context == "analysis":
        return year in ALL_YEARS
    return False


def validate_mgrs_tile(tile: str) -> bool:
    """Validate MGRS tile identifier."""
    return tile in CA_MGRS_TILES


def get_utm_zone_for_tile(tile: str) -> int:
    """Get UTM zone for MGRS tile."""
    return UTM_ZONES.get(tile, UTM_ZONE_PRIMARY)


# ============================================================================
# CLI ENTRY POINT (for testing configuration)
# ============================================================================

if __name__ == "__main__":
    logger = setup_logging(__name__)
    logger.info("Focal years: %s", FOCAL_YEARS)
    logger.info("Pre-focal window: %s", PRE_FOCAL_WINDOW)
    logger.info("CA MGRS tiles: %s", CA_MGRS_TILES)
    logger.info("Base directory: %s", BASE_DIR)
    logger.info("Output directories created")
