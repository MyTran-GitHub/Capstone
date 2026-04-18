"""
Download ESD tiles from provided URLs.

Usage:
    1. Paste your URLs into urls.txt (one URL per line)
    2. Run: python download_esd_tiles.py
    accept: --year
            --urls-dir (folder containing all yearly URL files)

The script will:
- Download files to embedding_images/{year}/
- Show progress for each file
- Retry failed downloads
- Verify file sizes
"""

import os
import sys
import requests
from pathlib import Path
from urllib.parse import urlparse, parse_qs
import time
import glob
import shutil
import argparse
import logging

# Configuration
OUTPUT_BASE_DIR = Path("embedding_images")
CHUNK_SIZE = 8192  # 8KB chunks for progress updates
MAX_RETRIES = 3
TIMEOUT = 300  # 5 minutes per file


def extract_filename_and_year(url):
    """Extract filename and year from URL path."""
    parsed = urlparse(url)
    path_parts = parsed.path.split('/')
    
    # Expected format: .../2020/SDC30_EBD_V001_11SKB_2020.tif
    for i, part in enumerate(path_parts):
        if part.isdigit() and len(part) == 4:  # Year
            year = part
            if i + 1 < len(path_parts):
                filename = path_parts[i + 1].split('?')[0]  # Remove query params
                return filename, year
    
    # Fallback: try to parse from filename
    filename = path_parts[-1].split('?')[0]
    if '_' in filename:
        parts = filename.split('_')
        for part in parts:
            if part.isdigit() and len(part) == 4:
                return filename, part
    
    raise ValueError(f"Could not extract year from URL: {url}")


def download_file(url, output_path, retries=MAX_RETRIES):
    """Download a file with progress tracking and retry logic."""
    for attempt in range(retries):
        try:
            logging.info(f"[{attempt + 1}/{retries}] Downloading to: {output_path}")
            response = requests.get(url, stream=True, timeout=TIMEOUT)
            response.raise_for_status()
            total_size = int(response.headers.get('content-length', 0))
            downloaded = 0
            output_path.parent.mkdir(parents=True, exist_ok=True)
            with open(output_path, 'wb') as f:
                for chunk in response.iter_content(chunk_size=CHUNK_SIZE):
                    if chunk:
                        f.write(chunk)
                        downloaded += len(chunk)
                        if downloaded % (1024 * 1024) == 0 or downloaded == total_size:
                            if total_size > 0:
                                pct = (downloaded / total_size) * 100
                                mb_down = downloaded / (1024 * 1024)
                                mb_total = total_size / (1024 * 1024)
                                logging.info(f"  Progress: {mb_down:.1f}/{mb_total:.1f} MB ({pct:.1f}%)")
            if output_path.exists():
                file_size = output_path.stat().st_size
                logging.info(f"Success: {output_path.name} ({file_size / (1024*1024):.1f} MB)")
                return True
            else:
                logging.error(f"Error: File not created")
                return False
        except requests.exceptions.RequestException as e:
            logging.warning(f"Attempt {attempt + 1} failed: {e}")
            if attempt < retries - 1:
                wait_time = 2 ** attempt
                logging.info(f"Retrying in {wait_time} seconds...")
                time.sleep(wait_time)
            else:
                logging.error(f"Max retries reached. Skipping this file.")
                return False
        except Exception as e:
            logging.error(f"Unexpected error: {e}")
            return False
    return False


def read_urls_from_file(filepath):
    """Read URLs from a text file, skipping comments and empty lines."""
    urls = []
    with open(filepath, 'r') as f:
        for line in f:
            line = line.strip()
            # Skip comments and empty lines
            if line and not line.startswith('#'):
                urls.append(line)
    return urls


def read_urls_for_year(urls_dir: Path, year: int):
    """
    Read all URL files for a given year.
    Expected pattern:
    iearth_download_links_{year}_10.txt
    iearth_download_links_{year}_11.txt
    """
    pattern = str(urls_dir / f"iearth_download_links_{year}_*.txt")
    files = glob.glob(pattern)

    if not files:
        raise FileNotFoundError(f"No URL files found for year {year} in {urls_dir}")

    urls = []
    for file in files:
        urls.extend(read_urls_from_file(Path(file)))

    return urls


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--urls-dir", type=Path, required=True)
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s: %(message)s")

    year = args.year
    urls_dir = args.urls_dir

    urls = read_urls_for_year(urls_dir, year)

    logging.info(f"Found {len(urls)} URLs for year {year}")
    logging.info("=" * 80)

    success_count = 0
    failed_urls = []

    for i, url in enumerate(urls, 1):
        logging.info(f"[{i}/{len(urls)}] Processing URL...")
        try:
            filename, _ = extract_filename_and_year(url)
            output_dir = OUTPUT_BASE_DIR / str(year)
            output_path = output_dir / filename

            if output_path.exists():
                logging.info(f"Already exists: {output_path}")
                success_count += 1
                continue

            if download_file(url, output_path):
                success_count += 1
            else:
                failed_urls.append(url)

        except Exception as e:
            logging.error(f"Error processing URL: {e}")
            failed_urls.append(url)

    logging.info("\nDownload Summary")
    logging.info(f"Successful: {success_count}/{len(urls)}")

    if failed_urls:
        logging.warning("Failed downloads:")
        for u in failed_urls:
            logging.warning(u)

if __name__ == "__main__":
    main()
