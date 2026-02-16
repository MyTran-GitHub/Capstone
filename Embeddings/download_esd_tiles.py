"""
Download ESD tiles from provided URLs.

Usage:
    1. Paste your URLs into urls.txt (one URL per line)
    2. Run: python download_esd_tiles.py

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
            print(f"\n[{attempt + 1}/{retries}] Downloading to: {output_path}")
            
            # Stream the download
            response = requests.get(url, stream=True, timeout=TIMEOUT)
            response.raise_for_status()
            
            # Get file size if available
            total_size = int(response.headers.get('content-length', 0))
            
            # Download with progress
            downloaded = 0
            output_path.parent.mkdir(parents=True, exist_ok=True)
            
            with open(output_path, 'wb') as f:
                for chunk in response.iter_content(chunk_size=CHUNK_SIZE):
                    if chunk:
                        f.write(chunk)
                        downloaded += len(chunk)
                        
                        # Show progress every MB
                        if downloaded % (1024 * 1024) == 0 or downloaded == total_size:
                            if total_size > 0:
                                pct = (downloaded / total_size) * 100
                                mb_down = downloaded / (1024 * 1024)
                                mb_total = total_size / (1024 * 1024)
                                print(f"  Progress: {mb_down:.1f}/{mb_total:.1f} MB ({pct:.1f}%)", end='\r')
            
            # Verify file was created
            if output_path.exists():
                file_size = output_path.stat().st_size
                print(f"\n✓ Success: {output_path.name} ({file_size / (1024*1024):.1f} MB)")
                return True
            else:
                print(f"\n✗ Error: File not created")
                return False
                
        except requests.exceptions.RequestException as e:
            print(f"\n✗ Attempt {attempt + 1} failed: {e}")
            if attempt < retries - 1:
                wait_time = 2 ** attempt  # Exponential backoff
                print(f"  Retrying in {wait_time} seconds...")
                time.sleep(wait_time)
            else:
                print(f"  Max retries reached. Skipping this file.")
                return False
        except Exception as e:
            print(f"\n✗ Unexpected error: {e}")
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


def main():
    """Main download workflow."""
    
    # Check for URLs file
    urls_file = Path("urls.txt")
    if not urls_file.exists():
        print("Error: urls.txt not found!")
        print("\nPlease create urls.txt with your download URLs (one per line)")
        print("Then run this script again.")
        sys.exit(1)
    
    # Read URLs
    urls = read_urls_from_file(urls_file)
    if not urls:
        print("Error: No URLs found in urls.txt")
        sys.exit(1)
    
    print(f"Found {len(urls)} URLs to download")
    print("=" * 80)
    
    # Download each file
    success_count = 0
    failed_urls = []
    
    for i, url in enumerate(urls, 1):
        print(f"\n[{i}/{len(urls)}] Processing URL...")
        
        try:
            filename, year = extract_filename_and_year(url)
            output_dir = OUTPUT_BASE_DIR / year
            output_path = output_dir / filename
            
            # Skip if already exists
            if output_path.exists():
                file_size = output_path.stat().st_size
                print(f"⊙ Already exists: {output_path} ({file_size / (1024*1024):.1f} MB)")
                print("  Skipping download.")
                success_count += 1
                continue
            
            # Download
            if download_file(url, output_path):
                success_count += 1
            else:
                failed_urls.append(url)
                
        except Exception as e:
            print(f"✗ Error processing URL: {e}")
            failed_urls.append(url)
    
    # Summary
    print("\n" + "=" * 80)
    print(f"\nDownload Summary:")
    print(f"  Total files: {len(urls)}")
    print(f"  Successful: {success_count}")
    print(f"  Failed: {len(failed_urls)}")
    
    if failed_urls:
        print(f"\nFailed URLs:")
        for url in failed_urls:
            print(f"  - {url}")
        
        # Save failed URLs for retry
        failed_file = Path("urls_failed.txt")
        with open(failed_file, 'w') as f:
            for url in failed_urls:
                f.write(url + '\n')
        print(f"\nFailed URLs saved to: {failed_file}")
        print("You can retry by renaming this file to urls.txt")


if __name__ == "__main__":
    main()
