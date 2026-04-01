import subprocess
import shutil
from pathlib import Path

START_YEAR = 2008
END_YEAR = 2018

URLS_DIR = Path("url_folder")  # Folder containing all yearly URL files
EMBEDDING_DIR = Path("embedding_images")

DOWNLOAD_SCRIPT = "01_download_esd_tiles.py"
EXTRACT_SCRIPT = "02_extract_embeddings_single_year.py"


def run_command(cmd):
    print(f"\nRunning: {' '.join(cmd)}")
    result = subprocess.run(cmd)
    if result.returncode != 0:
        raise RuntimeError("Command failed")


def delete_year_folder(year):
    year_path = EMBEDDING_DIR / str(year)
    if year_path.exists():
        print(f"Deleting tiles for {year} to free storage...")
        shutil.rmtree(year_path)


def main():
    for year in range(START_YEAR, END_YEAR + 1):
        print("\n" + "=" * 100)
        print(f"PROCESSING YEAR {year}")
        print("=" * 100)

        # 1️⃣ Download
        run_command([
            "python",
            DOWNLOAD_SCRIPT,
            "--year", str(year),
            "--urls-dir", str(URLS_DIR)
        ])

        # 2️⃣ Extract embeddings
        run_command([
            "python",
            EXTRACT_SCRIPT,
            "--year", str(year)
        ])

        # 3️⃣ Delete tiles
        delete_year_folder(year)

        print(f"Year {year} complete.\n")

    print("ALL YEARS COMPLETE.")


if __name__ == "__main__":
    main()