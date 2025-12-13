#!/bin/bash
# Complete pipeline to build conifer environmental panel from scratch

set -e  # Exit on error

echo "======================================"
echo "Conifer Environmental Panel Pipeline"
echo "======================================"

# Step 0: Create conifer mask
echo ""
echo "[1/6] Creating conifer mask from CAL FIRE vegetation..."
Rscript R/00_create_conifer_mask.R

# Step 1: Attach elevation
echo ""
echo "[2/6] Downloading and attaching elevation data..."
Rscript R/01_attach_elevation_conifer.R

# Step 2: Process Daymet climate
echo ""
echo "[3/6] Extracting Daymet climate variables..."
Rscript R/02_download_daymet_conifer.R

# Step 3: Process MTBS severity
echo ""
echo "[4/6] Processing MTBS fire severity..."
Rscript R/03_process_mtbs_conifer.R

# Step 4: Process tree cover
echo ""
echo "[5/6] Extracting tree canopy cover..."
Rscript R/04_process_tree_cover_conifer.R

# Step 5: Process prescribed fire
echo ""
echo "[6/6] Processing prescribed fire treatments..."
Rscript R/05_process_prescribed_conifer.R

# Final merge
echo ""
echo "[Final] Merging all layers into conifer panel..."
Rscript R/06_merge_conifer_panel.R

echo ""
echo "======================================"
echo "✓ Pipeline complete!"
echo "Output: data/processed_data/conifer_environmental_panel.RDS"
echo "======================================"
