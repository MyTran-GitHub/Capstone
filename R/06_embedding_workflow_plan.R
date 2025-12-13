# Embedding-Based Donor Selection Workflow Plan
# ================================================

## Overview
# This workflow will:
# 1. Extract Landsat imagery for the study region and timeframe
# 2. Generate embeddings using Panopticon foundation model (Python)
# 3. Restrict donor pool based on embedding similarity
# 4. Re-run SCM with restricted donors and compare results

## Step 1: Landsat Data Acquisition
# ---------------------------------
# Options:
#   A. Google Earth Engine (Python API) - recommended for efficiency
#   B. USGS EarthExplorer bulk download
#   C. Microsoft Planetary Computer

# Region: Northern Sierra Nevada
#   lon: [-122, -119]
#   lat: [37.5, 40.0]

# Timeframe: Pre-treatment window (e.g., 2007-2011 or 2009-2011)
# Bands: RGB + NIR at minimum (Panopticon expects multi-spectral)
# Resolution: Aggregate to match 1km grid if needed

## Step 2: Grid Alignment & Chip Extraction
# -----------------------------------------
# For each pixel in subset_conifer2012_region.rds:
#   - Extract 1km x 1km chip (or 3x3 pixel window at 30m resolution)
#   - Composite over pre-treatment years (median/mean)
#   - Save as GeoTIFF or NumPy array

## Step 3: Panopticon Embedding Extraction (Python)
# --------------------------------------------------
# Required packages:
#   - torch, torchvision
#   - rasterio or gdal
#   - panopticon (if available) or pre-trained vision transformer
#   - numpy, pandas

# Pseudocode:
#   for each chip:
#       img = load_chip(chip_path)
#       embedding = panopticon_model.encode(img)  # e.g., 512-d vector
#       save_embedding(unit_id, embedding)

# Output: embeddings.csv or embeddings.parquet
#   columns: unit, emb_1, emb_2, ..., emb_512

## Step 4: Similarity-Based Donor Selection (R or Python)
# --------------------------------------------------------
# Load embeddings and join with subset data by unit

# For treated unit:
#   treated_emb = embeddings[unit == treated_unit]
#   
#   For each potential donor:
#     donor_emb = embeddings[unit == donor_unit]
#     similarity = cosine_similarity(treated_emb, donor_emb)
#   
#   Select top-K most similar OR use clustering:
#     - K-means: assign units to clusters, use same cluster as treated
#     - HDBSCAN: density-based clusters
#     - Threshold: keep donors with cosine similarity > 0.8

## Step 5: Re-run SCM with Restricted Donors
# -------------------------------------------
# Modify R/03_baseline_scm.R to:
#   - Load embedding-based donor pool
#   - Build donor_matrix only from restricted set
#   - Solve QP and generate plots

# Compare metrics:
#   - Baseline pre-RMSPE vs. embedding-restricted pre-RMSPE
#   - Baseline balance vs. embedding-restricted balance
#   - Post-treatment gap estimates

## Step 6: Sensitivity Analysis
# ------------------------------
# Vary:
#   - K (number of similar donors)
#   - Similarity threshold
#   - Embedding model (different pre-training)
#   - Timeframe for imagery (single year vs. composite)

## Implementation Timeline
# ------------------------
# Phase 1 (Current): Baseline SCM working ✓
# Phase 2: Set up Python env + acquire small Landsat sample
# Phase 3: Run Panopticon on sample chips
# Phase 4: Implement similarity selection and comparison
# Phase 5: Scale to full subset and document results

## Python Environment Setup
# -------------------------
# Create conda/venv environment:
#   conda create -n capstone python=3.10
#   conda activate capstone
#   pip install torch torchvision earthengine-api rasterio pandas numpy scikit-learn

# For Panopticon:
#   # Check if public model is available or use alternative:
#   # - Prithvi (IBM/NASA foundation model for remote sensing)
#   # - SatMAE
#   # - Generic vision transformer (ViT) pre-trained on ImageNet

message("Embedding workflow plan ready. See R/06_embedding_workflow_plan.R for details.")
