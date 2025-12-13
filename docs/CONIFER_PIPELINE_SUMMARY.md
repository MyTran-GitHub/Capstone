# Full Conifer Pipeline Implementation Summary

## ✅ Phase 1 Complete: Conifer-Only Baseline SCM

### What's Working

**Data Processing Pipeline** (`data_processing/`):
1. ✅ `conifer_grid_setup.R` - 599,916 CA pixels (awaiting real vegetation/elevation)
2. ✅ `build_conifer_analysis_datasets.R` - Creates yearly analysis files (2010-2014)
3. ✅ Placeholder `.fst` files for: disturbance, tree_cover, fire_severity, prescribed_fire

**Analysis Outputs**:
- 5 analysis datasets: `analysis_treated{2010-2014}_conifer.RDS` (~598K pixels each)
- **2012 example**: 3,966 treated units (low-intensity burns), 594K+ potential donors
- Fire history lags automatically computed from FIRMS

**Synthetic Control** (`R/10_scm_conifer_full.R`):
- ✅ Loads conifer analysis dataset
- ✅ Samples 500 donors for computational efficiency
- ✅ Computes QP weights, pre-RMSPE, post-gap
- ✅ Generates 3 figures per focal year

**Current Results (2012)**:
- Pre-RMSPE: 0.0 (perfect fit - treated unit had no pre-fire)
- Avg post-gap: +0.005 (minimal effect)
- Donor pool: 500 sampled from 594K candidates

---

## 🎯 Next Steps: Real Data → Embedding Extension

### Immediate: Add Environmental Layers

**Priority 1: Vegetation (CAL FIRE)**
- Enables accurate conifer filtering (codes 31, 32 vs. hardwood 51, 52)
- Current: all pixels included (dummy fveg = NA)
- **Source**: https://frap.fire.ca.gov/mapping/gis-data/

**Priority 2: Elevation (GMTED)**
- 1km DEM for topographic covariate
- **Source**: https://www.usgs.gov/coastal-changes-and-impacts/gmted2010

**Priority 3: Daymet Climate**
- Monthly temp/precip for balance diagnostics
- Can sample subset of pixels initially
- **Method**: `daymetr` R package (point-based or tile download)

**Optional**: MTBS severity, tree cover, disturbance
- Already have processing scripts ready
- Add incrementally to test covariate balance improvements

### Phase 2: Embedding-Based Donor Selection

**Step 1: Imagery Acquisition**
Python script to extract Landsat chips:
```python
# python/extract_landsat_chips.py
import ee
ee.Initialize()

# Load conifer units from R
units = pd.read_csv('data/processed_data/conifer_units.csv')

# For each unit, extract 1km chip (pre-treatment composite 2009-2011)
for unit in units:
    chip = get_landsat_composite(lat, lon, years=[2009,2010,2011])
    chip.save(f'data/imagery/{unit}.tif')
```

**Step 2: Embedding Extraction**
```python
# python/compute_embeddings.py
from prithvi import PrithviModel  # or SatMAE, Panopticon

model = PrithviModel.load_pretrained()
embeddings = []

for chip_path in glob('data/imagery/*.tif'):
    chip = load_chip(chip_path)
    emb = model.encode(chip)  # 512-d vector
    embeddings.append({'unit': unit_id, 'embedding': emb})

pd.DataFrame(embeddings).to_parquet('data/processed_data/embeddings.parquet')
```

**Step 3: Similarity-Based Donor Selection**
```r
# R/11_embedding_donor_selection.R
library(lsa)  # cosine similarity

embeddings <- read_parquet('data/processed_data/embeddings.parquet')

# For treated unit, compute similarity to all donors
treated_emb <- embeddings[embeddings$unit == treated_unit, 'embedding']
donor_embs <- embeddings[embeddings$unit %in% donor_pool, ]

similarities <- cosine(treated_emb, donor_embs)
top_K <- order(similarities, decreasing=TRUE)[1:50]

restricted_donors <- donor_pool[top_K]

# Re-run R/10_scm_conifer_full.R with restricted_donors
```

**Step 4: Comparison**
| Method | Donors | Pre-RMSPE | Avg Gap | Balance (std diff) |
|--------|--------|-----------|---------|-------------------|
| Baseline (random 500) | 500 | ? | ? | ? |
| Embedding (top-50) | 50 | ? | ? | ? |
| Embedding (top-100) | 100 | ? | ? | ? |

**Hypothesis**: Embedding-restricted donors have:
- Lower pre-RMSPE (better visual similarity)
- Better covariate balance (implicit matching on unobserved factors)
- More credible post-treatment estimates

---

## 📂 Current File Structure

```
data_processing/
  ✅ conifer_grid_setup.R
  ✅ build_conifer_analysis_datasets.R
  ✅ {daymet,disturbance,fractional_cover,fire_severity,prescribed_fire}_processing.R

R/
  ✅ 00-05: Original baseline scripts
  ✅ 10_scm_conifer_full.R (new conifer SCM)
  🔜 11_embedding_donor_selection.R
  
python/ (to create)
  🔜 extract_landsat_chips.py
  🔜 compute_embeddings.py

data/processed_data/
  ✅ conifer_grid.RDS (600K pixels)
  ✅ analysis_conifer/analysis_treated{2010-2014}_conifer.RDS
  ✅ scm_conifer_2012.rds (baseline result)
  🔜 embeddings.parquet
  🔜 scm_conifer_2012_embedding.rds

figures/
  ✅ conifer_pre_fit_2012.png
  ✅ conifer_post_2012.png
  ✅ conifer_gap_2012.png
```

---

## 🚀 Quick Commands

**Re-run conifer SCM** (any year):
```r
# Edit focal_year in R/10_scm_conifer_full.R
source("R/10_scm_conifer_full.R")
```

**Rebuild analysis datasets** (after adding new layers):
```r
source("data_processing/build_conifer_analysis_datasets.R")
```

**Add vegetation filter** (once CAL FIRE shapefile obtained):
```r
# Place shapefile in data/raw_data/calfire_veg.shp
source("data_processing/conifer_grid_setup.R")
source("data_processing/build_conifer_analysis_datasets.R")
```

---

## 📊 Performance Notes

- **Conifer grid**: ~600K pixels (all CA, placeholder vegetation)
- **Analysis datasets**: 598K pixels each, 3-4K treated/year
- **SCM runtime**: ~2-3 min with 500 donors
- **Memory**: ~2-3 GB for full pipeline

**Scaling tips**:
- Use `max_donors` parameter to limit donor pool size
- Process one focal year at a time
- Sample conifer grid spatially if needed (e.g., Northern CA only)

---

## 🔬 Research Questions Ready to Answer

1. **Does low-intensity fire reduce high-intensity risk?** (baseline)
2. **Does embedding-based selection improve pre-treatment fit?**
3. **How does donor pool size affect estimates?** (50 vs 100 vs 500)
4. **Which covariates matter most for balance?** (FIRMS vs climate vs vegetation)

**Next action**: Choose either:
- A) Add CAL FIRE vegetation and re-run (improves conifer accuracy)
- B) Start embedding pipeline with current dummy conifer grid
- C) Run sensitivity analysis on multiple focal years (2010-2014)
