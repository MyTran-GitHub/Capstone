# Methodological Extension: Embedding-Based Donor Selection for Synthetic Control

## Concept

One weakness of standard synthetic control is **arbitrary donor pool construction**. Typically, we use:
- Geographic proximity (e.g., same state/region)
- Covariate similarity (manual selection of matching variables)
- Random sampling from large pools (computational constraint)

**Novel contribution:** Use **geospatial foundation model embeddings** to select donors based on comprehensive landscape similarity captured by satellite imagery.

## Implementation

### Step 1: Extract Pre-Treatment Landsat Imagery
For each pixel (treated and potential donors), extract a 1km × 1km Landsat 8 composite from the 3 years immediately before treatment:

```python
# For 2012 treatment, extract 2009-2011 median composite
# Bands: B2 (Blue), B3 (Green), B4 (Red), B5 (NIR), B6 (SWIR1), B7 (SWIR2)
```

This captures:
- Vegetation density and health (NDVI from NIR/Red)
- Fuel moisture (SWIR bands)
- Topographic shadows (visible bands)
- Land cover composition

### Step 2: Compute Embeddings with Prithvi Foundation Model
Use IBM/NASA's Prithvi-100M model (or similar geospatial transformer) to encode each Landsat chip into a 512-dimensional embedding vector:

```python
from prithvi_encoder import PrithviEncoder

model = PrithviEncoder.from_pretrained("ibm/Prithvi-100M")
embeddings = model.encode(landsat_chips)  # (N pixels, 512 dims)
```

Prithvi is trained on 1 billion Landsat pixels globally, learning representations that capture:
- Vegetation type and structure
- Seasonal phenology
- Disturbance history
- Climatic zones

### Step 3: Donor Selection via Cosine Similarity
For each treated pixel, select top-K most similar donors based on embedding distance:

```python
from sklearn.metrics.pairwise import cosine_similarity

# For treated pixel i
similarities = cosine_similarity(embedding_i, embedding_donors)
top_k_donors = np.argsort(similarities)[-50:]  # Top 50 most similar
```

### Step 4: Synthetic Control on Embedding-Restricted Pool
Run standard synthetic control using only the embedding-selected donors:

```r
# Instead of random 500 donors, use top-50 embedding-selected donors
restricted_donors <- embedding_similarity_df %>% 
  filter(rank <= 50) %>% 
  pull(unit)

# Construct synthetic control as before
weights <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
```

## Advantages

### 1. Theoretically Motivated Donor Selection
Embedding similarity captures **comprehensive landscape context** that would be impossible to specify manually:
- Microclimate (temperature inversions, local precipitation patterns)
- Topographic complexity (slope, aspect, solar radiation)
- Vegetation structure (canopy density, species composition, fuel arrangement)
- Disturbance history (past fires, logging, insect outbreaks)

Standard approaches use only a handful of observable covariates (elevation, lat/lon, annual temperature). Embeddings use 512 dimensions learned from billions of pixels.

### 2. Improved Pre-Treatment Fit
**Hypothesis:** Embedding-selected donors should achieve tighter pre-treatment balance on fire activity (lower RMSPE) because they match on the latent factors that drive fire behavior.

**Test:** Compare pre-treatment RMSPE:
- Baseline: Random 500 donors → RMSPE = 0.42
- Embedding-restricted: Top-50 donors → RMSPE = ??

If embedding RMSPE < baseline RMSPE, we've improved covariate balance through better donor selection.

### 3. Interpretability via Embedding Space Visualization
Can create a 2D embedding space (via UMAP/t-SNE) showing:
- Treated pixels (red)
- Embedding-selected donors (blue)
- Excluded donors (gray)

Shows that embedding method selects donors that cluster with treated pixels in latent feature space.

## Extension to Writing Sample

Add as **Section 3.3: "Embedding-Based Donor Selection"** (2 pages):

```markdown
### 3.3 Extension: Embedding-Based Donor Selection

While the baseline analysis uses random sampling from a geographically-restricted 
donor pool, I also explore a novel donor selection mechanism using geospatial 
foundation model embeddings. This approach leverages recent advances in 
self-supervised learning for satellite imagery to identify donors with 
comprehensive landscape similarity.

#### Methodological Contribution

Standard synthetic control donor pools are constructed using a small number of 
observable covariates (e.g., elevation, climate, prior fire history). This 
assumes we have correctly specified the key confounders driving both treatment 
assignment and outcomes. In complex environmental systems, this is a strong 
assumption—many latent factors (soil moisture, microclimates, vegetation 
structure) affect fire behavior but are difficult to observe.

Geospatial foundation models offer an alternative: by training on billions of 
satellite pixels globally, models like Prithvi (Jakubik et al. 2023) learn 
representations that capture comprehensive landscape context. Using these 
embeddings for donor selection has three advantages:

1. **High-dimensional matching**: 512-dimensional embeddings vs. 5-10 manual covariates
2. **Learned features**: Model discovers relevant factors through self-supervision
3. **Scalable**: Once embeddings are computed, donor selection is fast (cosine similarity)

#### Implementation

For each pixel (treated and potential donors), I extract pre-treatment Landsat 8 
imagery (2009-2011 median composite, 6 bands) and encode it using the Prithvi-100M 
transformer model. Donors are then selected by ranking cosine similarity in embedding 
space, retaining the top-K most similar pixels (K = 50).

Synthetic control weights are constructed on this restricted donor pool using the 
same quadratic programming procedure as baseline.

#### Results

[Would include comparison table showing:]
- Baseline (random 500 donors): Pre-RMSPE = 0.42, Post-Gap = -0.71
- Embedding-restricted (top-50 donors): Pre-RMSPE = ??, Post-Gap = ??

If embedding approach achieves lower pre-RMSPE with a smaller donor pool, it 
demonstrates that foundation model representations capture policy-relevant landscape 
similarity better than manual covariate selection.

This represents a promising direction for spatial causal inference: using 
machine learning for improved matching/weighting, not as a replacement for causal 
identification but as a tool for better balance on unobservables.
```

## Why This Strengthens the Writing Sample

### For Political Methodology Programs
Shows you're at the frontier of causal inference methods:
- Aware of recent ML advances (foundation models, self-supervised learning)
- Thoughtful about incorporating ML into causal inference pipelines
- Understand the difference between ML for prediction vs. ML for causal estimation

### For Substantive Programs
Demonstrates innovation in addressing a hard problem (donor selection in spatial settings) with practical tools (satellite imagery, pretrained models).

### For Methods Workshops/Talks
This is a **compelling methodological story**:
1. Problem: Donor pool construction is ad-hoc
2. Opportunity: Foundation models learn comprehensive representations
3. Solution: Use embeddings for high-dimensional matching
4. Validation: Compare pre-treatment fit and treatment effect estimates

## Talking Points

**Question:** "How is this different from just adding more covariates to synthetic control?"

**Answer:** "Manual covariate selection faces two problems. First, you can't observe everything (microclimate, fuel structure). Second, even if you could, you'd run into curse of dimensionality—matching on 50+ variables is hard with finite samples. Foundation model embeddings solve both: they extract 512 dimensions of learned features from satellite imagery that capture latent landscape factors we couldn't manually specify. And cosine similarity in embedding space naturally handles high-dimensional matching."

**Question:** "Isn't using black-box ML for donor selection risky for causal inference?"

**Answer:** "Absolutely—if we used ML to estimate treatment effects directly, we'd lose causal interpretation. But here, ML is only used for *matching*—to find good control pixels. The causal identification still comes from synthetic control's parallel trends assumption, which we can validate by checking pre-treatment fit. In fact, embeddings *improve* transparency because we can visualize similarity in embedding space and show why certain donors were selected."

**Question:** "Has anyone done this before?"

**Answer:** "Not in published work that I'm aware of. There's growing interest in using ML for improved matching in causal inference (Athey & Imbens 2019, Kallus et al. 2021), but applications to spatial policy evaluation with satellite embeddings are novel. This could be a paper in itself for a methods journal."

## Bottom Line

Adding this embedding extension shows:
- You're pushing methodological boundaries
- You can integrate computer science tools into social science research
- You think carefully about when/how to use ML in causal inference

That's exactly what top PhD programs want to see.
