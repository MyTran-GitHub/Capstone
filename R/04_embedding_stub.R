# Placeholder for embedding-based donor selection workflow
# Steps (to implement later):
# 1. Acquire Landsat (or Sentinel) image chips for bounding box & pre-treatment years.
# 2. Run Panopticon (Python) model to compute embeddings per pixel/patch.
# 3. Join embeddings with panel via nearest coordinate lookup.
# 4. Compute similarity (cosine) between treated unit embeddings and donor pool.
# 5. Restrict donor pool to top-K or cluster match.
# 6. Re-run 03_baseline_scm.R logic on restricted pool and compare pre-fit metrics.

message("Embedding donor selection stub ready. Implement in Python later.")
