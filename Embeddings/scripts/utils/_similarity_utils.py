"""Similarity utilities shared between selection scripts.

Provides a vectorized `compute_all_similarities()` function that computes cosine
similarities from each treated pixel to all control pixels and returns a dict
mapping treated indices -> np.ndarray of (control_idx, similarity) sorted
descending by similarity.

This module is intended to be imported by both the one-time precompute CLI and
the K-selection orchestrator to avoid duplicating heavy computation logic.
"""
from pathlib import Path
import logging
import numpy as np
import pandas as pd

logger = logging.getLogger(__name__)


def compute_all_similarities(embeddings_df: pd.DataFrame) -> dict:
    """Compute cosine similarities for all treated-control pairs (vectorized).

    Returns dict: treated_idx -> np.ndarray([(control_idx, similarity), ...])
    """
    logger.info("Computing similarities for all treated-control pairs (vectorized)...")
    try:
        from sklearn.metrics.pairwise import cosine_similarity as sklearn_cosine_similarity
    except Exception as e:
        logger.error("scikit-learn is required for similarity computation: pip install scikit-learn")
        raise

    treated_mask = embeddings_df['treated'] == 1
    control_mask = embeddings_df['treated'] == 0
    treated_indices = embeddings_df[treated_mask].index.tolist()
    control_indices = embeddings_df[control_mask].index.tolist()

    embedding_cols = [col for col in embeddings_df.columns if col.startswith('band_')]
    treated_embeddings = embeddings_df.loc[treated_mask, embedding_cols].values
    control_embeddings = embeddings_df.loc[control_mask, embedding_cols].values

    logger.info(f"  Computing {len(treated_indices)} × {len(control_indices)} similarities...")
    similarity_matrix = sklearn_cosine_similarity(treated_embeddings, control_embeddings)

    # Replace any NaNs with 0
    if np.isnan(similarity_matrix).any():
        logger.warning("NaN values found in similarity matrix; replacing with 0")
        similarity_matrix = np.nan_to_num(similarity_matrix, nan=0.0)

    similarities = {}
    for i, t_idx in enumerate(treated_indices):
        sims = [(control_indices[j], float(similarity_matrix[i, j])) for j in range(len(control_indices))]
        sims.sort(key=lambda x: x[1], reverse=True)
        similarities[int(t_idx)] = np.array(sims, dtype=object)

    logger.info(f"  ✓ Computed similarities for {len(treated_indices)} treated pixels")
    return similarities
