
"""
Simulated Data Analysis for Synthetic Control Deliverable
This script generates synthetic data and produces key outputs for placebo-based inference, bootstrap CIs, covariate exploration, and embedding interpretability, as described in the Capstone deliverable.
All outputs are based on simulated data and are for demonstration only.
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score
from scipy.stats import wilcoxon, ttest_rel
import umap
np.random.seed(42)

# --- 1. Simulate Data ---
N = 4000  # number of units (pixels)
T_pre = 9  # pre-treatment years
T_post = 6  # post-treatment years
K_embed = 50
K_base = 500

# Simulate covariates
covariates = pd.DataFrame({
    'elevation': np.random.normal(1500, 500, N),  # more variance
    'canopy': np.random.normal(70, 20, N),
    'precip': np.random.normal(1100, 400, N),
    'veg_class': np.random.choice(['conifer', 'hardwood'], N, p=[0.8, 0.2]),
    'prior_fire': np.random.poisson(0.8, N)
})

# Simulate embeddings (less correlated with covariates)
embedding_dim = 8
embeddings = np.random.normal(0, 1, (N, embedding_dim))
# Add weaker structure: first 3 dims weakly correlated with covariates
embeddings[:,0] += 0.3 * (covariates['elevation'] - 1500) / 500
embeddings[:,1] += 0.3 * (covariates['canopy'] - 70) / 20
embeddings[:,2] += 0.3 * (covariates['precip'] - 1100) / 400

# Simulate pre- and post-treatment outcomes
pre_outcomes = np.random.normal(12, 3, (N, T_pre))  # more noise
# Treatment effect: -0.3 for treated, -0.1 for baseline, more noise
is_treated = np.zeros(N, dtype=bool)
is_treated[:int(N/2)] = True
np.random.shuffle(is_treated)

post_outcomes = np.where(is_treated[:,None],
                         np.random.normal(11.7, 3, (N, T_post)),  # treated
                         np.random.normal(11.9, 3, (N, T_post)))  # control

# Simulate synthetic control weights (baseline vs embedding)
weights_base = np.random.dirichlet(np.ones(K_base), N)
weights_embed = np.random.dirichlet(np.ones(K_embed), N)

# Simulate synthetic control outcomes (less precise)
synth_base = pre_outcomes.mean(axis=1) + np.random.normal(0, 1.0, N)
synth_embed = pre_outcomes.mean(axis=1) + np.random.normal(0, 0.8, N)

# --- 2. Main ATT and Bootstrap CIs ---
att_base = (post_outcomes.mean(axis=1) - synth_base)[is_treated]
att_embed = (post_outcomes.mean(axis=1) - synth_embed)[is_treated]

# Bootstrap CIs
def bootstrap_ci(data, n_boot=500, alpha=0.05):
    boot_means = [np.mean(np.random.choice(data, size=len(data), replace=True)) for _ in range(n_boot)]
    lower = np.percentile(boot_means, 100*alpha/2)
    upper = np.percentile(boot_means, 100*(1-alpha/2))
    return np.mean(data), lower, upper

att_base_mean, att_base_lo, att_base_hi = bootstrap_ci(att_base)
att_embed_mean, att_embed_lo, att_embed_hi = bootstrap_ci(att_embed)

# --- 3. Placebo-based CIs (permutation test) ---
perm_diffs = []
for _ in range(500):
    perm = np.random.permutation(is_treated)
    perm_att = (post_outcomes.mean(axis=1) - synth_embed)[perm]
    perm_diffs.append(np.mean(perm_att))
placebo_lo = np.percentile(perm_diffs, 2.5)
placebo_hi = np.percentile(perm_diffs, 97.5)

# --- 4. Correlation Matrix: Embeddings vs Covariates ---
embed_df = pd.DataFrame(embeddings, columns=[f'emb_{i+1}' for i in range(embedding_dim)])
corrs = embed_df.join(covariates[['elevation','canopy','precip']]).corr()

# --- 5. Correlation: Pre-treatment outcome similarity vs embedding similarity ---
from scipy.spatial.distance import cdist
# Take 100 random pairs
idx = np.random.choice(N, 100, replace=False)
pairwise_embed_dist = cdist(embeddings[idx], embeddings[idx], metric='euclidean')
pairwise_pre_diff = cdist(pre_outcomes[idx], pre_outcomes[idx], metric='euclidean')
embed_flat = pairwise_embed_dist[np.triu_indices(100, 1)]
pre_flat = pairwise_pre_diff[np.triu_indices(100, 1)]
embed_pre_corr = np.corrcoef(embed_flat, pre_flat)[0,1]

# --- 6. Feature Importance: Predict covariates from embeddings ---
rf = RandomForestRegressor(n_estimators=100)
rf.fit(embeddings, covariates['elevation'])
elev_r2 = r2_score(covariates['elevation'], rf.predict(embeddings))
rf.fit(embeddings, covariates['canopy'])
canopy_r2 = r2_score(covariates['canopy'], rf.predict(embeddings))
rf.fit(embeddings, covariates['precip'])
precip_r2 = r2_score(covariates['precip'], rf.predict(embeddings))

# --- 7. Covariate Exploration Plots ---

# More realistic ATT distribution plot
plt.figure(figsize=(7,5))
sns.histplot(att_base, color='gray', label='Baseline', kde=True, stat='density', bins=30, alpha=0.6)
sns.histplot(att_embed, color='royalblue', label='Embedding', kde=True, stat='density', bins=30, alpha=0.5)
plt.axvline(np.mean(att_base), color='black', linestyle='--', label='Baseline Mean')
plt.axvline(np.mean(att_embed), color='blue', linestyle='--', label='Embedding Mean')
plt.xlabel('Average Treatment Effect (ATT)')
plt.ylabel('Density')
plt.title('ATT Distribution: Baseline vs. Embedding-based SCM')
plt.legend()
plt.tight_layout()
plt.savefig('att_distribution.png')
plt.close()


# More realistic RMSPE distribution plot
rmspe_base = np.abs(synth_base - pre_outcomes.mean(axis=1))
rmspe_embed = np.abs(synth_embed - pre_outcomes.mean(axis=1))
plt.figure(figsize=(7,5))
sns.histplot(rmspe_base, color='gray', label='Baseline', kde=True, stat='density', bins=30, alpha=0.6)
sns.histplot(rmspe_embed, color='royalblue', label='Embedding', kde=True, stat='density', bins=30, alpha=0.5)
plt.axvline(np.mean(rmspe_base), color='black', linestyle='--', label='Baseline Mean')
plt.axvline(np.mean(rmspe_embed), color='blue', linestyle='--', label='Embedding Mean')
plt.xlabel('Pre-treatment RMSPE')
plt.ylabel('Density')
plt.title('Pre-treatment RMSPE Distribution: Baseline vs. Embedding')
plt.legend()
plt.tight_layout()
plt.savefig('rmspe_distribution.png')
plt.close()

plt.figure(figsize=(6,4))
sns.heatmap(corrs, annot=True, fmt='.2f', cmap='coolwarm')
plt.title('Correlation Matrix: Embeddings and Covariates')
plt.tight_layout()
plt.savefig('embedding_covariate_corr.png')
plt.close()

# --- 8. Paired Statistical Test: RMSPE ---
rmspe_base = np.abs(synth_base - pre_outcomes.mean(axis=1))
rmspe_embed = np.abs(synth_embed - pre_outcomes.mean(axis=1))
stat, pval = wilcoxon(rmspe_base, rmspe_embed)

# --- 9. Save summary results ---
with open('sim_results.txt', 'w') as f:
    f.write(f"ATT Baseline: {att_base_mean:.3f} (95% CI: {att_base_lo:.3f}, {att_base_hi:.3f})\n")
    f.write(f"ATT Embedding: {att_embed_mean:.3f} (95% CI: {att_embed_lo:.3f}, {att_embed_hi:.3f})\n")
    f.write(f"Placebo-based CI for Embedding ATT: ({placebo_lo:.3f}, {placebo_hi:.3f})\n")
    f.write(f"Correlation (embedding-pre outcome similarity): {embed_pre_corr:.2f}\n")
    f.write(f"R2 for predicting elevation from embeddings: {elev_r2:.2f}\n")
    f.write(f"R2 for predicting canopy from embeddings: {canopy_r2:.2f}\n")
    f.write(f"R2 for predicting precip from embeddings: {precip_r2:.2f}\n")
    f.write(f"Wilcoxon p-value for RMSPE (baseline vs embedding): {pval:.4f}\n")

# --- 10. Print summary to console ---
print(f"ATT Baseline: {att_base_mean:.3f} (95% CI: {att_base_lo:.3f}, {att_base_hi:.3f})")
print(f"ATT Embedding: {att_embed_mean:.3f} (95% CI: {att_embed_lo:.3f}, {att_embed_hi:.3f})")
print(f"Placebo-based CI for Embedding ATT: ({placebo_lo:.3f}, {placebo_hi:.3f})")
print(f"Correlation (embedding-pre outcome similarity): {embed_pre_corr:.2f}")
print(f"R2 for predicting elevation from embeddings: {elev_r2:.2f}")
print(f"R2 for predicting canopy from embeddings: {canopy_r2:.2f}")
print(f"R2 for predicting precip from embeddings: {precip_r2:.2f}")
print(f"Wilcoxon p-value for RMSPE (baseline vs embedding): {pval:.4f}")

# --- Executive-level Diagnostic Plots ---

# 1. Paired Difference Histogram (ATT_baseline - ATT_embedding)
att_diff = att_base - att_embed
plt.figure(figsize=(7,5))
sns.histplot(att_diff, bins=30, color='purple', kde=True, alpha=0.7)
plt.axvline(np.mean(att_diff), color='black', linestyle='--', label=f'Mean: {np.mean(att_diff):.3f}')
plt.xlabel('ATT Baseline - ATT Embedding')
plt.ylabel('Count')
plt.title('Paired Difference in ATT (Baseline - Embedding)')
plt.legend()
plt.tight_layout()
plt.savefig('att_paired_difference.png')
plt.close()

# 2. Scatterplot of ATT vs. RMSPE for both methods
plt.figure(figsize=(7,5))
plt.scatter(rmspe_base[is_treated], att_base, color='gray', alpha=0.5, label='Baseline')
plt.scatter(rmspe_embed[is_treated], att_embed, color='royalblue', alpha=0.5, label='Embedding')
plt.xlabel('Pre-treatment RMSPE')
plt.ylabel('ATT (Treated Units)')
plt.title('ATT vs. Pre-treatment RMSPE')
plt.legend()
plt.tight_layout()
plt.savefig('att_vs_rmspe_scatter.png')
plt.close()

import umap
from sklearn.cluster import KMeans

# Fit UMAP to embeddings
reducer = umap.UMAP(random_state=42)
embedding_2d = reducer.fit_transform(embeddings)

# Cluster embeddings (e.g., k=8)
kmeans = KMeans(n_clusters=8, random_state=42)
clusters = kmeans.fit_predict(embeddings)

# Plot: Treated units and controls, clusters
import matplotlib.pyplot as plt
plt.figure(figsize=(8,6))
plt.scatter(embedding_2d[~is_treated,0], embedding_2d[~is_treated,1], c=clusters[~is_treated], cmap='tab10', alpha=0.3, label='Controls')
plt.scatter(embedding_2d[is_treated,0], embedding_2d[is_treated,1], c='red', marker='x', label='Treated')
plt.xlabel('UMAP 1')
plt.ylabel('UMAP 2')
plt.title('Embedding Space: Clusters and Treated Units')
plt.legend()
plt.tight_layout()
plt.savefig('embedding_umap_clusters.png')
plt.close()

import seaborn as sns
embed_df = pd.DataFrame(embeddings, columns=[f'emb_{i+1}' for i in range(embeddings.shape[1])])
corrs = embed_df.join(covariates[['elevation','canopy','precip']]).corr()
plt.figure(figsize=(8,6))
sns.heatmap(corrs.iloc[:embeddings.shape[1], -3:], annot=True, cmap='coolwarm')
plt.title('Correlation: Embedding Dimensions vs. Covariates')
plt.tight_layout()
plt.savefig('embedding_covariate_corr_heatmap.png')
plt.close()

from scipy.spatial.distance import cdist

# For a random treated unit, compare distances to included (same cluster) and excluded controls
treated_idx = np.where(is_treated)[0][0]
treated_emb = embeddings[treated_idx]
control_idx = np.where(~is_treated)[0]
included = control_idx[clusters[control_idx] == clusters[treated_idx]]
excluded = control_idx[clusters[control_idx] != clusters[treated_idx]]

dist_included = cdist([treated_emb], embeddings[included]).flatten()
dist_excluded = cdist([treated_emb], embeddings[excluded]).flatten()

plt.figure(figsize=(6,5))
sns.boxplot(data=[dist_included, dist_excluded], palette=['royalblue','gray'])
plt.xticks([0,1], ['Included (Same Cluster)', 'Excluded'])
plt.ylabel('Embedding Distance')
plt.title('Embedding Distances: Included vs. Excluded Donors')
plt.tight_layout()
plt.savefig('embedding_distance_boxplot.png')
plt.close()

# Example for elevation
plt.figure(figsize=(7,5))
sns.kdeplot(covariates['elevation'][is_treated], label='Treated', color='red')
sns.kdeplot(covariates['elevation'][included], label='Embedding Controls', color='blue')
sns.kdeplot(covariates['elevation'][excluded], label='Other Controls', color='gray')
plt.xlabel('Elevation')
plt.title('Elevation Distribution: Treated vs. Controls')
plt.legend()
plt.tight_layout()
plt.savefig('elevation_distribution_controls.png')
plt.close()

from sklearn.manifold import TSNE
import matplotlib.pyplot as plt


# Remove n_iter if it causes an error
tsne = TSNE(n_components=2, random_state=42, perplexity=30)
embedding_2d_tsne = tsne.fit_transform(embeddings)

# Plot: Treated units and controls, clusters
plt.figure(figsize=(8,6))
plt.scatter(embedding_2d_tsne[~is_treated,0], embedding_2d_tsne[~is_treated,1], c=clusters[~is_treated], cmap='tab10', alpha=0.3, label='Controls')
plt.scatter(embedding_2d_tsne[is_treated,0], embedding_2d_tsne[is_treated,1], c='red', marker='x', label='Treated')
plt.xlabel('t-SNE 1')
plt.ylabel('t-SNE 2')
plt.title('t-SNE of Embedding Space: Clusters and Treated Units')
plt.legend()
plt.tight_layout()
plt.savefig('embedding_tsne_clusters.png')
plt.close()

from sklearn.metrics.pairwise import cosine_similarity
import numpy as np
import matplotlib.pyplot as plt


# --- Similarity/Distance Metrics: Cosine, Euclidean, Mahalanobis ---
from sklearn.metrics.pairwise import cosine_similarity, euclidean_distances
from scipy.spatial.distance import mahalanobis

treated_idx = np.where(is_treated)[0]
control_idx = np.where(~is_treated)[0]
sample_treated = treated_idx[:100]  # sample for speed

# Cosine similarity
cos_sims = []
for t in sample_treated:
    sims = cosine_similarity([embeddings[t]], embeddings[control_idx]).flatten()
    cos_sims.extend(sims)

# Euclidean distance
euc_dists = []
for t in sample_treated:
    dists = euclidean_distances([embeddings[t]], embeddings[control_idx]).flatten()
    euc_dists.extend(dists)

# Mahalanobis distance (using pooled covariance)
import numpy as np
cov = np.cov(embeddings[control_idx].T)
cov_inv = np.linalg.pinv(cov)
mah_dists = []
for t in sample_treated:
    for c in control_idx:
        mah_dists.append(mahalanobis(embeddings[t], embeddings[c], cov_inv))

# Plot all three metrics for comparison
plt.figure(figsize=(10,6))
plt.hist(cos_sims, bins=40, color='slateblue', alpha=0.5, label='Cosine Similarity')
plt.xlabel('Cosine Similarity')
plt.ylabel('Count')
plt.title('Cosine Similarity: Treated vs. Controls')
plt.tight_layout()
plt.savefig('figure1a_cosine_similarity_hist.png')
plt.close()

plt.figure(figsize=(10,6))
plt.hist(euc_dists, bins=40, color='seagreen', alpha=0.5, label='Euclidean Distance')
plt.xlabel('Euclidean Distance')
plt.ylabel('Count')
plt.title('Euclidean Distance: Treated vs. Controls')
plt.tight_layout()
plt.savefig('figure1b_euclidean_distance_hist.png')
plt.close()

plt.figure(figsize=(10,6))
plt.hist(mah_dists, bins=40, color='darkorange', alpha=0.5, label='Mahalanobis Distance')
plt.xlabel('Mahalanobis Distance')
plt.ylabel('Count')
plt.title('Mahalanobis Distance: Treated vs. Controls')
plt.tight_layout()
plt.savefig('figure1c_mahalanobis_distance_hist.png')
plt.close()

# Combined comparison plot
plt.figure(figsize=(10,6))
plt.hist(cos_sims, bins=40, color='slateblue', alpha=0.4, label='Cosine Similarity')
plt.hist(euc_dists, bins=40, color='seagreen', alpha=0.4, label='Euclidean Distance')
plt.hist(mah_dists, bins=40, color='darkorange', alpha=0.4, label='Mahalanobis Distance')
plt.xlabel('Similarity / Distance')
plt.ylabel('Count')
plt.title('Comparison of Similarity/Distance Metrics: Treated vs. Controls')
plt.legend()
plt.tight_layout()
plt.savefig('figure1d_similarity_distance_comparison.png')
plt.close()