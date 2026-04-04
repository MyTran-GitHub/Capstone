#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  pkgs <- c('data.table','ggplot2','FNN','viridis')
  for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, repos='https://cloud.r-project.org')
  library(data.table); library(ggplot2); library(FNN); library(viridis)
})

# --- Params (can be overridden via command-line) ---
args <- commandArgs(trailingOnly=TRUE)
emb_dir <- 'Embeddings/data/embeddings'
out_fig_dir <- 'Embeddings/data/figures/pooled_embedding_diagnostics'
cache_file <- file.path('Embeddings','data','embeddings','pooled_embedding_cache.rds')
load_cache <- FALSE
if (length(args)>0) {
  for (a in args) {
    if (grepl('^emb_dir=', a)) emb_dir <- sub('^emb_dir=', '', a)
    if (grepl('^out_fig_dir=', a)) out_fig_dir <- sub('^out_fig_dir=', '', a)
    if (grepl('^cache_file=', a)) cache_file <- sub('^cache_file=', '', a)
    if (grepl('^load_cache=', a)) load_cache <- as.logical(sub('^load_cache=', '', a))
  }
}

files <- list.files(emb_dir, pattern='embeddings_.*\\.csv$', full.names=TRUE)
stopifnot(length(files)>0)
dir.create(out_fig_dir, recursive=TRUE, showWarnings=FALSE)

# Helper to save cache
save_cache <- function(path, obj) {
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  saveRDS(obj, path)
}

# Helper to load cache if requested
if (load_cache && file.exists(cache_file)) {
  message('Loading cached pooled results from: ', cache_file)
  cache <- readRDS(cache_file)
  DT <- cache$DT
  band_cols <- cache$band_cols
  keep_idx <- cache$keep_idx
  Xs <- cache$Xs
  treated_pos_in_keep <- cache$treated_pos_in_keep
  samp_controls <- cache$samp_controls
  sel_pos <- cache$sel_pos
  nn_all <- cache$nn_all
} else {

message('Reading and pooling embeddings from ', length(files), ' files...')
# Read and pool. Keep unit, treated, year and all band_ cols
pool_list <- vector('list', length(files))
for (i in seq_along(files)) {
  f <- files[[i]]
  message('Reading: ', f)
  hdr <- names(fread(f, nrows=0))
  cols_needed <- intersect(c('unit','treated','year', grep('^band_', hdr, value=TRUE)), hdr)
  dt <- fread(f, select=cols_needed)
  pool_list[[i]] <- dt
}
DT <- rbindlist(pool_list)
message('Pooled rows: ', nrow(DT))

band_cols <- grep('^band_', names(DT), value=TRUE)
if (length(band_cols)==0) stop('no band columns found')

# Keep all treated, sample controls up to max_controls_total
treated_idx <- which(DT$treated==1)
control_idx <- which(DT$treated==0)
message('Total treated=', length(treated_idx), ' controls=', length(control_idx))
max_controls_total <- 20000
if (length(control_idx) > max_controls_total) {
  set.seed(42)
  samp_controls <- sample(control_idx, max_controls_total)
} else samp_controls <- control_idx
keep_idx <- sort(c(treated_idx, samp_controls))
message('Using ', length(keep_idx), ' rows for pooled diagnostics (', length(treated_idx),' treated + ', length(samp_controls),' controls)')

X <- as.matrix(DT[keep_idx, ..band_cols])
# scale features
Xs <- scale(X)

k <- 10
k <- 10
neighk <- 50
message('Computing k-NN (k=', neighk, ') for treated rows (will extract k=10 later)...')
# prepare query points: treated rows positions within keep_idx
treated_pos_in_keep_all <- which(keep_idx %in% treated_idx)
# sample treated queries if too many to limit runtime
max_treated_sample <- 1000
if (length(treated_pos_in_keep_all) > max_treated_sample) {
  set.seed(123)
  treated_pos_in_keep <- sample(treated_pos_in_keep_all, max_treated_sample)
  message('Sampling treated queries: ', length(treated_pos_in_keep), ' of ', length(treated_pos_in_keep_all))
} else treated_pos_in_keep <- treated_pos_in_keep_all

# compute neighbors up to neighk and reuse
nn_all <- get.knnx(Xs, Xs[treated_pos_in_keep, , drop=FALSE], k=neighk)
# distances to k=10 (local radius)
k <- 10
d_t_k <- nn_all$nn.dist[,k]

# find selected donors across years if available
sel_files <- list.files('Embeddings/data/cbps_integration', pattern='selected_controls_.*\\.csv$', recursive=TRUE, full.names=TRUE)
sel_units <- character()
if (length(sel_files)>0) {
  for (sf in sel_files) {
    s <- fread(sf)
    if (!'unit' %in% names(s)) names(s)[1] <- 'unit'
    sel_units <- c(sel_units, as.character(s$unit))
  }
  sel_units <- unique(sel_units)
  message('Found selected controls files; total unique selected units=', length(sel_units))
} else {
  message('No selected_controls files found; skipping selected-donor metrics')
}

sel_pos <- integer(0)
if (length(sel_units)>0) {
  sel_pos_all <- which(DT$unit %in% sel_units)
  # map to keep_idx positions
  sel_pos <- which(keep_idx %in% sel_pos_all)
  message('Selected donors present in pooled sample: ', length(sel_pos))
}

# distances from treated to nearest selected donor
if (length(sel_pos)>0) {
  nn_sel <- get.knnx(Xs[sel_pos,,drop=FALSE], Xs[treated_pos_in_keep,,drop=FALSE], k=1)
  d_t_sel <- nn_sel$nn.dist[,1]
} else {
  d_t_sel <- rep(NA, length(d_t_k))
}

# random donors baseline: sample controls equal to number of selected donors
if (length(sel_pos)>0 && length(sel_pos) > 0) {
  control_positions_in_keep <- which(keep_idx %in% samp_controls)
  set.seed(1)
  rand_pos <- sample(control_positions_in_keep, min(length(sel_pos), length(control_positions_in_keep)))
  nn_rand <- get.knnx(Xs[rand_pos,,drop=FALSE], Xs[treated_pos_in_keep,,drop=FALSE], k=1)
  d_t_rand <- nn_rand$nn.dist[,1]
} else {
  d_t_rand <- rep(NA, length(d_t_k))
}

plot_dt <- data.table(
  treated_id = as.character(DT$unit[keep_idx][treated_pos_in_keep]),
  dist_all_k = d_t_k,
  dist_to_selected = d_t_sel,
  dist_to_rand = d_t_rand
)

melt_dt <- melt(plot_dt, id.vars='treated_id', measure.vars=c('dist_all_k','dist_to_selected','dist_to_rand'), variable.name='metric', value.name='dist')

p_ecdf <- ggplot(melt_dt[!is.na(dist)], aes(x=dist, color=metric)) + stat_ecdf(size=1) + theme_minimal() + ggtitle('Pooled ECDF: treated -> k-neighborhood / treated -> nearest selected / treated -> nearest random')
ggsave(file.path(out_fig_dir,'pooled_ecdf_knn_distances.png'), p_ecdf, width=8, height=5)

p_violin <- ggplot(melt_dt[!is.na(dist)], aes(x=metric, y=dist, fill=metric)) + geom_violin(alpha=0.6) + geom_boxplot(width=0.08) + theme_minimal() + ggtitle('Pooled distance comparisons (treated)')
ggsave(file.path(out_fig_dir,'pooled_violin_knn_distances.png'), p_violin, width=6, height=4)

# purity: reuse neighbors from nn_all (we computed up to neighk)
nn_idx <- nn_all$nn.index[, 1:k, drop=FALSE]
purity <- rep(NA, nrow(nn_idx))
if (length(sel_pos)>0) {
  for (i in seq_len(nrow(nn_idx))) {
    nbrs <- nn_idx[i,]
    purity[i] <- mean(nbrs %in% sel_pos)
  }
  pur_dt <- data.table(purity=purity)
  p_purity <- ggplot(pur_dt, aes(x=purity)) + geom_histogram(bins=30, fill='#0072B2', color='white') + theme_minimal() + ggtitle(sprintf('Pooled k-NN purity (k=%d): fraction of k neighbors that are selected donors', k))
  ggsave(file.path(out_fig_dir,'pooled_purity_histogram.png'), p_purity, width=6, height=4)
}

# small heatmap for a few treated and their neighbors
ntake <- min(6, length(treated_pos_in_keep))
neighk <- 50
nn50 <- get.knnx(Xs, Xs[treated_pos_in_keep,,drop=FALSE], k=neighk)$nn.index[1:ntake, , drop=FALSE]
uids <- unique(as.vector(nn50))
mat <- as.matrix(dist(Xs[uids, , drop=FALSE]))
# robustly build a long-form table for the heatmap and label with unit ids
nr <- nrow(mat)
if (nr > 0) {
  idx <- which(matrix(TRUE, nrow=nr, ncol=nr), arr.ind=TRUE)
  hm <- data.table::data.table(Var1 = idx[,1], Var2 = idx[,2], value = mat[cbind(idx[,1], idx[,2])])
  # map numeric positions to unit ids for clearer labels if DT is available
  lab_vals <- tryCatch(as.character(DT$unit[keep_idx][uids]), error=function(e) as.character(uids))
  hm[, Var1 := factor(Var1, levels=seq_along(lab_vals), labels=lab_vals)]
  hm[, Var2 := factor(Var2, levels=seq_along(lab_vals), labels=lab_vals)]
  p_hm <- ggplot(hm, aes(x=Var1, y=Var2, fill=value)) + geom_raster() + scale_fill_viridis() + theme_minimal() + ggtitle('Pooled pairwise distances (small subset)') + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(out_fig_dir,'pooled_pairwise_small_heatmap.png'), p_hm, width=6, height=5)
}

message('Saved pooled diagnostics to ', out_fig_dir)

  # save cache with key results so downstream analysis can reuse heavy outputs
  cache_obj <- list(
    DT_keep = DT[keep_idx],
    band_cols = band_cols,
    keep_idx = keep_idx,
    Xs = Xs,
    treated_pos_in_keep = treated_pos_in_keep,
    samp_controls = samp_controls,
    sel_pos = sel_pos,
    nn_all = list(nn.index = nn_all$nn.index, nn.dist = nn_all$nn.dist),
    params = list(max_controls_total = max_controls_total, max_treated_sample = max_treated_sample, neighk = neighk, k = k),
    saved_time = Sys.time()
  )
  try({
    save_cache(cache_file, cache_obj)
    message('Cached pooled results to: ', cache_file)
  }, silent=TRUE)
}
