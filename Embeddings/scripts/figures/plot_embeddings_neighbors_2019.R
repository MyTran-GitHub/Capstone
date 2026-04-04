#!/usr/bin/env Rscript
# Plot UMAP of embeddings: treated vs all controls, and treated vs selected donors
suppressPackageStartupMessages({
  pkgs <- c('data.table','uwot','ggplot2')
  for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, repos='https://cloud.r-project.org')
  library(data.table); library(uwot); library(ggplot2)
})

emb_f <- 'Embeddings/data/embeddings/embeddings_2019.csv'
sel_k10 <- 'Embeddings/data/cbps_integration/2019/selected_controls_k10_2019.csv'
sel_k100 <- 'Embeddings/data/cbps_integration/2019/selected_controls_k100_2019.csv'
out_dir <- 'Embeddings/data/Mock/plots'
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
set.seed(2026)

# Read embeddings (only necessary columns)
msg <- function(...) cat(sprintf(...), "\n")
msg('Reading embeddings (this may take a moment)')
dt <- fread(emb_f, showProgress=FALSE)
# detect band columns
band_cols <- grep('^band_', names(dt), value=TRUE)
if (length(band_cols)==0) stop('No band_ columns found')
msg('Bands found: %d', length(band_cols))

# optionally filter to year 2019 (file likely contains 2019 only)
dt <- dt[year==2019]

# subsample to speed up UMAP if extremely large, but keep all treated
n_total <- nrow(dt)
msg('Total rows (2019): %d', n_total)

treated_idx <- which(dt$treated==1)
n_treated <- length(treated_idx)
msg('Found treated: %d', n_treated)

# sample up to 20000 controls for UMAP (but include all treated)
controls_idx <- which(dt$treated==0)
max_controls <- 20000
if (length(controls_idx) > max_controls) {
  set.seed(42)
  samp_controls <- sample(controls_idx, max_controls)
} else samp_controls <- controls_idx
keep_idx <- sort(unique(c(treated_idx, samp_controls)))
msg('Using %d rows for UMAP (%d treated + %d controls sampled)', length(keep_idx), n_treated, length(samp_controls))

X <- as.matrix(dt[keep_idx, ..band_cols])
# run UMAP
msg('Running UMAP...')
um <- uwot::umap(X, n_neighbors=15, min_dist=0.1, n_components=2, metric='cosine', verbose=TRUE)

coords <- data.table(unit = dt$unit[keep_idx], lat = dt$lat[keep_idx], lon = dt$lon[keep_idx], treated = dt$treated[keep_idx])
coords[, U1 := um[,1]]
coords[, U2 := um[,2]]

# read selected controls (k10 and k100 if available)
read_sel <- function(path) {
  if (!file.exists(path)) return(NULL)
  sel <- fread(path, showProgress=FALSE)
  # ensure column named 'unit'
  if (!'unit' %in% names(sel)) names(sel)[1] <- 'unit'
  sel$unit
}
sel10 <- read_sel(sel_k10)
sel100 <- read_sel(sel_k100)

# Plot 1: treated vs all controls (subset used)
p1 <- ggplot(coords, aes(x=U1, y=U2, color=factor(treated), shape=factor(treated))) +
  geom_point(alpha=0.6, size=0.9) +
  scale_color_manual(values=c('0'='#888888','1'='#D55E00'), labels=c('Control','Treated')) +
  scale_shape_manual(values=c('0'=16,'1'=17), labels=c('Control','Treated')) +
  theme_minimal() + labs(color='', shape='') + ggtitle('UMAP: Treated vs All Controls (2019)')

# Plot 2: treated vs selected donors (use sel100 if available else sel10)
sel_units <- if (!is.null(sel100)) sel100 else sel10
coords[, selected := ifelse(unit %in% sel_units, 1, 0)]
# keep only points present in coords (we sampled controls)
coords[, type := ifelse(treated==1, 'treated', ifelse(selected==1, 'selected', 'control'))]

p2 <- ggplot(coords, aes(x=U1, y=U2)) +
  geom_point(data=coords[type=='control'], aes(x=U1, y=U2), color='#DDDDDD', alpha=0.4, size=0.7) +
  geom_point(data=coords[type=='selected'], aes(x=U1, y=U2), color='#0072B2', alpha=0.9, size=1.2) +
  geom_point(data=coords[type=='treated'], aes(x=U1, y=U2), color='#D55E00', alpha=0.9, size=1.1, shape=17) +
  theme_minimal() + ggtitle('UMAP: Treated (orange) vs Embedding-selected donors (blue)')

# save
ggsave(file.path(out_dir, 'umap_treated_vs_all_2019.png'), p1, width=7, height=5)
ggsave(file.path(out_dir, 'umap_treated_vs_selected_2019.png'), p2, width=7, height=5)
msg('Saved plots to %s', out_dir)
