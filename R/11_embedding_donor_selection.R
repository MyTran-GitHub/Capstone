# Embedding-based donor selection for synthetic control
# Uses cosine similarity in embedding space to restrict donor pool
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
if (!requireNamespace("lsa", quietly = TRUE)) install.packages("lsa")

library(dplyr)
library(here)
library(lsa)  # For cosine similarity

setwd(here::here())

# Load embeddings
embeddings_file <- file.path("data", "processed_data", "embeddings.csv")
if (!file.exists(embeddings_file)) {
  stop("Embeddings file not found. Run python/compute_embeddings.py first.")
}

message("Loading embeddings...")
embeddings <- read.csv(embeddings_file)

# Extract embedding matrix (all columns starting with 'emb_')
emb_cols <- grep("^emb_", names(embeddings), value = TRUE)
emb_matrix <- as.matrix(embeddings[, emb_cols])
rownames(emb_matrix) <- embeddings$unit

message("Loaded ", nrow(embeddings), " embeddings with dimension ", length(emb_cols))

# Load conifer analysis dataset
focal_year <- 2012
analysis_file <- file.path("data", "processed_data", "analysis_conifer", 
                           paste0("analysis_treated", focal_year, "_conifer.RDS"))
dat <- readRDS(analysis_file)

# Get treated units
treated_units <- dat %>% filter(treated == 1) %>% pull(unit) %>% unique()
donor_pool <- dat %>% filter(treated == 0) %>% pull(unit) %>% unique()

message("Treated units: ", length(treated_units))
message("Potential donors: ", length(donor_pool))

# Filter to units with embeddings
treated_with_emb <- intersect(treated_units, embeddings$unit)
donors_with_emb <- intersect(donor_pool, embeddings$unit)

message("Treated with embeddings: ", length(treated_with_emb))
message("Donors with embeddings: ", length(donors_with_emb))

if (length(treated_with_emb) == 0) {
  stop("No treated units have embeddings. Extract chips for treated units first.")
}

# Select first treated unit for demonstration
treated_unit <- treated_with_emb[1]
message("\nUsing treated unit: ", treated_unit)

# Get treated unit embedding
treated_emb <- emb_matrix[treated_unit, ]

# Compute cosine similarities to all donors
donor_emb_matrix <- emb_matrix[donors_with_emb, ]
similarities <- apply(donor_emb_matrix, 1, function(donor_emb) {
  cosine(treated_emb, donor_emb)
})

# Create similarity dataframe
similarity_df <- data.frame(
  unit = donors_with_emb,
  similarity = similarities
) %>% arrange(desc(similarity))

message("\nTop 10 most similar donors:")
print(head(similarity_df, 10))

# Select top-K donors
K <- 50  # Number of similar donors to use
top_K_donors <- similarity_df$unit[1:K]

message("\nSelected top ", K, " donors by embedding similarity")
message("Similarity range: ", round(min(similarity_df$similarity[1:K]), 3), 
        " to ", round(max(similarity_df$similarity[1:K]), 3))

# Save restricted donor list
restricted_list <- list(
  treated_unit = treated_unit,
  focal_year = focal_year,
  K = K,
  donors = top_K_donors,
  similarities = similarity_df$similarity[1:K]
)

saveRDS(restricted_list, 
        file.path("data", "processed_data", 
                  paste0("embedding_donors_K", K, "_", focal_year, ".rds")))

message("\nSaved restricted donor list to embedding_donors_K", K, "_", focal_year, ".rds")

# Diagnostic plot: similarity distribution
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

png(file.path("figures", paste0("embedding_similarity_dist_", focal_year, ".png")),
    width = 800, height = 600)
hist(similarity_df$similarity, 
     breaks = 50,
     main = paste("Cosine Similarity Distribution\nTreated unit:", treated_unit),
     xlab = "Cosine Similarity",
     col = "steelblue",
     border = "white")
abline(v = similarity_df$similarity[K], col = "red", lwd = 2, lty = 2)
text(similarity_df$similarity[K], par("usr")[4] * 0.9, 
     paste("Top", K, "cutoff"), pos = 4, col = "red")
dev.off()

message("Saved similarity distribution plot")
