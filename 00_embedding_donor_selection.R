# 00_embedding_donor_selection.R
# Placeholder-embeddings -> nearest-neighbor donor map -> save filtered dataset
library(dplyr)
library(FNN)   # get.knnx
set.seed(123)

# 1) Load the base panel (use the df_final_1 that's already in data/processed)
df <- read.csv("data/processed/df_final_1.csv", stringsAsFactors = FALSE)

# 2) Build unit-level list of unique IDs (pixel / AOI units)
unit_ids <- sort(unique(df$ID_new))
n_units <- length(unit_ids)
cat("Number of unique units:", n_units, "\n")

# 3) Create placeholder embeddings per unit (replace with real embeddings later)
embedding_dim <- 32          # pick a moderate dim for testing
emb_mat <- matrix(rnorm(n_units * embedding_dim), nrow = n_units, ncol = embedding_dim)
emb_df <- as.data.frame(emb_mat)
colnames(emb_df) <- paste0("emb_", seq_len(ncol(emb_df)))
emb_df$ID_new <- unit_ids

# 4) Merge embeddings back into the main df (unit-level values will repeat across times)
df_embed <- merge(df, emb_df, by = "ID_new", all.x = TRUE)

# 5) Aggregate one embedding vector per unit (mean across rows -> same as emb_df)
unit_embed <- emb_df

# 6) Identify treated vs control units (treat==1 ever -> treated)
treated_flag <- df %>% group_by(ID_new) %>% summarise(treated = max(treated, na.rm = TRUE))
unit_embed <- merge(unit_embed, treated_flag, by = "ID_new")

treated_units <- unit_embed %>% filter(treated == 1)
control_units <- unit_embed %>% filter(treated == 0)

cat("Treated units:", nrow(treated_units), " Control units:", nrow(control_units), "\n")

# 7) Compute k nearest neighbors in embedding space (control -> treated)
k <- 20   # top-k donors per treated unit for testing
control_matrix <- as.matrix(control_units %>% select(starts_with("emb_")))
treated_matrix <- as.matrix(treated_units %>% select(starts_with("emb_")))

nn_out <- get.knnx(data = control_matrix, query = treated_matrix, k = pmin(k, nrow(control_units)))

# 8) Build donor map table: for each treated ID, list its k closest control IDs
donor_rows <- lapply(seq_len(nrow(treated_units)), function(i){
  treated_id <- treated_units$ID_new[i]
  indices <- nn_out$nn.index[i, ]
  control_ids <- control_units$ID_new[indices]
  data.frame(treated_id = treated_id, control_id = control_ids, stringsAsFactors = FALSE)
})
donor_map <- do.call(rbind, donor_rows)

# 9) Save donor map
write.csv(donor_map, "data/processed/donor_map_embeddings.csv", row.names = FALSE)
cat("Saved: data/processed/donor_map_embeddings.csv\n")

# 10) Restrict original df to only units present in donor_map (treated + their candidate donors)
valid_units <- unique(c(donor_map$treated_id, donor_map$control_id))
df_filtered <- df_embed %>% filter(ID_new %in% valid_units)

write.csv(df_filtered, "data/processed/df_final_embeddings.csv", row.names = FALSE)
cat("Saved: data/processed/df_final_embeddings.csv  (rows:", nrow(df_filtered), ")\n")
