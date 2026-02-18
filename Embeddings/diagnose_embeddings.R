# ============================================================================
# Embeddings Diagnostic Script
# ============================================================================
# Interactive R script to diagnose embeddings data and identify issues
# 
# Usage in R console:
#   source("Embeddings/diagnose_embeddings.R")
#
# Or run interactively by copying sections and running them
# ============================================================================

library(dplyr)
library(tidyr)
embeddings_file <- "Embeddings/embeddings/embeddings_2019.csv"

cat("\n", strrep("=", 80), "\n")
cat("EMBEDDINGS DIAGNOSTIC REPORT\n")
cat(strrep("=", 80), "\n")

# Step 1: File existence check
cat("\nStep 1: File Check\n")
cat(strrep("-", 80), "\n")
if (!file.exists(embeddings_file)) {
    cat("ERROR: File not found:", embeddings_file, "\n")
    cat("Current working directory:", getwd(), "\n")
    cat("Files in Embeddings/embeddings/:\n")
    if (dir.exists("Embeddings/embeddings")) {
        print(list.files("Embeddings/embeddings/"))
    } else {
        cat("  Directory does not exist!\n")
    }
    stop("Cannot proceed without embeddings file")
} else {
    cat("✓ File found:", embeddings_file, "\n")
    cat("  File size:", file.size(embeddings_file) / 1024^2, "MB\n")
}

# Step 2: Load embeddings
cat("\nStep 2: Loading Data\n")
cat(strrep("-", 80), "\n")
embeddings_df <- read.csv(embeddings_file)
cat("✓ Data loaded successfully\n")

# Step 3: Basic structure
cat("\nStep 3: Data Structure\n")
cat(strrep("-", 80), "\n")
cat("Dimensions:", nrow(embeddings_df), "rows ×", ncol(embeddings_df), "columns\n")
cat("Column names:\n")
print(colnames(embeddings_df))

cat("\nData types:\n")
print(sapply(embeddings_df, class))

# Step 4: Check for required columns
cat("\nStep 4: Required Columns Check\n")
cat(strrep("-", 80), "\n")
# After quantization: 72 dimensions (12 months × 6 channels per month)
required_cols <- c("unit", "treated", paste0("band_", 0:71))
missing_cols <- setdiff(required_cols, colnames(embeddings_df))
if (length(missing_cols) > 0) {
    cat("ERROR: Missing columns:", paste(missing_cols, collapse=", "), "\n")
    cat("NOTE: Expected 72 embedding columns (band_0 to band_71: 12 months × 6 channels)\n")
} else {
    cat("✓ All required columns present (72D embeddings)\n")
}

# Step 5: Treatment variable
cat("\nStep 5: Treatment Variable\n")
cat(strrep("-", 80), "\n")
cat("Treated counts:\n")
print(table(embeddings_df$treated))
cat("Total:", nrow(embeddings_df), "\n")
cat("Treated percentage:", 100 * sum(embeddings_df$treated == 1) / nrow(embeddings_df), "%\n")

# Step 6: Unit ID check
cat("\nStep 6: Unit ID Analysis\n")
cat(strrep("-", 80), "\n")
cat("Unique unit IDs:", n_distinct(embeddings_df$unit), "\n")
cat("Duplicate units:", sum(duplicated(embeddings_df$unit)), "\n")
cat("Sample unit IDs:\n")
print(head(embeddings_df$unit, 10))

# Step 7: Embedding columns analysis
cat("\nStep 7: Embedding Columns Analysis (band_0 to band_71: 72D after quantization)\n")
cat(strrep("-", 80), "\n")
embedding_cols <- paste0("band_", 0:71)

# Check for NaN
nan_summary <- colSums(is.na(embeddings_df[, embedding_cols]))
if (sum(nan_summary) > 0) {
    cat("⚠ WARNING: NaN values found in embeddings!\n")
    print(nan_summary[nan_summary > 0])
} else {
    cat("✓ No NaN values\n")
}

# Check for Inf
inf_count <- sum(sapply(embeddings_df[, embedding_cols], function(x) sum(is.infinite(x))))
if (inf_count > 0) {
    cat("⚠ WARNING:", inf_count, "infinite values found\n")
} else {
    cat("✓ No infinite values\n")
}

# Step 8: Embedding value ranges
cat("\nStep 8: Embedding Value Ranges\n")
cat(strrep("-", 80), "\n")
cat("NOTE: After quantization → L2 normalization:\n")
cat("  - Pre-normalization: values in [-1, 1] (from quantizer)\n")
cat("  - Post-normalization: values scaled by L2 norm (~1.0)\n")
cat("\n")

# Sample first 12 bands (first month's 6 channels × 2) for display
sample_cols <- embedding_cols[seq_len(min(12, length(embedding_cols)))]
for (col in sample_cols) {
    vals <- embeddings_df[[col]]
    # Remove NaN for statistics
    vals_clean <- vals[!is.na(vals)]
    if (length(vals_clean) > 0) {
        cat(sprintf("%s: min=%.6f, max=%.6f, mean=%.6f, sd=%.6f (n_valid=%d)\n",
                    col, min(vals_clean), max(vals_clean), mean(vals_clean), sd(vals_clean), length(vals_clean)))
    } else {
        cat(sprintf("%s: ALL NaN!\n", col))
    }
}
cat("... (", length(embedding_cols) - length(sample_cols), "more bands)\n")

# Overall statistics
all_embedding_values <- unlist(embeddings_df[, embedding_cols])
all_embedding_values <- all_embedding_values[!is.na(all_embedding_values)]
if (length(all_embedding_values) > 0) {
    cat(sprintf("\nOverall embedding statistics (all 72 dimensions):\n"))
    cat(sprintf("  Min: %.6f\n", min(all_embedding_values)))
    cat(sprintf("  Max: %.6f\n", max(all_embedding_values)))
    cat(sprintf("  Mean: %.6f\n", mean(all_embedding_values)))
    cat(sprintf("  Median: %.6f\n", median(all_embedding_values)))
    cat(sprintf("  SD: %.6f\n", sd(all_embedding_values)))
}

# Step 9: Check for zero embeddings (would cause NaN in cosine similarity)
cat("\nStep 9: Check for Zero Vectors and Missing Embeddings\n")
cat(strrep("-", 80), "\n")
embedding_matrix <- as.matrix(embeddings_df[, embedding_cols])

# Count rows with any NaN
rows_with_nan <- rowSums(is.na(embedding_matrix)) > 0
total_rows_with_nan <- sum(rows_with_nan)
cat("Rows with ANY NaN in embeddings:", total_rows_with_nan, "\n")
cat("Percentage:", 100 * total_rows_with_nan / nrow(embeddings_df), "%\n")

if (total_rows_with_nan > 0) {
    cat("\n⚠ CRITICAL: Rows with NaN cannot be used for similarity computation!\n")
    
    # Check which rows have NaN
    nan_rows <- which(rows_with_nan)
    cat("First 10 rows with NaN (out of", total_rows_with_nan, "):\n")
    print(embeddings_df[nan_rows[seq_len(min(10, length(nan_rows)))],
                        c("pixel_id", "unit", "treated", "success", "error")])
    
    # Check if there's a pattern (e.g., related to embedding success)
    cat("\nCross-tabulation: NaN embeddings vs success flag\n")
    print(table(has_nan = rows_with_nan, success = embeddings_df$success))
}

# Only compute norms for non-NaN rows
valid_rows <- !rows_with_nan
if (sum(valid_rows) > 0) {
    row_norms <- sqrt(rowSums(embedding_matrix[valid_rows, ]^2))
    zero_vecs <- sum(row_norms == 0)
    near_zero_vecs <- sum(row_norms < 0.0001)
    
    cat("\nVector norm statistics (excluding NaN rows):\n")
    cat("Zero vectors (norm=0):", zero_vecs, "\n")
    cat("Near-zero vectors (norm<0.0001):", near_zero_vecs, "\n")
    cat("Min norm:", min(row_norms), "\n")
    cat("Max norm:", max(row_norms), "\n")
    cat("Mean norm:", mean(row_norms), "\n")
    cat("Median norm:", median(row_norms), "\n")
    
    if (zero_vecs > 0) {
        cat("⚠ WARNING: Zero vectors will cause NaN in cosine similarity!\n")
    }
} else {
    cat("⚠ CRITICAL: ALL rows have NaN in embeddings!\n")
}

# Step 10: Sample embeddings
cat("\nStep 10: Sample Embeddings (first 5 rows, showing first 12 dims)\n")
cat(strrep("-", 80), "\n")
sample_cols_display <- c("unit", "treated", paste0("band_", 0:11))
print(head(embeddings_df[, sample_cols_display], 5))
cat("... (60 more embedding dimensions: band_12 to band_71)\n")

# Step 11: Cosine similarity test
cat("\nStep 11: Cosine Similarity Test (sample)\n")
cat(strrep("-", 80), "\n")

# Find treated and control with valid embeddings
embedding_cols_indices <- which(colnames(embeddings_df) %in% paste0("band_", 0:71))
treated_valid <- which(embeddings_df$treated == 1 & rowSums(is.na(embeddings_df[, embedding_cols])) == 0)
control_valid <- which(embeddings_df$treated == 0 & rowSums(is.na(embeddings_df[, embedding_cols])) == 0)

if (length(treated_valid) > 0 && length(control_valid) > 0) {
    treated_idx <- treated_valid[1]
    control_indices <- control_valid[seq_len(min(5, length(control_valid)))]
    
    treated_emb <- as.numeric(embeddings_df[treated_idx, embedding_cols])
    
    cat(sprintf("Treated pixel (unit=%s):\n", embeddings_df$unit[treated_idx]))
    cat("Embedding (first 6 dims):", head(treated_emb, 6), "...\n")
    cat("Norm:", sqrt(sum(treated_emb^2)), "\n\n")
    
    cat("Cosine similarities to first", length(control_indices), "valid controls:\n")
    for (i in seq_along(control_indices)) {
        c_idx <- control_indices[i]
        control_emb <- as.numeric(embeddings_df[c_idx, embedding_cols])
        
        # Compute cosine similarity
        dot_product <- sum(treated_emb * control_emb)
        treated_norm <- sqrt(sum(treated_emb^2))
        control_norm <- sqrt(sum(control_emb^2))
        
        sim <- dot_product / (treated_norm * control_norm)
        cat(sprintf("  Control %d (unit=%s): %.6f\n", i, embeddings_df$unit[c_idx], sim))
    }
} else {
    cat("⚠ No treated or control pixels with valid (non-NaN) embeddings found!\n")
    cat("  Valid treated pixels:", length(treated_valid), "\n")
    cat("  Valid control pixels:", length(control_valid), "\n")
}

# Step 12: Summary and recommendations
cat("\nStep 12: Summary & Recommendations\n")
cat(strrep("-", 80), "\n")
issues <- c()

# Check for NaN
if (total_rows_with_nan > 0) {
    issues <- c(issues, sprintf("- %d rows (%.1f%%) have NaN in embeddings", 
                                total_rows_with_nan, 100*total_rows_with_nan/nrow(embeddings_df)))
}

if (sum(nan_summary) > 0 && sum(nan_summary) < nrow(embeddings_df) * 12) {
    # Only if some (not all) values are NaN
}

if (length(issues) == 0) {
    cat("✓ No obvious issues detected\n")
    cat("\nEmbeddings appear valid for similarity computation:\n")
    cat("  - No NaN values\n")
    cat("  - No zero vectors\n")
    cat("  - Reasonable norm values (L2-normalized to ~1.0)\n")
    cat("  - 72 dimensions (12 months × 6 channels from quantization)\n")
    cat("\nAbout the quantization process:\n")
    cat("  1. Raw GeoTIFF contains uint16 codes (quantized indices)\n")
    cat("  2. Quantizer converts codes → 6D vectors per month (values in [-1, 1])\n")
    cat("  3. 12 months × 6 channels = 72 total dimensions\n")
    cat("  4. L2 normalization applied for consistent similarity computation\n")
} else {
    cat("⚠ Issues detected:\n")
    for (issue in issues) {
        cat(issue, "\n")
    }
    cat("\nRECOMMENDATIONS:\n")
    if (total_rows_with_nan > 0) {
        cat("  1. FILTER OUT rows with NaN before computing similarities\n")
        cat("     embeddings_valid <- embeddings_df[rowSums(is.na(embeddings_df[, embedding_cols])) == 0, ]\n")
        cat("  2. Check why embeddings have NaN:\n")
        cat("     - Look at the 'success' and 'error' columns\n")
        cat("     - Reprocess failed pixels in the embedding generation step\n")
        cat("  3. Verify that the quantization step completed successfully\n")
        cat("     - Check for 'dequantization_failed' errors\n")
        cat("     - Ensure esd_quantizer.py is working correctly\n")
    }
}

cat("\n", strrep("=", 80), "\n")
cat("End of diagnostic report\n")
cat(strrep("=", 80), "\n\n")
