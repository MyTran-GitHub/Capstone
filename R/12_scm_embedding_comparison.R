# SCM with embedding-restricted donor pool
# Compares baseline (random donors) vs embedding-selected donors
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("quadprog", quietly = TRUE)) install.packages("quadprog")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(dplyr)
library(quadprog)
library(ggplot2)
library(here)
library(sf)

setwd(here::here())

# Load embedding-restricted donor list
focal_year <- 2012
K <- 50  # Must match value from 11_embedding_donor_selection.R

restricted_file <- file.path("data", "processed_data", 
                             paste0("embedding_donors_K", K, "_", focal_year, ".rds"))

if (!file.exists(restricted_file)) {
  stop("Embedding donor file not found. Run R/11_embedding_donor_selection.R first.")
}

restricted_list <- readRDS(restricted_file)
treated_unit <- restricted_list$treated_unit
restricted_donors <- restricted_list$donors

message("Using treated unit: ", treated_unit)
message("Restricted donors: ", length(restricted_donors))

# Pre/post years
pre_years <- (focal_year - 9):(focal_year - 1)
post_years <- focal_year:(focal_year + 6)

# Load FIRMS
firms <- readRDS(file.path("data", "processed_data", "FIRMS.RDS"))
firms <- st_drop_geometry(firms)
firms$unit <- paste0(firms$LATITUDE, firms$LONGITUDE)

# Treated series
treated_series <- firms %>% 
  filter(unit == treated_unit, year %in% pre_years) %>% 
  arrange(year) %>%
  select(year, max_FRP)

if (nrow(treated_series) < length(pre_years)) {
  missing_years <- setdiff(pre_years, treated_series$year)
  padding <- data.frame(year = missing_years, max_FRP = 0)
  treated_series <- bind_rows(treated_series, padding) %>% arrange(year)
}

treated_vec <- treated_series$max_FRP

# Build EMBEDDING-RESTRICTED donor matrix
donor_matrix_emb <- sapply(restricted_donors, function(u) {
  s <- firms %>% filter(unit == u, year %in% pre_years) %>% arrange(year)
  if (nrow(s) == length(pre_years)) {
    s$max_FRP
  } else {
    vals <- rep(0, length(pre_years))
    if (nrow(s) > 0) {
      idx <- match(s$year, pre_years)
      vals[idx] <- s$max_FRP
    }
    vals
  }
})

message("Embedding-restricted donor pool: ", ncol(donor_matrix_emb), " units")

# Solve QP for embedding-restricted
Dmat_emb <- t(donor_matrix_emb) %*% donor_matrix_emb + diag(1e-6, ncol(donor_matrix_emb))
dvec_emb <- t(donor_matrix_emb) %*% treated_vec
Amat_emb <- cbind(rep(1, ncol(donor_matrix_emb)), diag(ncol(donor_matrix_emb)))
bvec_emb <- c(1, rep(0, ncol(donor_matrix_emb)))

res_emb <- tryCatch({
  solve.QP(Dmat_emb, dvec_emb, Amat_emb, bvec_emb, meq = 1)
}, error = function(e) NULL)

if (is.null(res_emb)) {
  weights_emb <- rep(1 / ncol(donor_matrix_emb), ncol(donor_matrix_emb))
} else {
  weights_emb <- res_emb$solution
}

synthetic_pre_emb <- as.numeric(donor_matrix_emb %*% weights_emb)

# Metrics
pre_rmspe_emb <- sqrt(mean((treated_vec - synthetic_pre_emb)^2))
pre_mae_emb <- mean(abs(treated_vec - synthetic_pre_emb))

message("\nEmbedding-restricted SCM:")
message("  Pre-RMSPE: ", round(pre_rmspe_emb, 3))
message("  Pre-MAE: ", round(pre_mae_emb, 3))
message("  Non-zero weights: ", sum(weights_emb > 0.01))

# Post-treatment
treated_post <- firms %>% 
  filter(unit == treated_unit, year %in% post_years) %>% 
  arrange(year)

if (nrow(treated_post) < length(post_years)) {
  missing_post <- setdiff(post_years, treated_post$year)
  pad <- data.frame(unit = treated_unit, year = missing_post, max_FRP = 0)
  treated_post <- bind_rows(treated_post %>% select(unit, year, max_FRP), pad) %>% arrange(year)
}

donor_post_matrix_emb <- sapply(restricted_donors, function(u) {
  s <- firms %>% filter(unit == u, year %in% post_years) %>% arrange(year)
  if (nrow(s) == length(post_years)) {
    s$max_FRP
  } else {
    vals <- rep(0, length(post_years))
    if (nrow(s) > 0) {
      idx <- match(s$year, post_years)
      vals[idx] <- s$max_FRP
    }
    vals
  }
})

synthetic_post_emb <- as.numeric(donor_post_matrix_emb %*% weights_emb)

post_df_emb <- data.frame(
  year = post_years,
  treated = treated_post$max_FRP,
  synthetic = synthetic_post_emb,
  gap = treated_post$max_FRP - synthetic_post_emb
)

avg_gap_emb <- mean(post_df_emb$gap)
message("  Post avg gap: ", round(avg_gap_emb, 3))

# Load baseline result for comparison
baseline_file <- file.path("data", "processed_data", paste0("scm_conifer_", focal_year, ".rds"))
if (file.exists(baseline_file)) {
  baseline <- readRDS(baseline_file)
  
  message("\nBaseline SCM (random donors):")
  message("  Pre-RMSPE: ", round(baseline$pre_rmspe, 3))
  message("  Post avg gap: ", round(baseline$avg_gap, 3))
  message("  Donors: ", length(baseline$donor_units))
  
  # Comparison table
  comparison <- data.frame(
    Method = c("Baseline (random)", "Embedding-restricted"),
    Donors = c(length(baseline$donor_units), length(restricted_donors)),
    Pre_RMSPE = c(baseline$pre_rmspe, pre_rmspe_emb),
    Pre_MAE = c(NA, pre_mae_emb),
    Post_Avg_Gap = c(baseline$avg_gap, avg_gap_emb)
  )
  
  message("\nComparison:")
  print(comparison)
}

# Plot comparison
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

pre_df_emb <- data.frame(
  year = pre_years,
  treated = treated_vec,
  synthetic_baseline = if(exists("baseline")) baseline$pre_df$synthetic else rep(NA, length(pre_years)),
  synthetic_embedding = synthetic_pre_emb
)

library(tidyr)
pre_df_long <- pre_df_emb %>%
  pivot_longer(cols = starts_with("synthetic"), 
               names_to = "method", 
               values_to = "synthetic",
               names_prefix = "synthetic_")

gg_compare <- ggplot(pre_df_long, aes(year)) +
  geom_line(aes(y = treated, color = "Treated"), linewidth = 1.2) +
  geom_point(aes(y = treated, color = "Treated"), size = 3) +
  geom_line(aes(y = synthetic, color = method, linetype = method), linewidth = 1) +
  geom_point(aes(y = synthetic, color = method), size = 2) +
  scale_color_manual(values = c("Treated" = "firebrick", 
                                 "baseline" = "grey50",
                                 "embedding" = "steelblue")) +
  scale_linetype_manual(values = c("baseline" = "dotted", "embedding" = "dashed")) +
  labs(title = "Pre-treatment Fit Comparison",
       subtitle = paste("Embedding RMSPE:", round(pre_rmspe_emb, 2)),
       y = "max_FRP", x = "Year") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave(file.path("figures", paste0("comparison_pre_", focal_year, ".png")),
       gg_compare, width = 8, height = 5)

# Save embedding result
result_emb <- list(
  focal_year = focal_year,
  treated_unit = treated_unit,
  method = "embedding_restricted",
  K = K,
  weights = weights_emb,
  donor_units = restricted_donors,
  similarities = restricted_list$similarities,
  pre_df = data.frame(year = pre_years, treated = treated_vec, synthetic = synthetic_pre_emb),
  post_df = post_df_emb,
  pre_rmspe = pre_rmspe_emb,
  pre_mae = pre_mae_emb,
  avg_gap = avg_gap_emb
)

saveRDS(result_emb, file.path("data", "processed_data", 
                              paste0("scm_embedding_K", K, "_", focal_year, ".rds")))

message("\n✓ Results saved")
message("✓ Comparison plot saved to figures/comparison_pre_", focal_year, ".png")
