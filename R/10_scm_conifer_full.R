# Synthetic control for conifer analysis dataset with full covariates
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

# Load conifer analysis dataset
focal_year <- 2012
analysis_file <- file.path("data", "processed_data", "analysis_conifer", 
                           paste0("analysis_treated", focal_year, "_conifer.RDS"))
stopifnot(file.exists(analysis_file))
dat <- readRDS(analysis_file)

message("Loaded ", nrow(dat), " pixels for year ", focal_year)
message("Treated units: ", sum(dat$treated, na.rm = TRUE))

# Pre-treatment years (expanded 9-year window)
pre_years <- (focal_year - 9):(focal_year - 1)

# Get treated candidates
treated_candidates <- dat %>% 
  filter(treated == 1) %>% 
  pull(unit) %>% 
  unique()

message("Treated candidates: ", length(treated_candidates))

if (length(treated_candidates) == 0) {
  stop("No treated units found in focal year ", focal_year)
}

treated_unit <- treated_candidates[1]
message("Using treated unit: ", treated_unit)

# Load FIRMS and drop geometry
firms <- readRDS(file.path("data", "processed_data", "FIRMS.RDS"))
firms <- st_drop_geometry(firms)
firms$unit <- paste0(firms$LATITUDE, firms$LONGITUDE)

# Extract treated unit's pre-treatment trajectory
treated_series <- firms %>% 
  filter(unit == treated_unit, year %in% pre_years) %>% 
  arrange(year) %>%
  select(year, max_FRP)

if (nrow(treated_series) < length(pre_years)) {
  message("Warning: Treated unit lacks full FIRMS history. Padding with zeros.")
  missing_years <- setdiff(pre_years, treated_series$year)
  padding <- data.frame(year = missing_years, max_FRP = 0)
  treated_series <- bind_rows(treated_series, padding) %>% arrange(year)
}

treated_vec <- treated_series$max_FRP

# Build donor pool
donor_candidates <- dat %>% 
  filter(treated == 0) %>% 
  pull(unit) %>% 
  unique()

message("Potential donors: ", length(donor_candidates))

# Sample if too large
max_donors <- 500
if (length(donor_candidates) > max_donors) {
  set.seed(123)
  donor_candidates <- sample(donor_candidates, max_donors)
  message("Sampled ", max_donors, " donors")
}

# Build donor matrix
donor_matrix <- sapply(donor_candidates, function(u) {
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

message("Final donor pool: ", ncol(donor_matrix), " units")

if (ncol(donor_matrix) == 0) stop("No valid donors")

# Solve QP
Dmat <- t(donor_matrix) %*% donor_matrix + diag(1e-6, ncol(donor_matrix))
dvec <- t(donor_matrix) %*% treated_vec
Amat <- cbind(rep(1, ncol(donor_matrix)), diag(ncol(donor_matrix)))
bvec <- c(1, rep(0, ncol(donor_matrix)))

res <- tryCatch({
  solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
}, error = function(e) NULL)

if (is.null(res)) {
  warning("QP failed; uniform weights")
  weights <- rep(1 / ncol(donor_matrix), ncol(donor_matrix))
} else {
  weights <- res$solution
}

synthetic_pre <- as.numeric(donor_matrix %*% weights)

pre_rmspe <- sqrt(mean((treated_vec - synthetic_pre)^2))
pre_mae <- mean(abs(treated_vec - synthetic_pre))
message("\nPre-treatment fit:")
message("  RMSPE: ", round(pre_rmspe, 3))
message("  MAE: ", round(pre_mae, 3))
message("  Non-zero weights: ", sum(weights > 0.01))

pre_df <- data.frame(
  year = pre_years,
  treated = treated_vec,
  synthetic = synthetic_pre
)

# Plots
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

gg_pre <- ggplot(pre_df, aes(year)) +
  geom_line(aes(y = treated, color = "Treated"), linewidth = 1) +
  geom_line(aes(y = synthetic, color = "Synthetic"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = treated, color = "Treated"), size = 2) +
  geom_point(aes(y = synthetic, color = "Synthetic"), size = 2) +
  labs(title = paste("Pre-treatment Fit (Conifer", focal_year, ")"),
       subtitle = paste("RMSPE:", round(pre_rmspe, 2), "| Donors:", ncol(donor_matrix)),
       y = "max_FRP") +
  scale_color_manual(values = c("Treated" = "firebrick", "Synthetic" = "steelblue")) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave(file.path("figures", paste0("conifer_pre_fit_", focal_year, ".png")), 
       gg_pre, width = 7, height = 5)

# Post-treatment (expanded 6-year window)
post_years <- focal_year:(focal_year + 6)
treated_post <- firms %>% 
  filter(unit == treated_unit, year %in% post_years) %>% 
  arrange(year)

if (nrow(treated_post) < length(post_years)) {
  missing_post <- setdiff(post_years, treated_post$year)
  pad <- data.frame(unit = treated_unit, year = missing_post, max_FRP = 0)
  treated_post <- bind_rows(treated_post %>% select(unit, year, max_FRP), pad) %>% arrange(year)
}

donor_post_matrix <- sapply(donor_candidates, function(u) {
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

synthetic_post <- as.numeric(donor_post_matrix %*% weights)

post_df <- data.frame(
  year = post_years,
  treated = treated_post$max_FRP,
  synthetic = synthetic_post,
  gap = treated_post$max_FRP - synthetic_post
)

avg_gap <- mean(post_df$gap)
message("\nPost-treatment:")
message("  Average gap: ", round(avg_gap, 3))

gg_post <- ggplot(post_df, aes(year)) +
  geom_line(aes(y = treated, color = "Treated"), linewidth = 1) +
  geom_line(aes(y = synthetic, color = "Synthetic"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = treated, color = "Treated"), size = 2) +
  geom_point(aes(y = synthetic, color = "Synthetic"), size = 2) +
  geom_vline(xintercept = focal_year - 0.5, linetype = "dotted") +
  labs(title = paste("Post-treatment (Conifer", focal_year, ")"),
       subtitle = paste("Avg gap:", round(avg_gap, 2)),
       y = "max_FRP") +
  scale_color_manual(values = c("Treated" = "firebrick", "Synthetic" = "steelblue")) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave(file.path("figures", paste0("conifer_post_", focal_year, ".png")), 
       gg_post, width = 7, height = 5)

gg_gap <- ggplot(post_df, aes(year, gap)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Treatment Effect", y = "Gap") +
  theme_minimal()

ggsave(file.path("figures", paste0("conifer_gap_", focal_year, ".png")), 
       gg_gap, width = 7, height = 5)

# Save
result <- list(
  focal_year = focal_year,
  treated_unit = treated_unit,
  weights = weights,
  donor_units = donor_candidates,
  pre_df = pre_df,
  post_df = post_df,
  pre_rmspe = pre_rmspe,
  avg_gap = avg_gap
)

saveRDS(result, file.path("data", "processed_data", paste0("scm_conifer_", focal_year, ".rds")))
message("\n✓ Results saved")
