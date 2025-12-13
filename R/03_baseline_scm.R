# Minimal synthetic control style weighting for one treated unit
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("quadprog", quietly = TRUE)) install.packages("quadprog")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(dplyr)
library(quadprog)
library(ggplot2)
library(here)

setwd(here::here())
subset_path <- file.path("data","processed_data","subset_conifer2012_region.rds")
stopifnot(file.exists(subset_path))
dat <- readRDS(subset_path)

focal_year <- 2012
pre_years_candidates <- list(
  (focal_year - 5):(focal_year - 1),  # 2007-2011
  (focal_year - 4):(focal_year - 1),  # 2008-2011
  (focal_year - 3):(focal_year - 1)   # 2009-2011
)

treated_candidates <- dat %>% filter(year == focal_year, treated == 1) %>% pull(unit) %>% unique()

# Try progressively shorter pre windows if none have full history
treated_units <- character(0)
chosen_pre_years <- NULL
for (pre_years in pre_years_candidates) {
  tu <- treated_candidates[sapply(treated_candidates, function(u) {
    nrow(dat %>% filter(unit == u, year %in% pre_years)) == length(pre_years)
  })]
  if (length(tu) > 0) {
    treated_units <- tu
    chosen_pre_years <- pre_years
    break
  }
}

if (length(treated_units) == 0) {
  stop("No treated units with complete pre-treatment history in 2007–2011, 2008–2011, or 2009–2011. Consider widening region or changing focal year.")
}

treated_unit <- treated_units[1]
message("Using treated unit: ", treated_unit)

treated_series <- dat %>% filter(unit == treated_unit, year %in% chosen_pre_years) %>% arrange(year)

# Donor pool: units never treated in focal year and not the treated unit
donors <- dat %>% filter(year %in% chosen_pre_years, unit != treated_unit)
donor_units <- donors %>% pull(unit) %>% unique()

# Build matrix of pre-treatment outcomes (max_FRP) for donors
donor_matrix <- sapply(donor_units, function(u) {
  s <- dat %>% filter(unit == u, year %in% chosen_pre_years) %>% arrange(year)
  if (nrow(s) == length(chosen_pre_years)) s$max_FRP else rep(NA_real_, length(chosen_pre_years))
})

# Remove donors with incomplete history
valid_cols <- colSums(is.na(donor_matrix)) == 0
donor_matrix <- donor_matrix[, valid_cols, drop = FALSE]
donor_units <- donor_units[valid_cols]

treated_vec <- treated_series$max_FRP

if (ncol(donor_matrix) == 0) stop("No valid donors with full pre-treatment history.")

# Solve quadratic program: minimize ||D w - t||^2 subject to w >=0, sum w =1
Dmat <- t(donor_matrix) %*% donor_matrix + diag(1e-6, ncol(donor_matrix))
dvec <- t(donor_matrix) %*% treated_vec
Amat <- cbind(rep(1, ncol(donor_matrix)), diag(ncol(donor_matrix)))
bvec <- c(1, rep(0, ncol(donor_matrix)))
res <- tryCatch({
  solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
}, error = function(e) NULL)

if (is.null(res)) {
  warning("QP failed; using uniform weights.")
  weights <- rep(1/ ncol(donor_matrix), ncol(donor_matrix))
} else {
  weights <- res$solution
}

synthetic_pre <- as.numeric(donor_matrix %*% weights)

pre_df <- data.frame(
  year = chosen_pre_years,
  treated = treated_vec,
  synthetic = synthetic_pre
)

gg_pre <- ggplot(pre_df, aes(year)) +
  geom_line(aes(y = treated, color = "Treated"), linewidth = 1) +
  geom_line(aes(y = synthetic, color = "Synthetic"), linewidth = 1, linetype = "dashed") +
  labs(title = paste("Pre-treatment fit for unit", treated_unit), y = "max_FRP") +
  scale_color_manual(values = c("Treated" = "firebrick", "Synthetic" = "steelblue")) +
  theme_minimal()
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)
ggsave(file.path("figures", paste0("pre_fit_", treated_unit, ".png")), gg_pre, width = 6, height = 4)

# Post-treatment comparison (extend 3 years)
post_years <- focal_year:(focal_year + 3)
treated_post <- dat %>% filter(unit == treated_unit, year %in% post_years) %>% arrange(year)
donor_post_matrix <- sapply(1:ncol(donor_matrix), function(j) {
  u <- donor_units[j]
  s <- dat %>% filter(unit == u, year %in% post_years) %>% arrange(year)
  if (nrow(s) == length(post_years)) s$max_FRP else rep(NA_real_, length(post_years))
})
synthetic_post <- colSums(t(donor_post_matrix) * weights, na.rm = TRUE)

post_df <- data.frame(
  year = post_years,
  treated = treated_post$max_FRP,
  synthetic = synthetic_post
)
post_df$gap <- post_df$treated - post_df$synthetic

gg_post <- ggplot(post_df, aes(year)) +
  geom_line(aes(y = treated, color = "Treated"), linewidth = 1) +
  geom_line(aes(y = synthetic, color = "Synthetic"), linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey50") +
  labs(title = paste("Post-treatment trajectory for unit", treated_unit), y = "max_FRP") +
  scale_color_manual(values = c("Treated" = "firebrick", "Synthetic" = "steelblue")) +
  theme_minimal()
ggsave(file.path("figures", paste0("post_traj_", treated_unit, ".png")), gg_post, width = 6, height = 4)

gg_gap <- ggplot(post_df, aes(year, gap)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = paste("Treated - Synthetic gap (unit", treated_unit, ")"), y = "FRP gap") +
  theme_minimal()
ggsave(file.path("figures", paste0("gap_", treated_unit, ".png")), gg_gap, width = 6, height = 4)

saveRDS(list(weights = weights, donor_units = donor_units, pre_df = pre_df, post_df = post_df),
        file = file.path("data","processed_data", paste0("scm_result_", treated_unit, ".rds")))
message("Saved SCM result object.")
