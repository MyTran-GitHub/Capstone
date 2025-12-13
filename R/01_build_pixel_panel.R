# Build a minimal pixel-level panel from FIRMS
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(dplyr)
library(data.table)
library(here)

setwd(here::here())
firms_path <- file.path("data", "processed_data", "FIRMS.RDS")
stopifnot(file.exists(firms_path))
firms <- readRDS(firms_path)

firms$unit <- paste0(firms$LATITUDE, firms$LONGITUDE)

# Outcome definitions
firms <- firms %>% mutate(
  any_fire = ifelse(max_FRP > 0, 1, 0),
  fire_class = dplyr::case_when(
    max_FRP == 0 ~ 0L,
    max_FRP > 0 & max_FRP < 100 ~ 1L,
    max_FRP >= 100 & max_FRP < 500 ~ 2L,
    max_FRP >= 500 & max_FRP < 1000 ~ 3L,
    max_FRP >= 1000 & max_FRP < 1500 ~ 4L,
    max_FRP >= 1500 ~ 5L,
    TRUE ~ NA_integer_
  )
)

# Simple fire history covariates (lags over previous 3 years where available)
setDT(firms)
firms <- firms[order(unit, year)]
firms[, any_fire_lag1 := shift(any_fire, 1, type = "lag"), by = unit]
firms[, any_fire_lag2 := shift(any_fire, 2, type = "lag"), by = unit]
firms[, any_fire_lag3 := shift(any_fire, 3, type = "lag"), by = unit]
firms[, max_FRP_lag1 := shift(max_FRP, 1, type = "lag"), by = unit]
firms[, max_FRP_lag2 := shift(max_FRP, 2, type = "lag"), by = unit]
firms[, max_FRP_lag3 := shift(max_FRP, 3, type = "lag"), by = unit]

# Replace NA lags with 0 for early years
lag_cols <- c("any_fire_lag1","any_fire_lag2","any_fire_lag3","max_FRP_lag1","max_FRP_lag2","max_FRP_lag3")
for (lc in lag_cols) firms[is.na(get(lc)), (lc) := 0]

pixel_panel <- as.data.frame(firms)
saveRDS(pixel_panel, file = file.path("data", "processed_data", "pixel_panel.rds"))
message("Saved pixel panel to data/processed_data/pixel_panel.rds")
