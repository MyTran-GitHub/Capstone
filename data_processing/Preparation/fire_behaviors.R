# Process historical FIRMS fire info, used as pre-treatment covariates in covariate balance (value 0 if no fires)
library(fst)
library("sf")
library("tidyverse")

outDir <- "/data/processed_data"
outDir <- "../data/processed_data"

# Load conifer-only grid (created by vegetation_class.R)
fveg_grid_ca <- readRDS(file.path(outDir, "fveg_grid_ca.RDS"))

# Load FIRMS and filter to conifer cells only
fire.df <- readRDS(file.path(outDir, "FIRMS.RDS"))

st_geometry(fire.df) <- NULL

# Add unit column for join
fire.df$unit <- paste0(fire.df$LATITUDE, fire.df$LONGITUDE)
fveg_grid_ca$unit <- paste0(fveg_grid_ca$LATITUDE, fveg_grid_ca$LONGITUDE)

# Filter FIRMS to conifer cells only
fire.df <- fire.df[fire.df$unit %in% fveg_grid_ca$unit, ]

# Use only conifer grid
df <- fveg_grid_ca[,1:2]
df$unit <- paste0(df$LATITUDE, df$LONGITUDE)

# define unit by the standardized geographic coordinates
gpw_grid_ca = readRDS(file.path(outDir, "gpw_grid_ca.RDS"))
df <- gpw_grid_ca[,1:2]
st_geometry(df) <- NULL
df$unit <- paste0(df$LATITUDE, df$LONGITUDE)

# for each grid, assign yearly historical fire information
Q <- list()  
for (j in 2000:2021) {
    p <- subset(fire.df, year == j)
    p.u <- unique(p[c("unit")])
    index <- match(p.u$unit, p$unit)
    fire <- table(p$unit)
    fire <- as.numeric(fire)
    p.new <- cbind(p[index, ], fire)
    Q <- append(Q, list(p.new))
}
fire.df.new <- do.call(rbind, Q)

var <- c("fire", "avg_BRIGHTNESS", "max_FRP")
parameters <- expand.grid(2000:2021, var)

for (par in 1:nrow(parameters)) {
  dfn <- data.frame(matrix(0, nrow = nrow(df), ncol = 1))
  colnames(dfn) <- paste0(parameters[par, 2], "_", parameters[par, 1])
  df <- cbind(df, dfn)
}

for (j in 2000:2021) {
  p <- subset(fire.df.new, year == j)
      index <- match(p$unit, df$unit)
      df[index, paste0("fire_", j)] <- 1
      
      for (i in 1:length(index)) {
        df[index[i], paste0("avg_BRIGHTNESS_", j)] <- p$avg_BRIGHTNESS[i]
        df[index[i], paste0("max_FRP_", j)] <- p$max_FRP[i]
      }   
}
df <- df[,-3]

write_fst(df, path = file.path(outDir, "fire_brightness_frp.fst"))
write_fst(df, path = file.path(outDir, "fire_brightness_frp_conifer.fst"))
saveRDS(df, file = file.path(outDir, "fire_brightness_frp_conifer.RDS"))
message("✓ Created conifer-only fire brightness and FRP data")