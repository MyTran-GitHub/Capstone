library(gsynth)
library(panelView)
library(gridExtra)
library(zoo)
require(dplyr)
library(data.table)
library(doParallel)
library(powerlmm)
library(ggpubr)
# devtools::install_github('xuyiqing/gsynth')


setwd()
source('src/synthetic_control_src.R')
path_processed_data_folder <- 'data/processed/'
results_path <- 'results/'

###########################

df_final_1 <- read.csv(paste(path_processed_data_folder,'df_final_1.csv', sep=''))
df_ <- df_final_1
df_ <- df_[order(df_$ID_new),]
dim(df_)
summary(df_)

covariates <- c('vs', 'tmmx', 'pr', 'bi')
outcome <- 'ndvi'
index_variables <- c("ID_new", "time")
variables <- c('treated', covariates)
outcome <- paste('values_', outcome, sep='')
final_df <- df_[complete.cases(df_[, c(index_variables, variables, outcome)]), 
                c(index_variables, variables, outcome)]

f <- as.formula(
  paste(outcome, paste(variables, ' + ', paste(covariates, collapse=' + '), sep = ''), sep=' ~ ')
)
min.T0 <- 26 * 10

system.time(out_mc <- gsynth(f, 
                          data = final_df,
                          index = index_variables,
                          force = "two-way", 
                          r = 2,
                          cov.ar = 1,
                          se = TRUE, 
                          CV=1, 
                          lambda = .01,
                          estimator='mc',
                          nboots = 100, 
                          parallel = TRUE, 
                          seed=12345,
                          min.T0=min.T0))

dir.create('results/synthetic_control_objects/out_mc_ndvi_10y_pt/')
saveRDS(out_mc, 'results/synthetic_control_objects/out_mc_ndvi_10y_pt/out_mc.R')
write.csv(out_mc$Y.tr, 'results/synthetic_control_objects/out_mc_ndvi_10y_pt/out_mc_ndvi_Y_tr.csv')
write.csv(out_mc$Y.ct, 'results/synthetic_control_objects/out_mc_ndvi_10y_pt/out_mc_ndvi_Y_ct.csv')
write.csv(out_mc$Y.co, 'results/synthetic_control_objects/out_mc_ndvi_10y_pt/out_mc_ndvi_Y_co.csv')
write.csv(out_mc$Y.bar, 'results/synthetic_control_objects/out_mc_ndvi_10y_pt/out_mc_ndvi_Y_bar.csv')
write.csv(out_mc$Y.tr.cnt, 'results/synthetic_control_objects/out_mc_ndvi_10y_pt/out_mc_ndvi_Y_tr_cnt.csv')
write.csv(out_mc$Y.ct.cnt, 'results/synthetic_control_objects/out_mc_ndvi_10y_pt/out_mc_ndvi_Y_ct_cnt.csv')

min.T0 <- 26 * 5

system.time(out_mc <- gsynth(f, 
                             data = final_df,
                             index = index_variables,
                             force = "two-way", 
                             r = 2,
                             cov.ar = 1,
                             se = TRUE, 
                             CV=1, 
                             lambda = .01,
                             estimator='mc',
                             nboots = 100, 
                             parallel = TRUE, 
                             seed=12345,
                             min.T0=min.T0))

dir.create('results/synthetic_control_objects/out_mc_ndvi_5y_pt/')
saveRDS(out_mc, 'results/synthetic_control_objects/out_mc_ndvi_5y_pt/out_mc.R')
write.csv(out_mc$Y.tr, 'results/synthetic_control_objects/out_mc_ndvi_5y_pt/out_mc_ndvi_Y_tr.csv')
write.csv(out_mc$Y.ct, 'results/synthetic_control_objects/out_mc_ndvi_5y_pt/out_mc_ndvi_Y_ct.csv')
write.csv(out_mc$Y.co, 'results/synthetic_control_objects/out_mc_ndvi_5y_pt/out_mc_ndvi_Y_co.csv')
write.csv(out_mc$Y.bar, 'results/synthetic_control_objects/out_mc_ndvi_5y_pt/out_mc_ndvi_Y_bar.csv')
write.csv(out_mc$Y.tr.cnt, 'results/synthetic_control_objects/out_mc_ndvi_5y_pt/out_mc_ndvi_Y_tr_cnt.csv')
write.csv(out_mc$Y.ct.cnt, 'results/synthetic_control_objects/out_mc_ndvi_5y_pt/out_mc_ndvi_Y_ct_cnt.csv')

min.T0 <- 26 * 5


system.time(out_gsynth <- gsynth(f, 
                             data = final_df,
                             index = index_variables,
                             force = "two-way", 
                             r = c(0,5),
                             cov.ar = 1,
                             se = TRUE, 
                             inference='parametric',
                             CV=TRUE, 
                             lambda = .01,
                             nboots = 100, 
                             seed=12345,
                             parallel = TRUE, 
                             min.T0=min.T0))

dir.create('results/synthetic_control_objects/out_gsynth_ndvi/')
saveRDS(out_gsynth, 'synthetic_control_objects/out_gsynth_ndvi/out_gsynth.R')
write.csv(out_gsynth$Y.tr, 'results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_tr.csv')
write.csv(out_gsynth$Y.ct, 'results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_ct.csv')
write.csv(out_gsynth$Y.co, 'results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_co.csv')
write.csv(out_gsynth$Y.bar, 'results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_bar.csv')
write.csv(out_gsynth$Y.tr.cnt, 'results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_tr_cnt.csv')
write.csv(out_gsynth$Y.ct.cnt, 'results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_ct_cnt.csv')


system.time(out_gsynth_normalized <- gsynth(f, 
                                 data = final_df,
                                 index = index_variables,
                                 force = "two-way", 
                                 r = c(0,5),
                                 cov.ar = 1,
                                 se = TRUE, 
                                 inference='parametric',
                                 CV=TRUE, 
                                 lambda = .01,
                                 nboots = 100, 
                                 seed=12345,
                                 parallel = TRUE, 
                                 min.T0=min.T0,
                                 normalize=TRUE))


write.csv(out_gsynth_normalized$Y.tr, '../../results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_tr.csv')
write.csv(out_gsynth_normalized$Y.ct, '../../results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_ct.csv')
write.csv(out_gsynth_normalized$Y.co, '../../results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_co.csv')
write.csv(out_gsynth_normalized$Y.bar, '../../results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_bar.csv')
write.csv(out_gsynth_normalized$Y.tr.cnt, '../../results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_tr_cnt.csv')
write.csv(out_gsynth_normalized$Y.ct.cnt, '../../results/synthetic_control_objects/out_gsynth_ndvi/out_gsynth_ndvi_Y_ct_cnt.csv')



##############################
##############################
