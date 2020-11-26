
################################
# Load libraries, ensure that all that installed properly, if not installed use install.packages('nameofpackage')
library(gsynth)
library(panelView)
library(gridExtra)
library(zoo)
require(dplyr)
library(doParallel)
library(powerlmm)
library(ggpubr)
# devtools::install_github('xuyiqing/gsynth') -> For symmetric error estimates you need the v.1.1.7 from Github

# Set Working Directory & relative paths to data and results folders
setwd()
source('src/synthetic_control_src.R')
path_processed_data_folder <- 'data/processed/'
results_path <- 'results/'
results_synthetic_objects_folder <- 'results/synthetic_control_objects/'

make_plots <- FALSE
dir.create(results_synthetic_objects_folder)
####################################
####################################

# Control 1 and 2 without stratifying
####################################

outcomes <- c('ndvi', 'ndmi', 'nbr')
groups_control <- 1:2
list_atts_all <- c()
count <- 1
for (group_ in groups_control){
  name_df_csv <- paste0('df_final_', group_, '.csv', sep='')
  df_ <- read.csv(paste(path_processed_data_folder,name_df_csv, sep=''))
  df_ <- df_[order(df_$ID_new),]
  dim(df_)
  for (outcome_ in outcomes){
    
    print(paste0('Starting synthetic control number ', count, '...', sep=''))
    print(paste0('Control: ', group_, ' Variable: ', outcome_, sep='' ))
    out <- synthetic_control_unique(outcome=outcome_,
                                    df=df_,
                                    stratify_group=NULL,
                                    results_path=results_path,
                                    control_group=group_, 
                                    covariates=c('vs', 'tmmx', 'pr', 'bi'))  
    list_atts_all[[paste0(outcome_, '_', group_, sep='')]] <- out
    if (make_plots == TRUE){
      plot(out, theme.bw=TRUE, main=paste0('Estimated ATT on Control ', 
                                         group_, ' on variable ', outcome_, sep=''))
    }
    print(paste0('Count of ATT computed: ', count,
                 ' out of ', length(outcomes)*length(groups_control)))
    count <- count + 1
    print(paste0('XXXXXXXXXXXXXXXX'))
    print(paste0('XXXXXXXXXXXXXXXX'))
    print(paste0('XXXXXXXXXXXXXXXX'))
  }
}

saveRDS(list_atts_all, file=paste(path_processed_data_folder,'synthetic_control_objects/gsynth_counterfactuals_all.RData', sep=''))

####################################
####################################

# Controls 1 stratified
###########################

df_final_1 <- read.csv(paste(path_processed_data_folder,'df_final_1.csv', sep=''))
df_ <- df_final_1
df_ <- df_[order(df_$ID_new),]
dim(df_)
summary(df_)
print(paste0('Counterfactual 1 regions are starting...'))
print(paste0('Group 1 of stratification has:'))
dim(df_[df_$stratified_group_by_probability == 0, ])

print(paste0('Group 2 of stratification has:'))
dim(df_[df_$stratified_group_by_probability == 1, ])

print(paste0('Group 3 of stratification has:'))
dim(df_[df_$stratified_group_by_probability == 2, ])

print(paste0('Group 4 of stratification has:'))
dim(df_[df_$stratified_group_by_probability == 3, ])

outcomes <- c('ndvi', 'ndmi', 'nbr')
groups_stratify <- 0:3
list_atts_counterfactual_1 <- c()
count <- 1
time_taken <- system.time(for (outcome_ in outcomes){
  for (group_ in groups_stratify){
    print(paste0('Number of unique observations in dataframe: ', 
                 length(unique(df_[df_$stratified_group_by_probability == group_, ]$ID_new))))
    out <- synthetic_control_unique(outcome=outcome_,
                                    df=df_,
                                    stratify_group=group_,
                                    results_path=results_path,
                                    control_group=group_control,
                                    covariates=c('vs', 'tmmx', 'pr'))  
    list_atts_counterfactual_1[[paste0(outcome_, '_', group_, sep='')]] <- out
    if (make_plots == TRUE){
      plot(out, theme.bw=TRUE, main=paste0('Estimated ATT on counterfactual 1 stratified ', group_, ' on variable ', outcome_, sep=''))
    }    
    print(paste0('Count of ATT computed: ', count,
                 ' out of ', length(outcomes)*length(groups_stratify), '. Counterfactuals 1.', sep=''))
    count <- count + 1
  }
})

saveRDS(list_atts_counterfactual_1, file=paste(path_processed_data_folder,'synthetic_control_objects/gsynth_counterfactual_1_stratified.RData', sep=''))

###########################
###########################

# Controls 2 stratified
###########################

df_final_2 <- read.csv(paste(path_processed_data_folder,'df_final_2.csv', sep=''))
dim(df_final_1) == dim(df_final_2)

df_ <- df_final_2 
df_ <- df_[order(df_$ID_new),]

print(pate0('Counterfactual 2 regions are starting...'))
print(paste0('Group 1 of stratification has:'))
dim(df_[df_$stratified_group_by_probability == 0, ])

print(paste0('Group 2 of stratification has:'))
dim(df_[df_$stratified_group_by_probability == 1, ])

print(paste0('Group 3 of stratification has:'))
dim(df_[df_$stratified_group_by_probability == 2, ])

print(paste0('Group 4 of stratification has:'))
dim(df_[df_$stratified_group_by_probability == 3, ])

outcomes <- c('ndvi', 'ndmi', 'nbr')
groups_stratify <- 0:3
list_atts_counterfactual_2 <- c()
for (outcome_ in outcomes){
  for (group_ in groups_stratify){
    print(paste0('Number of unique observations in dataframe: ', 
                 length(unique(df_[df_$stratified_group_by_probability == group_, ]$ID_new))))
    out <- synthetic_control_unique(outcome=outcome_,
                                    df=df_, 
                                    stratify_group=group_, 
                                    results_path=results_path, 
                                    control_group=group_control,
                                    covariates=c('vs', 'tmmx', 'pr', 'bi')) 
    list_atts_counterfactual_2[[paste0(outcome_, '_', group_, sep='')]] <- out
    if (make_plots == TRUE){
      plot(out, theme.bw=TRUE, main=paste0('Estimated ATT on counterfactual 2 stratified ', group_, ' on variable ', outcome_, sep=''))
    }    
    print(paste0('Count of ATT computed: ', count,
                 ' out of ', length(outcomes)*length(groups_stratify), '. Counterfactuals 2.', sep=''))
    count <- count + 1
  }
}

saveRDS(list_atts_counterfactual_2, file=paste(path_processed_data_folder,'synthetic_control_objects/gsynth_counterfactual_2_stratified.RData', sep=''))

####################################
####################################

# Counterfactual one, detecting different changes over decadal periods
####################################

df_final_1 <- read.csv(paste(path_processed_data_folder,'df_final_1.csv', sep=''))
data <- df_final_1

dim(data)
summary(data)
outcomes <- c('ndvi', 'ndmi', 'nbr')
list_atts_counterfactual_time <- c()
count <- 1
for (group_ in 1:2){
  df_final_ <- read.csv(paste(path_processed_data_folder,'df_final_', group_, '.csv', sep=''))
  data <- df_final_
  for (outcome_ in outcomes){
    count_times <- 1
    df_ <- data[which((data$time > 0) & data$time < 380), ]
    print(paste0('Number of unique observations in dataframe: ', 
                 length(unique(df_))))
    out <- synthetic_control_unique(outcome=outcome_,
                                    df=df_,
                                    stratify_group=NULL, 
                                    results_path=results_path,
                                    control_group=group_, 
                                    covariates=c('vs', 'tmmx', 'pr', 'bi'), 
                                    min.T0 = 72)  
    list_atts_counterfactual_time[[paste0(outcome_, '_', group_, '_<380', sep='')]] <- out
    df_ <- data[which(data$time > 389), ]
    out_2 <- synthetic_control_unique(outcome=outcome_,
                                    df=df_,
                                    stratify_group=NULL, 
                                    results_path=results_path,
                                    control_group=group_, 
                                    covariates=c('vs', 'tmmx', 'pr', 'bi'), 
                                    min.T0 = 72)  
    
    list_atts_counterfactual_time[[paste0(outcome_, '_', group_, '_>379', sep='')]] <- out_2
    if (make_plots == TRUE){
      plt_1 <- plot(out, theme.bw=TRUE, main=paste0('Estimated ATT on group', group_, 'pre 2002 on variable ', outcome_, sep=''))
      plt_2 <- plot(out_2, theme.bw=TRUE, main=paste0('Estimated ATT on group', group_, 'post 2002 on variable ', outcome_, sep=''))
      arngd_plot <- ggarrange(plot(out, theme.bw=TRUE, main=paste0('Estimated ATT on group', group_, 'pre 2002 on variable ', outcome_, sep='')),
                              plot(out_2, theme.bw=TRUE, main=paste0('Estimated ATT on group', group_, 'post 2002 on variable ', outcome_, sep='')), nrow = 2, ncol = 1)
    }
    print(paste0('Count of ATT computed: ', count,
                 ' out of ', length(outcomes)*length(3), ' counterfactuals .', sep=''))
    count <- count + 1
    count_times <- count_times + 1
    }
  }
saveRDS(list_atts_counterfactual_time, file=paste(path_processed_data_folder,'synthetic_control_objects/gsynth_counterfactual_changes_time.RData', sep=''))
