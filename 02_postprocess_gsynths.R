############################# 
# Load library
library(gsynth)


# Set working directory
setwd()
path_processed_data_folder <- 'data/processed/'
results_path <- 'results/'
results_synthetic_objects_folder <- 'results/synthetic_control_objects/'
list_atts_counterfactual_1 <- readRDS(file = paste(results_synthetic_objects_folder, 'gsynth_counterfactual_1_stratified.RData',sep=''))
list_atts_counterfactual_2 <- readRDS(file = paste(results_synthetic_objects_folder, 'gsynth_counterfactual_2_stratified.RData', sep=''))
list_atts_counterfactual_averages <- readRDS(file = paste(results_path, 'synthetic_control_objects/gsynth_counterfactuals_all.RData', sep=''))
list_atts_counterfactual_time <- readRDS(file = paste(results_path, 'synthetic_control_objects/gsynth_counterfactual_changes_time.RData',sep=''))

#list_atts_counterfactual_1_symmetric <- readRDS(file = paste(results_path, 'synthetic_control_objects/gsynth_counterfactual_1_stratified_symmetric_errors.RData', sep=''))
#list_atts_counterfactual_3 <- readRDS(file = paste(results_path, 'synthetic_control_objects/gsynth_counterfactual_3_stratified.RData', sep=''))
#list_atts_counterfactual_averages_symmetric <- readRDS(file = paste(results_synthetic_objects_folder, 'gsynth_counterfactuals_all_symmetric_errors.RData', sep=''))
#list_atts_counterfactual_time_symmetric <- readRDS(file = paste(results_synthetic_objects_folder, 'gsynth_counterfactual_changes_time_symmetric_errors.RData', sep=''))

# check lengths of lists
length(list_atts_counterfactual_1)
length(list_atts_counterfactual_2)
length(list_atts_counterfactual_averages)
length(list_atts_counterfactual_time)

# Write basic data to explore in further notebooks as time series data

dir.create(paste(results_synthetic_objects_folder,'list_atts_counterfactual_1_ndvi_0', sep=''))
write.csv(list_atts_counterfactual_1$ndvi_0$Y.tr, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_0/list_atts_counterfactual_1_ndvi_0_Y_tr.csv', sep=''))
write.csv(list_atts_counterfactual_1$ndvi_0$Y.ct, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_0/list_atts_counterfactual_1_ndvi_0_Y_ct.csv', sep=''))
write.csv(list_atts_counterfactual_1$ndvi_0$Y.co, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_0/list_atts_counterfactual_1_ndvi_0_Y_co.csv', sep=''))
write.csv(list_atts_counterfactual_1$ndvi_0$Y.bar, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_0/list_atts_counterfactual_1_ndvi_0_Y_bar.csv', sep=''))
write.csv(list_atts_counterfactual_1$ndvi_0$Y.tr.cnt, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_0/list_atts_counterfactual_1_ndvi_0_Y_tr_cnt.csv', sep=''))
write.csv(list_atts_counterfactual_1$ndvi_0$Y.ct.cnt, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_0/list_atts_counterfactual_1_ndvi_0_Y_ct_cnt.csv', sep=''))

dir.create(paste(results_synthetic_objects_folder,'list_atts_counterfactual_1_ndvi_all_units', sep=''))
write.csv(list_atts_counterfactual_averages$ndvi_1$Y.tr, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_all_units/list_atts_counterfactual_1_ndvi_0_Y_tr.csv', sep=''))
write.csv(list_atts_counterfactual_averages$ndvi_1$Y.ct, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_all_units/list_atts_counterfactual_1_ndvi_0_Y_ct.csv', sep=''))
write.csv(list_atts_counterfactual_averages$ndvi_1$Y.co, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_all_units/list_atts_counterfactual_1_ndvi_0_Y_co.csv', sep=''))
write.csv(list_atts_counterfactual_averages$ndvi_1$Y.bar, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_all_units/list_atts_counterfactual_1_ndvi_0_Y_bar.csv', sep=''))
write.csv(list_atts_counterfactual_averages$ndvi_1$Y.tr.cnt, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_all_units/list_atts_counterfactual_1_ndvi_0_Y_tr_cnt.csv', sep=''))
write.csv(list_atts_counterfactual_averages$ndvi_1$Y.ct.cnt, paste(results_synthetic_objects_folder, 'list_atts_counterfactual_1_ndvi_all_units/list_atts_counterfactual_1_ndvi_0_Y_ct_cnt.csv', sep=''))



####################################
# 

groups <- 0:3
counterfactuals <- c('1', '2', 'averages')#_symmetric','averages_symmetric')# '2', '3', 
outcomes <- c('ndvi', 'ndmi', 'nbr')
data_processed <- data.frame(1:757)
count <- 0
for (group_ in groups){
  for (outcome_ in outcomes){
    for (counter_ in counterfactuals){
      count <- count +1 
      print(paste('COUNT: ', count, '-', group_, outcome_, counter_))
      eval(parse(text=paste('data_processed$', outcome_, '_group_', group_,'_counter_', counter_, '_att <- ',
                            "list_atts_counterfactual_", counter_, "$", outcome_, '_', group_, '$est.att[, 1][1:757]',
                            sep='')))     
      eval(parse(text=paste('data_processed$', outcome_, '_group_', group_,'_counter_', counter_, '_se <- ',
                              "list_atts_counterfactual_", counter_, "$", outcome_, '_', group_, '$est.att[, 2][1:757]',
                              sep='')))     
      eval(parse(text=paste('data_processed$', outcome_, '_group_', group_,'_counter_', counter_, '_cilower <- ',
                            "list_atts_counterfactual_", counter_, "$", outcome_, '_', group_, '$est.att[, 3][1:757]',
                            sep='')))     
      eval(parse(text=paste('data_processed$', outcome_, '_group_', group_,'_counter_', counter_, '_ciupper <- ',
                            "list_atts_counterfactual_", counter_, "$", outcome_, '_', group_, '$est.att[, 4][1:757]',
                            sep='')))     
      eval(parse(text=paste('data_processed$', outcome_, '_group_', group_,'_counter_', counter_, '_pvalue <- ',
                            "list_atts_counterfactual_", counter_, "$", outcome_, '_', group_, '$est.att[, 5][1:757]',
                            sep='')))     
      eval(parse(text=paste('data_processed$', outcome_, '_group_', group_,'_counter_', counter_, '_ntreated <- ',
                            "list_atts_counterfactual_", counter_, "$", outcome_, '_', group_, '$est.att[, 6][1:757]',
                            sep='')))     
    }
  }
}
dir.create('results/gsynth_results')
write.csv(data_processed, 'results/gsynth_results/postprocessed_data_gsynth.csv')

########

groups <- 0:3
counterfactuals <- c('1', '2', 'averages')
outcomes <- c('ndvi', 'ndmi', 'nbr')
data_averages_processed <- data.frame(1:589)
for (group_ in groups){
  for (outcome_ in outcomes){
    for (counter_ in counterfactuals){
      count <- count +1 
      print(paste('COUNT: ', count, '-', group_, outcome_, counter_))
      eval(parse(text=paste('data_averages_processed$control_', outcome_, '_group_', group_,'_counter_', counter_, '_att <- ',
                            "list_atts_counterfactual_", counter_, "$", outcome_, '_', group_, '$Y.ct.cnt[1:589]',
                            sep='')))     
      eval(parse(text=paste('data_averages_processed$treated_', outcome_, '_group_', group_,'_counter_', counter_, '_att <- ',
                            "list_atts_counterfactual_", counter_, "$", outcome_, '_', group_, '$Y.tr.cnt[1:589]',
                            sep='')))     
    }
  }
}

dim(data_averages_processed)

for (outcome_ in outcomes){
  for (counter_ in counterfactuals){
    count <- count +1 
    print(paste('COUNT: ', count, '-', outcome_, counter_))
    eval(parse(text=paste('data_averages_processed$control_', outcome_, '_counter_', counter_, '_att <- ',
                          "list_atts_counterfactual_averages$", outcome_, '_', counter_, '$Y.ct.cnt[1:589]',
                          sep='')))     
    eval(parse(text=paste('data_averages_processed$treated_', outcome_, '_counter_', counter_, '_att <- ',
                          "list_atts_counterfactual_averages$", outcome_, '_', counter_, '$Y.tr.cnt[1:589]',
                          sep='')))     
  }
}

dim(data_averages_processed)
write.csv(data_averages_processed, 'results/gsynth_results/postprocessed_data_averages_gsynth.csv')

##########################
## Stratified cumulative effects

stratification_groups <- c('0', '1', '2', '3')
outcomes <- c('ndvi', 'ndmi', 'nbr')
data_cumeffs_processed <- data.frame(1:585)
counter_ <- 0
count <- 0
for (outcome_ in outcomes){
  for (strat_ in stratification_groups){
    count <- count + 1 
    print(paste('COUNT: ', count, '-', outcome_, strat_))
    eval(parse(text=paste0('data_cumeffs_processed$`', outcome_,'_', strat_, 
                           '_cum_att`<- cumuEff(list_atts_counterfactual_1$',
                           outcome_,'_', strat_, ')$est.catt[,1][1:585]',
                           sep='')))
    eval(parse(text=paste0('data_cumeffs_processed$`', outcome_,'_', strat_, 
                           '_cum_SE`<- cumuEff(list_atts_counterfactual_1$',
                           outcome_, '_',strat_, ')$est.catt[,2][1:585]',
                           sep='')))
    eval(parse(text=paste0('data_cumeffs_processed$`', outcome_, '_',strat_, 
                           '_cum_lower`<- cumuEff(list_atts_counterfactual_1$',
                           outcome_, '_',strat_, ')$est.catt[,3][1:585]',
                           sep='')))
    eval(parse(text=paste0('data_cumeffs_processed$`', outcome_,'_', strat_, 
                           '_cum_upper`<- cumuEff(list_atts_counterfactual_1$',
                           outcome_, '_',strat_, ')$est.catt[,4][1:585]',
                           sep='')))
    eval(parse(text=paste0('data_cumeffs_processed$`', outcome_, '_',strat_, 
                           '_cum_pvalue`<- cumuEff(list_atts_counterfactual_1$',
                           outcome_, '_',strat_, ')$est.catt[,5][1:585]',
                           sep='')))
  }
}
dim(data_cumeffs_processed)
write.csv(data_cumeffs_processed, 'results/gsynth_results/postprocessed_data_cumeffs_control_group_0.csv')

######################
## Comparison of two time periods <2005, >= 2005

list_times <- c('<380', '>379')
outcomes <- c('ndvi', 'ndmi', 'nbr')
data_processed_times <- data.frame(1:379)
counter_ <- 0
count <- 0
groups <- 0:3
for (time_ in list_times){
  for (outcome_ in outcomes){
      count <- count +1 
      print(paste('COUNT: ', count, '-', outcome_, counter_))
      eval(parse(text=paste('data_processed_times$`', outcome_,'_1_', time_, '_ct` <- ',
                            "list_atts_counterfactual_time$`", outcome_, '_1_', time_, '`$Y.ct.cnt[1:379]',
                            sep='')))     
      eval(parse(text=paste('data_processed_times$`', outcome_,'_1_', time_, '_tr` <- ',
                            "list_atts_counterfactual_time$`", outcome_, '_1_', time_, '`$Y.tr.cnt[1:379]',
                            sep='')))     
  }
}
dim(data_processed_times)
write.csv(data_processed_times, 'results/gsynth_results/postprocessed_data_time_gsynth_non_cumeffs.csv')


# time comparison
list_times <- c('<380', '>379')
outcomes <- c('ndvi', 'ndmi', 'nbr')
data_cumeff_times <- data.frame(1:379)
counter_ <- 0
count <- 0
for (outcome_ in outcomes){
  for (time_ in list_times){
    count <- count + 1 
    print(paste('COUNT: ', count, '-', outcome_, counter_))
    eval(parse(text=paste0('data_cumeff_times[["variable_', outcome_, '_time_', time_, '_cum_att"]] <- ',
                          'cumuEff(list_atts_counterfactual_time$`', outcome_, '_1_', time_, '`)$est.catt[,1][1:379]',
                          sep='')))
    
    eval(parse(text=paste('data_cumeff_times[["variable_', outcome_, '_time_', time_, '_cum_SE"]] <- ',
                          "cumuEff(list_atts_counterfactual_time$`", outcome_, '_1_', time_, '`)$est.catt[,2][1:379]',
                          sep='')))     
    eval(parse(text=paste('data_cumeff_times[["variable_', outcome_, '_time_', time_, '_cum_lower"]] <- ',
                          "cumuEff(list_atts_counterfactual_time$`", outcome_, '_1_', time_, '`)$est.catt[,3][1:379]',
                          sep='')))     
    eval(parse(text=paste('data_cumeff_times[["variable_', outcome_, '_time_', time_, '_cum_upper"]] <- ',
                          "cumuEff(list_atts_counterfactual_time$`", outcome_, '_1_', time_, '`)$est.catt[,4][1:379]',
                          sep='')))     
    eval(parse(text=paste('data_cumeff_times[["variable_', outcome_, '_time_', time_, '_cum_pvalue"]] <- ',
                          "cumuEff(list_atts_counterfactual_time$`", outcome_, '_1_', time_, '`)$est.catt[,5][1:379]',
                          sep='')))     
    
  }
}
dim(data_cumeff_times)
write.csv(data_cumeff_times, 'results/gsynth_results/postprocessed_data_time_gsynth.csv')



