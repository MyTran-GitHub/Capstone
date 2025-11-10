##########################################
# 04_synthetic_control_no_clustering.R
##########################################

# Load libraries
library(gsynth)
library(panelView)
library(gridExtra)
library(zoo)
library(dplyr)
library(doParallel)
library(ggpubr)

# Set working directory
setwd("/Users/macbook/Downloads/Capstone/wildfires_effects")  # change to your folder

# Source synthetic control function
source('src/synthetic_control_src.R')

# Paths
path_processed_data <- 'data/processed/'
results_path <- 'results/'
results_synthetic_objects <- 'results/synthetic_control_objects/'

# Load main data
df_final_1 <- read.csv(paste0(path_processed_data,'df_final_1.csv'))
df_ <- df_final_1[order(df_final_1$ID_new), ]

# Load donor map
donor_map <- read.csv(paste0(path_processed_data,'donor_map_embeddings.csv'))

# Covariates and outcomes
covariates <- c('vs','tmmx','pr','bi')
outcomes <- c('ndvi','ndmi','nbr')

# Treated unit IDs
treated_ids <- unique(df_$ID_new[df_$treated == 1])

# Loop over outcomes and treated units
for (outcome_ in outcomes){
  for (t_id in treated_ids){
    
    # Get donor IDs for this treated unit
    donor_ids <- donor_map %>% 
      filter(treated_id == t_id) %>% 
      pull(control_id)
    
    # Filter df_ to only include treated + donor units
    df_subset <- df_ %>% 
      filter(ID_new %in% c(t_id, donor_ids))
    
    print(paste0("Running synthetic control for treated unit: ", t_id, 
                 ", outcome: ", outcome_, 
                 ", donors: ", length(donor_ids)))
    
    # Run synthetic control
    out <- synthetic_control_unique(
      outcome = outcome_,
      df = df_subset,
      stratify_group = NULL,
      results_path = results_path,
      control_group = 1,  # treated==1
      covariates = covariates
    )
    
    # Save results per treated unit
    saveRDS(out, file=paste0(results_synthetic_objects, 
                             "gsynth_counterfactual_unit_", t_id, "_", outcome_, ".RData"))
  }
}

print("All done!")
