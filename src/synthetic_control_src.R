

synthetic_control_unique <- function(outcome, df, stratify_group, results_path, control_group=1, covariates=NULL, min.T0=125){
  # Function to pass the given arguments and run in cluster the synthetic control
  #     in parallel. 
  # 
  #   outcome: :char: Desired variable to compute the synthetic control over.
  #   df: :data.frame: Dataframe over which to compute the synthetic control.
  #   group to statify: :int: Risk probability to stratify for. If non passed,
  #                     then all observations are used. Options: 0, 1, 2, 3.
  #   control group: :int: Integer of the "name" of the buffer. Options: 1,2,3.
  #                       If 0 passed, then no counterfactual region will be used.
  #   covariates: :list of characters: List of the names to be used as covariates in the
  #                       synthetic control.
  #
  
  index_variables <- c("ID_new", "time")
  variables <- c('treated', covariates)
  outcome <- paste('values_', outcome, sep='')
  final_df <- df_[complete.cases(df_[, c(index_variables, variables, outcome)]), 
                  c(index_variables, variables, outcome, 'stratified_group_by_probability')]
  
  if (is.null(stratify_group)){
    print(paste0('No group specified.'))
  }
  
  else {
    final_df <- final_df[final_df$stratified_group_by_probability == stratify_group,]  
  }
  if (is.null(covariates)){
    f <- as.formula(
      paste(outcome, 
            paste(variables, collapse = " + "), 
            sep = " ~ "))
  } else {
    f <- as.formula(
      paste(outcome, paste(variables, ' + ', paste(covariates, collapse=' + '), sep = ''), sep=' ~ ')
    )
  }
  print(f)

  
  system.time(out <- gsynth(f, 
                            data = final_df,
                            index = index_variables,
                            force = "two-way", 
                            r = 2,
                            se = TRUE, 
                            CV=1, 
                            lambda = .01,
                            estimator='mc',
                            nboots = 10, 
                            parallel = TRUE, 
                            min.T0=min.T0))
  
  plot(out, main=paste('ATT on ', outcome, ' using counterfactual ', stratify_group, sep=''), 
       theme.bw=TRUE, ylim = c(-0.2, 0.2), xlim=c(-200, 350))
  ## storing plots of results
  png(filename=paste(results_path, 'ATT_control_', '_
                                    stratified_', stratify_group, '_areas.png',
                     sep=''), width = 1000, height=800)
  plot(out, main=paste('ATT on ', outcome, ' using counterfactual ', stratify_group, sep=''), 
       theme.bw=TRUE, ylim = c(-0.2, 0.2), xlim=c(-200, 350))
  dev.off()
  print(out)
  print(paste0('ESTIMATED ATT on ', outcome))
  print(out$est.att)
  print(paste0('ESTIMATED AVG on ', outcome))
  print(out$est.avg)
  print(paste0('ESTIMATED BETA on ', outcome))
  print(out$est.beta)
  return(out)
}
