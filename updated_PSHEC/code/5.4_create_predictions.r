# This file is to plot the accumulated  and extract the relationship 

library(DALEXtra)
library(dplyr)
library(ggplot2)
library(rsample)
library(tidymodels)
library(finetune)
library(doParallel) # set up parallel backend
library(foreach)

# combine all models for each crop
result_function <- function(crop, management) {

  #' 
  #' # Read in data
  #' 
  ## ----data------------------------------------------------------------------------------------------------------------------------------------------
  
  data <- readRDS(paste0('~/Documents/updated_PSHEC/merged_data/',crop,'_all_data_soil_weather_N_n15_om10_AFT.rds'))
    
  if (management %in% c('irrigated', 'rainfed')) {
      data <- data %>%
        dplyr::filter(irrigated == management)
    } 
   
  # print the progress
  cat("Start the workflow for:", paste0(management, "_", crop), "\n")
  
  #' 
  #' # Start the tidymodels workflow
  #' 
  #' ## 1. Split the data using the 80:20 for training-to-test and resample the training set using five repeats of 10-fold cross-validation
  ## ----splitdata-------------------------------------------------------------------------------------------------------------------------------------
  
  # Splitting the data into 80% training and 20% testing
  # Using strata to make sure the resamples have equivalent proportions as the original data between rainfed and irrigated if apporporate
  set.seed(1501)  # Set a seed for reproducibility
  corn_split <- initial_split(data, prop = 0.8) # strata = irrigated
  
  # Extract the training and testing sets
  corn_train <- training(corn_split)
  corn_test  <- testing(corn_split)
  
  # Resample the training set using five repeats of 10-fold cross-validation
  set.seed(1502)
  corn_folds <- 
    vfold_cv(corn_train, repeats = 5) #, strata = irrigated
  
  # define the input directory for the fit objective
  input_dir <- paste0('/home/meng/Documents/updated_PSHEC/models/',crop,'_model/')
  
  final_rf_fit <- readRDS(file = paste0(input_dir,management,'_',crop,'_model_selection_fit_objective.rds'))
  
  #' 
  #' # Explain the final model using Partial and Accumulated local profile: 
  #' Partial dependence profiles show how the expected value of a model prediction, like the predicted yield, changes as a function of om. See details here: https://ema.drwhy.ai/partialDependenceProfiles.html
  #' Accumulated local profile is better than Pdp if there are correlations in variables: https://ema.drwhy.ai/accumulatedLocalProfiles.html

  # explainer of the model
  explainer_rf <- 
    explain_tidymodels(
      final_rf_fit, 
      data = data, 
      y = data$Yield_decomp_add,
      verbose = FALSE
    )
  
  # extract mean SOM of each county 
  som_county <- data %>%
    dplyr::select(state_alpha, GEOID, ssurgo_om_mean) %>%
    unique()

  ## ----predition-------------------------------------------------------------------------------------------------------------------------------------
  
  # Define a global grid for ssurgo_om_mean based on the entire dataset
  global_grid <- seq(round(min(data$ssurgo_om_mean, na.rm = TRUE), digits =2),
                     round(max(data$ssurgo_om_mean, na.rm = TRUE), digits =2),
                     0.01)
  
  # Compute the ALE profile using the common grid
  state_profile <- model_profile(explainer_rf,
                           N = NULL,
                           groups = 'state_alpha',
                           variables = "ssurgo_om_mean",
                           type = "accumulated",
                           variable_splits = list(ssurgo_om_mean = global_grid)
                           #grid_points = 100,
                           #variable_splits_type = 'uniform' #get uniform grid of points
  )
  
  #------- process state-specific ALE -----
  
  # Get the unique states and counties
  states <- unique(data$state_alpha)
  
  # Initialize a list to store ALE data frames for each state
  state_profiles_list <- list()
  
  # Loop over each state to compute and adjust ALE curves
  for (st in states) {
    # Subset the data for the current state
    data_state <- data %>% filter(state_alpha == st)
    
    # Extract the ALE curve data (the structure might vary; here we assume agr_profiles)
    ale_df <- state_profile$agr_profiles
    
    min_grid_val<-min(abs(ale_df$`_x_`))
    
    # Compute a state-specific baseline:
    # Replace ssurgo_om_mean in the state data with the lowest value from global_grid,
    # then get the average prediction from the model
    data_state_baseline <- data_state %>% mutate(ssurgo_om_mean = min_grid_val)
    baseline_pred <- mean(predict(explainer_rf$model, data_state_baseline)$.pred)
    
    # Find the ALE value at the minimum grid point.
    baseline_yhat <- ale_df$`_yhat_`[which.min(abs(ale_df$`_x_` - min_grid_val))]
    
    # Un-center the ALE curve by adding the baseline prediction to the ALE values.
    # (Normally ALE curves are centered so that the value at the baseline grid point is the same for all groups)
    state_ale_df <- ale_df %>% 
      rename(state_alpha = `_groups_`,
             ssurgo_om_mean = `_x_`) %>%
      filter(state_alpha == st) %>%
      # remove the global baseline and add state specific baseline
      mutate(pred_yield = `_yhat_` - baseline_yhat + baseline_pred) %>%
      dplyr::select(state_alpha, ssurgo_om_mean, pred_yield)
      
    
    state_profiles_list[[st]] <- state_ale_df
  }
  
  # Combine all state ALE curves into one data frame
  all_profiles_state <- bind_rows(state_profiles_list) 
  
  write.csv(all_profiles_state, 
            file = paste0('/home/meng/Documents/updated_PSHEC/results/',crop,'_',management,'_accumulated_local_pred_state.csv'))
  
  # Plot the adjusted ALE curves for each state
  al_plot_state <- ggplot(all_profiles_state, 
                          aes(x = ssurgo_om_mean, y = pred_yield, color = state_alpha)) +
    geom_line() +
    labs(x = "ssurgo_om_mean", 
         y = "Model Prediction (Accumulated Local)",
         title = "State-Specific ALE Curves")
  
  al_plot_state
  
  ggsave(al_plot_state, filename = paste0('/home/meng/Documents/updated_PSHEC/results/',crop,'_',management,'_accumulated_local_pred_state_profile.png'))
  
  rm(state_profiles_list, all_profiles_state, al_plot_state)
  
  #---- county ----
  
  county_profile <- model_profile(explainer_rf,
                                  N = NULL,
                                  groups = 'GEOID',
                                  variables = "ssurgo_om_mean",
                                  type = "accumulated",
                                  variable_splits = list(ssurgo_om_mean = global_grid)
                                  #grid_points = 100,
                                  #variable_splits_type = 'uniform' #get uniform grid of points
  )
  
  
  # Initialize a list to store ALE data frames for each county
  county_profiles_list <- list()
  
  counties <- unique(data$GEOID)
  # Loop over each county to compute and adjust ALE curves
  for (ct in counties) {
    # Subset the data for the current county
    data_county <- data %>% filter(GEOID == ct)
    
    # Extract the ALE curve data (the structure might vary; here we assume agr_profiles)
    ale_df <- county_profile$agr_profiles
    
    min_grid_val<-min(abs(ale_df$`_x_`))
    
    # Compute a county-specific baseline:
    # Replace ssurgo_om_mean in the county data with the lowest value from global_grid,
    # then get the average prediction from the model
    data_county_baseline <- data_county %>% mutate(ssurgo_om_mean = min_grid_val)
    baseline_pred <- mean(predict(explainer_rf$model, data_county_baseline)$.pred)
    
    # Find the ALE value at the minimum grid point.
    baseline_yhat <- ale_df$`_yhat_`[which.min(abs(ale_df$`_x_` - min_grid_val))]
    
    # Un-center the ALE curve by adding the baseline prediction to the ALE values.
    # (Normally ALE curves are centered so that the value at the baseline grid point is 0.)
    ale_df_county <- ale_df %>% 
      rename(GEOID = `_groups_`,
             ssurgo_om_mean = `_x_`) %>%
      filter(GEOID == ct) %>%
      mutate(pred_yield = `_yhat_` - baseline_yhat + baseline_pred) %>%
      dplyr::select(GEOID, ssurgo_om_mean, pred_yield)
    
    
    county_profiles_list[[ct]] <- ale_df_county
  }
  
  # Combine all county ALE curves into one data frame
  all_profiles_county <- bind_rows(county_profiles_list) 
  
  write.csv(all_profiles_county, 
            file = paste0('/home/meng/Documents/updated_PSHEC/results/',crop,'_',management,'_accumulated_local_pred_county.csv'))
  
  # Plot the adjusted ALE curves for each county
  al_plot_county <- ggplot(all_profiles_county, aes(x = ssurgo_om_mean, y = pred_yield, color = GEOID)) +
    geom_line() +
    labs(x = "ssurgo_om_mean", 
         y = "Model Prediction (Accumulated Local)",
         title = "county-Specific ALE Curves") +
    theme(legend.position = 'none')
  
  al_plot_county
  
  ggsave(al_plot_county, filename = paste0('/home/meng/Documents/updated_PSHEC/results/',crop,'_',management,'_accumulated_local_pred_county_profile.png'))
  
  rm(county_profiles_list, all_profiles_county, al_plot_county)
  
  gc()
  
  invisible(NULL)
  
}

# Define the crop list
crop_list <- c('CORN','COTTON','BARLEY','SORGHUM','WHEAT','SOYBEANS','PEANUTS',
                      'OATS','SUGARBEETS','TOBACCO')

management_list <- c('all','irrigated','rainfed')


# Loop through each crop and each management type
lapply(crop_list, function(crop) {
  lapply(management_list, function(mgmt) {
    result_function(crop, mgmt)
  })
})


# For parrallel computing:  this requires a very large RAM

# Create a data frame with all combinations
#combinations <- expand.grid(crop = crop_list, management = management_list, 
#                            stringsAsFactors = FALSE)

# Register parallel backend
#cl <- makePSOCKcluster(2)
#registerDoParallel(cl)

# loop through crop and management
#results <- foreach(i = 1:nrow(combinations), 
#                   .packages = c("dplyr", "ggplot2", "DALEXtra", "rsample", "tidymodels")) %dopar% {
#                     crop <- combinations$crop[i]
#                     management <- combinations$management[i]
                     
#                     # Print progress 
#                     cat("Processing:", management, "_", crop, "\n")
                     
#                     library(DALEXtra)
                     # Call the function 
#                     result_function(crop, management)
                     
                     # Return a message to track completion for this combo
#                     paste(management, crop, "done")
#                   }

# Stop the cluster after parallel processing
#stopCluster(cl)

# Optionally, inspect the results from each worker
#print(results)