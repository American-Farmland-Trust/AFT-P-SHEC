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
  
  data <- readRDS(paste0('updated_PSHEC/data/merged_data/',crop,'_all_data_soil_weather_N_n15_om10_AFT.rds'))
  
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
  all_profile <- model_profile(explainer_rf,
                                 N = NULL,
                                 variables = "ssurgo_om_mean",
                                 type = "accumulated",
                                 variable_splits = list(ssurgo_om_mean = global_grid)
                                 #grid_points = 100,
                                 #variable_splits_type = 'uniform' #get uniform grid of points
  )
  
  #------- process crop-specific ALE -----
    
  # Extract the ALE curve data 
  ale_df <- all_profile$agr_profiles %>%
      rename(ssurgo_om_mean = `_x_`) %>%
      mutate(pred_yield = `_yhat_` ) %>%
      dplyr::select(ssurgo_om_mean, pred_yield) %>%
      mutate(crop_name = crop)
  
  
  # Plot the adjusted ALE curves for each state
  ale_plot <- ggplot(ale_df, aes(x = ssurgo_om_mean, y = pred_yield)) +
    geom_line() +
    labs(x = "ssurgo_om_mean", 
         y = "Model Prediction (Accumulated Local)",
         title = paste0(crop, " ALE Curve"))
  
  ale_plot
  
  # save plot and file
  write.csv(ale_df, 
            file = paste0('result_analyses/intermediate_results/average_yield_ale/',crop,'_',management,'_accumulated_local_pred_national.csv'))
  
  
  ggsave(ale_plot, filename = paste0('result_analyses/intermediate_results/average_yield_ale/',crop,'_',management,'_accumulated_local_pred_national_profile.png'))
  
  rm(all_profile, ale_df, ale_plot)
  
  gc()
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