
library(stringr)
# produce prediction interval and/or confidence interval
# check which parsnip supported models have modules for prediction intervals or quantiles#
#envir <- parsnip::get_model_env()
#ls(envir) %>% 
#  tibble(name = .) %>% 
#  filter(str_detect(name, "_predict")) %>% 
#  mutate(prediction_modules  = map(name, parsnip::get_from_env)) %>% 
#  unnest(prediction_modules) %>% 
#  filter(str_detect(type, "pred_int|quantile"))

# since there is no built-in functions to generate predition intervals for randomforest/boosting trees, 
#I will use the 'workboots' package to generate bootstrap prediction intervals

# install from CRAN
#install.packages("workboots")

# or the development version
#devtools::install_github("markjrieke/workboots")

library(workboots)
library(DALEXtra)
library(dplyr)
library(ggplot2)
library(rsample)
library(tidymodels)
library(finetune)
library(doParallel) # set up parallel backend
library(foreach)

# combine all models for each crop
result_function <- function(crop) {
  
  #' 
  #' # Read in data
  #' 
  ## ----data------------------------------------------------------------------------------------------------------------------------------------------
  
  data <- readRDS(paste0('~/Documents/updated_PSHEC/merged_data/',crop,'_all_data_soil_weather_N_n15_om10_AFT.rds'))
  
  # print the progress
  cat("Start the workflow for:", crop, "\n")
  
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
  
  # define the input directory for the fit objective
  input_dir <- paste0('/home/meng/Documents/updated_PSHEC/models/',crop,'_model/')
  
  race_results <- readRDS(file = paste0(input_dir,'all_',crop,'_race_results.rds'))
  
  # Identify all workflow IDs in the workflow set
  wflow_ids <- race_results %>%
    distinct(wflow_id) %>%
    filter(str_detect(wflow_id,pattern = 'xgb_boost_tree')) %>%
    pull(wflow_id)
  
  best_results <- 
    race_results %>% 
    extract_workflow_set_result(wflow_ids) %>% 
    select_best(metric = "rmse")
  
  final_wf <- 
    race_results %>% 
    extract_workflow(wflow_ids) %>% 
    finalize_workflow(best_results) 
  
  rm(race_results)
  gc()
  ## ----predition-------------------------------------------------------------------------------------------------------------------------------------
  
  # Define a global grid for ssurgo_om_mean based on the entire dataset
  global_grid <- seq(round(min(data$ssurgo_om_mean, na.rm = TRUE), digits =2),
                     round(max(data$ssurgo_om_mean, na.rm = TRUE), digits =2),
                     0.01)

  
  # Define the sample sizes (N) to test in ALE curves and number of bootstrap replicates
  #N_values <- c(1000, 2000, 5000)
  n_boot <- 1000
  
  # Create a data frame with all combinations of N and bootstrap replicate number
  #combos <- expand.grid(N_val = N_values, boot_rep = 1:n_boot, stringsAsFactors = FALSE)
  
  boot_rep <- 1:n_boot
  
  cat("Start the boostrap for:", crop, "\n")
  
  # Run the bootstrap loop in parallel
  boot_results_list <-mclapply(boot_rep, function(i) {
                            # Extract current N value and bootstrap replicate number
                            #current_N <- combos$N_val[i]
                            current_boot <- boot_rep[i]
                            
                            # Create a bootstrap sample of the training data (with replacement)
                            boot_data <- corn_train[sample(nrow(corn_train), replace = TRUE), ]
                            
                            final_rf_fit <- 
                              final_wf %>% 
                              fit(boot_data)
                            
                            # NOTE: xplainer is built on the full dataset; rebuild it for boot_data.
                            all_data <- bind_rows(boot_data,corn_test)
                            
                            # explainer of the model
                            explainer_rf <- 
                              explain_tidymodels(
                                final_rf_fit, 
                                data = all_data, 
                                y = all_data$Yield_decomp_add,
                                verbose = FALSE
                              )
                          
                            # Compute the ALE profile using the current_N and common grid
                            county_profile <- model_profile(explainer_rf,
                                                     N = NULL,
                                                     groups = 'GEOID',
                                                     variables = "ssurgo_om_mean",
                                                     type = "accumulated",
                                                     variable_splits = list(ssurgo_om_mean = global_grid))
                            
                            # Initialize a list to store ALE data frames for each county
                            county_profiles_list <- list()
                            
                            counties <- unique(all_data$GEOID)
                            # Loop over each county to compute and adjust ALE curves
                            for (ct in counties) {
                              # Subset the data for the current county
                              data_county <- all_data %>% filter(GEOID == ct)
                              
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
                            # Add identifiers for the bootstrap replicate and current N
                            all_profiles_county <- bind_rows(county_profiles_list) %>%
                              mutate(bootstrap = current_boot)
                            
                            rm(county_profiles_list)
                            gc()
                            
                            return(all_profiles_county)

                          }, mc.cores = min(8, detectCores() - 2))

  # Combine all results from the bootstrap replicates into one data frame
  boot_results <- bind_rows(boot_results_list)
  
  rm(boot_results_list)
  gc()
  
  # Now aggregate the bootstrap results:
  # For each grid point (_x_) and group (_groups_) for a given N value, compute the quantiles of _yhat_
  intervals <- boot_results %>%
    group_by(ssurgo_om_mean, GEOID) %>%
    summarise(pred_lower = quantile(pred_yield, 0.05, na.rm = TRUE),
              #pred_median = quantile(pred_yield, 0.5, na.rm = TRUE),
              pred_upper = quantile(pred_yield, 0.95, na.rm = TRUE),
              .groups = "drop") 
  
  saveRDS(intervals, 
            file = paste0('/home/meng/Documents/updated_PSHEC/results/prediction_interval/',crop,'_accumulated_local_pred_county_pred_interval.rds'))
  
}

# Define the crop list

crop_list <- c('SUGARBEETS','PEANUTS','SORGHUM','TOBACCO','OATS','BARLEY','COTTON','WHEAT','SOYBEANS','CORN')

# Loop through each crop and each management type
lapply(crop_list, result_function)

