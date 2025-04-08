# After checking drought model metrics results 

# The combined model containing these crops works the best for more than half of crops:
# Sugarbeets and tobacco have less than 200 observations, so they are not included in the model
# c("CORN","COTTON","BARLEY","SORGHUM","WHEAT","SOYBEANS","PEANUTS","OATS")

# Filter LAR > 0.2, DSCI > 400

# generate predicitons under drought 

#' 
#' # Read in data
#' 
## ----data------------------------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(rsample)
library(finetune) # model tuning capabilities in the tidymodels framework.
library(DALEXtra)
library(tidymodels)

# read and combine data for crops

input_dir <- 'data/merged_drought_data/'

crop_name <- c("CORN","COTTON","BARLEY","SORGHUM","WHEAT","SOYBEANS","PEANUTS","OATS")

data_list<- lapply(crop_name, function(crop) {
  
  df <- readRDS(paste0(input_dir,'LAR_0.2/',crop,'_LAR_0.2_DSCI_400_merged_drought_data.rds'))
  
  # conditionally change column names so that all crops have the same column names
  
  if (crop %in% c('OATS','SUGARBEETS','TOBACCO')) {
    df <- df %>%
      dplyr::rename(Ninput_kgN.ha.yr_mean = FarmLbsN_perAc_mean)
  } else if (crop %in% c('SOYBEANS','PEANUTS')) {
    df <- df %>%
      dplyr::mutate(Ninput_kgN.ha.yr_mean = 0)
  }
  
  return(df)
  
})

data <- dplyr::bind_rows(data_list)

# convert state_alpha from characters to factors 
data$state_alpha <- as.factor(data$state_alpha)
data$crop <- as.factor(data$crop)

# Reorder drainage levels as this is an ordinal variable
# the order of drainage class is as follows
data <- data %>%
  mutate(ssurgo_drainage_class_mode = factor(ssurgo_drainage_class_mode, levels = c("Excessively drained", "Somewhat excessively drained",
                                                                                    "Well drained", "Moderately well drained",
                                                                                    "Somewhat poorly drained", "Poorly drained",
                                                                                    "Very poorly drained")))

#saveRDS(data, file = 'data/intermediate_data/drought_merged_data.rds')

#' 
#' # Start the tidymodels workflow
#' 
#' ## 1. Split the data using the 80:20 for training-to-test and resample the training set using five repeats of 10-fold cross-validation
## ----splitdata-------------------------------------------------------------------------------------------------------------------------------------

# Splitting the data into 80% training and 20% testing
# Using strata to make sure the resamples have equivalent proportions as the original data among crops
set.seed(1501)  # Set a seed for reproducibility

if(length(unique(data$crop)) > 1) {
  corn_split <- initial_split(data, prop = 0.8, strata = crop)
} else {
  corn_split <- initial_split(data, prop = 0.8)
}

# Extract the training and testing sets
corn_train <- training(corn_split)
corn_test  <- testing(corn_split)

# Resample the training set using five repeats of 10-fold cross-validation
set.seed(1502)

if(length(unique(corn_train$crop)) > 1) {
  corn_folds <- vfold_cv(corn_train, repeats = 5, strata = crop)
} else {
  corn_folds <- vfold_cv(corn_train, repeats = 5)
}

# Read the final fit objective
final_rf_fit <- readRDS(file = 'models/drought_models/COR_COT_BAR_SOR_WHE_SOY_PEA_OAT_LAR_0.2_model/COR_COT_BAR_SOR_WHE_SOY_PEA_OAT_LAR_0.2_model_selection_fit_objective.rds')

#' 
#' # Explain the final model using Accumulated local profile: 
#' Partial dependence profiles show how the expected value of a model prediction, like the predicted yield, changes as a function of om. See details here: https://ema.drwhy.ai/partialDependenceProfiles.html
#' Accumulated local profile is better than Pdp if there are correlations in variables: https://ema.drwhy.ai/accumulatedLocalProfiles.html

# Define a global grid for ssurgo_om_mean based on the entire dataset
global_grid <- seq(round(min(data$ssurgo_om_mean, na.rm = TRUE), digits =2),
                   round(max(data$ssurgo_om_mean, na.rm = TRUE), digits =2),
                   0.01)

# Compute predictions under DSCI = 300
data_DSCI_300 <- data %>% 
  mutate(DSCI.mean = 300) %>%
  unique()

# explainer of the model
explainer_rf <- 
  explain_tidymodels(
    final_rf_fit, 
    data = data_DSCI_300, 
    y = data_DSCI_300$Yield_change,
    verbose = FALSE
  )

explainer_rf$data <- explainer_rf$data %>%
  mutate(group_var = paste(GEOID, crop, sep = "_"))

explainer_data <- explainer_rf$data %>%
  mutate(group_var = paste(GEOID, crop, sep = "_"))

county_profile <- model_profile(explainer_rf,
                                N = NULL,
                                groups = "group_var",
                                variables = "ssurgo_om_mean",
                                type = "accumulated",
                                variable_splits = list(ssurgo_om_mean = global_grid))

# Initialize a list to store ALE data frames for each county
county_profiles_list <- list()

counties <- unique(explainer_data$group_var)
# Loop over each county to compute and adjust ALE curves
for (ct in counties) {
  # Subset the data for the current county
  data_county <- explainer_data %>% filter(group_var == ct)
  
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
    rename(group_var = `_groups_`,
           ssurgo_om_mean = `_x_`) %>%
    filter(group_var == ct) %>%
    mutate(pred_yield = `_yhat_` - baseline_yhat + baseline_pred) %>%
    dplyr::select(group_var,ssurgo_om_mean, pred_yield)
  
  
  county_profiles_list[[ct]] <- ale_df_county
}

# Combine all county ALE curves into one data frame
all_profiles_county <- bind_rows(county_profiles_list) 

meta_data<- data %>%
  dplyr::select(crop,GEOID,state_alpha,ssurgo_om_mean) %>%
  rename(ssurgo_om_county_mean = ssurgo_om_mean) %>%
  unique()

all_profiles_county.1 <- all_profiles_county %>%
  rename(pred_yield_change_percentage = pred_yield) %>%
  separate(group_var, into = c('GEOID','crop'), sep = '_') %>%
  left_join(meta_data, by = c('GEOID','crop'))

# save results to different crops
crop_name <- c("CORN","COTTON","BARLEY","SORGHUM","WHEAT","SOYBEANS","PEANUTS","OATS")

write_results <- function(i) {
  
  crop <- all_profiles_county.1 %>%
    filter(crop == i) %>%
    droplevels()
  
  saveRDS(crop, file = paste0("results/drought_predictions/",i,"_yield_change_predictions_drought_DSCI300.rds"))

  # Plot the adjusted ALE curves for each county if needed
  #al_plot_county <- ggplot(crop, aes(x = ssurgo_om_mean, y = pred_yield_change_percentage, color = GEOID)) +
  #  geom_line() +
  #  labs(x = "ssurgo_om_mean", 
  #     y = "Yield Change Prediction (Accumulated Local)",
  #     title = paste0("county-Specific ALE Curves_",i)) +
  #  theme(legend.position = 'none')
  
  #ggsave(al_plot_county, filename = paste0("results/drought_predictions/",i,"_yield_change_predictions_drought_DSCI300_county_profile.png"))
  
  }

lapply(crop_name, write_results)


##### Extract an all-county list ####

# Define the folder containing the drought result files
folder_path <- "results/drought_predictions/"

# Get a list of all files in the folder
all_files <- list.files(path = folder_path, pattern = "*.rds", full.names = TRUE)

# Create a container to store data from each file
lst <- list()

# Loop through each file
for (file in all_files) {
  # Read the CSV file
  data <- readRDS(file)

  # Extract unique GEOID values
  crop_drought_summary <- data %>%
    dplyr::select(crop,state_alpha,GEOID) %>%
    unique()
  
  # Store the result in the list
  lst[[file]] <- crop_drought_summary
}

# Combine all the unique rows from all files
all_crop_county <- do.call(dplyr::bind_rows,lst) %>% 
  unique() 

# Transform the data into a wide format
all_crop_county_wide <- all_crop_county %>%
  # Create binary indicators for each crop
  mutate(present = 1) %>%
  pivot_wider(
    names_from = crop,      # Create one column for each crop
    values_from = present,  # Fill with the binary indicator (1 for presence)
    values_fill = 0         # Fill missing values with 0
  ) %>%
  # Create a new column with the sum of all crop columns
  mutate(crop_sum = rowSums(across(BARLEY:WHEAT))) %>%
  # Order by crop_sum (lowest to highest)
  arrange(desc(crop_sum))

write.csv(all_crop_county_wide, file = 'data/intermediate_data/all_10_crops_drought_county_list_ordered_2000_2023.csv',
          row.names = FALSE)

# combine yield and drought county list

yield_list <- read.csv('data/intermediate_data/all_10_crops_merged_data_county_list_ordered_2000_2023.csv')
# Ensure the 'GEOID' column is a string with 5 characters, left-padded with zeros
yield_list$GEOID <- formatC(yield_list$GEOID, width = 5, format = 'd' ,flag = '0')

yield_list_long <- yield_list %>%
  dplyr::select(-crop_sum) %>%
  pivot_longer(names_to = 'crop',cols = c(BARLEY:WHEAT)) %>%
  filter(value == 1) %>%
  rename(yield_prediction = value) 

# n = 5248

drought_list <- all_crop_county %>%
  mutate(drought_prediction = 1)

# n = 4066

final_county_list <- yield_list_long %>%
  left_join(drought_list, by = c('state_alpha','GEOID','crop'))

write.csv(final_county_list, file = 'data/intermediate_data/all_crops_final_county_list_2000_2023.csv', row.names = FALSE)
