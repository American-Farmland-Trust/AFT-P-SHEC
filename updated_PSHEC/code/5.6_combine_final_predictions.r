# combine final predictions and prediction intervals

library(dplyr)
library(tidyverse)
library(tigris)

data(fips_codes) #ML: this is a dataset from the package tigris
fips_codes 
fips_codes <- fips_codes %>%
  mutate(GEOID = paste(state_code,county_code, sep = "")) %>%
  mutate(county = str_remove(county, " County")) %>%
  rename(state_alpha = state) %>%
  dplyr::select(state_alpha, GEOID, county)
  

# combine predictions and intervals

combine_function <- function(crop) {
  
  # read merged data to extract county SOM mean
  merged_data <- readRDS(paste0('/home/meng/Documents/updated_PSHEC/merged_data/',crop,'_all_data_soil_weather_N_n15_om10_AFT.rds'))
  
  merged_data <- merged_data %>%
    dplyr::select(crop, GEOID, state_alpha, ssurgo_om_mean) %>%
    rename(ssurgo_om_county_mean = ssurgo_om_mean) %>%
    unique()
  
  # read predictions
  predictions <- read.csv(file = paste0('/home/meng/Documents/updated_PSHEC/results/',crop,'_all_accumulated_local_pred_county.csv'), row.names = 'X')
  predictions$GEOID <- formatC(predictions$GEOID, width = 5, format = 'd' ,flag = '0')
  predictions <- predictions %>%
    mutate(ssurgo_om_mean = round(ssurgo_om_mean, 2))
  
  # read prediction intervals
  pred_int <- readRDS(file = paste0('/home/meng/Documents/updated_PSHEC/results/prediction_interval/',crop,'_accumulated_local_pred_county_pred_interval.rds'))
  pred_int <- pred_int %>%
    mutate(ssurgo_om_mean = round(ssurgo_om_mean, 2))
  
  # combine datasets
  
  results <- predictions %>%
    left_join(pred_int, by = c('GEOID','ssurgo_om_mean')) %>%
    left_join(merged_data, by = 'GEOID') %>%
    left_join(fips_codes, by = c('state_alpha', 'GEOID')) %>%
    rename(county_name = county) %>%
    dplyr::select(state_alpha, GEOID, ssurgo_om_mean, ssurgo_om_county_mean, pred_yield, pred_lower, pred_upper,county_name)
  
  saveRDS(results, file = paste0('/home/meng/Documents/updated_PSHEC/results/yield_predictions/',crop,'_Pred_Normal.rds'))

  }

# Define the crop list

crop_list <- c('SUGARBEETS','PEANUTS','SORGHUM','TOBACCO','OATS','BARLEY','COTTON','WHEAT','SOYBEANS','CORN')

# Loop through each crop and each management type
lapply(crop_list, combine_function)
