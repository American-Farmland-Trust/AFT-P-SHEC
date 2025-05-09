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
  merged_data <- readRDS(paste0('data/merged_data/',crop,'_all_data_soil_weather_N_n15_om10_AFT.rds'))
  
  merged_data <- merged_data %>%
    dplyr::select(crop, GEOID, state_alpha, ssurgo_om_mean) %>%
    rename(ssurgo_om_county_mean = ssurgo_om_mean) %>%
    unique()
  
  # read predictions
  predictions <- read.csv(file = paste0('models/yield_predictions_ALE/',crop,'_all_accumulated_local_pred_county.csv'), row.names = 'X')
  predictions$GEOID <- formatC(predictions$GEOID, width = 5, format = 'd' ,flag = '0')
  predictions <- predictions %>%
    mutate(ssurgo_om_mean = round(ssurgo_om_mean, 2))
  
  # read prediction intervals
  pred_int <- readRDS(file = paste0('models/yield_prediction_intervals/',crop,'_accumulated_local_pred_county_pred_interval.rds'))
  pred_int <- pred_int %>%
    mutate(ssurgo_om_mean = round(ssurgo_om_mean, 2))
  
  # combine datasets
  
  results <- predictions %>%
    left_join(pred_int, by = c('GEOID','ssurgo_om_mean')) %>%
    left_join(merged_data, by = 'GEOID') %>%
    left_join(fips_codes, by = c('state_alpha', 'GEOID')) %>%
    rename(county_name = county) %>%
    dplyr::select(state_alpha, GEOID, ssurgo_om_mean, ssurgo_om_county_mean, pred_yield, pred_lower, pred_upper,county_name)
  
  
  #Original units reported
  #Corn	bu/acre
  #Soybean	bu/acre
  #Wheat	bu/acre
  #Barley	bu/acre
  #Oats	bu/acre
  #Sorghum	bu/acre
  #Cotton lb/acre
  #Peanuts lb/acre
  #Tobacco lb/acre
  #Sugarbeets ton/acre
  
  # since 0.0628 was mistakenly used for all crops in the NASS process step to convert units to Mg/ha (this is good for corn and sorghum,
  # but need to be changed for other crops as show below), 
  # I used two steps to get the correct numbers:
  #Step 1: Undo the incorrect conversion using pred_yield / 0.0628
  #Step 2: Recalculate Mg/ha using the correct crop-specific factor: (pred_yield/0.0628)*crop specific transition factor 
  
  #Crop specific factor
  
  #Corn:	no change
  #Sorghum:	no change
  #Soybean:	0.0673
  #Wheat:	0.0673
  #Barley: 0.0538
  #Oats: 0.0359
  #Cotton: 0.00112085
  #Peanuts: 0.00112085
  #Tobacco: 0.00112085
  #Sugarbeets: 2.242

  # pred_yield_Mg_ha are converted to Mg/ha for all crops using the right conversion factor,
  # pred_yield_lb_acre was used for cotton and peanuts
  # pred_yield_ton_acre was used for sugar beets
  # pred_yield_bu_acre was used for the rest of crops
  
  if (crop %in% c('COTTON','PEANUTS','TOBACCO')) {
    results <- results %>%
      mutate(pred_yield_lb_acre = pred_yield / 0.0628,
             pred_lower_lb_acre = pred_lower / 0.0628,
             pred_upper_lb_acre = pred_upper / 0.0628,
             pred_yield_Mg_ha = (pred_yield / 0.0628)*0.00112085,
             pred_lower_Mg_ha = (pred_lower / 0.0628)*0.00112085,
             pred_upper_Mg_ha = (pred_upper / 0.0628)*0.00112085)
  } else if ( crop %in% c('CORN', 'SORGHUM')) {    
    results <- results %>%
      mutate(pred_yield_bu_acre = pred_yield / 0.0628,
             pred_lower_bu_acre = pred_lower / 0.0628,
             pred_upper_bu_acre = pred_upper / 0.0628,
             pred_yield_Mg_ha = pred_yield, # no change
             pred_lower_Mg_ha = pred_lower,
             pred_upper_Mg_ha = pred_upper)
  } else if (crop %in% c('SOYBEANS', 'WHEAT')) {
    results <- results %>%
      mutate(pred_yield_bu_acre = pred_yield / 0.0628,
             pred_lower_bu_acre = pred_lower / 0.0628,
             pred_upper_bu_acre = pred_upper / 0.0628,
             pred_yield_Mg_ha = (pred_yield / 0.0628)*0.0673,
             pred_lower_Mg_ha = (pred_lower / 0.0628)*0.0673,
             pred_upper_Mg_ha = (pred_upper / 0.0628)*0.0673)
  } else if (crop == 'BARLEY') {
    results <- results %>%
      mutate(pred_yield_bu_acre = pred_yield / 0.0628,
             pred_lower_bu_acre = pred_lower / 0.0628,
             pred_upper_bu_acre = pred_upper / 0.0628,
             pred_yield_Mg_ha = (pred_yield / 0.0628)*0.0538,
             pred_lower_Mg_ha = (pred_lower / 0.0628)*0.0538,
             pred_upper_Mg_ha = (pred_upper / 0.0628)*0.0538)
  } else if (crop == 'OATS') {
    results <- results %>%
      mutate(pred_yield_bu_acre = pred_yield / 0.0628,
             pred_lower_bu_acre = pred_lower / 0.0628,
             pred_upper_bu_acre = pred_upper / 0.0628,
             pred_yield_Mg_ha = (pred_yield / 0.0628)*0.0359,
             pred_lower_Mg_ha = (pred_lower / 0.0628)*0.0359,
             pred_upper_Mg_ha = (pred_upper / 0.0628)*0.0359)
  } else if (crop == 'SUGARBEETS') {
    results <- results %>%
      mutate(pred_yield_ton_acre = pred_yield / 0.0628,
             pred_lower_ton_acre = pred_lower / 0.0628,
             pred_upper_ton_acre = pred_upper / 0.0628,
             pred_yield_Mg_ha = (pred_yield / 0.0628)*2.242,
             pred_lower_Mg_ha = (pred_lower / 0.0628)*2.242,
             pred_upper_Mg_ha = (pred_upper / 0.0628)*2.242)
  }

  results <- results %>%
    dplyr::select(-pred_yield, -pred_lower, -pred_upper)
  
  saveRDS(results, file = paste0('results/yield_predictions/',crop,'_Pred_Normal.rds'))

  }

# Define the crop list

crop_list <- c('SUGARBEETS','PEANUTS','SORGHUM','TOBACCO','OATS','BARLEY','COTTON','WHEAT','SOYBEANS','CORN')

# Loop through each crop and each management type
lapply(crop_list, combine_function)
