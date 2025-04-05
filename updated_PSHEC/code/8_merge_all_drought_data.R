# This script is used to 
# combine datasets for drought analyses

# combine DSCI and RMA, yield, weather, soil data for all crops

library(dplyr)
library(tidyverse)
library(purrr)

####-------------read data --------------####
# define data extract function for separate crops
data_function <- function(crop_name, LAR_threshold) {
  
  # read RMA data
  sob_col <- readRDS(file = paste0('data/rma_data/',crop_name,'_SOB_COL_summary_stats.rds'))
  
  # read DSCI data
  DSCI <- readRDS(file = paste0('data/DSCI_data/',crop_name,'_DSCI_mean_county_by_year.rds'))
  
  # read yield data
  yield <- read.csv(file = paste0('data/nass_processed_yield/nass_processed_yield',crop_name,'_yield_2000_2023_n_15.csv'))
  yield$GEOID <- formatC(yield$GEOID, width = 5, format = 'd' ,flag = '0')
  
  # read all merged data
  merged <- readRDS(file = paste0('data/merged_data/',crop_name,'_all_data_soil_weather_N_n15_om10_AFT.rds'))
  merged_counties <- unique(merged$GEOID)
  
  ####------------combine dataset---------####
  # run the filter
  drought_LCR <- sob_col %>%
    group_by(GEOID, year,state_alpha, cause_category_drought) %>%
    reframe(sum_LCR = sum(LCR_liability), sum_LAR = sum(LAR)) %>%
    full_join(DSCI, by = c('GEOID', 'year','state_alpha')) %>%
    arrange(GEOID,year, desc(sum_LCR)) %>% 
    group_by(GEOID,year) %>%
    slice(1) %>%
    filter((cause_category_drought == 'Drought' & sum_LCR >= 0 & sum_LAR >= LAR_threshold) | DSCI.mean >= 400 )  %>%
    # define drought following the above threshold
    dplyr::select(year, crop, GEOID,state_alpha) %>%
    left_join(DSCI, by = c('crop','GEOID', 'year','state_alpha')) 
  
  drought_LCR.1 <- yield %>%
    mutate(Yield_change = Detrend_resids/Detrend_predictions * 100) %>%
    dplyr::select(year,crop,GEOID, state_alpha,Yield_change) %>%
    inner_join(merged,by = c('year','GEOID','state_alpha','crop')) %>%
    inner_join(drought_LCR, by = c('year','GEOID','state_alpha','crop')) 
    
  # check outliers: LCR
  out_LCR <- boxplot.stats(drought_LCR.1$Yield_change, coef = 2)$out
  out_ind_LCR <- which(drought_LCR.1$Yield_change %in% c(out_LCR))
  
  if (length(out_ind_LCR) > 0) {
    drought_LCR.1 <- drought_LCR.1[-out_ind_LCR,]
  } else {drought_LCR.1}  
  
  saveRDS(drought_LCR.1, file = paste0('data/merged_drought_data/LAR_',LAR_threshold,'/',crop_name,'_LAR_',LAR_threshold,'_DSCI_400_merged_drought_data.rds'))
  
  rm(sob_col,DSCI,yield,merged,drought_LCR)
  
  gc()
  
}

# Define the parameters
crop_list <- c('SUGARBEETS','PEANUTS','SORGHUM','TOBACCO','OATS','BARLEY','COTTON','WHEAT','SOYBEANS','CORN')
LAR_thresholds <- c(0.2, 0.3)

# Create a grid of parameters
params <- expand.grid(
  crop_name = crop_list,
  LAR_threshold = LAR_thresholds,
  stringsAsFactors = FALSE
)

# Use pmap to loop through each combination
pmap(params, data_function)
