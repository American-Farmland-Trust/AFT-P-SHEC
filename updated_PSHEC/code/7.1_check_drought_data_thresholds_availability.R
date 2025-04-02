# combine DSCI and RMA data for all crops
# check data availability for all crops

library(dplyr)
library(tidyverse)
library(purrr)

####-------------read data --------------####
# define data extract function
data_function <- function(crop_name, LAR_threshold, DSCI_threshold) {

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
  # run the final filter
  drought_LCR <- sob_col %>%
    group_by(GEOID, year,state_alpha, cause_category_drought) %>%
    reframe(sum_LCR = sum(LCR_liability), sum_LAR = sum(LAR)) %>%
    full_join(DSCI, by = c('GEOID', 'year','state_alpha')) %>%
    arrange(GEOID,year, desc(sum_LCR)) %>% 
    group_by(GEOID,year) %>%
    slice(1) %>%
    filter((cause_category_drought == 'Drought' & sum_LCR >= 0 & sum_LAR >= LAR_threshold) | DSCI.mean >= DSCI_threshold )  %>%
    # define drought following the above threshold
    mutate(drought_merge = 'Drought') #

  drought_LCR.1 <- yield %>%
    filter(GEOID %in% merged_counties) %>%
    inner_join(drought_LCR, by = c('year','GEOID','state_alpha','crop')) %>%
    mutate(Yield_change = Detrend_resids/Detrend_predictions * 100) 

  # check outliers: LCR
  out_LCR <- boxplot.stats(drought_LCR.1$Yield_change, coef = 2)$out
  out_ind_LCR <- which(drought_LCR.1$Yield_change %in% c(out_LCR))

  if (length(out_ind_LCR) > 0) {
    drought_LCR.1 <- drought_LCR.1[-out_ind_LCR,]
  } else {drought_LCR.1}  

####--- check number of obervations in each state and the ratio of counties in each state---####
 # if filter by respresentative counties
  #drought_all <- yield %>%
  #  left_join(drought_LCR.1, by = c('year','GEOID','crop','state_alpha')) 

  #drought_count <- drought_LCR.1 %>%
  #  group_by(state_alpha) %>%
  #  summarise(n_drought_obs=n()) # calculate the number of observations in each state
  
  #drought_ratio <- drought_all %>%
  #  group_by(state_alpha) %>%
  #  nest() %>%
  #  mutate(n_GEOID = purrr::map(data, ~length(unique(.$GEOID)))) %>% #number of GEOID in each state
  #  unnest(data) %>%
  #  filter(drought_merge == 'Drought') %>%
  #  group_by(state_alpha, n_GEOID) %>%
  #  nest() %>%
  #  mutate(n_GEOID_drought = purrr::map(data, ~length(unique(.$GEOID)))) %>%  #number of GEOID in each state
  #  select(-data) %>%
  #  unnest(cols = c(n_GEOID, n_GEOID_drought)) %>%
  #  mutate(drought_county_ratio = n_GEOID_drought/n_GEOID) %>%
  #  left_join(drought_count, by = 'state_alpha') %>% # both count and county ratio
  #  #filter(drought_county_ratio > 0.2  & n_GEOID_drought > 3 & n > 5) 
  #  # filter so that the final dataset a representative dataset of > than 3 drought counties per state
  #  mutate(drought_ratio = length(which(drought_LCR.1$Yield_change < 0 , TRUE)) / length(drought_LCR.1$Yield_change)) %>%
  #  mutate(crop = crop_name, 
  #         drought_sample_size = nrow(drought_LCR.1))
  
  # if not filtering
  drought_ratio <- data.frame(
    crop = crop_name,
    LAR = LAR_threshold,
    DSCI = DSCI_threshold,
    n_states = length(unique(drought_LCR.1$state_alpha)),
    n_obs = nrow(drought_LCR.1),
    drought_ratio = length(which(drought_LCR.1$Yield_change < 0 , TRUE)) / length(drought_LCR.1$Yield_change)
  )

}

# Define the parameters
crop_list <- c('SUGARBEETS','PEANUTS','SORGHUM','TOBACCO','OATS','BARLEY','COTTON','WHEAT','SOYBEANS','CORN')
LAR_thresholds <- c(0, 0.1, 0.2, 0.3)
DSCI_thresholds <- c(300, 350, 400, 450)

# Create a grid of parameters
params <- expand.grid(
  crop_name = crop_list,
  LAR_threshold = LAR_thresholds,
  DSCI_threshold = DSCI_thresholds,
  stringsAsFactors = FALSE
)

# Use pmap to loop through each combination
results <- pmap(params, data_function)

results <- bind_rows(results) %>%
  arrange(crop, LAR, DSCI)

write.csv(results, file = 'data/intermediate_data/drought_data_availability_DSCI_RMA_thresholds.csv', row.names = FALSE)
