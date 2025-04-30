library(dplyr)
library(tidyverse)

##### Extract an all-county list ####

# Define the folder containing the drought result files
folder_path <- "results/drought_predictions/"

# Get a list of all files in the folder
all_files <- list.files(path = folder_path, pattern = "_yield_change_predictions_drought_DSCI300.rds", full.names = TRUE)

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

# add county names to the file
library(tigris)
data(fips_codes) #ML: this is a dataset from the package tigris
fips_codes #ML: 3247 observations
fips_codes <- fips_codes %>%
  mutate(CH.GEOID = paste(state,county_code, sep = ""), 
         GEOID = paste(state_code,county_code, sep = "")) %>%
  #filter(GEOID %in% unique(nass_15$GEOID)) %>%  
  mutate(county = str_remove(county, " County")) 

final_county_list_1 <- final_county_list %>% 
  left_join(fips_codes, by = 'GEOID') %>%
  dplyr::select(state_alpha,state_name, GEOID, county, crop, yield_prediction,drought_prediction)

saveRDS(final_county_list_1, file = 'data/intermediate_data/Final_all_predictions_state_county_list_2000_2023.rds')
