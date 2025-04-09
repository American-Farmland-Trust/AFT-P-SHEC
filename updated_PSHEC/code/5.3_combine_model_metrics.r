# This file is to combine model metrics from all crops and models 

library(dplyr)

# combine all models for each crop

for (crop in c('CORN','COTTON','BARLEY','SORGHUM','WHEAT','SOYBEANS','PEANUTS',
               'OATS','SUGARBEETS','TOBACCO')) {
  
  # Define the output directory ------####
  output_dir <- paste0('/home/meng/Documents/updated_PSHEC/models/',crop,'_model/')
  
  rmse_combine_all <- read.csv(file = paste0(output_dir,'all_',crop,'_model_selection_metrics.csv'), row.names = 'X')
  
  rmse_combine_irrigate <-  read.csv(file = paste0(output_dir,'irrigated_',crop,'_model_selection_metrics.csv'), row.names = 'X')
  
  rmse_combine_rainfed <-  read.csv(file = paste0(output_dir,'rainfed_',crop,'_model_selection_metrics.csv'), row.names = 'X')
  
  rmse_combine <- bind_rows(list('all' = rmse_combine_all,
                            'irrigated' = rmse_combine_irrigate,
                            'rainfed' = rmse_combine_rainfed),
                            .id = 'model') %>%
                  mutate(crop = crop)
  
  
  write.csv(rmse_combine, file = paste0(output_dir,crop,'_combined_model_selection_metrics.csv'), row.names = FALSE)
  
}


# combine all crops together

main_folder <- '/home/meng/Documents/updated_PSHEC/models/'

crops <- c('CORN','COTTON','BARLEY','SORGHUM','WHEAT','SOYBEANS','PEANUTS',
               'OATS','SUGARBEETS','TOBACCO') 

list_path <- function(crop) {
  
  input_file <- paste0('/home/meng/Documents/updated_PSHEC/models/',crop,'_model/',crop,'_combined_model_selection_metrics.csv')
  
  return(input_file)
  
}
  
folder_path <- lapply(crops, list_path)
  
data_combine <- lapply(folder_path, read.csv) %>% bind_rows()

write.csv(data_combine, file = paste0(main_folder,'combined_model_selection_metrics.csv'), row.names = FALSE)
