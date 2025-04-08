# Combine model metrics

# This file is to combine model metrics from drought models 

library(dplyr)

# combine all models 
# Define the input directory ------####
file_list <- list.files('models/drought_models/')

input_dir <- 'models/drought_models/'

combine_rmse <- function(model) {
  
  rmse_model <- read.csv(file = paste0(input_dir, model,'/',model,'_selection_metrics.csv'), row.names = 'X')
  
  rmse_model <- rmse_model %>%
    mutate(model_name = model)
 
  return(rmse_model)

}


# combine all models together

model_results <- lapply(file_list, combine_rmse)

combine_results <- bind_rows(model_results) %>% unique()

write.csv(combine_results, file = 'data/intermediate_data/combined_drought_model_selection_metrics.csv', row.names = FALSE)
