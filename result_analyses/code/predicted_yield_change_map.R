# create yield maps 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(usmap)
library(ggpubr)
library(paletteer)

# get state boundaries
states <- us_map(regions = 'states', exclude = c('AK','HI'))

# get county boundaries 
county <- us_map(regions = 'counties', exclude = c('AK','HI'))


#----------------------------------yield --------------------------------------#
# create a function to create maps
yield_map <- function(crop) {
  
  #read predicted yield data and county level variables
  yield <- read_rds(paste0('results/yield_predictions/',crop,'_Pred_Normal.rds'))
  variables <- read_rds(paste0('data/merged_data/',crop,'_all_data_soil_weather_N_n15_om10_AFT.rds')) 
  
  # Set yield column names based on crop
  if (crop %in% c('CORN','BARLEY','SORGHUM','WHEAT','SOYBEANS','OATS')) {
    yield1_col <- "pred_yield_bu_acre"
    yield2_col <- "pred_yield_Mg_ha"
  } else if (crop %in% c('COTTON','PEANUTS','TOBACCO')) {
    yield1_col <- "pred_yield_lb_acre"
    yield2_col <- "pred_yield_Mg_ha"
  } else if (crop %in% c('SUGARBEETS')) {
    yield1_col <- "pred_yield_ton_acre"
    yield2_col <- "pred_yield_Mg_ha"
  } else {
    stop("Unknown crop! Please specify the yield column names for this crop.")
  }
  
  #calculate yield change and percentage with 1% increase on SOM 
  max_SOM <- max(yield$ssurgo_om_mean)
  
  yield_SOM_1 <- yield %>%
    filter(
      ssurgo_om_mean == round(ssurgo_om_county_mean, digits = 2) |
      ssurgo_om_mean  == round(ssurgo_om_county_mean+1, digits = 2) | 
      ssurgo_om_mean == max_SOM) %>% 
    group_by(GEOID) %>%
    add_count() %>%
    filter(!(ssurgo_om_mean == max_SOM & n == 3)) %>%
    select(state_alpha, GEOID, ssurgo_om_mean, county_name,
           !!sym(yield1_col), !!sym(yield2_col)) %>%  # name dynamically
    group_by(state_alpha,GEOID, county_name) %>%
    nest() %>%
    mutate(
      yield1_diff = map_dbl(data, ~ if(nrow(.x) == 2) diff((.x %>% arrange(ssurgo_om_mean))[[yield1_col]]) else NA_real_),
      yield2_diff = map_dbl(data, ~ if(nrow(.x) == 2) diff((.x %>% arrange(ssurgo_om_mean))[[yield2_col]]) else NA_real_),
      pred_yield_Mg_ha_diff_perc = map_dbl(data, ~ if(nrow(.x) == 2) {
        ordered <- .x %>% arrange(ssurgo_om_mean)
        100 * diff(ordered[[yield2_col]]) / ordered[[yield2_col]][1]
      } else NA_real_)
    ) %>% 
    left_join(unique(variables[,c("crop","GEOID","ssurgo_om_mean","Mean.percent.irrigated","irrigated")]), by = 'GEOID') %>%
    select(-data) %>%
    ungroup()
  
  
  # create the map----------------------------
  # combine data
  all_counties <- yield_SOM_1 %>%
    left_join(county, by = c('GEOID'='fips'))
  
  max_val <- max(abs(all_counties$pred_yield_Mg_ha_diff_perc), na.rm = TRUE)
  
  #create the map
  all_counties_map<- ggplot() +
    geom_polygon(data = all_counties, aes(x =x, y=y, group = group, fill = pred_yield_Mg_ha_diff_perc),alpha = 1) +
    geom_polygon(data = county, aes(x = x, y = y, group = group), fill = 'darkgray',alpha = 0.1, color = 'gray20') +
    geom_polygon(data = states, aes(x = x, y = y, group = group), fill = NA, color = 'black', linewidth =1) +
    scale_fill_paletteer_c("grDevices::Spectral", direction = 1, limits = c(-max_val, max_val),name = 'Predicted Yield Change (%)') + #
    ggtitle(paste0('Predicted yield change (%) for a 1% increase in SOM\n',crop)) +
    coord_equal() +
    theme_void() +
    theme(plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'))
  
  all_counties_map
  
  ggsave(all_counties_map, file= paste0('../result_analyses/predicted_yield_change_SOM1%_maps/',crop,'_all_counties_map.png'))
  
  #create the map with dark background
  all_counties_map_dark<- ggplot() +
    geom_polygon(data = county, aes(x = x, y = y, group = group), fill = 'black',alpha = 0.7, color = 'gray20') +
    geom_polygon(data = all_counties, aes(x =x, y=y, group = group, fill = pred_yield_Mg_ha_diff_perc),alpha = 1,color = 'gray20', linewidth = 0.1) +
    geom_polygon(data = states, aes(x = x, y = y, group = group), fill = NA, color = 'black', linewidth =1) + 
    #scale_fill_gradient2(name = 'Predicted Yield Change (%)', midpoint = 0, mid = 'khaki', high = 'darkgreen', low = 'darkred') + #,limits = c(-max_val, max_val)
    #scale_fill_viridis_c(name = 'Predicted Yield Change (%)', option = "C",limits = c(-max_val, max_val)) +  # "C" is a good diverging palette, "D" is high
    #scale_fill_scico(palette = 'roma', name = 'Predicted Yield Change (%)', limits = c(-max_val, max_val)) +
    #scale_fill_distiller(palette = 'Sequential') +
    scale_fill_paletteer_c("grDevices::Spectral", direction = 1, limits = c(-max_val, max_val),name = 'Predicted Yield Change (%)') + #
    ggtitle(paste0('Predicted yield change (%) for a 1% increase in SOM\n',crop)) +
    coord_equal() +
    theme_void() +
    theme(plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'))
  
  all_counties_map_dark
  
  ggsave(all_counties_map_dark, file= paste0('../result_analyses/intermediate_results/predicted_yield_change_SOM1%_maps/',crop,'_all_counties_map_dark.png'))
  
  # Set diff column names for output
  if (crop %in% c('CORN','BARLEY','SORGHUM','WHEAT','SOYBEANS','OATS')) {
    yield1_diff_name <- "pred_yield_bu_acre_diff"
    yield2_diff_name <- "pred_yield_Mg_ha_diff"
  } else if (crop %in% c('COTTON','PEANUTS','TOBACCO')) {
    yield1_diff_name <- "pred_yield_lb_acre_diff"
    yield2_diff_name <- "pred_yield_Mg_ha_diff"
  } else if (crop %in% c('SUGARBEETS')) {
    yield1_diff_name <- "pred_yield_ton_acre_diff"
    yield2_diff_name <- "pred_yield_Mg_ha_diff"
  }
  
  yield_SOM_1_table <- yield_SOM_1 %>%
    rename(
      !!yield1_diff_name := yield1_diff,
      !!yield2_diff_name := yield2_diff
    )
  
  write.csv(yield_SOM_1_table, file = paste0('../result_analyses/intermediate_results/predicted_yield_change_SOM1%_maps/',crop,'_all_counties_yield_change_percent.csv'),row.names = FALSE)
  
}

# Define the crop list
crop_list <- c('CORN','COTTON','BARLEY','SORGHUM','WHEAT','SOYBEANS','PEANUTS',
               'OATS','SUGARBEETS','TOBACCO')

# run the function for each crop
lapply(crop_list, yield_map) 

#---------------------------------drought--------------------------------------#

drought_map <- function(crop) {
  
  #read predicted drought data and county level variables
  yield <- readRDS(paste0('results/drought_predictions/',crop,'_yield_change_predictions_drought_DSCI300.rds'))
  variables <- readRDS(paste0('data/merged_data/',crop,'_all_data_soil_weather_N_n15_om10_AFT.rds')) 
  
  #calculate yield change and percentage with 1% increase on SOM 
  max_SOM <- max(yield$ssurgo_om_mean)
  
  yield_SOM_1 <- yield %>%
    mutate(ssurgo_om_mean = round(ssurgo_om_mean, digits = 2)) %>%
    # Keep only rows where ssurgo_om_mean matches one of three values:
    #  1) the county mean (rounded),
    #  2) one unit above the county mean (rounded), or
    #  3) the maximum SOM (rounded)
    filter(
      ssurgo_om_mean == round(ssurgo_om_county_mean, digits = 2) |
        ssurgo_om_mean == round(ssurgo_om_county_mean+1, digits = 2)) %>% 
    group_by(GEOID) %>%
    add_count() %>%
    select(state_alpha, GEOID, ssurgo_om_mean, pred_yield_change_percentage) %>%
    group_by(state_alpha,GEOID) %>%
    nest() %>%
    mutate(
      pred_yield_change_diff_perc = map_dbl(data, ~ 
                                              if(nrow(.x) == 2) {
                                                .x %>% 
                                                  arrange(ssurgo_om_mean) %>%
                                                  pull(pred_yield_change_percentage) %>%
                                                  diff ()
      } else {NA_real_})
      ) %>% 
    left_join(unique(variables[,c("crop","GEOID","ssurgo_om_mean","Mean.percent.irrigated","irrigated")]), by = 'GEOID') %>%
    select(-data) %>%
    ungroup()
  
  
  # create the map----------------------------
  # combine data
  all_counties <- yield_SOM_1 %>%
    left_join(county, by = c('GEOID'='fips'))
  
  max_val <- max(abs(all_counties$pred_yield_change_diff_perc), na.rm = TRUE)
  
  #create the map
  all_counties_map<- ggplot() +
    geom_polygon(data = all_counties, aes(x =x, y=y, group = group, fill = pred_yield_change_diff_perc),alpha = 1) +
    geom_polygon(data = county, aes(x = x, y = y, group = group), fill = 'darkgray',alpha = 0.1, color = 'gray20') +
    geom_polygon(data = states, aes(x = x, y = y, group = group), fill = NA, color = 'black', linewidth =1) +
    scale_fill_paletteer_c("grDevices::Spectral", direction = 1, limits = c(-max_val, max_val),name = 'Yield Loss Change (%)') + #
    ggtitle(paste0('Avoided yield loss under drought (%) for a 1% increase in SOM\n',crop)) +
    coord_equal() +
    theme_void() +
    theme(plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'))
  
  all_counties_map
  
  ggsave(all_counties_map, file= paste0('../result_analyses/intermediate_results/drought_yield_loss_change_SOM1%_maps/',crop,'_drought_loss_change_map.png'))
  
  #create the map with dark background
  all_counties_map_dark<- ggplot() +
    geom_polygon(data = county, aes(x = x, y = y, group = group), fill = 'black',alpha = 0.7, color = 'gray20') +
    geom_polygon(data = all_counties, aes(x =x, y=y, group = group, fill = pred_yield_change_diff_perc),alpha = 1,color = 'gray20', linewidth = 0.1) +
    geom_polygon(data = states, aes(x = x, y = y, group = group), fill = NA, color = 'black', linewidth =1) + 
    #scale_fill_gradient2(name = 'Predicted Yield Change (%)', midpoint = 0, mid = 'khaki', high = 'darkgreen', low = 'darkred') + #,limits = c(-max_val, max_val)
    #scale_fill_viridis_c(name = 'Predicted Yield Change (%)', option = "C",limits = c(-max_val, max_val)) +  # "C" is a good diverging palette, "D" is high
    #scale_fill_scico(palette = 'roma', name = 'Predicted Yield Change (%)', limits = c(-max_val, max_val)) +
    #scale_fill_distiller(palette = 'Sequential') +
    scale_fill_paletteer_c("grDevices::Spectral", direction = 1, limits = c(-max_val, max_val),name = 'Yield Loss Change (%)') + #
    ggtitle(paste0('Avoided yield loss under drought (%) for a 1% increase in SOM\n',crop)) +
    coord_equal() +
    theme_void() +
    theme(plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'))
  
  all_counties_map_dark
  
  ggsave(all_counties_map_dark, file= paste0('../result_analyses/intermediate_results/drought_yield_loss_change_SOM1%_maps/',crop,'_drought_loss_change_map_dark.png'))
  
  write.csv(yield_SOM_1, file = paste0('../result_analyses/intermediate_results/drought_yield_loss_change_SOM1%_maps/',crop,'_drought_loss_change_percent.csv'),row.names = FALSE)
  
}

# Define the crop list
crop_list <- c('CORN','COTTON','BARLEY','SORGHUM','WHEAT','SOYBEANS','PEANUTS','OATS')

# run the function for each crop
lapply(crop_list, drought_map) 

