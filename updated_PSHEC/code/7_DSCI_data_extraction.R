# extract DSCI index for all crops and counties

# code was modifided based on Dane Kane et al., 2018 

library(httr)
library(dplyr)
library(tidyverse)
library(parallel)

# Read in unique county FIPS codes
#tidyverse package includes ability to run read_rds function

#Needs to be updated: This file tells the API which counties to pull DSCI data for

#------------read county list for all crops----------#
all_crops_county <- read.csv('data/merged_data/all_10_crops_merged_data_county_list_ordered_2000_2023.csv')
all_crops_county$GEOID <- formatC(all_crops_county$GEOID, width = 5, format = 'd' ,flag = '0')

all_counties <- unique(all_crops_county$GEOID)

#########################################################################################################
#Create list of URLs for API calls for each county
#This should pull weekly data from Jan 1, 2000, to December 31, 2023 
#Should pull county DSCI statistics-> no need to calculate DSCI codes in subsequent drought-data-processing.R code 
#County codes need to be five digit FIPS, all_counties file contains FIPS codes
#statistics type = {1 (traditional), 2 (categorical)}, for DSCI it doesn't matter if 1 or 2 is selected, returns the same results

#https://droughtmonitor.unl.edu/DmData/DataDownload/WebServiceInfo.aspx#comp -> Information on how to create URL 

#########################################################################################################

#----------- all counties --------------------------####
#lapply command applies a function over a list: lapply(list,fxn,...)
#pulling DSCI 

start_date <- '5/1/1999' # include 1999 for the calculation for winter crops 
end_date <- '12/31/2023' 

# create the list of counties and date to pull data
URL_by_county <- lapply(all_counties, 
                        function(x) paste0("https://usdmdataservices.unl.edu/api/CountyStatistics/GetDSCI?aoi=",x,"&startdate=",start_date,"&enddate=",end_date,"&statisticsType=1")) 

# pull data
data_by_county <- lapply(URL_by_county, function(x) httr::content(httr::GET(url = x)))

# combine the list of data
library(purrr)
library(dplyr)

data_by_county_2 <- data_by_county |>
  map(~ mutate(., FIPS = as.character(FIPS))) |>
  bind_rows()


#ldply command is command that, for each element of a list, applies a function, then combined result into a data frame
data_by_county_2 <- ldply(data_by_county, function(x) ldply(x, function(x) data.frame(t(unlist(x)))))

# Save the data
saveRDS(data_by_county_2, file = "data/intermediate_data/DSCI_all_crop_drought_by_county_1999_2023.rds")

rm(data_by_county)

# Split release date into Y-M-D 
data_by_county_2 <- readRDS("data/intermediate_data/DSCI_all_crop_drought_by_county_1999_2023.rds")
data_by_county_2$MapDate <- as.character(data_by_county_2$MapDate)

data_by_county_2 <- data_by_county_2 %>%
  separate(MapDate, into = c("Year","Month","Day"), sep = c(4,6))

#------------read crop progress data ----------------#

# crop progress data
#crop_calendar <- read.csv(file = 'data/intermediate_data/Crop_calendar_plant_harvest_day.csv', row.names = 'X')
crop_progress <- readRDS(file = 'data/intermediate_data/Crop_progress_plant_mature.rds')

#----------- Split release date into Y-M-D and subset to growing season for each crop------------#
library(lubridate)

calc_dsci <- function(crop_name) {
  
  # Filter crop progress for the current crop and relevant columns
  calendar = crop_progress %>%
    filter(crop == crop_name) %>%
    distinct(state_alpha,planted, mature)
  
  # Join with precipitation data for the relevant GEOIDs and years
  temp = data_by_county_2 %>%
    inner_join(calendar, by = c('State'='state_alpha')) %>%
    # filter by crop counties
    filter(FIPS %in% all_crops_county$GEOID[all_crops_county[[crop_name]]==1]) %>%
    mutate(Date = make_date(Year, Month, Day),
           yday = yday(Date),
           Year = as.integer(Year))
  
  # Case 1: Plant and harvest occur within the same year
  same_year_mean <- temp %>%
    group_by(FIPS,State, Year) %>%
    filter(planted <= mature & Year > 1999) %>% # don't include same year crop for 1999
    dplyr::group_by(Year, FIPS, State, County) %>%
    dplyr::summarise(DSCI.mean = 
                       mean(DSCI[yday >= planted & yday <= mature], na.rm = TRUE), .groups = 'drop') %>%
    dplyr::rename("year" = Year, 'GEOID' = FIPS, 'state_alpha' = State) 
  
  # Case 2: Plant occurs in previous year, harvest in current year
  cross_year <- temp %>%
    group_by(FIPS,State, Year) %>%
    filter(planted > mature)
  
  cross_year_mean <- cross_year %>%
    # Calculate contributions from previous and current years
    dplyr::group_by(Year, FIPS, State, County) %>%
    summarise(
      prev_sum = sum(
        if_else(yday >= planted, DSCI, 0),  # From planting to end of prev year
        na.rm = TRUE
      ),
      
      prev_n = sum(if_else(yday >= planted, 1, 0), na.rm = TRUE), # calculate the number of rows (weeks) 
      
      current_sum = sum(
        if_else(yday <= mature, DSCI, 0),  # From start of the year to harvest in current year
        na.rm = TRUE
      ),
      
      current_n = sum(if_else(yday <= mature, 1, 0), na.rm = TRUE) # calculate the number of rows (weeks)
      
    ) %>%
    group_by(FIPS, State) %>%
    arrange(Year) %>% # Ensure data is ordered by year within each group
    mutate(
      # For each year, add the previous year's sum/count (using lag)
      total_sum = lag(prev_sum, default = 0) + current_sum,  # Previous year's prev_sum + current_sum
      total_n = lag(prev_n, default = 0) + current_n, # total number of weeks
      DSCI.mean = total_sum/total_n
    ) %>%
    ungroup() %>%
    dplyr::select(-c(prev_sum,current_sum, prev_n, current_n,total_sum, total_n)) %>%
    filter(Year != 1999) %>% # don't include 1999
    dplyr::rename("year" = Year, 'GEOID' = FIPS, 'state_alpha' = State) 
  
  # Combine results and add crop identifier
  test <- bind_rows(same_year_mean, cross_year_mean) %>%
    mutate(crop = crop_name) 
  
  saveRDS(test, file = paste0("data/DSCI_data/",crop_name,"_DSCI_mean_county_by_year.rds"))
  
}

crop_list <- c('SUGARBEETS','PEANUTS','SORGHUM','TOBACCO','OATS','BARLEY','COTTON','WHEAT','SOYBEANS','CORN')

lapply(crop_list, calc_dsci)


