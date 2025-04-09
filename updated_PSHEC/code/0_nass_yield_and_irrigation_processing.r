
# Load all libraries 
library(rnassqs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(caret)
library(parallel)
library(readr)
library(purrr)

# read NASS api key
api_key <- as.character(read.csv("nass_key.csv", header = F)[1,1])      
#api_key <- as.character(read.csv("nass_key.csv", header = F)[1,1])      

# Call in all yield data via NASS API ####
nassqs_auth(key = api_key)

# Pull data from NASS

# Specify years and pull yield data from nass 'SURVEY'
survey_years <- as.list(2000:2023)  

# Specify years and pull yield data from nass 'CENSUS'
census.years <- as.list(c(2002,2007,2012, 2017,2022))

# Specify the parameters to pull
params.name <- c("source_desc",           "sector_desc",           "group_desc",           
                 "commodity_desc",        "class_desc",            "prodn_practice_desc",  
                 "util_practice_desc",    "statisticcat_desc",     "unit_desc",            
                 "short_desc",            "domain_desc",           "domaincat_desc")

# ----- pull Survey data for all crops -----#
d <- plyr::ldply(survey_years, function(x){
  
  params <- list(
    year = x,
    agg_level_desc = "COUNTY", 
    source_desc = "SURVEY",  
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs_yields(params) 
  )
}) 

#unique(d$commodity_desc)

# [1] "BARLEY"         "BEANS"          "CANOLA"         "CHICKPEAS"      "CORN"          
# [6] "COTTON"         "FLAXSEED"       "HAY"            "HAY & HAYLAGE"  "LENTILS"       
#[11] "MUSTARD"        "OATS"           "PEANUTS"        "PEAS"           "RICE"          
#[16] "RYE"            "SAFFLOWER"      "SORGHUM"        "SOYBEANS"       "SUGARBEETS"    
#[21] "SUGARCANE"      "SUNFLOWER"      "TOBACCO"        "WHEAT"          "APPLES"        
#[26] "PEACHES"        "POTATOES"       "SWEET CORN"     "SWEET POTATOES" "TOMATOES"      
#[31] "PECANS"         "PEPPERS"  

# ---- Target crop list -----#

#'CORN','SOYBEANS','WHEAT','COTTON','SORGHUM',"BARLEY","TOBACCO","OATS","PEANUTS",
#"BEANS", "SUGARBEETS", "SUNFLOWER"

# ---- Extract yield data and filter to counties with at least 15 years of yield data ----

# ---- corn ----
d.CORN <- d %>%
  filter(commodity_desc == "CORN" & 
           util_practice_desc == "GRAIN" &
           year %in% survey_years &
           agg_level_desc == "COUNTY" & 
           source_desc == "SURVEY" &  
           domain_desc == "TOTAL" &
           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
           county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  mutate(crop = 'CORN') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres for the crop

d.acres.total.corn <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'CORN',
    util_practice_desc = "GRAIN",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("CORN, GRAIN - ACRES HARVESTED",
                               "CORN, GRAIN, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})

# calculate the mean percentage of irrigated acres in each county 

d.acres.corn <- d.acres.total.corn %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.CORN$GEOID,!is.na(`CORN, GRAIN - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`CORN, GRAIN, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `CORN, GRAIN, IRRIGATED - ACRES HARVESTED` / `CORN, GRAIN - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05  ~ 'rainfed',
                               TRUE ~ 'irrigated'))
rm(d.acres.total.corn) # remove intermediate dataset

# combine yield and irrigation data

d.CORN <- d.CORN %>%
  left_join(d.acres.corn, by = 'GEOID')

# ---- soybeans ----
d.SOYBEANS <- d %>%
  filter(commodity_desc == "SOYBEANS" & 
           util_practice_desc == "ALL UTILIZATION PRACTICES" &
           year %in% survey_years &
           agg_level_desc == "COUNTY" & 
           source_desc == "SURVEY" &  
           domain_desc == "TOTAL" &
           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
           county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  mutate(crop = 'SOYBEANS') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres percentage
d.acres.total.soy <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'SOYBEANS',
    util_practice_desc = "ALL UTILIZATION PRACTICES",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("SOYBEANS - ACRES HARVESTED",
                               "SOYBEANS, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})
# percentage of irrigated acres in each county 
d.acres.soy <- d.acres.total.soy %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.SOYBEANS$GEOID,!is.na(`SOYBEANS - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`SOYBEANS, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `SOYBEANS, IRRIGATED - ACRES HARVESTED`/`SOYBEANS - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05 ~ 'rainfed',
    TRUE ~ 'irrigated'))
rm(d.acres.total.soy) # remove intermediate dataset

# combine yield and irrigation data
d.SOYBEANS <- d.SOYBEANS %>%
  left_join(d.acres.soy, by = 'GEOID')

# ---- winter wheat, which makes up up to 80% of wheat production ----
# note: spring wheat is not included
d.WHEAT <- d %>%
  filter(commodity_desc == "WHEAT" & 
           util_practice_desc == "ALL UTILIZATION PRACTICES" &
           year %in% survey_years &
           agg_level_desc == "COUNTY" & 
           source_desc == "SURVEY" &  
           domain_desc == "TOTAL" &
           short_desc == "WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE" &
           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
           county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  mutate(crop = 'WHEAT') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres percentage
d.acres.total.wheat <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'WHEAT',
    util_practice_desc = "ALL UTILIZATION PRACTICES",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("WHEAT, WINTER - ACRES HARVESTED",
                               "WHEAT, WINTER, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})
# percentage of irrigated acres in each county 
d.acres.wheat <- d.acres.total.wheat %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.WHEAT$GEOID,!is.na(`WHEAT, WINTER - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`WHEAT, WINTER, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `WHEAT, WINTER, IRRIGATED - ACRES HARVESTED`/`WHEAT, WINTER - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05 ~ 'rainfed',
    TRUE ~ 'irrigated'))

rm(d.acres.total.wheat) # remove intermediate dataset

# combine yield and irrigation data
d.WHEAT <- d.WHEAT %>%
  left_join(d.acres.wheat, by = 'GEOID')


# ---- upland cotton, which makes up 95% of US cotton production ----
#  (note: pima cotton is not included)
d.COTTON <- d %>%
  filter(
    commodity_desc == "COTTON" &
    util_practice_desc == "ALL UTILIZATION PRACTICES" &
    year %in% survey_years &
    agg_level_desc == "COUNTY" &
    source_desc == "SURVEY" &  
    domain_desc == "TOTAL" &
    short_desc == "COTTON, UPLAND - YIELD, MEASURED IN LB / ACRE"  &
    prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
    county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  mutate(crop = 'COTTON') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres percentage
d.acres.total.cotton <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'COTTON',
    util_practice_desc = "ALL UTILIZATION PRACTICES",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("COTTON, UPLAND - ACRES HARVESTED",
                               "COTTON, UPLAND, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})
# percentage of irrigated acres in each county 
d.acres.cotton <- d.acres.total.cotton %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.COTTON$GEOID,!is.na(`COTTON, UPLAND - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`COTTON, UPLAND, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `COTTON, UPLAND, IRRIGATED - ACRES HARVESTED`/`COTTON, UPLAND - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05 ~ 'rainfed',
    TRUE ~ 'irrigated'))

rm(d.acres.total.cotton) # remove intermediate dataset

# combine yield and irrigation data
d.COTTON <- d.COTTON %>%
  left_join(d.acres.cotton, by = 'GEOID')

# ---- sorghum ----
d.SORGHUM <- d %>%
  filter(commodity_desc == "SORGHUM" &
           util_practice_desc == "GRAIN" &
           year %in% survey_years &
           agg_level_desc == "COUNTY" & 
           source_desc == "SURVEY" &  # change source to source_desc
           domain_desc == "TOTAL" &
           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
           county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  mutate(crop = 'SORGHUM') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres percentage
d.acres.total.sorghum <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'SORGHUM',
    util_practice_desc = "GRAIN",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("SORGHUM, GRAIN - ACRES HARVESTED",
                               "SORGHUM, GRAIN, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})

# percentage of irrigated acres in each county 
d.acres.sorghum <- d.acres.total.sorghum %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.SORGHUM$GEOID,!is.na(`SORGHUM, GRAIN - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`SORGHUM, GRAIN, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `SORGHUM, GRAIN, IRRIGATED - ACRES HARVESTED`/`SORGHUM, GRAIN - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05 ~ 'rainfed',
    TRUE ~ 'irrigated'))

rm(d.acres.total.sorghum) # remove intermediate dataset

# combine yield and irrigation data
d.SORGHUM <- d.SORGHUM %>%
  left_join(d.acres.sorghum, by = 'GEOID')

# ---- barley ----
d.BARLEY <- d %>%
  filter(commodity_desc == "BARLEY" &
           #util_practice_desc == "GRAIN" & # this needs to be disabled for barley
           year %in% survey_years &
           agg_level_desc == "COUNTY" & 
           source_desc == "SURVEY" &  # change source to source_desc
           domain_desc == "TOTAL" &
           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
           county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  mutate(crop = 'BARLEY') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres percentage
d.acres.total.barley <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'BARLEY',
    #util_practice_desc = "GRAIN",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("BARLEY - ACRES HARVESTED",
                               "BARLEY, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})

# percentage of irrigated acres in each county 
d.acres.barley <- d.acres.total.barley %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.BARLEY$GEOID,!is.na(`BARLEY - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`BARLEY, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `BARLEY, IRRIGATED - ACRES HARVESTED`/`BARLEY - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05 ~ 'rainfed',
    TRUE ~ 'irrigated'))

rm(d.acres.total.barley) # remove intermediate dataset

# combine yield and irrigation data
d.BARLEY <- d.BARLEY %>%
  left_join(d.acres.barley, by = 'GEOID')


# ---- oats ----
d.OATS <- d %>%
  filter(commodity_desc == "OATS" &
           #util_practice_desc == "GRAIN" & 
           year %in% survey_years &
           agg_level_desc == "COUNTY" & 
           source_desc == "SURVEY" &  # change source to source_desc
           domain_desc == "TOTAL" &
           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
           county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  mutate(crop = 'OATS') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres percentage
d.acres.total.oat <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'OATS',
    #util_practice_desc = "GRAIN",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("OATS - ACRES HARVESTED",
                               "OATS, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})

# percentage of irrigated acres in each county 
d.acres.oat <- d.acres.total.oat %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.OATS$GEOID,!is.na(`OATS - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`OATS, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `OATS, IRRIGATED - ACRES HARVESTED`/`OATS - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05 ~ 'rainfed',
    TRUE ~ 'irrigated'))

rm(d.acres.total.oat) # remove intermediate dataset

# combine yield and irrigation data
d.OATS <- d.OATS %>%
  left_join(d.acres.oat, by = 'GEOID')

# ---- peanut ----
d.PEANUTS <- d %>%
  filter(commodity_desc == "PEANUTS" &
           #util_practice_desc == "GRAIN" & 
           year %in% survey_years &
           agg_level_desc == "COUNTY" & 
           source_desc == "SURVEY" &  # change source to source_desc
           domain_desc == "TOTAL" &
           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
           county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  mutate(crop = 'PEANUTS') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres percentage
d.acres.total.peanut <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'PEANUTS',
    #util_practice_desc = "GRAIN",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("PEANUTS - ACRES HARVESTED",
                               "PEANUTS, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})

# percentage of irrigated acres in each county 
d.acres.peanut <- d.acres.total.peanut %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.PEANUTS$GEOID,!is.na(`PEANUTS - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`PEANUTS, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `PEANUTS, IRRIGATED - ACRES HARVESTED`/`PEANUTS - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05 ~ 'rainfed',
    TRUE ~ 'irrigated'))

rm(d.acres.total.peanut) # remove intermediate dataset

# combine yield and irrigation data
d.PEANUTS <- d.PEANUTS %>%
  left_join(d.acres.peanut, by = 'GEOID')

# ---- sugarbeets ----
d.SUGARBEETS <- d %>%
  filter(commodity_desc == "SUGARBEETS" &
           #util_practice_desc == "GRAIN" & 
           year %in% survey_years &
           agg_level_desc == "COUNTY" & 
           source_desc == "SURVEY" &  # change source to source_desc
           domain_desc == "TOTAL" &
           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
           county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  mutate(crop = 'SUGARBEETS') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres percentage
d.acres.total.sugarbeet <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'SUGARBEETS',
    #util_practice_desc = "GRAIN",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("SUGARBEETS - ACRES HARVESTED",
                               "SUGARBEETS, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})

# percentage of irrigated acres in each county 
d.acres.sugarbeet <- d.acres.total.sugarbeet %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.SUGARBEETS$GEOID,!is.na(`SUGARBEETS - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`SUGARBEETS, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `SUGARBEETS, IRRIGATED - ACRES HARVESTED`/`SUGARBEETS - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05 ~ 'rainfed',
    TRUE ~ 'irrigated'))

rm(d.acres.total.sugarbeet) # remove intermediate dataset

# combine yield and irrigation data
d.SUGARBEETS <- d.SUGARBEETS %>%
  left_join(d.acres.sugarbeet, by = 'GEOID')

# ---- sunflower ----
# has both oil-type and non-oil type, as well as the average
# only 546 left after the filtering
# remove

#d.SUNFLOWER <- d %>%
#  filter(commodity_desc == "SUNFLOWER" &
#           #util_practice_desc == "GRAIN" & 
#           year %in% survey_years &
#           agg_level_desc == "COUNTY" & 
#           source_desc == "SURVEY" &  # change source to source_desc
#           domain_desc == "TOTAL" &
#           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
#           short_desc == 'SUNFLOWER, OIL TYPE - YIELD, MEASURED IN LB / ACRE' &
#           county_ansi != "") %>%
#  mutate(
#    GEOID = paste(state_ansi, county_ansi, sep = ""),
#    Yield_mg_ha = as.numeric(Value) * 0.0628
#  ) %>%
#  mutate(crop = 'SUNFLOWER') %>%
#  dplyr::select(
#    year,
#    crop,
#    GEOID,
#    state_alpha,
#    state_ansi,
#    county_ansi,
#    county_name,
#    Yield_mg_ha, 
#    short_desc) %>%
#  group_by(GEOID) %>%
#  add_count(GEOID) %>%
#  filter(n >= 15) %>% # Filter to >=15 corn yield observations
#  ungroup(.) %>%
#  select(-n) 

# ---- beans ----
# it contains 12 different types 
# the most common type is 'BEANS, DRY EDIBLE, INCL CHICKPEAS - YIELD, MEASURED IN LB / ACRE'
# after filtering: only 786 left
# remove

#d.BEANS <- d %>%
#  filter(commodity_desc == "BEANS" &
#           #util_practice_desc == "GRAIN" & 
#           year %in% survey_years &
#           agg_level_desc == "COUNTY" & 
#           source_desc == "SURVEY" &  # change source to source_desc
#           domain_desc == "TOTAL" &
#           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
#           short_desc == 'BEANS, DRY EDIBLE, INCL CHICKPEAS - YIELD, MEASURED IN LB / ACRE' &
#           county_ansi != "") %>%
#  mutate(
#    GEOID = paste(state_ansi, county_ansi, sep = ""),
#    Yield_mg_ha = as.numeric(Value) * 0.0628
#  ) %>%
#  mutate(crop = 'BEANS') %>%
#  dplyr::select(
#    year,
#    crop,
#    GEOID,
#    state_alpha,
#    state_ansi,
#    county_ansi,
#    county_name,
#    Yield_mg_ha, 
#    short_desc) %>%
#  group_by(GEOID) %>%
#  add_count(GEOID) %>%
#  filter(n >= 15) %>% # Filter to >=15 corn yield observations
#  ungroup(.) %>%
#  select(-n) 


# ---- tobacco ----
# tobacoo contains 18 levels: divided by different processing methods
# the most common one is: TOBACCO, AIR-CURED LIGHT BURLEY (TYPE 31) - YIELD, MEASURED IN LB / ACRE
# after filtering: 1089 is left
 
d.TOBACCO <- d %>%
  filter(commodity_desc == "TOBACCO" &
           #util_practice_desc == "GRAIN" & 
           year %in% survey_years &
           agg_level_desc == "COUNTY" & 
           source_desc == "SURVEY" &  # change source to source_desc
           domain_desc == "TOTAL" &
           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
           short_desc == 'TOBACCO, AIR-CURED LIGHT BURLEY (TYPE 31) - YIELD, MEASURED IN LB / ACRE' &
           county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  mutate(crop = 'TOBACCO') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres percentage
d.acres.total.tobacco <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'TOBACCO',
    #util_practice_desc = "GRAIN",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("TOBACCO - ACRES HARVESTED",
                               "TOBACCO, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})

# percentage of irrigated acres in each county 
d.acres.tobacco <- d.acres.total.tobacco %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.TOBACCO$GEOID,!is.na(`TOBACCO - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`TOBACCO, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `TOBACCO, IRRIGATED - ACRES HARVESTED`/`TOBACCO - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05 ~ 'rainfed',
    TRUE ~ 'irrigated'))

rm(d.acres.total.tobacco) # remove intermediate dataset

# combine yield and irrigation data
d.TOBACCO <- d.TOBACCO %>%
  left_join(d.acres.tobacco, by = 'GEOID')

# Not run!!
# ----------- Hay: alfalfa -----------#

d.hay <- d %>%
  filter(commodity_desc == "HAY" & 
           util_practice_desc == "ALL UTILIZATION PRACTICES" &
           year %in% survey_years &
           agg_level_desc == "COUNTY" & 
           source_desc == "SURVEY" &  
           domain_desc == "TOTAL" &
           #short_desc == "WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE" &
           prodn_practice_desc == "ALL PRODUCTION PRACTICES" &
           county_ansi != "") %>%
  mutate(
    GEOID = paste(state_ansi, county_ansi, sep = ""),
    Yield_mg_ha = as.numeric(Value) * 0.0628
  ) %>%
  filter(short_desc == "HAY, ALFALFA - YIELD, MEASURED IN TONS / ACRE") %>%
  #filter(short_desc == "HAY, (EXCL ALFALFA) - YIELD, MEASURED IN TONS / ACRE") %>%
  #filter(short_desc == "HAY - YIELD, MEASURED IN TONS / ACRE")
  mutate(crop = 'ALFALFA') %>%
  dplyr::select(
    year,
    crop,
    GEOID,
    state_alpha,
    state_ansi,
    county_ansi,
    county_name,
    Yield_mg_ha, 
    short_desc) %>%
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n) 

# extract total and irrigation acres for the crop
d.acres.total.alfalfa <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = 'HAY',
    class_desc = "ALFALFA",
    util_practice_desc = "ALL UTILIZATION PRACTICES",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      filter(short_desc %in% c("HAY, ALFALFA - ACRES HARVESTED",
                               "HAY, ALFALFA, IRRIGATED - ACRES HARVESTED")) %>%
      mutate(GEOID = paste(state_ansi, county_ansi, sep = "")) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Value,
        short_desc
      )
  )
  
})

# percentage of irrigated acres in each county 
d.acres.alfalfa <- d.acres.total.alfalfa %>%
  pivot_wider(
    names_from = short_desc,
    values_from = Value
  ) %>%
  # filter counties that contain yield data
  filter(GEOID %in% d.hay$GEOID,!is.na(`HAY, ALFALFA - ACRES HARVESTED`)) %>%
  # replace NA with zero
  replace_na(list(`HAY, ALFALFA, IRRIGATED - ACRES HARVESTED` = 0)) %>%
  mutate(Percent_irrigated = `HAY, ALFALFA, IRRIGATED - ACRES HARVESTED`/`HAY, ALFALFA - ACRES HARVESTED`) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  ) %>%
  mutate(irrigated = case_when(
    Mean.percent.irrigated <= 0.05 ~ 'rainfed',
    TRUE ~ 'irrigated'))

rm(d.acres.total.tobacco) # remove intermediate dataset

# combine yield and irrigation data
d.alfalfa <- d.hay %>%
  left_join(d.acres.alfalfa, by = 'GEOID')

length(unique(d.alfalfa$state_alpha))
length(unique(d.alfalfa$GEOID))


# ---- for the final crop list:
#'CORN','SOYBEANS','WHEAT','COTTON','SORGHUM',"BARLEY","TOBACCO","OATS","PEANUTS",
# "SUGARBEETS"

#### The next step is to detrend the yield by county ####

# ---- define the function ----

mod <- function(df){
  df <- df
  
  grid <- expand.grid(span = seq(0.3, 0.5, len = 5), degree = seq(0,1, len=2) )
  
  grid.control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    search = "grid")
  
  train_loess <- caret::train(Yield_mg_ha ~ year, 
                              method = "gamLoess",
                              tuneGrid=grid,
                              trControl=grid.control,
                              data = df)
  
  df$Detrend_resids <- as.numeric(residuals(train_loess))
  df$Detrend_predictions <- as.numeric(predict(train_loess))
  return(df)
}

# run the function through every crop
# 1. create list of datasets with crop names for file naming
data.list <- list(
  CORN = d.CORN, SOYBEANS = d.SOYBEANS, WHEAT = d.WHEAT, COTTON = d.COTTON,
  SORGHUM = d.SORGHUM, BARLEY = d.BARLEY, OATS = d.OATS, PEANUTS = d.PEANUTS,
  SUGARBEETS = d.SUGARBEETS, TOBACCO = d.TOBACCO
)

# Directory for output files
output_dir <- "/home/aftadmin/updated_data/nass_processed_yield/"

# Check if output directory exists; create if not
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Process each crop dataset
for (crop_name in names(data.list)) {
  crop_data <- data.list[[crop_name]]
  
  # convert year to integer 
  crop_data$year <- as.integer(crop_data$year) 
  
  # Ensure Yield_mg_ha is numeric
  crop_data$Yield_mg_ha <- as.numeric(crop_data$Yield_mg_ha)
  
  # Split data by county
  crop_list <- split(crop_data, f = crop_data$GEOID) 
  
  # Apply LOESS detrending in parallel 
  crop_list <- mclapply(X = crop_list,FUN = mod, mc.cores = 16) 
  
  # Combine results
  crop_data <- dplyr::bind_rows(crop_list)
  
  # calculate de-trended yield and yield change
  crop_data.1 <- crop_data %>%
    group_by(GEOID) %>%
    mutate(County_avg_yield = mean(Yield_mg_ha, na.rm = TRUE)) %>%
    ungroup(.) %>%
    mutate(
      Yield_decomp_add = County_avg_yield + Detrend_resids,  # de-trended yield 
      Yield_decomp_mult = Yield_mg_ha/Detrend_predictions) 
  
  # Save results to a CSV file
  output_file <- paste0(output_dir, crop_name, "_yield_2000_2023_n_15.csv")
  write.csv(crop_data.1, row.names = FALSE, 
            file  = output_file)
  
  cat("Processed and saved:", crop_name, "\n")
  
}


#### Extract an all-county list ####

# Define the folder containing the CSV files
folder_path <- "/home/aftadmin/updated_data/nass_processed_yield"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Create a container to store data from each file
lst <- list()

# Loop through each CSV file
for (file in csv_files) {
  # Read the CSV file
  data <- read.csv(file)
  
  # Ensure the 'GEOID' column is a string with 5 characters, left-padded with zeros
  data$GEOID <- formatC(data$GEOID, width = 5, format = 'd' ,flag = '0')
  
  # Extract unique GEOID values
  crop_yield_summary <- data %>%
    dplyr::select(crop,state_alpha,GEOID) %>%
    unique()

  # Store the result in the list
  lst[[file]] <- crop_yield_summary
}

# Combine all the unique rows from all files
all_crop_county <- do.call(dplyr::bind_rows,lst) %>% 
  unique() 

# Define the list of crops
crops <- c("BARLEY",     "CORN",       "COTTON",     "OATS",       "PEANUTS",
           "SORGHUM",    "SOYBEANS",   "SUGARBEETS", "TOBACCO",     "WHEAT" )

# Transform the data into a wide format
all_crop_county_wide <- all_crop_county %>%
  # Create binary indicators for each crop
  mutate(present = 1) %>%
  pivot_wider(
    names_from = crop,      # Create one column for each crop
    values_from = present,  # Fill with the binary indicator (1 for presence)
    values_fill = 0         # Fill missing values with 0
  )

write.csv(all_crop_county_wide, file = paste0(folder_path,'/all_10_crops_county_list_2000_2023.csv'),
          row.names = FALSE)
