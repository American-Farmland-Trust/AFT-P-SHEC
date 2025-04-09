# Check difference between seperate models vs. single model

library(ggplot2)
library(dplyr)
library(tidyverse)

#-------corn -----#
corn_all <- read.csv('/home/meng/Documents/updated_PSHEC/results/CORN_all_accumulated_local_pred_county.csv', row.names = 'X')
corn_all$GEOID <- formatC(corn_all$GEOID, width = 5, format = 'd' ,flag = '0')

corn_rainfed <- read.csv('/home/meng/Documents/updated_PSHEC/results/CORN_rainfed_accumulated_local_pred_county.csv', row.names = 'X')
corn_rainfed$GEOID <- formatC(corn_rainfed$GEOID, width = 5, format = 'd' ,flag = '0')

# compare plots
rainfed_counties <- c("01003", "01009", "01015",
                      "18031", "18033", "18035",
                      "27143", "27147", "27155",
                      "36025", "36027", "36029",
                      "47017", "47021", "47023",
                      "51081", "51083", "51093")

combine_df <- corn_rainfed %>%
  left_join(corn_all, by = c('GEOID','ssurgo_om_mean'),
            suffix = c("_rainfed", "_all")) %>%
  pivot_longer(names_to = 'system', values_to = 'pred_yield',cols = pred_yield_rainfed:pred_yield_all) %>%
  filter(GEOID %in% rainfed_counties)

ggplot(combine_df)+
  geom_line(aes(x = ssurgo_om_mean, y = pred_yield, color = GEOID))+
  facet_wrap(~system)

# ---- soybeans------
soy_all <- read.csv('/home/meng/Documents/updated_PSHEC/results/SOYBEANS_all_accumulated_local_pred_county.csv', row.names = 'X')
soy_all$GEOID <- formatC(soy_all$GEOID, width = 5, format = 'd' ,flag = '0')

soy_rainfed <- read.csv('/home/meng/Documents/updated_PSHEC/results/SOYBEANS_rainfed_accumulated_local_pred_county.csv', row.names = 'X')
soy_rainfed$GEOID <- formatC(soy_rainfed$GEOID, width = 5, format = 'd' ,flag = '0')

# compare plots
rainfed_counties <- c("01003", "01009", "01015",
                      "18031", "18033", "18035",
                      "27143", "27147", "27155",
                      "36021", "36023", "36029",
                      "47017", "47021", "47023",
                      "51081", "51083", "51093")
  
  
combine_df <- soy_rainfed %>%
  left_join(soy_all, by = c('GEOID','ssurgo_om_mean'),
            suffix = c("_rainfed", "_all")) %>%
  pivot_longer(names_to = 'system', values_to = 'pred_yield',cols = pred_yield_rainfed:pred_yield_all) %>%
  filter(GEOID %in% rainfed_counties)

ggplot(combine_df)+
  geom_line(aes(x = ssurgo_om_mean, y = pred_yield, color = GEOID))+
  facet_wrap(~system)

# ---- wheat------
wheat_all <- read.csv('/home/meng/Documents/updated_PSHEC/results/WHEAT_all_accumulated_local_pred_county.csv', row.names = 'X')
wheat_all$GEOID <- formatC(wheat_all$GEOID, width = 5, format = 'd' ,flag = '0')

wheat_rainfed <- read.csv('/home/meng/Documents/updated_PSHEC/results/WHEAT_rainfed_accumulated_local_pred_county.csv', row.names = 'X')
wheat_rainfed$GEOID <- formatC(wheat_rainfed$GEOID, width = 5, format = 'd' ,flag = '0')

# compare plots
rainfed_counties <- c("05003", "05021", "05029",
                      "13211", "13269", "16009",
                      "20033", "20035", "20037",
                      "30065", "30067", "30069",
                      "42097", "42099", "42107",
                      "55019", "55021", "55025")


combine_df <- wheat_rainfed %>%
  left_join(wheat_all, by = c('GEOID','ssurgo_om_mean'),
            suffix = c("_rainfed", "_all")) %>%
  pivot_longer(names_to = 'system', values_to = 'pred_yield',cols = pred_yield_rainfed:pred_yield_all) %>%
  filter(GEOID %in% rainfed_counties)

ggplot(combine_df)+
  geom_line(aes(x = ssurgo_om_mean, y = pred_yield, color = GEOID))+
  facet_wrap(~system)

