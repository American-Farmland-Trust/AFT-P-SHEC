# this file is to process RMA datasets for all crops

library(httr)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggpubr)

#download data for Cause of Loss data from RMA: https://www.rma.usda.gov/tools-reports/summary-business/cause-loss
#download data for submmary of business from RMA: https://www.rma.usda.gov/tools-reports/summary-of-business/state-county-crop-summary-business

# unzip files and save to a dataframe

#----------COL-----------------#
# create a list of zipped files
zip_files <- list.files(path = 'data/raw_data/RMA_cause_of_loss', pattern = '.zip', full.names = TRUE)

# Pull data 
col_raw <-lapply(zip_files, function(x) fread(unzip(x, exdir = 'data/raw_data/RMA_cause_of_loss'))) # unzip and read table

col_raw.1 <- do.call(rbind.data.frame, col_raw) # combine list to dataframes

# add colnames to data
colnames(col_raw.1) <- c('Commodity Year Identifier',	'State Code',	'State Abbreviation', 	'County Code',	'County Name',	'Commodity Code',	'Commodity Name',	'Insurance Plan Code',	'Insurance Plan Name Abbreviation',	'Coverage Category',	'Stage Code',	'Cause of Loss Code',	'Cause of Loss Description',	'Month of Loss',	'Month of Loss Name',	'Year of Loss',	'Policies Earning Premium',	'Policies Indemnified',	'Net Planted Quantity',	'Net Endorsed Acres',	'Liability',	'Total Premium',	'Producer Paid Premium',	'Subsidy',	'State/Private Subsidy',	'Additional Subsidy',	'EFA Premium Discount',	'Net Determined Quantity',	'Indemnity Amount',	'Loss Ratio')

saveRDS(col_raw.1, file = 'data/raw_data/RMA_cause_of_loss/cause_of_loss_2000-2023.rds')

rm(col_raw,col_raw.1,zip_files)

#-----------SOB----------------#
# create a list of zipped files
sob_zip_files <- list.files(path = 'data/raw_data/RMA_summary_of_business', pattern = '.zip', full.names = TRUE)

# Pull data 
sob_raw <-lapply(sob_zip_files, function(x) fread(unzip(x, exdir = 'data/raw_data/RMA_summary_of_business'))) # unzip and read table

sob_raw.1 <- do.call(rbind.data.frame, sob_raw) # combine list to dataframes

# add colnames to data
colnames(sob_raw.1) <- c('Commodity Year','Location State Code','Location State Abbreviation','Location County Code','Location County Name',
                         'Commodity Code','Commodity Name','Insurance Plan Code','Insurance Plan Name Abbreviation','Coverage Category',
                         'Delivery Type','Coverage Level','Policies Sold Count','Policies Earning Premium Count','Policies Indemnified Count',
                         'Units Earning Premium Count','Units Indemnified Count','Quantity Type','Net Reported Quantity','Endorsed/Companion Acres',
                         'Liability Amount','Total Premium Amount','Subsidy Amount','State/Private Subsidy','Additional Subsidy','EFA Premium Discount',
                         'Indemnity Amount','Loss Ratio')

saveRDS(sob_raw.1, file = 'data/raw_data/RMA_summary_of_business/summary_of_business_2000-2023.rds')

rm(sob_raw, sob_raw.1, sob_zip_files)


#------------ extract data for each crop-------------#

# loop through each crop 
col_raw.1 <- readRDS('data/raw_data/RMA_cause_of_loss/cause_of_loss_2000-2023.rds')
sob_raw.1 <- readRDS('data/raw_data/RMA_summary_of_business/summary_of_business_2000-2023.rds')

colnames(col_raw.1) <- gsub(".", " ", colnames(col_raw.1), fixed=TRUE)
colnames(sob_raw.1) <- gsub(".", " ", colnames(sob_raw.1), fixed=TRUE)

# extract crop_code
crop_name <- col_raw.1 %>%
  dplyr::select(`Commodity Code`, `Commodity Name`) %>%
  unique() %>%
  filter(`Commodity Name` %in% c('CORN','SOYBEANS','WHEAT','COTTON',
                                 'BARLEY','OATS','GRAIN SORGHUM','PEANUTS',
                                 'SUGAR BEETS','BURLEY TOBACCO')) %>%
  mutate(crop = case_when(
    `Commodity Name` == 'GRAIN SORGHUM' ~ 'SORGHUM',
    `Commodity Name` == 'SUGAR BEETS' ~ 'SUGARBEETS',
    `Commodity Name` == 'BURLEY TOBACCO' ~ 'TOBACCO',
    TRUE ~ `Commodity Name`
  ))

crop_code <- crop_name$`Commodity Code`

# create a function to extract data 

# for winter crops, it seems like that we don't need to define the harvest year as it is defined during the reporting phase

extract_function <- function(code) {
  
  # extract col 
  col_corn <- col_raw.1 %>%
    mutate(GEOID = paste0(formatC(`State Code`,width = 2,format = 'd', flag = '0'), formatC(`County Code`,width = 3,format = 'd', flag = '0'))) %>%
    dplyr::select(GEOID, `Commodity Year Identifier`,`State Abbreviation`,`County Name`,`Commodity Code`,`Commodity Name`,`Stage Code`,`Cause of Loss Code`,`Cause of Loss Description`,`Month of Loss`,`Month of Loss Name`,Liability,`Total Premium`,`Indemnity Amount`,`Net Endorsed Acres`,`Net Determined Quantity`) %>%
    dplyr::rename(year = `Commodity Year Identifier`,
         state_alpha = `State Abbreviation`,
         county = `County Name`,
         crop_code = `Commodity Code`,
         crop = `Commodity Name`,
         stage_code = `Stage Code`,
         cause_of_loss = `Cause of Loss Description`,
         cause_of_loss_code = `Cause of Loss Code`,
         month = `Month of Loss`,
         month_name = `Month of Loss Name`,
         total_premium_col = `Total Premium`,
         indemnity_col = `Indemnity Amount`,
         liability_col = Liability,
         net_determined_quantity_col = `Net Determined Quantity`,
         net_endorsed_acres_col = `Net Endorsed Acres`) %>%
    filter(!(stage_code %in% c('P2','PT','PF'))) %>%# filter prevented planting 
    filter(crop_code == code)
  
  # extract sob 
  sob_corn <- sob_raw.1 %>%
    mutate(GEOID = paste0(formatC(`Location State Code`,width = 2,format = 'd', flag = '0'), formatC(`Location County Code`,width = 3,format = 'd', flag = '0'))) %>%
    dplyr::select(GEOID, `Commodity Year`,`Location State Abbreviation`,`Location County Name`,`Commodity Code`,`Commodity Name`,`Quantity Type`,`Net Reported Quantity`,`Endorsed/Companion Acres`,`Liability Amount`,`Total Premium Amount`,`Indemnity Amount`) %>%
    dplyr::rename(year = `Commodity Year`,
         state_alpha = `Location State Abbreviation`,
         county = `Location County Name`,
         crop = `Commodity Name`,
         crop_code = `Commodity Code`,
         quantity_type_sob = `Quantity Type`,
         net_reported_quantity_sob = `Net Reported Quantity`,
         endorsed_companion_acres_sob = `Endorsed/Companion Acres`,
         total_premium_sob = `Total Premium Amount`,
         indemnity_sob = `Indemnity Amount`,
         liability_sob = `Liability Amount`) %>%
    filter(crop_code == code)


  col_sum_corn <- col_corn %>%
    select(GEOID,year,state_alpha,county,cause_of_loss,indemnity_col, liability_col,net_determined_quantity_col,total_premium_col) %>%
    mutate(cause_category = case_when(str_detect(cause_of_loss, 'Irrigation') ~ 'Irrigation failure',
                                    str_detect(cause_of_loss, 'Mycotoxin|Insect|Disease') ~ 'Pest and Disease',
                                    str_detect(cause_of_loss, 'Crops Only|Crops') ~ 'Area Plan Crops Only',
                                    str_detect(cause_of_loss, 'Snow') ~ 'Other (Snow,Lightning,etc)',
                                    .default = cause_of_loss))  %>%
    group_by(GEOID, year, state_alpha,cause_category) %>% #
    summarise(indemnity_col = sum(indemnity_col),liability_col = sum(liability_col), 
            net_determined_quantity_col = sum(net_determined_quantity_col),total_premium_col = sum(total_premium_col)) 

  sob_sum_corn <- sob_corn %>%
    select(GEOID,year,state_alpha,county,indemnity_sob, liability_sob,net_reported_quantity_sob,total_premium_sob,quantity_type_sob) %>%
    group_by(GEOID, year, state_alpha) %>%
    summarise(indemnity_sob = sum(indemnity_sob),liability_sob = sum(liability_sob), 
            net_reported_quantity_sob = sum(net_reported_quantity_sob),total_premium_sob = sum(total_premium_sob)) 

# combine two datasets and compare results; extract loss caused by nature related events that may influence yield: combine drought and heat for drought
  sob_col_corn <- sob_sum_corn %>%
    left_join(col_sum_corn, by = c('GEOID','year','state_alpha')) %>% 
    mutate(LCR_liability = indemnity_col/liability_sob, LR_premium = indemnity_col/total_premium_sob, LAR = net_determined_quantity_col/net_reported_quantity_sob) %>%
    filter(cause_category %in% c('Drought','Excess Moisture/Precipitation/Rain','Heat','Cold Wet Weather','Freeze','Flood','Frost',
                               'Wind/Excess Wind','Hurricane/Tropical Depression', 'Hot Wind','Other (Snow,Lightning,etc)',
                               "Tornado" ,'Hail',"Cold Winter","Cyclone","Fire","Volcanic Eruption","Wildlife" )) %>%
    mutate(cause_category_drought = case_when(cause_category != 'Drought' & cause_category != 'Heat' ~ 'Non-Drought',
                                            .default = 'Drought'),
         cause_category_wet = case_when(cause_category != 'Excess Moisture/Precipitation/Rain' & cause_category != 'Flood' ~ 'Non-Wet',
                                        .default = 'Wet'))
  
  name = crop_name %>% filter(`Commodity Code` == code) %>% pull(crop)

  saveRDS(sob_col_corn, file = paste0('data/rma_data/',name,'_SOB_COL_summary_stats.rds'))

}

lapply(crop_code,extract_function)
