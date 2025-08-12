#Import data from US_COMET_Planner_data_June_2023.xlsx
library(readxl)
library(dplyr)
library(stringr)

COMET_Base<-read_excel("C:\\Users\\ChellieMaples\\OneDrive - American Farmland Trust\\Desktop\\Desktop Excel\\US_COMET-Planner_data_June_2023.xlsx", "All States Filtered")
BD_Base<-read_excel("C:\\Users\\ChellieMaples\\OneDrive - American Farmland Trust\\Desktop\\Desktop Excel\\bulk_density_by_county_gssurgo_2024_CM Copy.xlsx")
#Assumes 30 cm depth for calculation 
#bulk density will vary by county
COMET_Base_Final <- COMET_Base[, c("state", "county", "fipsint", "cps_name", "planner_implementation", "soil_carbon_co2")]
COMET_Base_Final <- COMET_Base_Final %>%
  mutate(fipsint = str_pad(fipsint, width = 5, side = "left", pad = "0"))

COMET_Base_Final <- COMET_Base_Final %>%
  mutate(
    C_Reduce_T_C_ACYR = soil_carbon_co2 * (12.011 / 44.009),
    MG_C_M3YR = C_Reduce_T_C_ACYR / (4046.86 * 0.30)
  )

COMET_Base_Final <- COMET_Base_Final %>%
  left_join(BD_Base %>% select(state, fipsint, bd_mean),
            by = c("state", "fipsint")) %>%
  filter(!is.na(bd_mean))

COMET_Base_Final <- COMET_Base_Final %>%
  mutate(
    #original line
    #MG_C_MG_Soil_Yr = (MG_C_M3YR * COMET_Base_Final$bd_mean),
    #updated line per ML 
    MG_C_MG_Soil_Yr = (MG_C_M3YR / COMET_Base_Final$bd_mean),
    MG_SOM_MG_Soil = MG_C_MG_Soil_Yr/0.58,
    '%SOM/yr' =  MG_SOM_MG_Soil*100
    
  )



saveRDS(COMET_Base_Final, "COMET_Base_Final.rds")
