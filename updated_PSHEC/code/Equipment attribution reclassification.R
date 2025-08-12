library(writexl)
library(dplyr)
library(stringr)
#Equipment Final Updated dataset used as base from Github
Equipment_Final_Updated <- Equipment_Final_Updated %>%
  mutate(`Equipment Type` = case_when(
    str_detect(Equipment, regex("Ripper|Rip|Harrow|Chisel|Cultipacker|Blade-Box|Blade-Scraper|Tillage|Lister|Roller|Bed-Subsoil|Middle Buster|Plow|Cultivate|Harrow|Cultivator|Hoe|Till|Cult|\\bdisk\\b|Disk|Subsoiler|Lay Off Peanut", ignore_case = TRUE)) ~ "Tillage",
    str_detect(Equipment, regex("Seeder|Planter|Peanut Plt|Drill|Levee Splitter|Plant|Broadcast Seeding|Peanut Ptlt&PreTwin", ignore_case = TRUE)) ~ "Planting",
    str_detect(Equipment, regex("Sprayer|Spray|Boom|Hi Boy", ignore_case = TRUE)) ~ "Chemical Application Equipment",
    str_detect(Equipment, regex("Spread|Manure|Side|Fert Appl|Dry Applicator", ignore_case = TRUE)) ~ "Fertilizer Application Equipment",
    str_detect(Equipment, regex("Hay|Bale|Baler|Combine|Hopper Bottom|Module Builder|Cart|Puller|Corn Silage, Self-propelled|Corn Silage, Pull-type|Drying|Picker|Harvester|Header|Boll Buggy|Peanut Dig|Peanut Lifter|Peanut Wagon|Peanut Re-shake|Peanut Conditioner|Cond.", ignore_case = TRUE)) ~ "Harvest",
    str_detect(Equipment, regex("Mower|Rotary Cutter|Stalk Shredder", ignore_case = TRUE)) ~ "Termination",
    str_detect(Equipment, regex("Gate|ATV|Gravity|Rake|LARice|Pipe Traile|Pipe Spool|Ditcher|Pickup|Plane|Tedder|Level Beds|Pull Levee|Levee Pull", ignore_case = TRUE)) ~ "Other",
    TRUE ~ NA_character_
  ))
# Save as RDS
saveRDS(
  Equipment_Final_Updated,
  file = "C:/Users/ChellieMaples/OneDrive - American Farmland Trust/Desktop/Equipment_Final_Updated.rds"
)