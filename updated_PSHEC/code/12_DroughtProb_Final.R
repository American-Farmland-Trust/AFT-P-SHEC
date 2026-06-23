library(dplyr)
library(markovchain)
library(purrr)
library(tidyr)


#Barley_DSCI_mean_county_by_year.rds base data

input_dir <- 'data/DSCI_data'
output_dir <- "data/drought_prob_data"

#Copy of original data

drought_prob_function <- function(crop) {

  Barley<-readRDS(file = file.path(input_dir, paste0(crop, "_DSCI_mean_county_by_year.rds")))

#sort Barley by state_alpha, county, then year
  Barley<-Barley %>%
    arrange(state_alpha, County, year) %>%
    #add column that assigns "states of weather" based on DSCI.mean
    mutate(Drought_Class = as.character(if_else(DSCI.mean>300, "Drought", "Normal")))

#generate transition probabilities for each county in each state
# Nest data by state and county
  #Barley_nested <- Barley %>%
  #  arrange(year) %>%
  #  group_by(state_alpha, County) %>%
  #  summarise(Drought_Seq = list(Drought_Class), .groups = "drop")

# Step 1: Make sure Drought_Class is character and valid
  Barley_clean <- Barley %>%
    filter(!is.na(Drought_Class)) %>%
    mutate(Drought_Class = as.character(Drought_Class)) %>%
    arrange(state_alpha, County, year)

# Step 2: Filter for counties that have both states present
  Barley_valid <- Barley_clean %>%
    group_by(state_alpha, County) %>%
    filter(length(unique(Drought_Class)) > 1) %>%
    ungroup()

# Step 3: Prepare transition sequences
  Barley_sequences <- Barley_valid %>%
    group_by(state_alpha, County) %>%
    summarise(Drought_Seq = list(Drought_Class), .groups = "drop")

# Step 4: Fit Markov chains and calculate steady state
  Barley_markov <- Barley_sequences %>%
    mutate(
      mc_model = map(Drought_Seq, ~ markovchainFit(data = .x)$estimate),
      steady_state = map(mc_model, ~ tryCatch(steadyStates(.x), error = function(e) NA))
    ) %>%
    mutate(
      Drought_prob = map_dbl(steady_state, function(x) {
        if (is.matrix(x) && "Drought" %in% colnames(x)) {
          return(x[1, "Drought"])
        } else {
          return(NA_real_)
        }
      })
    ) %>%
    select(state_alpha, County, Drought_prob)

# View results
#print(Barley_results)

#head(Barley_results)
  
  # Handle counties with only one state
  Barley_single_state <- Barley_clean %>%
    dplyr::group_by(state_alpha, County) %>%
    dplyr::summarise(
      n_states = dplyr::n_distinct(Drought_Class),
      only_state = unique(Drought_Class)[1],
      .groups = "drop"
    ) %>%
    dplyr::filter(n_states == 1) %>%
    dplyr::mutate(
      Drought_prob = dplyr::case_when(
        only_state == "Drought" ~ 1,
        only_state == "Normal" ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(state_alpha, County, Drought_prob)

  Barley_results <- dplyr::bind_rows(
    Barley_markov,
    Barley_single_state) %>% 
    mutate(Normal_prob = 1 - Drought_prob) %>%
    mutate(Crop = stringr::str_to_title(stringr::str_to_lower(crop))) #add title case crop name
  
#Number of counties with Drought and Normal in Drought_Class

#Barley %>%
#  filter(!is.na(Drought_Class)) %>%
#  group_by(state_alpha, County) %>%
#  summarise(states_present = n_distinct(Drought_Class), .groups = "drop") %>%
#  filter(states_present == 2) %>%
#  summarise(counties_with_both_states = n())

# Step 1: Prepare a lookup table of GEOIDs
  geoid_lookup <- Barley %>%
    select(state_alpha, County, GEOID) %>%
    distinct()

# Step 2: Join GEOID back to the results
  Barley_final <- Barley_results %>%
    left_join(geoid_lookup, by = c("state_alpha", "County")) %>%
    mutate(County = str_remove(County, ' County$')) %>% #The $ means it only removes " County" when it appears at the end of the county name.
    select(state_alpha,Drought_prob,Normal_prob,Crop,GEOID,County)

  saveRDS(Barley_final, file = file.path(output_dir, paste0(crop, "_drought_prob_markov.rds")))
  
  
  # save drought requency 
  freq_results <- Barley_clean %>%
    dplyr::group_by(state_alpha, County) %>%
    dplyr::summarise(
      n_years = dplyr::n(),
      n_drought_years = sum(Drought_Class == "Drought", na.rm = TRUE),
      n_normal_years = sum(Drought_Class == "Normal", na.rm = TRUE),
      empirical_Drought_prob = mean(Drought_Class == "Drought", na.rm = TRUE),
      empirical_Normal_prob = 1 - empirical_Drought_prob,
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      geoid_lookup,
      by = c("state_alpha", "County")
      ) %>%
      dplyr::mutate(
        Crop = stringr::str_to_title(stringr::str_to_lower(crop)), #add title case crop name
        County = stringr::str_remove(County, " County$")
    ) %>%
    dplyr::select(
      state_alpha,
      Drought_prob = empirical_Drought_prob,
      Normal_prob = empirical_Normal_prob,
      Crop,
      GEOID,
      County,
      n_years,
      n_drought_years,
      n_normal_years
    )

  saveRDS(
    freq_results,
    file = file.path(output_dir, paste0(crop, "_drought_prob_frequency.rds"))
  )
  
}

crop_list <- c("BARLEY","CORN","COTTON","OATS","PEANUTS","SORGHUM","SOYBEANS","SUGARBEETS","TOBACCO","WHEAT" )

lapply(crop_list, drought_prob_function)

# combine prob files 
prob_dir <- 'data/drought_prob_data'

combine_prob_files <- function(method) {
  
  all_results <- purrr::map_dfr(
    crop_list,
    function(crop) {
      readRDS(
        file = file.path(prob_dir, paste0(crop, "_drought_prob_", method, ".rds"))
      )
    }
  )
  
  saveRDS(
    all_results,
    file = file.path(prob_dir, paste0("all_crops_drought_prob_", method, ".rds"))
  )
  
  return(all_results)
}

all_markov_results <- combine_prob_files("markov")
all_frequency_results <- combine_prob_files("frequency")

# filter drought prob = 1 and 0 out
filtered_markov_results <- all_crops_drought_prob_markov %>%
  filter(Drought_prob > 0 & Drought_prob < 1)

saveRDS(filtered_markov_results, file = '../RShiny_General_Info_Data/drought_prob.rds')
#old_filter <- drought_prob %>%
#  filter(Drought_prob > 0 & Drought_prob < 1)
