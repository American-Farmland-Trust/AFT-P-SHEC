library(dplyr)
library(markovchain)
library(purrr)
library(tidyr)


#Tobacco_DSCI_mean_county_by_year.rds base data
#Copy of original data
Tobacco<-TOBACCO_DSCI_mean_county_by_year

#sort Tobacco by state_alpha, county, then year
Tobacco<-Tobacco %>%
  arrange(state_alpha, County, year)

#add column that assigns "states of weather" based on DSCI.mean
Tobacco <- Tobacco %>%
  mutate(Drought_Class = as.character(if_else(DSCI.mean>300, "Drought", "Normal")))

#generate transition probabilities for each county in each state
# Nest data by state and county
# Tobacco_nested <- Tobacco %>%
#   arrange(year) %>%
#   group_by(state_alpha, County) %>%
#   summarise(Drought_Seq = list(Drought_Class), .groups = "drop")
Tobacco_nested <- Tobacco %>%
  arrange(year) %>%
  group_by(state_alpha, County, GEOID) %>%
  summarise(Drought_Seq = list(Drought_Class), .groups = "drop")



# Step 1: Make sure Drought_Class is character and valid
Tobacco_clean <- Tobacco %>%
  filter(!is.na(Drought_Class)) %>%
  mutate(Drought_Class = as.character(Drought_Class)) %>%
  arrange(state_alpha, County, year)

# Step 2: Filter for counties that have both states present
Tobacco_valid <- Tobacco_clean %>%
  group_by(state_alpha, County) %>%
  filter(length(unique(Drought_Class)) > 1) %>%
  ungroup()

# Step 3: Prepare transition sequences
Tobacco_sequences <- Tobacco_valid %>%
  group_by(state_alpha, County) %>%
  summarise(Drought_Seq = list(Drought_Class), .groups = "drop")

# Step 4: Fit Markov chains and calculate steady state
Tobacco_results <- Tobacco_sequences %>%
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
print(Tobacco_results)


head(Tobacco_results)

Tobacco_results <- Tobacco_results %>%
  mutate(Normal_prob = 1 - Drought_prob)
#Number of counties with Drought and Normal in Drought_Class

Tobacco %>%
  filter(!is.na(Drought_Class)) %>%
  group_by(state_alpha, County) %>%
  summarise(states_present = n_distinct(Drought_Class), .groups = "drop") %>%
  filter(states_present == 2) %>%
  summarise(counties_with_both_states = n())

Tobacco_results <- Tobacco_results %>%
  mutate(Crop = "Tobacco")

# Step 1: Prepare a lookup table of GEOIDs
geoid_lookup <- Tobacco %>%
  select(state_alpha, County, GEOID) %>%
  distinct()

# Step 2: Join GEOID back to the results
Tobacco_results <- Tobacco_results %>%
  left_join(geoid_lookup, by = c("state_alpha", "County"))
