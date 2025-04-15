library(dplyr)
library(markovchain)
library(purrr)
library(tidyr)


#Sugarbeets_DSCI_mean_county_by_year.rds base data
#Copy of original data
Sugarbeets<-SUGARBEETS_DSCI_mean_county_by_year

#sort Sugarbeets by state_alpha, county, then year
Sugarbeets<-Sugarbeets %>%
  arrange(state_alpha, County, year)

#add column that assigns "states of weather" based on DSCI.mean
Sugarbeets <- Sugarbeets %>%
  mutate(Drought_Class = as.character(if_else(DSCI.mean>300, "Drought", "Normal")))

#generate transition probabilities for each county in each state
# Nest data by state and county
# Sugarbeets_nested <- Sugarbeets %>%
#   arrange(year) %>%
#   group_by(state_alpha, County) %>%
#   summarise(Drought_Seq = list(Drought_Class), .groups = "drop")
Sugarbeets_nested <- Sugarbeets %>%
  arrange(year) %>%
  group_by(state_alpha, County, GEOID) %>%
  summarise(Drought_Seq = list(Drought_Class), .groups = "drop")



# Step 1: Make sure Drought_Class is character and valid
Sugarbeets_clean <- Sugarbeets %>%
  filter(!is.na(Drought_Class)) %>%
  mutate(Drought_Class = as.character(Drought_Class)) %>%
  arrange(state_alpha, County, year)

# Step 2: Filter for counties that have both states present
Sugarbeets_valid <- Sugarbeets_clean %>%
  group_by(state_alpha, County) %>%
  filter(length(unique(Drought_Class)) > 1) %>%
  ungroup()

# Step 3: Prepare transition sequences
Sugarbeets_sequences <- Sugarbeets_valid %>%
  group_by(state_alpha, County) %>%
  summarise(Drought_Seq = list(Drought_Class), .groups = "drop")

# Step 4: Fit Markov chains and calculate steady state
Sugarbeets_results <- Sugarbeets_sequences %>%
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
print(Sugarbeets_results)


head(Sugarbeets_results)

Sugarbeets_results <- Sugarbeets_results %>%
  mutate(Normal_prob = 1 - Drought_prob)
#Number of counties with Drought and Normal in Drought_Class

Sugarbeets %>%
  filter(!is.na(Drought_Class)) %>%
  group_by(state_alpha, County) %>%
  summarise(states_present = n_distinct(Drought_Class), .groups = "drop") %>%
  filter(states_present == 2) %>%
  summarise(counties_with_both_states = n())

Sugarbeets_results <- Sugarbeets_results %>%
  mutate(Crop = "Sugarbeets")

# Step 1: Prepare a lookup table of GEOIDs
geoid_lookup <- Sugarbeets %>%
  select(state_alpha, County, GEOID) %>%
  distinct()

# Step 2: Join GEOID back to the results
Sugarbeets_results <- Sugarbeets_results %>%
  left_join(geoid_lookup, by = c("state_alpha", "County"))
