# Potential Block Group Population Attributes
# ESVA Livability Tool, population context tab
# 2025-10-21 mpc

# ....................................................
# 1. Setup ----
library(tidyverse)
library(tidycensus)

# acs_sub <- load_variables(2023, "acs5/subject", cache = TRUE)
# acs_det <- load_variables(2023, "acs5", cache = TRUE)

# Census api key
# census_api_key("", install = TRUE, overwrite = TRUE) # add key

# Variables/Tables of interest -
##  - Total population -- B01003_001 (variable)
##  - Race ethnicity -- B03002 (variables)
##  - Poverty status -- C17002 (variables)
###  - Age, population under 18/65 and over -- B01001 (table)

##  - Total households, occupied/vacant -- B25002 (variables)
##  - Home ownership/rental -- B25003 (variables)
##  - Median HH Income -- B19013 (variable)
##  - HH above 200K inc -- B19001	(variable)
##  - Own-Occ med house value -- B25077 (variable)
##  - Rent-occ med rent -- B25064 (variable)
###  - Own-Occ house value above (500K) -- B25075 (table)
###  - Rent-occ rent value above (2K) -- B25063 (table)

# ....................................................
# 2. Define localities, variables, pull tables ----

# Parameters
region <- c("001", "131")
state <- "VA"
year <- 2023
survey <- "acs5"

## variables ----
# define varlist
varlist = c("pop_total" = "B01003_001", 
            "pop_white" = "B03002_003",
            "pop_black" = "B03002_004",
            "pop_multi" = "B03002_009",
            "pop_hisp" = "B03002_012",
            "pop_belowpov_den" = "C17002_001",
            "pop_belowpov" = "C17002_002",
            "hh_total" = "B25002_001", 
            "hh_occupied" = "B25002_002",
            "hh_vacant" = "B25002_003",  
            "hh_owned" = "B25003_002",  
            "hh_rented" = "B25003_003",
            "hh_medinc" = "B19013_001",
            "hh_inc200" = "B19001_017",
            "hh_inc200_den" = "B19001_001",
            "hh_owned_medvalue" = "B25077_001",
            "hh_rented_medrent" = "B25064_001")

blkgrp1 <- get_acs(geography = "block group",
                   variables = varlist,
                   state = state,
                   region = region,
                   year = year,
                   survey = survey,
                   output = "wide")
# it's not filtering to requested regions?
blkgrp1 <- blkgrp1 %>% 
  mutate(countyfips = str_sub(GEOID,3,5)) %>% 
  filter(countyfips %in% c("001", "131"))


## tables ----
##  - Age, population under 18/65 and over -- B01001 (table)
blkgrp_age <- get_acs(geography = "block group", 
                      table = "B01001", 
                      state = state, 
                      county = region, 
                      survey = survey,
                      year = year)

# under 18:  3,4,5,6 and 27,28,29,30 (den is pop_totalE)
# 65 and over: 20,21,22,23,24,25 and 44,45,46,47,48,49 (den is pop_totalE)
blkgrp_u18 <- blkgrp_age %>% 
  filter(variable %in% c("B01001_003", "B01001_004", "B01001_005", "B01001_006",
                         "B01001_027", "B01001_028", "B01001_029", "B01001_030")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(pop_under18E = sum(estimate),
            pop_under18M = moe_sum(moe, estimate))

blkgrp_65o <- blkgrp_age %>% 
  filter(variable %in% c("B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
                         "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(pop_65overE = sum(estimate),
            pop_65over18M = moe_sum(moe, estimate))

##  - Own-Occ house value above (500K) -- B25075 (table)
blkgrp_houseval <- get_acs(geography = "block group", 
                        table = "B25075", 
                        state = state, 
                        county = region, 
                        survey = survey,
                        year = year)

# 500K and above; 23,24,25,26.27 (den is hh_ownedE)
blkgrp_house500 <- blkgrp_houseval %>% 
  filter(variable %in% c("B25075_023", "B25075_024", "B25075_025", "B25075_026", "B25075_027")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(hh_homeval500E = sum(estimate),
            hh_homeval500M = moe_sum(moe, estimate))

##  - Rent-occ rent value above (2K) -- B25063 (table)
blkgrp_rentval <- get_acs(geography = "block group", 
                           table = "B25063", 
                           state = state, 
                           county = region, 
                           survey = survey)

# 2000 and above: 23,24,25,26 (den is hh_rentedE)
blkgrp_rent2k <- blkgrp_rentval %>% 
  filter(variable %in% c("B25063_023", "B25063_024", "B25063_025", "B25063_026", "B25063_027")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(hh_rentval2kE = sum(estimate),
            hh_rentval2kM = moe_sum(moe, estimate))

## Combine variables/tables ----
blkgrp_data <- blkgrp1 %>% 
  left_join(blkgrp_u18) %>% 
  left_join(blkgrp_65o) %>% 
  left_join(blkgrp_house500) %>% 
  left_join(blkgrp_rent2k)


# ....................................................
# 3. Derive variables ----
blkgrp_data <- blkgrp_data %>% 
  # percent race/ethnicity
  mutate(pop_per_white = (pop_whiteE/pop_totalE)*100,
         pop_per_black = (pop_blackE/pop_totalE)*100,
         pop_per_hisp = (pop_hispE/pop_totalE)*100,
         pop_per_multi = (pop_multiE/pop_totalE)*100,
         pop_per_remainingrace = 100-pop_per_white - pop_per_black - pop_per_hisp - pop_per_multi) %>% 
  # percent age groups
  mutate(pop_per_under18 = (pop_under18E/pop_totalE)*100,
         pop_per_65over = (pop_65overE/pop_totalE)*100) %>% 
  # percent poverty
  mutate(pop_per_poverty = (pop_belowpovE/pop_belowpov_denE)*100) %>% 
  # percent vacant housing
  mutate(hh_per_vacant = (hh_vacantE/hh_totalE)*100) %>% 
  # percent hh income over $200K
  mutate(hh_per_hhinc_200K = (hh_inc200E/hh_inc200_denE)*100) %>% 
  # percent rented housing
  mutate(hh_per_rent = (hh_rentedE/hh_occupiedE)*100) %>% 
  # percent owned housing 500K+
  mutate(hh_per_owned_500k = (hh_homeval500E/hh_ownedE)*100) %>% 
  # percent rented housing 2K+
  mutate(hh_per_rented_2k = (hh_rentval2kE/hh_rentedE)*100)
# Add MoEs?

# ....................................................
# 4. Brief review ----
blkgrp_data %>% 
  ggplot(aes(x = hh_per_hhinc_200K)) +
  geom_histogram()

# ....................................................
# 5. Save ----
saveRDS(blkgrp_data, file = "blkgrp_data.RDS") 
# blkgrp_data <- readRDS("blkgrp_data.RDS")

# key variables:
# population: pop_totalE, pop_per_white, pop_per_black, pop_per_hisp, pop_per_multi, pop_per_remainingrace,
#             pop_per_under18, pop_per_65over, pop_per_poverty
# household: hh_totalE, hh_per_vacant, hh_per_rent, hh_medincE, hh_per_hhinc_200K, 
#            hh_owned_medvalueE, hh_rented_medrentE, hh_per_owend_500K, hh_per_rented_2k


# ....................................................
# 6. Removed ----
##  - Percent unemployment (Population 16 and over) -- B23025 (variables)

# varlist <- c("pop_16over_lf" = "B23025_001",
#              "pop_labforce" = "B23025_002",
#              "pop_unemp" = "B23025_005")

# blkgrp_data <- blkgrp_data %>% 
#   # percent unemployed
#   mutate(pop_per_unemp = (pop_unempE/pop_16over_lfE)*100)
  

