####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire data for snapshot/change front dashborad
# Last updated: 01/24/2020
# Metrics from ACS (in common with tract level): 
# * Total population
# * Child poverty and BA attain (youth and education)
# * Median HH Income or gini index (jobs and wages)
# * Rent burneded or home ownership rate (housing)
# * Life expectancy (health)?
# * Human development index? (not yet)
# 
# Based on: ACS5 over time, elsewhere?
# Geography: Localities in Charlottesville region
#     Charlottesville, Albemarle, Greene, Louisa, 
#     Fluvanna, Nelson, Buckingham, Madison, Orange
#     (include Augusta, Waynesboro, Staunton?)
####################################################
# 1. Load libraries, provide api key (if needed), identify variables
# 2. Define variables, pull data
# 3. Metrics specific to locality level 
# 4. Reduce, derive estimates, combine
# 5. Summarize/Examine
# 6. Save
####################################################

# ....................................................
# 1. Load libraries, provide api key (if needed), identify variables ----

# Load libraries
library(tidyverse)
library(tidycensus)


# Census api key
# census_api_key("", install = TRUE, overwrite = TRUE) # add key

# Variable view helper
# acs_var <- load_variables(2018, "acs5", cache = TRUE)
# acs_var <- load_variables(2018, "acs5/subject", cache = TRUE)
# acs_var <- load_variables(2018, "acs5/profile", cache = TRUE)

# Variable of interest -
##  - Total population -- B01003_001
##  - Child poverty rate -- S1701_C03_002
##  - Median HH Income -- S1901_C01_012	
##  - Gini Index of Income Inequality -- B19083_001
##  - Percent of cost-burdened renters -- B25070_007+B25070_008+B25070_009+B25070_010/B25070_001
##  - Home ownership rate
##  - For HDI: median personal earnings, hs/ba/grad attain, school enroll
##  - Percent high school graduate or higher -- S1501_C02_014
##  - Percent bachelor's degree or higher -- S1501_C02_015
##  - Percent graduate degree or higher -- S1501_C02_013
##  - School enrollment for the population age 3 to 24 -- S1401_C02_014, S1401_C02_016, S1401_C02_018, S1401_C02_020, S1401_C02_022, S1401_C21_024
## (Life expectancy)

# ....................................................
# 2. Define localities, variables, pull tables ----

# List of desired localities by FIPS
ccode <- read_csv("datacode/county_codes.csv")
region <- ccode$code # list of desired counties
# - 003 Albemarle County, 015 Augusta County, 029 Buckingham County, 540 Charlottesville,
#   065 Fluvanna County,  079 Greene County, 109 Louisa County, 113 Madison County
#   125 Nelson County, 137 Orange County, 790 Staunton, 820 Waynesboro

# years: define yearlist
year_list = c(2012, 2013, 2014, 2015, 2016, 2017, 2018)

# variables: define varlist
varlist_s = c("S1701_C03_002",   # cpovrate
              "S1901_C01_012",   # hhinc
              "S1501_C02_014",   # hsmore
              "S1501_C02_015",   # bamore
              "S1501_C02_013",   # gradmore
              "S2001_C01_002")   # earn

varlist_b = c("B01003_001", # totalpop
              "B19083_001",  # gini
              "B25070_007",  # 30-35 rent burdened
              "B25070_008",  # 35-40 rent burdened
              "B25070_009",  # 40-50 rent burdened
              "B25070_010",  # 50+ rent burdened
              "B25070_001",  # all renters
              "B25003_002",  # owner-occupied housing units
              "B25003_001")  # occupied housing units


# Get Variables
county_data_s <- map_df(year_list, function(x) {
  get_acs(geography = "county",
          state = "VA",
          county = region,
          survey = "acs5",
          year = x, 
          variables = varlist_s,
          output = "wide")
})

county_data_b <- map_df(year_list, function(x) {
  get_acs(geography = "county",
          state = "VA",
          county = region,
          survey = "acs5",
          year = x, 
          variables = varlist_b,
          output = "wide")
})

# rename variables
names(county_data_s) = c("GEOID", "NAME",
                         "cpovrateE", "cpovrateM",
                         "hhincE", "hhincM",
                         "hsmoreE", "hsmoreM",
                         "bamoreE", "bamoreM",
                         "gradmoreE", "gradmoreM",
                         "earnE", "earnM")

names(county_data_b) = c("GEOID", "NAME",  
                         "totalpopE", "totalpopM",
                         "giniE", "giniM",
                         "rent30E", "rent30M",
                         "rent35E", "rent35M",
                         "rent40E", "rent40M",
                         "rent50E", "rent50M",
                         "rentallE", "rentallM",
                         "ownoccE", "ownoccM",
                         "occhseE", "occhseM")

# Derive some variables, add year
county_data_s <- county_data_s %>% 
  mutate(year = rep(year_list, each = 12)) %>% 
  mutate(year = as.character(year)) 
 
county_data_b <- county_data_b %>% 
  mutate(year = rep(year_list, each = 12)) %>% 
  mutate(year = as.character(year)) %>% 
  mutate(rentersumE = (rent30E+rent35E+rent40E+rent50E),
         rentersumM = (rent30M/1.645)^2 + (rent35M/1.645)^2 + (rent40M/1.645)^2 + (rent50M/1.645)^2,
         rentersumM = sqrt(rentersumM)*1.645,
         renter30E = round((rentersumE/rentallE)*100,1),
         renter30M = moe_prop(rentersumE, rentallE, rentersumM, rentallM),
         renter30M = round(renter30M*100,1)) %>% 
  mutate(homeownE = round((ownoccE/occhseE)*100,1),
         homeownM = moe_prop(ownoccE, occhseE, ownoccM, occhseM),
         homeownM = round(homeownM*100, 1)) %>% 
  select(-c(rentersumE, rentersumM,rent30E:occhseM))


# Get Tables
# pull tables (easier to just pull tables separately)

county_enroll <- map_df(year_list, function(x) {
  get_acs(geography = "county",
          state = "VA",
          county = region,
          survey = "acs5",
          year = x, 
          table = "S1401")
})

# Derive metrics, add year
year_list_a <- c(2012, 2013, 2014) # need pop by age table to calculate for these years; ignore for now
year_list_b <- c(2015, 2016, 2017, 2018)

# 6 groups (3-4, 5-9, 10-14, 15-17, 18-19, 20-24) must be summed
#             population and enrolled, and divided
county_schl_num_b <- county_enroll %>% 
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  mutate(year = rep(year_list, each = 72)) %>% 
  filter(year %in% year_list_b) %>% 
  mutate(year = as.character(year)) %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

county_schl_den_b <- county_enroll %>% 
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>%
  mutate(year = rep(year_list, each = 72)) %>% 
  filter(year %in% year_list_b) %>% 
  mutate(year = as.character(year)) %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

county_schl_ratio_b <- left_join(county_schl_num_b, county_schl_den_b)

county_schl_b <- county_schl_ratio_b %>% 
  mutate(schlE = round((schl_num/schl_den)*100, 1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1))


# Combine indicators
# joining columns
county_data <- county_data_s %>% 
  left_join(county_data_b) %>% 
  left_join(county_schl_b) 

county_data <- county_data %>% 
 separate(GEOID, into = c("state", "locality"), 
           sep = c(2), remove = FALSE) 

# ....................................................
# 5. Summarize/Examine indicators ----
county_data %>% select_at(vars(ends_with("E"))) %>% summary()


# Add life expectancy? Unclear if there's a consistent over-time source

# ....................................................
# 6. Save ----
# save rcaa_recap and rcaa_recap_geo
saveRDS(county_data, file = "data/county_data_dash.RDS") 
# county_data <- readRDS("data/county_data_dash.RDS")



