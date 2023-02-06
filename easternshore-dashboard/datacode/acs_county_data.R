####################################################
# Eastern Shore Virginia Equity Atlas               
####################################################
# Original scripts written by Lee LeBoeuf
# adapted for Eastern Shore by Chris Barber
# Acquire ACS data
# Last updated: 01/27/2023
  # Updates include: pulling 2021 ACS data and adding a few more variables 
# Metrics from ACS (in common with tract level): 
# * Total population
# * Poverty, child poverty 
# * Median HH Income, Gini income inequality index
# * Educational attainment: HS and more, BA and more
# * Unemployment 
# * Health insurance, and Public health insurance
# * Race/ethnicity: White (NH), Black, Asian, Hispanic, Indigenous, Multiracial, Other
# * Age groups: 0-17, 18-24, 25-64, 65 or more
# * Median personal earnings
# * Net school enrollment
#
# Metrics specific to locality level (from Decennial or ACS):
# * Median HH Income by Race/Ethnicity
# 
# Based on: ACS 2017-2021 
# Geography: Localities in VA's Eastern Shore
#   Accomack County, VA
#   Northampton County, VA
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
# acs_var <- load_variables(2021, "acs5", cache = TRUE)
# acs_var <- load_variables(2021, "acs5/subject", cache = TRUE)
# acs_var <- load_variables(2021, "acs5/profile", cache = TRUE)

# Variable of interest -
##  - Total population -- B01003_001
##  - Poverty rate -- S1701_C03_001
##  - Child poverty rate -- S1701_C03_002
##  - Median HH Income -- S1901_C01_012	
##  - Gini Index of Income Inequality -- B19083_001
##  - Percent high school graduate or higher -- S1501_C02_014
##  - Percent bachelor's degree or higher -- S1501_C02_015
##  - Percent graduate degree or higher -- S1501_C02_013
##  - Percent white alone -- DP05_0077P
##  - Percent black or African American alone -- DP05_0078P
##  - Percent American Indian and Alaska Native alone -- DP05_0079P
##  - Percent Asian alone -- DP05_0080P
##  - Percent Native Hawaiian and Other Pacific Islander alone -- DP05_0081P
##  - Percent Some other race alone -- DP05_0082P
##  - Percent Two or more races -- DP05_0083P
##  - Percent Hispanic or Latino -- DP05_0071P
##  - Percent unemployment (Population 16 and over) -- S2301_C04_001
##  - Percent with health insurance (Civilian noninstitutionalized population) -- S2701_C03_001	
##  - Percent with public health insurance (Civilian noninstitutionalized population) -- S2704_C03_001
##  - Age, population under 18 -- S0101_C02_022
##  - Age, population 18 to 24 -- S0101_C02_023	
##  - Age, 26 to 64 -- sum(S0101_C02_007, S0101_C02_008, S0101_C02_009, S0101_C02_010, S0101_C02_011, S0101_C02_012, S0101_C02_013, S0101_C02_014)
##  - Age, 65 and over --S0101_C02_030
##  - Median HH Income by Race --  B19013B_001 (Black alone),  B19013D_001 (Asian alone), 
##                                 B19013G_001 (Two or more races), B19013H_001 (NH White alone),  
##                                 B19013I_001 (Hispanic)
##  - School enrollment for the population age 3 to 24 -- S1401_C02_014, S1401_C02_016, S1401_C02_018, S1401_C02_020, S1401_C02_022, S1401_C21_024
##  - Median personal earnings of all workers with earnings ages 16 and older -- S2001_C01_002
##  - Percent of cost-burdened renters -- B25070_007+B25070_008+B25070_009+B25070_010/B25070_001
##  - Home ownership rates -- B25003_002/B25003_002
##  - Housing vacant unitss -- B25002_003/B25002_001
##  - Number of households who receive cash public assistance/SNAP benefits -- B19058_002
##  - Number of foreign-born residents -- B05002_013
##  - Number of residents who have a disability -- C18130_003 + C18130_010 + C18130_017


# ....................................................
# 2. Define localities, variables, pull tables ----

# List of desired localities by FIPS
ccode <- read_csv("datacode/county_codes.csv")
ccode <- ccode[1:2,]
region <- ccode$code # list of desired counties
# - 001 Accomack County  
# - 131 Northampton County

# Get Data
# variables: define varlist
varlist_s = c("S1701_C03_001", # povrate
              "S1701_C03_002",   # cpovrate
              "S1901_C01_012",   # hhinc
              "S1501_C02_014",   # hsmore
              "S1501_C02_015",   # bamore
              "S1501_C02_013",   # gradmore
              "S2301_C04_001",   # unemp
              "S2701_C03_001",   # hlthins
              "S2704_C03_001",   # pubins
              "S2001_C01_002")   # earn

varlist_b = c("B01003_001", # totalpop
              "B19083_001",  # gini
              "B25070_007",  # 30-35 rent burdened
              "B25070_008",  # 35-40 rent burdened
              "B25070_009",  # 40-50 rent burdened
              "B25070_010",  # 50+ rent burdened
              "B25070_001",  # all renters
              "B25003_002",  # owner-occupied housing units
              "B25003_001",  # occupied housing units
              "B25002_003",  # vacant housing units
              "B25002_001",  # housing units
              "B19058_002",  # SNAP
              "B05002_013")  # Foreign-born


# Pull variables
county_data_s <- get_acs(geography = "county",
                        variables = varlist_s,
                        state = "VA", 
                        county = region, 
                        survey = "acs5",
                        year = 2021, 
                        output = "wide")

county_data_b <- get_acs(geography = "county",
                        variables = varlist_b,
                        state = "VA", 
                        county = region, 
                        survey = "acs5",
                        year = 2021, 
                        output = "wide")

# rename variables
names(county_data_s) = c("GEOID", "NAME",
                        "povrateE", "povrateM",
                        "cpovrateE", "cpovrateM",
                        "hhincE", "hhincM",
                        "hsmoreE", "hsmoreM",
                        "bamoreE", "bamoreM",
                        "gradmoreE", "gradmoreM",
                        "unempE", "unempM",
                        "hlthinsE", "hlthinsM",
                        "pubinsE", "pubinsM",
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
                        "occhseE", "occhseM",
                        "vachseE", "vachseM",
                        "allhseE", "allhseM",
                        "snapE", "snapM",
                        "foreignbE", "foreignbM")

# Derive some variables
county_data_b <- county_data_b %>% 
  mutate(rentersumE = (rent30E+rent35E+rent40E+rent50E),
         rentersumM = (rent30M/1.645)^2 + (rent35M/1.645)^2 + (rent40M/1.645)^2 + (rent50M/1.645)^2,
         rentersumM = sqrt(rentersumM)*1.645,
         renter30E = round((rentersumE/rentallE)*100,1),
         renter30M = moe_prop(rentersumE, rentallE, rentersumM, rentallM),
         renter30M = round(renter30M*100,1)) %>% 
  mutate(homeownE = round((ownoccE/occhseE)*100,1),
         homeownM = moe_prop(ownoccE, occhseE, ownoccM, occhseM),
         homeownM = round(homeownM*100, 1)) %>% 
  mutate(vacrateE = round((vachseE/allhseE)*100,1),
         vacrateM = moe_prop(vachseE, allhseE, vachseM, allhseM),
         vacrateM = round(vacrateM*100, 1)) %>% 
  select(-c(rentersumE, rentersumM,rent30E:occhseM))

# Derive snap variables
county_data_b <- county_data_b %>% 
  mutate(perc_snaphseE = round((snapE / allhseE)*100,1),
         perc_snaphseM = round(moe_prop(snapE, allhseE, snapM, allhseM), 2),
         .keep = "all")

# Derive foreign born variables
county_data_b <- county_data_b %>% 
  mutate(perc_forbE = round((foreignbE / totalpopE)*100,1),
         perc_forbM = round(moe_prop(foreignbE, totalpopE, foreignbM, totalpopM), 2),
         .keep = "all")


# Get Data
# pull tables (easier to just pull tables separately)
county_race <- get_acs(geography = "county", 
          table = "DP05", 
          state = "VA", 
          county = region, 
          survey = "acs5",
          year = 2021)

county_age <- get_acs(geography = "county", 
          table = "S0101", 
          state = "VA", 
          county = region, 
          survey = "acs5",
          year = 2021)

county_enroll <- get_acs(geography = "county", 
          table = "S1401", 
          state = "VA", 
          county = region, 
          survey = "acs5", 
          year = 2021)

county_disability <- get_acs(geography = "county", 
                            table = "C18130", 
                            state = "VA", 
                            county = region, 
                            survey = "acs5",
                            year = 2021) 


# ....................................................
# 3. Metrics specific to locality level  ----
varlist_race <- c("B19013B_001",  # hhinc_black
                  "B19013D_001",  # hhinc_asion
                  "B19013G_001",  # hhinc_multi
                  "B19013H_001",  # hhinc_white
                  "B19013I_001")  # hhinc_ltnx

county_hhinc_race <- get_acs(geography = "county",
                             variables = varlist_race,
                             state = "VA",
                             county = region,
                             survey = "acs5",
                             year = 2021,
                             output = "wide")

names(county_hhinc_race) = c("GEOID", "NAME",
                         "hhinc_blackE", "hhinc_blackM",
                         "hhinc_asianE", "hhinc_asianM",
                         "hhinc_multiE", "hhinc_multiM",
                         "hhinc_whiteE", "hhinc_whiteM",
                         "hhinc_ltnxE", "hhinc_ltnxM")

# ....................................................
# 4. Reduce and Combine data ----

# Reduce tables: tract_age, tract_race, tract_enroll
# tract_age: three age groups present as rows in the table,
#             one group must be summed
county_age17 <- county_age %>% 
  filter(variable == "S0101_C02_022") %>% 
  rename(age17E = estimate,
         age17M = moe) %>% 
  select(-variable)

county_age24 <- county_age %>% 
  filter(variable == "S0101_C02_023") %>% 
  rename(age24E = estimate,
         age24M = moe) %>% 
  select(-variable)

county_age64 <- county_age %>% 
  filter(variable %in% c("S0101_C02_007", "S0101_C02_008", "S0101_C02_009",
                         "S0101_C02_010", "S0101_C02_011", "S0101_C02_012",
                         "S0101_C02_013", "S0101_C02_014")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(age64E = sum(estimate),
            age64M = moe_sum(moe = moe, estimate = estimate))

county_age65 <- county_age %>% 
  filter(variable == "S0101_C02_030") %>% 
  rename(age65E = estimate,
         age65M = moe) %>% 
  select(-variable)

# tract_race: all groups present as rows in the table
#             some other race and native hawaiian/pacific islander combined
#             due to very small values
county_white <- county_race %>% 
  filter(variable == "DP05_0077P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  select(-variable)

county_black <- county_race %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  select(-variable)

county_indig <- county_race %>% 
  filter(variable == "DP05_0079P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  select(-variable)

county_asian <- county_race %>% 
  filter(variable == "DP05_0080P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  select(-variable)

county_othrace <- county_race %>% 
  filter(variable %in% c("DP05_0081P", "DP05_0082P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate))

county_multi <- county_race %>% 
  filter(variable == "DP05_0083P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  select(-variable)

county_ltnx <- county_race %>% 
  filter(variable == "DP05_0071P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  select(-variable)

# tract_schl: 6 groups (3-4, 5-9, 10-14, 15-17, 18-19, 20-24) must be summed
#             population and enrolled, and divided
county_schl_num <- county_enroll %>% 
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

county_schl_den <- county_enroll %>% 
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

county_schl_ratio <- left_join(county_schl_num, county_schl_den)

county_schl <- county_schl_ratio %>% 
  summarize(schlE = round((schl_num/schl_den)*100, 1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1))

county_dis <- county_disability %>%
  filter(variable == "C18130_003" | variable == "C18130_010" | variable == "C18130_017") %>%
  group_by(GEOID, NAME) %>% 
  summarize(disability_numE = sum(estimate), 
            disability_numM = moe_sum(moe = moe, estimate = estimate))


# Combine indicators
# joining columns
county_data <- county_data_s %>% 
  left_join(county_data_b) %>% 
  left_join(county_schl) %>% 
  left_join(county_white) %>% 
  left_join(county_black) %>% 
  left_join(county_indig) %>% 
  left_join(county_asian) %>% 
  left_join(county_othrace) %>% 
  left_join(county_multi) %>% 
  left_join(county_ltnx) %>% 
  left_join(county_age17) %>% 
  left_join(county_age24) %>% 
  left_join(county_age64) %>% 
  left_join(county_age65) %>% 
  left_join(county_hhinc_race) %>%
  left_join(county_dis)

county_data <- county_data %>% 
  mutate(geoid = GEOID,
         year = "2021") %>% 
  separate(geoid, into = c("state", "locality"), 
           sep = c(2)) %>% 
  select(GEOID, NAME, year, totalpopE, totalpopM, whiteE, whiteM, blackE, blackM, asianE, asianM, indigE, indigM, othraceE, othraceM, multiE, multiM, ltnxE, ltnxM, everything())


# ....................................................
# 5. Summarize/Examine indicators ----
county_data %>% select_at(vars(ends_with("E"))) %>% summary()


# ....................................................
# 6. Save ----
# save rcaa_recap and rcaa_recap_geo
saveRDS(county_data, file = "data/county_data.RDS") 



