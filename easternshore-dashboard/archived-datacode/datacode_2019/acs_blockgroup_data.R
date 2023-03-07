####################################################
# Eastern Shore Profile
####################################################
# Acquire ACS data
# Last updated: 03/01/2021
# Metrics from ACS (in common with locality level): 
# * Total population
# * Poverty, child poverty 
# * Median HH Income, Gini income inequality index
# * Educational attainment: HS and more, BA and more, grad/prof and more
# * Unemployment 
# * Health insurance, and Public health insurance
# * Race/ethnicity: White (NH), Black, Asian, Hispanic, Indigenous, Multiracial, Other
# * Age groups: 0-17, 18-24, 25-64, 65 or more
# * Median personal earnings
# * Net school enrollment
#
# Based on: ACS 2015-2019 
# Geography: Accomack, Northhampton
####################################################
# 1. Load libraries, provide api key (if needed), identify variables
# 2. Define variables, pull data
# # 3. Metrics specific to locality level 
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
# acs_var <- load_variables(2017, "acs5", cache = TRUE)
# acs_var <- load_variables(2017, "acs5/subject", cache = TRUE)
# acs_var <- load_variables(2017, "acs5/profile", cache = TRUE)
# dec_var <- load_variables(2010, "sf1", cache = TRUE)

# Variable of interest -
##  - Total population -- B01003_001
##  - Median HH Income -- B19013	
##  - Percent high school graduate or higher -- B15003
##  - Percent bachelor's degree or higher -- B15003
##  - Percent master's degree or higher -- B15003
##  - Percent white alone -- B03002
##  - Percent black or African American alone -- B03002
##  - Percent American Indian and Alaska Native alone -- B03002
##  - Percent Asian alone -- B03002
##  - Percent Native Hawaiian and Other Pacific Islander alone -- B03002
##  - Percent Some other race alone -- B03002
##  - Percent Two or more races -- B03002
##  - Percent Hispanic or Latino -- B03002
##  - Percent unemployment (Population 16 and over) -- B23025
##  - Percent with health insurance (Civilian noninstitutionalized population) -- B27010
##  - Percent with public health insurance (Civilian noninstitutionalized population) -- B27010
##  - Age, population under 18 -- B01001
##  - Age, population 18 to 24 -- B01001	
##  - Age, 26 to 64 -- B01001
##  - Age, 65 and over -- B01001
##  - Median personal earnings of all workers with earnings ages 16 and older -- B20002
##  - Percent of cost-burdened renters -- B25070_007+B25070_008+B25070_009+B25070_010/B25070_001
##  - Housing vacant unitss -- B25002_003/B25002_001
##  - Home ownership rates -- B25003_002/B25003_002

##  - Poverty rate -- NOT AVAILABLE
##  - Child poverty rate -- NOT AVAILABLE
##  - Gini Index of Income Inequality -- NOT AVAILABLE
##  - School enrollment for the population age 3 to 24 -- NOT AVAILABLE



# ....................................................
# 2. Define localities, variables, pull tables ----

# List of desired localities by FIPS
ccode <- read_csv("datacode/county_codes.csv")
ccode <- ccode %>% mutate(
  code = as.character(code),
  code = str_pad(code, width=3, side="left", pad = "0")
)
region <- ccode$code # list of desired counties
# - 001 Accomakc County  
# - 131 Northampton County

# region <- str_pad(as.character(ccode$code), width = 3, pad = "0") # list of desired counties


# Get Data
# variables: define varlist
varlist_b = c("B01003_001",  # totalpop
              "B19013_001",  # hhinc
              "B20002_001",  # earn
              "B25070_007",  # 30-35 rent burdened
              "B25070_008",  # 35-40 rent burdened
              "B25070_009",  # 40-50 rent burdened
              "B25070_010",  # 50+ rent burdened
              "B25070_001",  # all renters
              "B25003_002",  # owner-occupied housing units
              "B25003_001",  # occupied housing units
              "B25002_003",  # vacant housing units
              "B25002_001")  # housing units


# Pull variables
blkgrp_data_b <- get_acs(geography = "block group",
                         variables = varlist_b,
                         state = "VA", 
                         county = region, 
                         survey = "acs5",
                         year = 2019, 
                         output = "wide")

# rename variables
names(blkgrp_data_b) = c("GEOID", "NAME",
                         "totalpopE", "totalpopM",
                         "hhincE", "hhincM",
                         "earnE", "earnM",
                         "rent30E", "rent30M",
                         "rent35E", "rent35M",
                         "rent40E", "rent40M",
                         "rent50E", "rent50M",
                         "rentallE", "rentallM",
                         "ownoccE", "ownoccM",
                         "occhseE", "occhseM",
                         "vachseE", "vachseM",
                         "allhseE", "allhseM")
                         
# Derive some variables
blkgrp_data_b <- blkgrp_data_b %>% 
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


# Get Data
# pull tables (easier to just pull tables separately)

# for blkgrp_hs, blkgrp_ba, blkgrp_grad
blkgrp_educ <- get_acs(geography = "block group", 
          table = "B15003", 
          state = "VA", 
          county = region, 
          survey = "acs5",
          year = 2019)

# for race
blkgrp_race <- get_acs(geography = "block group", 
          table = "B03002", 
          state = "VA", 
          county = region, 
          survey = "acs5",
          year = 2019)

# for blkgrp_unemp
blkgrp_emp <- get_acs(geography = "block group", 
          table = "B23025", 
          state = "VA", 
          county = region, 
          survey = "acs5",
          year = 2019)

# for blkgrp_hlthins, blkgrp_pubins
blkgrp_insur <- get_acs(geography = "block group", 
          table = "B27010", 
          state = "VA", 
          county = region, 
          survey = "acs5",
          year = 2019)

# for age
blkgrp_age <- get_acs(geography = "block group", 
          table = "B01001", 
          state = "VA", 
          county = region, 
          survey = "acs5",
          year = 2019)


# ....................................................
# 3. Reduce and Combine data ----

# Derive from tables
# blkgrp_educ for blkgrp_hs, blkgrp_ba, blkgrp_grad
blkgrp_25over <- blkgrp_educ %>% 
  filter(variable == "B15003_001") %>% select(-variable)

blkgrp_hs <- blkgrp_educ %>% 
  filter(variable %in% c("B15003_017", "B15003_018", "B15003_019",
                         "B15003_020", "B15003_021", "B15003_022",
                         "B15003_023", "B15003_024", "B15003_025")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(hsE = sum(estimate),
            hsM = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_25over) %>% 
  mutate(hsmoreE = round((hsE/estimate)*100, 2),
         hsmoreM = round((moe_prop(hsE, estimate, hsM, moe))*100, 2))  %>% 
  select(-c(hsE, hsM, estimate, moe))

blkgrp_ba <- blkgrp_educ %>% 
  filter(variable %in% c("B15003_022","B15003_023", "B15003_024", "B15003_025")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(baE = sum(estimate),
            baM = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_25over) %>% 
  mutate(bamoreE = round((baE/estimate)*100, 2),
         bamoreM = round((moe_prop(baE, estimate, baM, moe))*100, 2))  %>% 
  select(-c(baE, baM, estimate, moe))

blkgrp_grad <- blkgrp_educ %>% 
  filter(variable %in% c("B15003_023", "B15003_024", "B15003_025")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(maE = sum(estimate),
            maM = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_25over) %>% 
  mutate(gradmoreE = round((maE/estimate)*100, 2),
         gradmoreM = round((moe_prop(maE, estimate, maM, moe))*100, 2))  %>% 
  select(-c(maE, maM, estimate, moe))

# blkgrp_race
blkgrp_tot <- blkgrp_race %>% 
  filter(variable == "B03002_001") %>% select(-variable)

blkgrp_white <- blkgrp_race %>% 
  filter(variable == "B03002_003") %>% 
  rename(whE = estimate,
         whM = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(whiteE = round((whE/estimate)*100, 2),
         whiteM = round((moe_prop(whE, estimate, whM, moe))*100, 2)) %>% 
  select(-c(whE, whM, estimate, moe))

blkgrp_black <- blkgrp_race %>% 
  filter(variable == "B03002_004") %>% 
  rename(blE = estimate,
         blM = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(blackE = round((blE/estimate)*100, 2),
         blackM = round((moe_prop(blE, estimate, blM, moe))*100, 2)) %>% 
  select(-c(blE, blM, estimate, moe))

blkgrp_indig <- blkgrp_race %>% 
  filter(variable == "B03002_005") %>% 
  rename(inE = estimate,
         inM = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(indigE = round((inE/estimate)*100, 2),
         indigM = round((moe_prop(inE, estimate, inM, moe))*100, 2)) %>% 
  select(-c(inE, inM, estimate, moe))

blkgrp_asian  <- blkgrp_race %>% 
  filter(variable == "B03002_006") %>% 
  rename(asE = estimate,
         asM = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(asianE = round((asE/estimate)*100, 2),
         asianM = round((moe_prop(asE, estimate, asM, moe))*100, 2)) %>% 
  select(-c(asE, asM, estimate, moe))

blkgrp_othrace  <- blkgrp_race %>% 
  filter(variable %in% c("B03002_007", "B03002_008")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(otE = sum(estimate),
            otM = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(othraceE = round((otE/estimate)*100, 2),
         othraceM = round((moe_prop(otE, estimate, otM, moe))*100, 2)) %>% 
  select(-c(otE, otM, estimate, moe))

blkgrp_multi  <- blkgrp_race %>% 
  filter(variable == "B03002_009") %>% 
  rename(mlE = estimate,
         mlM = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(multiE = round((mlE/estimate)*100, 2),
         multiM = round((moe_prop(mlE, estimate, mlM, moe))*100, 2)) %>% 
  select(-c(mlE, mlM, estimate, moe))

blkgrp_ltnx  <- blkgrp_race %>% 
  filter(variable == "B03002_012") %>% 
  rename(lxE = estimate,
         lxM = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(ltnxE = round((lxE/estimate)*100, 2),
         ltnxM = round((moe_prop(lxE, estimate, lxM, moe))*100, 2)) %>% 
  select(-c(lxE, lxM, estimate, moe))

# blkgrp_emp for blkgrp_unemp
blkgrp_lab <- blkgrp_emp %>% filter(variable == "B23025_003") %>% select(-variable)

blkgrp_unemp <- blkgrp_emp %>% 
  filter(variable == "B23025_005") %>%
  rename(unE = estimate,
         unM = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_lab) %>% 
  mutate(unempE = round((unE/estimate)*100, 2),
         unempM = round((moe_prop(unE, estimate, unM, moe))*100, 2)) %>% 
  select(-c(unE, unM, estimate, moe))

# blkgrp_insur for for blkgrp_hlthins, blkgrp_pubins
blkgrp_hlthins <- blkgrp_insur %>% 
  filter(variable %in% c("B27010_017", "B27010_033", "B27010_050", "B27010_066")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(nohiE = sum(estimate),
            nohiM = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(hlthinsE = 100-round((nohiE/estimate)*100, 2),
         hlthinsM = round((moe_prop(nohiE, estimate, nohiM, moe))*100, 2))  %>% 
  select(-c(nohiE, nohiM, estimate, moe))

blkgrp_pubins <- blkgrp_insur %>% 
  filter(variable %in% c("B27010_006", "B27010_007", "B27010_009", "B27010_013", 
                         "B27010_022", "B27010_023", "B27010_025", "B27010_029",
                         "B27010_038", "B27010_039", "B27010_041", "B27010_046",
                         "B27010_055", "B27010_057", "B27010_062")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(pubE = sum(estimate),
            pubM = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(pubinsE = round((pubE/estimate)*100, 2),
         pubinsM = round((moe_prop(pubE, estimate, pubM, moe))*100, 2))  %>% 
  select(-c(pubE, pubM, estimate, moe))

# blkgrp_age
blkgrp_age17 <- blkgrp_age %>% 
  filter(variable %in% c("B01001_003", "B01001_004", "B01001_005", "B01001_006",
                         "B01001_027", "B01001_028", "B01001_029", "B01001_030")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(ageE = sum(estimate),
            ageM = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(age17E = round((ageE/estimate)*100, 2),
         age17M = round((moe_prop(ageE, estimate, ageM, moe))*100, 2))  %>% 
  select(-c(ageE, ageM, estimate, moe))

blkgrp_age24 <- blkgrp_age %>% 
  filter(variable %in% c("B01001_007", "B01001_008", "B01001_009", "B01001_010",
                         "B01001_031", "B01001_032", "B01001_033", "B01001_034")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(ageE = sum(estimate),
            ageM = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(age24E = round((ageE/estimate)*100, 2),
         age24M = round((moe_prop(ageE, estimate, ageM, moe))*100, 2))  %>% 
  select(-c(ageE, ageM, estimate, moe))

blkgrp_age64 <- blkgrp_age %>% 
  filter(variable %in% c("B01001_011", "B01001_012", "B01001_013", "B01001_014",
                         "B01001_015", "B01001_016", "B01001_017", "B01001_018",
                         "B1001_019",
                         "B01001_035", "B01001_036", "B01001_037", "B01001_038",
                         "B01001_039", "B01001_040", "B01001_041", "B01001_042",
                         "B01001_043")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(ageE = sum(estimate),
            ageM = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(age64E = round((ageE/estimate)*100, 2),
         age64M = round((moe_prop(ageE, estimate, ageM, moe))*100, 2))  %>% 
  select(-c(ageE, ageM, estimate, moe))

blkgrp_age65 <- blkgrp_age %>% 
  filter(variable %in% c("B01001_020", "B01001_021", "B01001_022", "B01001_023",
                         "B01001_024", "B01001_025", 
                         "B01001_044", "B01001_045", "B01001_046", "B01001_047",
                         "B01001_048", "B01001_049")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(ageE = sum(estimate),
            ageM = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(age65E = round((ageE/estimate)*100, 2),
         age65M = round((moe_prop(ageE, estimate, ageM, moe))*100, 2))  %>% 
  select(-c(ageE, ageM, estimate, moe))


# Combine indicators
# joining columns
blkgrp_data <- blkgrp_data_b %>% 
  left_join(blkgrp_hs) %>% 
  left_join(blkgrp_ba) %>% 
  left_join(blkgrp_grad) %>% 
  left_join(blkgrp_unemp) %>% 
  left_join(blkgrp_hlthins) %>% 
  left_join(blkgrp_pubins) %>% 
  left_join(blkgrp_white) %>% 
  left_join(blkgrp_black) %>% 
  left_join(blkgrp_indig) %>% 
  left_join(blkgrp_asian) %>% 
  left_join(blkgrp_othrace) %>% 
  left_join(blkgrp_multi) %>% 
  left_join(blkgrp_ltnx) %>% 
  left_join(blkgrp_age17) %>% 
  left_join(blkgrp_age24) %>% 
  left_join(blkgrp_age64) %>% 
  left_join(blkgrp_age65) 

blkgrp_data <- blkgrp_data %>% 
  mutate(year = "2019") %>% 
  select(GEOID, NAME, year, totalpopE, totalpopM, whiteE, whiteM, blackE, blackM, asianE, asianM, indigE, indigM, othraceE, othraceM, multiE, multiM, ltnxE, ltnxM, everything())

blkgrp_data <- blkgrp_data %>% 
  mutate(geoid = GEOID) %>% 
  separate(geoid, into = c("state", "locality", "tract", "blockgroup"), 
           sep = c(2,5,11)) 

# ....................................................
# 4. Summarize/Examine indicators ----
blkgrp_data %>% select_at(vars(ends_with("E"))) %>% summary()


# ....................................................
# 5. Save ----
saveRDS(blkgrp_data, file = "data/blkgrp_data.RDS") 
# blkgrp_data <- readRDS("data/blkgrp_data.RDS")
