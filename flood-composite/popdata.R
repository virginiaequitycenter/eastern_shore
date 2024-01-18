# Flood risk composite app
# Potential population data
# Block group (wide) and county (long) aggregates
# 2023-11-18 mpc


## Libraries ----
library(tidyverse)
library(tidycensus)
library(lehdr)


## ACS locality, year ----
fips <- c("001", "131")
year <- 2022

### Block group ----
# to show variation
# structured as a wide table

#### race ----
blkgrp_race <- get_acs(geography = "block group", 
                      table = "B03002", 
                      state = "VA", 
                      county = fips, 
                      survey = "acs5",
                      year = year)

# derive race/ethnicity
blkgrp_tot <- blkgrp_race %>% 
  filter(variable == "B03002_001") %>% 
  select(-variable) %>% 
  rename(totpop_est = estimate,
         totpop_moe = moe)

blkgrp_white <- blkgrp_race %>% 
  filter(variable == "B03002_003") %>% 
  rename(whitepop_est = estimate,
         whitepop_moe = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(whiteper_est = round((whitepop_est/totpop_est)*100, 2),
         whiteper_moe = round((moe_prop(whitepop_est, totpop_est, whitepop_moe, totpop_moe))*100, 2)) %>% 
  select(GEOID, NAME, totpop_est, totpop_moe, everything())

blkgrp_black <- blkgrp_race %>% 
  filter(variable == "B03002_004") %>% 
  rename(blackpop_est = estimate,
         blackpop_moe = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(blackper_est = round((blackpop_est/totpop_est)*100, 2),
         blackper_moe = round((moe_prop(blackpop_est, totpop_est, blackpop_moe, totpop_moe))*100, 2)) %>% 
  select(-c(totpop_est, totpop_moe))

blkgrp_ltnx  <- blkgrp_race %>% 
  filter(variable == "B03002_012") %>% 
  rename(ltnxpop_est = estimate,
         ltnxpop_moe = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(ltnxper_est = round((ltnxpop_est/totpop_est)*100, 2),
         ltnxper_moe = round((moe_prop(ltnxpop_est, totpop_est, ltnxpop_moe, totpop_moe))*100, 2)) %>% 
  select(-c(totpop_est, totpop_moe))

blkgrp_remain <- blkgrp_race %>% 
  filter(variable %in% c("B03002_005", "B03002_006", "B03002_007", "B03002_008", "B03002_009")) %>% 
  select(-variable) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(remainpop_est = sum(estimate),
            remainpop_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(remainper_est = round((remainpop_est/totpop_est)*100, 2),
         remainper_moe = round((moe_prop(remainpop_est, totpop_est, remainpop_moe, totpop_moe))*100, 2)) %>% 
  select(-c(totpop_est, totpop_moe))

race <- blkgrp_white %>% 
  left_join(blkgrp_black) %>% 
  left_join(blkgrp_ltnx) %>% 
  left_join(blkgrp_remain)

rm(blkgrp_race, blkgrp_white, blkgrp_black, blkgrp_ltnx, blkgrp_remain)

#### age ----
# v22 <- load_variables(year = 2022, dataset = "acs5", cache = TRUE)

blkgrp_age <- get_acs(geography = "block group", 
                      table = "B01001", 
                      state = "VA", 
                      county = fips, 
                      survey = "acs5",
                      year = year)

blkgrp_age17 <- blkgrp_age %>% 
  filter(variable %in% c("B01001_003", "B01001_004", "B01001_005", "B01001_006",
                         "B01001_027", "B01001_028", "B01001_029", "B01001_030")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(age17pop_est = sum(estimate),
            age17pop_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(age17per_est = round((age17pop_est/totpop_est)*100, 2),
         age17per_moe = round((moe_prop(age17pop_est, totpop_est, age17pop_moe, totpop_moe))*100, 2))  %>% 
  select(-c(totpop_est, totpop_moe))

blkgrp_age65 <- blkgrp_age %>% 
  filter(variable %in% c("B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
                         "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(age65pop_est = sum(estimate),
            age65pop_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(age65per_est = round((age65pop_est/totpop_est)*100, 2),
         age65per_moe = round((moe_prop(age65pop_est, totpop_est, age65pop_moe, totpop_moe))*100, 2))  %>% 
  select(-c(totpop_est, totpop_moe))

blkgrp_age18to64 <- blkgrp_age %>% 
  filter(variable %in% c("B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", 
                         "B01001_012", "B01001_013", "B01001_014", "B01001_015", "B01001_016", 
                         "B01001_017", "B01001_018", "B01001_019",
                         "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", 
                         "B01001_036", "B01001_037", "B01001_038", "B01001_039", "B01001_040",
                         "B01001_041", "B01001_042","B01001_043")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(age18to64pop_est = sum(estimate),
            age18to64pop_moe = moe_sum(moe = moe, estimate = estimate)) %>% 
  left_join(blkgrp_tot) %>% 
  mutate(age18to64per_est = round((age18to64pop_est/totpop_est)*100, 2),
         age18to64per_moe = round((moe_prop(age18to64pop_est, totpop_est, age18to64pop_moe, totpop_moe))*100, 2))  %>% 
  select(-c(totpop_est, totpop_moe))

age <- blkgrp_age17 %>% 
  left_join(blkgrp_age65) %>% 
  left_join(blkgrp_age18to64) 

rm(blkgrp_age, blkgrp_age17, blkgrp_age65, blkgrp_tot, blkgrp_age18to64)

#### tenure ----
blkgrp_ten <- get_acs(geography = "block group", 
                      table = "B25003", 
                      state = "VA", 
                      county = fips, 
                      survey = "acs5",
                      year = year)

blkgrp_hh <- blkgrp_ten %>% 
  filter(variable == "B25003_001") %>% 
  select(-variable) %>% 
  rename(tothh_est = estimate,
         tothh_moe = moe)

blkgrp_ownhh <- blkgrp_ten %>% 
  filter(variable == "B25003_002") %>% 
  rename(ownhh_est = estimate,
         ownhh_moe = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_hh) %>% 
  mutate(ownhhper_est = round((ownhh_est/tothh_est)*100, 2),
         ownhhper_moe = round((moe_prop(ownhh_est, tothh_est, ownhh_moe, tothh_moe))*100, 2)) %>% 
  select(GEOID, NAME, tothh_est, tothh_moe, everything())

blkgrp_renthh <- blkgrp_ten %>% 
  filter(variable == "B25003_003") %>% 
  rename(renthh_est = estimate,
         renthh_moe = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_hh) %>% 
  mutate(renthhper_est = round((renthh_est/tothh_est)*100, 2),
         renthhper_moe = round((moe_prop(renthh_est, tothh_est, renthh_moe, tothh_moe))*100, 2)) %>% 
  select(-c(tothh_est, tothh_moe))

tenure <- blkgrp_ownhh %>% 
  left_join(blkgrp_renthh) 

rm(blkgrp_ten, blkgrp_ownhh, blkgrp_renthh)

#### online ----
blkgrp_online <- get_acs(geography = "block group", 
                      table = "B28003", 
                      state = "VA", 
                      county = fips, 
                      survey = "acs5",
                      year = year)

online <- blkgrp_online %>% 
  filter(variable == "B28003_004") %>% 
  rename(onlinehh_est = estimate,
         onlinehh_moe = moe) %>% 
  select(-variable) %>% 
  left_join(blkgrp_hh) %>% 
  mutate(onlineper_est = round((onlinehh_est/tothh_est)*100, 2),
         onlineper_moe = round((moe_prop(onlinehh_est, tothh_est, onlinehh_moe, tothh_moe))*100, 2)) %>% 
  select(-c(tothh_est, tothh_moe))

rm(blkgrp_online, blkgrp_hh)

#### age of housing stock ----
blkgrp_bldgage <- get_acs(geography = "block group", 
                           table = "B25034", 
                           state = "VA", 
                           county = fips, 
                           survey = "acs5",
                           year = year)

blkgrp_bldg <- blkgrp_bldgage %>% 
  filter(variable == "B25034_001") %>% 
  select(-variable) %>% 
  rename(totbldg_est = estimate,
         totbldg_moe = moe)

bldgage <- blkgrp_bldgage %>% 
  filter(variable %in% c("B25034_008", "B25034_009", "B25034_010", "B25034_011")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(bldgage70_est = sum(estimate),
            bldgage70_moe = moe_sum(moe, estimate)) %>% 
  left_join(blkgrp_bldg) %>% 
  mutate(bldgage70per_est = round((bldgage70_est/totbldg_est)*100, 2),
         bldgage70per_moe = round((moe_prop(bldgage70_est, totbldg_est, bldgage70_moe, totbldg_moe))*100, 2)) %>% 
  select(GEOID, NAME, totbldg_est, totbldg_moe, everything()) %>% 
  ungroup()

rm(blkgrp_bldg, blkgrp_bldgage)

#### money ----
# median hh income, gross rent, home value
varlist = c(medhhinc = "B19013_001",  # hhinc
            medrent = "B25064_001", # median gross rent
            medhome = "B25077_001") # median home value

money <- get_acs(geography = "block group",
                         variables = varlist,
                         state = "VA", 
                         county = fips, 
                         survey = "acs5",
                         year = year, 
                         output = "wide") %>% 
  select(-NAME) %>% 
  rename_with(~str_replace(., "M$", "_moe")) %>% 
  rename_with(~str_replace(., "E$", "_est"))


#### join ----
blkgrp_data <- race %>% 
  left_join(age) %>% 
  left_join(online) %>% 
  left_join(tenure) %>% 
  left_join(bldgage) %>% 
  left_join(money)

rm(race, age, online, tenure, bldgage, money)

# citizenship status not available by block group
# language status not available by block group
# migration status not availabley by block group
# disability status not available by block group

summary(blkgrp_data %>% select(ends_with("est")))


### County ----
# to summarize
# structured as a long table

#### race ----
county_race <- get_acs(geography = "county", 
                       variables = c(whitepop = "B03002_003",
                                     blackpop = "B03002_004",
                                     ltnxpop = "B03002_012"), 
                       state = "VA", 
                       county = fips, 
                       survey = "acs5",
                       year = year,
                       summary_var = "B03002_001")

county_race_remain <- county_race %>% 
  group_by(GEOID, NAME) %>% 
  summarize(popestimate = sum(estimate), summary_est = median(summary_est)) %>% 
  mutate(estimate = summary_est - popestimate,
         variable = "remainpop") %>% 
  select(-popestimate)

county_race <- bind_rows(county_race, county_race_remain) %>% 
  select(GEOID, NAME, variable, estimate, totalpop = summary_est) %>% 
  mutate(demographic = "race")

rm(county_race_remain)

#### age ----
county_age17 <- get_acs(geography = "county", 
                      variables = c("B01001_003", "B01001_004", "B01001_005", "B01001_006",
                                    "B01001_027", "B01001_028", "B01001_029", "B01001_030"), 
                      state = "VA", 
                      county = fips, 
                      survey = "acs5",
                      year = year, 
                      summary_var = "B01001_001") %>% 
  group_by(GEOID, NAME) %>% 
  summarize(estimate = sum(estimate), summary_est = median(summary_est)) %>% 
  mutate(variable = "age17pop")

county_age65 <- get_acs(geography = "county", 
                        variables = c("B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
                                      "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"), 
                        state = "VA", 
                        county = fips, 
                        survey = "acs5",
                        year = year, 
                        summary_var = "B01001_001") %>% 
  group_by(GEOID, NAME) %>% 
  summarize(estimate = sum(estimate), summary_est = median(summary_est)) %>% 
  mutate(variable = "age65pop")

county_age <- bind_rows(county_age17, county_age65)

county_age_remain <- county_age %>% 
  group_by(GEOID, NAME) %>% 
  summarize(popestimate = sum(estimate), summary_est = median(summary_est)) %>% 
  mutate(estimate = summary_est - popestimate,
         variable = "age18to64pop") %>% 
  select(-popestimate)

county_age <- bind_rows(county_age, county_age_remain) %>% 
  rename(totalpop = summary_est) %>% 
  mutate(demographic = "age")

rm(county_age_remain, county_age17, county_age65)

#### tenure ----
county_ten <- get_acs(geography = "county", 
                      variables = c(ownhh = "B25003_002", renthh = "B25003_003"), 
                      state = "VA", 
                      county = fips, 
                      survey = "acs5",
                      year = year,
                      summary_var = "B25003_001") %>% 
  select(GEOID, NAME, variable, estimate, totalhh = summary_est) %>% 
  mutate(demographic = "own_rent")

#### online ----
county_online <- get_acs(geography = "county", 
                         variable = c(comp_and_bb = "B28003_004"), 
                         state = "VA", 
                         county = fips, 
                         survey = "acs5",
                         year = year,
                         summary_var = "B28003_001")

county_notonline <- county_online %>% 
  group_by(GEOID, NAME) %>% 
  mutate(estimate = summary_est - estimate,
         variable = "not_comp_bb")
  
county_online <- bind_rows(county_online, county_notonline) %>% 
  select(GEOID, NAME, variable, estimate, totalhh = summary_est) %>% 
  mutate(demographic = "online")

#### age of housing stock, pre 1970 ----
county_bldg <- get_acs(geography = "county", 
                      variables = c(bldg60 = "B25034_008", bldg50 = "B25034_009",
                                    bldg40 = "B25034_010", bldg30 = "B25034_011"), 
                      state = "VA", 
                      county = fips, 
                      survey = "acs5",
                      year = year,
                      summary_var = "B25034_001") 

county_bldgage <- county_bldg %>% 
  select(GEOID, NAME, variable, estimate, totalbldg = summary_est) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(estimate = sum(estimate),
            totalbldg = first(totalbldg)) %>% 
  ungroup() %>% 
  mutate(demographic = "housing_age",
         variable = "bldgpre70")

county_bldgage2 <- county_bldg %>% 
  select(GEOID, NAME, variable, estimate, totalbldg = summary_est) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(bldgpre70 = sum(estimate),
            totalbldg = first(totalbldg)) %>% 
  ungroup() %>% 
  mutate(estimate = totalbldg - bldgpre70, 
         demographic = "housing_age",
         variable = "bldgpost70") %>% 
  select(-bldgpre70)

county_bldgage <- bind_rows(county_bldgage, county_bldgage2)

#### median hh income, gross rent, home value ----
varlist = c(medhhinc = "B19013_001",  # hhinc
            medrent = "B25064_001", # median gross rent
            medhome = "B25077_001") # median home value

county_money <- get_acs(geography = "county",
                 variables = varlist,
                 state = "VA", 
                 county = fips, 
                 survey = "acs5",
                 year = year, 
                 output = "tidy") %>% 
  mutate(demographic = case_when(variable == "medhhinc" ~ "income", 
                                 variable == "medrent" ~ "rent_cost",
                                 variable == "medhome" ~ "housing_value")) %>% 
  select(-moe)

#### join ----
county_data <- bind_rows(county_race, county_age, county_online, 
                         county_ten, county_bldgage, county_money)
county_data <- county_data %>% 
  select(GEOID, NAME, demographic, variable, estimate, totalpop, totalhh, totalbldg)

rm(county_age, county_money, county_online, county_notonline, county_race, county_ten)


## LODES ----
# low-income employment (LODES)
# number of low income employees and all employees by place of residence, by place of work
# using lehdr: https://cran.rstudio.com/web/packages/lehdr/index.html

### Block group summaries ----
work_bg <- grab_lodes(state = "va", 
                    year = 2020, 
                    version = "LODES8", 
                    lodes_type = "wac", 
                    job_type = "JT00",
                    segment = "S000", 
                    state_part = "main", 
                    agg_geo = "bg") %>% 
  mutate(w_county3 = str_sub(w_bg, 3, 5)) %>% 
  filter(w_county3 %in% fips) %>%
  select(GEOID = w_bg,
         jobs_total_w = C000, jobs_lowage_w = CE01) %>% 
  mutate(percent_lowage_w = (jobs_lowage_w / jobs_total_w)*100 )

home_bg <- grab_lodes(state = "va", 
                      year = 2020, 
                      version = "LODES8", 
                      lodes_type = "rac", 
                      job_type = "JT00",
                      segment = "S000", 
                      state_part = "main", 
                      agg_geo = "bg") %>% 
  mutate(h_county3 = str_sub(h_bg, 3, 5)) %>% 
  filter(h_county3 %in% fips) %>%
  select(GEOID = h_bg,
         jobs_total_h = C000, jobs_lowage_h = CE01) %>% 
  mutate(percent_lowage_h = (jobs_lowage_h / jobs_total_h)*100 )

### County summaries ----
# number of low, mid, and high income employees and all employees by place of residence, by place of work
work_county <- grab_lodes(state = "va", 
                   year = 2020, 
                   version = "LODES8", 
                   lodes_type = "wac", 
                   job_type = "JT00",
                   segment = "S000", 
                   state_part = "main", 
                   agg_geo = "county") %>% 
  mutate(w_county3 = str_sub(w_county, 3, 5)) %>% 
  filter(w_county3 %in% fips) %>%
  select(GEOID = w_county, 
         total_jobs = C000, jobs_lowage_w = CE01, jobs_midwage_w = CE02, jobs_hiwage_w = CE03) %>% 
  pivot_longer(jobs_lowage_w:jobs_hiwage_w, names_to = "variable", values_to = "estimate") %>% 
  mutate(demographic = "jobs",
         NAME = ifelse(GEOID == "51001", "Accomack County, Virginia", "Northampton County, Virginia"))

home_county <- grab_lodes(state = "va", 
                      year = 2020, 
                      version = "LODES8", 
                      lodes_type = "rac", 
                      job_type = "JT00",
                      segment = "S000", 
                      state_part = "main", 
                      agg_geo = "county") %>% 
  mutate(h_county3 = str_sub(h_county, 3, 5)) %>% 
  filter(h_county3 %in% fips) %>%
  select(GEOID = h_county,
         total_jobs = C000, jobs_lowage_h = CE01, jobs_midwage_h = CE02, jobs_hiwage_h = CE03) %>% 
  pivot_longer(jobs_lowage_h:jobs_hiwage_h, names_to = "variable", values_to = "estimate") %>% 
  mutate(demographic = "jobs",
         NAME = ifelse(GEOID == "51001", "Accomack County, Virginia", "Northampton County, Virginia"))


## Join ACS and LODES ----
blkgrp_data <- blkgrp_data %>% 
  left_join(work_bg) %>% 
  left_join(home_bg)

county_data <- bind_rows(county_data, work_county, home_county)


## Save ----
write_csv(blkgrp_data, "population_blkgrp.csv")
write_csv(county_data, "population_county.csv")
