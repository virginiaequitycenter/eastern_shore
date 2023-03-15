####################################################
# Eastern Shore Virginia Equity Atlas
####################################################
# Original script written by Lee LeBoeuf
# adapted for Eastern Shore by Chris Barber
# Acquire Additional Tract-Level data: for post-2020 census tracts
# Last updated: 01/23/2023
# New data has not been published since the 2010-2015 estimates as of 07/17/2022
# * Segregation measures (from ACS data, but with more derivation)
#
# Geography: Tracts in Localities in VA's Eastern Shore
#     Accomack County, VA
#     Northampton County, VA
####################################################
# 1. Load libraries
# 2. Segregation measures
# 3. ADD Small-area life expectancy estimates if UPDATED
####################################################

# ....................................................
# 1. Load libraries, provide api key (if needed), identify localities ----

# Load libraries
library(tidyverse)
library(tidycensus)

ccode <- read_csv("datacode/county_codes.csv")
ccode <- ccode[1:2,]  
region <-ccode$code # list of desired counties
# - 001 Accomack County  
# - 131 Northampton County

# ....................................................
# 2. Segregation measures ----
# a. acquire block group data ----
race_table <-get_acs(geography = "block group", 
                       year = 2021, 
                       state = "VA",
                       county = region, 
                       table = "B03002", 
                       survey = "acs5",
                       geometry = F, 
                       output="wide", 
                       cache_table = T)

# rename
seg_blkgrp <- race_table %>%
  mutate(white = B03002_003E,
         black = B03002_004E,
         asian = B03002_006E,
         indig = B03002_005E,
         other = B03002_007E + B03002_008E,
         multi = B03002_009E,
         hisp = B03002_012E, 
         total = B03002_001E,
         year = 2021,
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5),
         tract = substr(GEOID, 6,11),
         blkgrp = substr(GEOID, 12, 12)) %>% 
  select(GEOID, white, black, indig, asian, other, multi, hisp, total, year, state, county, tract, blkgrp) 

# b. acquire tract data ----
race_table <- get_acs(geography = "tract", 
                        year = 2021, 
                        state = "VA",
                        county = region, 
                        table = "B03002", 
                        survey = "acs5",
                        geometry = F, 
                        output="wide", 
                        cache_table = T)

# rename
seg_tract <- race_table %>%
  mutate(cowhite = B03002_003E,
         coblack = B03002_004E,
         coasian = B03002_006E,
         coindig = B03002_005E,
         coother = B03002_007E + B03002_008E,
         comulti = B03002_009E,
         cohisp = B03002_012E, 
         cototal = B03002_001E,
         year = "2021",
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5),
         tract = substr(GEOID, 6,11)) %>% 
  select(GEOID, cowhite, coblack, coindig, coasian, coother, comulti, cohisp, cototal, year, state, county, tract) 


# c. Limit to region and derive metrics ----
# nice explanations for segregation measures: 
# https://sejdemyr.github.io/r-tutorials/statistics/measuring-segregation.html
# https://rstudio-pubs-static.s3.amazonaws.com/473785_e782a2a8458d4263ba574c7073ca5057.html

# add county totals
seg_blkgrp <- left_join(seg_blkgrp, seg_tract, by=c("county", "tract"))

# generate seg measures
dissim_wb <- seg_blkgrp %>%
  mutate(d.wb = abs(white/cowhite - black/coblack)) %>%
  group_by(county, tract) %>%
  summarise(dissim_wb = .5*sum(d.wb, na.rm=T))

dissim_wh <- seg_blkgrp %>%
  mutate(d.wh = abs(white/cowhite - hisp/cohisp)) %>%
  group_by(county, tract) %>%
  summarise(dissim_wh = .5*sum(d.wh, na.rm=T))

inter_bw <- seg_blkgrp %>%
  mutate(int.bw=(black/coblack * white/total))%>%
  group_by(county, tract) %>%
  summarise(inter_bw= sum(int.bw, na.rm=T))

inter_hw <- seg_blkgrp %>%
  mutate(int.hw=(hisp/cohisp * white/total))%>%
  group_by(county, tract) %>%
  summarise(inter_hw= sum(int.hw, na.rm=T))

isol_b <- seg_blkgrp %>%
  mutate(isob=(black/coblack * black/total) )%>%
  group_by(county, tract) %>%
  summarise(iso_b = sum(isob, na.rm=T))

isol_h <- seg_blkgrp %>%
  mutate(isoh=(hisp/cohisp * hisp/total)) %>%
  group_by(county, tract) %>%
  summarise(iso_h = sum(isoh, na.rm=T))

seg_tract <- dissim_wb %>% 
  left_join(dissim_wh) %>% 
  left_join(inter_bw) %>% 
  left_join(inter_hw) %>% 
  left_join(isol_b) %>% 
  left_join(isol_h)
# could estimate spatial segregation with seg package as well

# round
seg_tract <- seg_tract %>% 
  mutate_if(is.numeric, round, 3) 

# check
summary(seg_tract)
pairs(seg_tract[3:8])

# d. save ----
saveRDS(seg_tract, file = "data/seg_tract.RDS")


# # ....................................................
# # 3. Small-area life expectancy estimates ----
# # a. acquire ----
# if (!dir.exists("data/tempdata")){
# dir.create("data/tempdata")}

# url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/VA_A.CSV"
# download.file(url, destfile="../tempdata/va_usasleep.csv", method="libcurl")
# 
# # read data and rename
# lifeexp <- read_csv("../tempdata/va_usasleep.csv")
# names(lifeexp) <- c("geoid", "state", "county", "tract", "life_exp", "se", "flag")
# 
# # b. Limit to region and derive metrics ----
# lifeexp <- lifeexp %>% 
#   filter(county %in% region) %>% # 5 missing tracts (80 of 85)
#   rename(lifeexpE = life_exp,
#          locality = county) %>% 
#   mutate(lifeexpM = 1.64*se,
#          year = "2019") %>% 
#   select(-se, -flag)
# 
# # check
# summary(lifeexp)
# 
# # c. save ----
# saveRDS(lifeexp, file = "../data/tract_life_exp.RDS") 
# # life_exp <- readRDS("../data/tract_life_exp.RDS")

