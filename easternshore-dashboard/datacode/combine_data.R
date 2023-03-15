####################################################
# Eastern Shore Virginia Equity Atlas
####################################################
# Combine data for shiny app
# Last updated: 03/15/2023
####################################################
# 1. Load libraries 
# 2. Load data
# 3. Merge tract, county data
# 4. Add county names var
# 5. Add geography 
# 6. Join variable attributes
# 7. Define color palettes
# 8. Save for app
####################################################


# ....................................................
# 1. Load libraries and data ----
# Libraries
library(tidyverse)
library(RColorBrewer)
library(googlesheets4)
library(sf)
library(tools)
library(tigris)
options(tigris_use_cache = TRUE)
library(sp)
library(geosphere)


# function to move variables to end
move_last <- function(DF, last_col) {
  match(c(setdiff(names(DF), last_col), last_col), names(DF))
}


# ....................................................
# 2. Load data ----
# block group data
blkgrp_data <- readRDS("data/blkgrp_data.RDS")

# tract level ACS
tract_data <- readRDS("data/tract_data.RDS")
seg_tract <- readRDS("data/seg_tract.RDS")

# county level ACS
county_data <- readRDS("data/county_data.RDS")
lifeexp_county <- readRDS("data/county_life_exp.RDS") 
lifeexp_county <- lifeexp_county %>% select(-year)
# not merging by year any longer; should probably remove from addl_county_data.R
seg_county <- readRDS("data/seg_county.RDS")

# points and polygons
parks_sf <- st_read("data/parks_OSM_sf.geojson") 
schools_sf <- st_read("data/schools_sf.geojson") # may want to segment by type (public, private)
sabselem_sf <- st_read("data/sabselem_sf.geojson")
sabshigh_sf <- st_read("data/sabshigh_sf.geojson")
mcd_sf <- st_read("data/mcd_sf.geojson") 
# other files as needed: polygons and points

ccode <- read_csv("datacode/county_codes.csv")
ccode <- ccode[1:2,]
ccode <- ccode %>% mutate(
  code = as.character(code),
  code = str_pad(code, width = 3, side = "left", pad = "0")
  )
region <- ccode$code # list of desired counties

# Tract names 
# Currently using tract name data from /tracts
tractnames <- readRDS("data/tractnames.RDS")
tractnames <- tractnames %>%
  select(c("TRACTCE", "locality", "GEOID", "names")) %>%
  st_drop_geometry() %>%
  rename(tract = TRACTCE, count = locality, tractnames = names)
tractnames$GEOID <- as.character(tractnames$GEOID)


# variable metadata/attributes
# pretty table contains better variable lables, sources, and descriptions
# gs_auth(new_user = TRUE)
# googlesheets4::gs4_deauth()

url_sheet <- "https://docs.google.com/spreadsheets/d/1Fi1sHsWcYOYKL7lzgySxzlz0WqGxGwMGGoYKiYAZtTs/edit?usp=sharing"
pretty <- googlesheets4::read_sheet(url_sheet, sheet = "acs_tract")
pretty$goodname <- toTitleCase(pretty$description)

pretty2 <- googlesheets4::read_sheet(url_sheet, sheet = "acs_county")
pretty2$goodname <- toTitleCase(pretty2$description)

pretty3 <- googlesheets4::read_sheet(url_sheet, sheet = "acs_blockgroup")
pretty3$goodname <- toTitleCase(pretty3$description)


# ....................................................
# 3. Merge tract, county data, derive HDI ----
# a. Merge tract data ----

# add segregation measures by county
tract_data <- tract_data %>% 
  left_join(seg_tract, by = c("locality" = "county", "tract" = "tract")) %>% 
  select(move_last(., c("state", "locality", "tract")))

# add tract names 
tract_data <- tract_data %>%
left_join(tractnames, by = c("GEOID" = "GEOID", "tract" = "tract")) %>%
  select(move_last(., c("state", "locality", "tract")))

# add tract names to block group (temporary solution without block group names)
blkgrp_data <- blkgrp_data %>% 
  left_join(tractnames, by = "tract") %>% 
  select(move_last(., c("state", "locality", "tract")))

# rm GEOID.y and rename GEOID.x
blkgrp_data <- blkgrp_data %>% 
  select(-GEOID.y) %>% 
  rename(GEOID = GEOID.x)

# b. Merge county data ----

# add life expectancy by county
county_data <- county_data %>% 
  left_join(lifeexp_county, by = c("GEOID" = "FIPS")) %>% 
  rename(locality = "locality.x") %>% select(-locality.y) %>% 
  select(move_last(., c("state", "locality"))) 

# add segregation measures by county
county_data <- county_data %>% 
  left_join(seg_county, by = c("locality" = "county", "year" = "year")) %>% 
  select(move_last(., c("state", "locality")))

# # Generate HDI measure for County
# #.  function of school enrollment, educ attainment; life expectancy; median personal earnings
# #   "goalposts" defined in methodology: http://measureofamerica.org/Measure_of_America2013-2014MethodNote.pdf
# #   earnings goalposts are adjusted for inflation -- set to 2015 values
# 
# county_data <- county_data %>% 
#   mutate(hlth_index = ( (lifeexpE-66) / (90-66) * 10),
#          inc_index = ( (log(earnE)-log(15776.86)) / (log(66748.26)-log(15776.86)) * 10),
#          attain_index = ( (((hsmoreE/100 + bamoreE/100 + gradmoreE/100)-0.5)/ (2-0.5)) *10),
#          enroll_index = (schlE-60)/(95-60)*10,
#          educ_index = attain_index*(2/3) + enroll_index*(1/3),
#          hd_index = round((hlth_index + educ_index + inc_index)/3,1))
# 
# county_data <- county_data %>% 
#   select(-c("hlth_index", "inc_index", "attain_index", "enroll_index", "educ_index")) %>% 
#   select(move_last(., c("state", "locality")))

# ....................................................
# 4. Add nice county names ----

# join pretty names to existing tract data
tab <- select(tract_data, locality, NAME)
tab <- separate(tab, NAME,
                into=c("tract","county.nice", "state"), sep=", ", remove=F)

tab <- unique(select(tab, locality, county.nice))
tract_data <- left_join(tract_data, tab, by="locality")

# join pretty names to existing county data
tab2 <- select(county_data, locality, NAME)
tab2 <- separate(tab2, NAME,
                 into=c("county.nice", "state"), sep=", ", remove=F)

tab2 <- unique(select(tab2, locality, county.nice))
county_data <- left_join(county_data, tab2, by=c("locality"))

# join pretty names to existing blockgroup data
tab3 <- select(blkgrp_data, locality, NAME)
tab3 <- separate(tab3, NAME,
                 into=c("block.group", "tract", "county.nice", "state"), sep=", ", remove=F)

tab3 <- unique(select(tab2, locality, county.nice))
blkgrp_data <- left_join(blkgrp_data, tab3, by=c("locality"))


# ....................................................
# 5. Add geography  ----
# get tract polygons
geo <- tracts(state = 'VA', county = region, year = 2021, cb = TRUE) %>% # from tigris / used 2021 bc 2022 caused error
  rename(tr = NAME)

# join coordinates to data
tract_data_geo <- merge(geo, tract_data, by = c("GEOID"), duplicateGeoms = TRUE) # from sp -- keep all obs (full_join)

# get locality polygons
counties_geo <- counties(state = 'VA', year = 2021, cb = TRUE) # from tigris / used 2021 bc 2022 caused error
counties_geo <- counties_geo %>% subset(COUNTYFP %in% region)

# join coordinates to data
county_data_geo <- merge(counties_geo, county_data, by = "GEOID", duplicateGeoms = TRUE) # from sp -- keep all obs (full_join)
names(county_data_geo)[names(county_data_geo)=="NAME.y"] <- "NAME"

# get block group polygons
blkgrp_geo <- block_groups(state = 'VA', county = region, year = 2021, cb = TRUE) # from tigris / used 2021 bc 2022 caused error

# join coordinates to data
blkgrp_data_geo <- merge(blkgrp_geo, blkgrp_data, by = "GEOID", duplicateGeoms = TRUE) # from sp -- keep all obs (full_join)
names(blkgrp_data_geo)[names(blkgrp_data_geo)=="NAME.y"] <- "NAME"

# ....................................................
# 6. Create attributes (CF Edits) ----

# combine into one data frame
all_data <- bind_rows("County" = county_data_geo, 
                      "Block Group" = blkgrp_data_geo, 
                      "Census Tract" = tract_data_geo, 
                      .id = "GEO_LEVEL")

# filter out tract 9901
all_data <- all_data %>% filter(ALAND > 0)

# fix 3 var names (fixed in googlesheet)
j <- match(pretty2$varname, names(all_data))
# remove hmda metadata until/unless county summaries are added
j <- j[c(1:65,76:83,85:85)]

# add pretty labels, sources and about to all_data
for(i in seq_along(j)){
  attr(all_data[[j[i]]], which = "goodname") <- pretty2$goodname[i]
  attr(all_data[[j[i]]], which = "source") <- pretty2$source[i]
  attr(all_data[[j[i]]], which = "about") <- pretty2$about[i]
}

# create data frame of group and varnames
group_df <- pretty2 %>% 
  select(varname, group, goodname) %>% 
  filter(!is.na(group)) 
# categories <- unique(group_df$group)

# create indicator lists based on geography
unique(all_data$GEO_LEVEL)
# [1] "County"       "Block Group"  "Census Tract"

# Block group
ind_bg <- all_data %>% 
  filter(GEO_LEVEL == "Block Group") %>% 
  select(group_df$varname[c(1:31, 37:43)]) %>% # indexing the list removes income by race, ahdi, and hmda vars; add back in if county summaries included
  map_lgl(~ !all(is.na(.x))) 

# census tract
ind_ct <- all_data %>% 
  filter(GEO_LEVEL == "Census Tract") %>% 
  select(group_df$varname[c(1:31, 37:43)]) %>% 
  map_lgl(~ !all(is.na(.x))) 

# add indicator logicals to group_df and sort
# column bg - TRUE if variable available for Block Group
# column ct - TRUE if variable available for Census Tract
# all vars available for County
# (again, index on group_df removes hmda vars)
group_df <- cbind(group_df[c(1:31, 37:43),], bg = ind_bg[-length(ind_bg)], 
                  ct = ind_ct[-length(ind_ct)]) %>% 
  arrange(group, goodname)

# different lists of available indicators by geo level
ind_choices_county <- split(group_df, group_df$group) %>% 
  map(function(x)pull(x, varname, name = goodname))

ind_choices_bg <- split(group_df, group_df$group) %>% 
  map(function(x)filter(x, bg)) %>% 
  map(function(x)pull(x, varname, name = goodname))

ind_choices_ct <- split(group_df, group_df$group) %>% 
  map(function(x)filter(x, ct)) %>% 
  map(function(x)pull(x, varname, name = goodname))

# create list of counties
counties <- levels(factor(tract_data_geo$county.nice))

# get helpers
source('datacode/helpers.R')


# ....................................................
# 7. Define color palettes ----
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(8, "YlGnBu"))(nb.cols)

# create data dictionary for download
data_dict <- bind_rows(pretty, pretty2, pretty3) %>% 
  unique() %>% 
  select(-c("description")) %>% 
  rename(variable_name = varname, description = goodname)

# ....................................................
# 8. Save for app ----
# create new app_data.Rdata file
save(counties_geo, counties, all_data, mycolors, 
     parks_sf, schools_sf, sabselem_sf, mcd_sf, group_df,
     ind_choices_county, ind_choices_bg, ind_choices_ct,
     helpers, data_dict,
     file = "data/app_data_2022_3_15_23.Rdata")
# load("data/app_data_2022.Rdata")

save(counties_geo, counties, all_data, mycolors, 
     parks_sf, schools_sf, sabselem_sf, mcd_sf, group_df,
     ind_choices_county, ind_choices_bg, ind_choices_ct,
     helpers, data_dict,
     file = "eastern-shore/www/app_data_2022.Rdata")

