####################################################
# Eastern Shore Virginia Equity Atlas
####################################################
# Original script written by Lee LeBoeuf
# adapted for Eastern Shore by Chris Barber
# Acquire Magisterial District geometry data
# Last updated: 01/25/2023
# From Tigris
#
# Geography: Magesterial Districts in VA's Eastern Shore
#     Accomack County, VA
#     Northampton County, VA
####################################################
# 1. Load libraries
# 2. Download data
# 3. Check and transform
# 4. Save as geojson
####################################################

# ....................................................
# 1. Load libraries ----
library(tidyverse)
library(sf)
library(tigris)


ccode <- read_csv("data/county_codes.csv")
ccode <- ccode[1:2]
region <- ccode$code # list of desired counties


# ....................................................
# 2. Download data ----

# pull county subdivisions and reduce to region
mcd <- county_subdivisions(state = "VA", 
                           county = region, 
                           year = "2022")
# will need to change back to county = region
# MPC: as of 1/10/2023, only 2022 tiger files are available,
#.  representing boundaries as of 1/1/2022;
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2022.html#list-tab-2YME570UCQ7S22N648


# LL 11/3/2022 note: As of today, it doesn't appear that the magisterial districts 
# for Albemarle county have changes to redistricting option 2 that was selected
# by the county. However, the documentation from tigris seems to suggest that it 
# should be for the year 2020.... I tried downloading voting districts as well, 
# and those also claim to be 2020. 
# test <- voting_districts(state = "VA", county = "003", year = "2023")

# ....................................................
# 3. Check and transform ----

# check
mcd_df <- fortify(mcd)

p <- ggplot() + 
  geom_sf(data=mcd_df,
          color="black", fill="white", size=0.25)
p

# change to sf format
mcd_sf <- st_as_sf(mcd)
st_crs(mcd_sf)

# transform crs
mcd_sf <- st_transform(mcd_sf, 4326)

p <- ggplot() + 
  geom_sf(data=mcd_df,
          color="black", fill="white", size=0.25)
p 

# ....................................................
# 4. Save as geojson ----

st_write(mcd_sf, "data/mcd_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE) 
# mcd_sf <- st_read("data/mcd_sf.geojson") 

