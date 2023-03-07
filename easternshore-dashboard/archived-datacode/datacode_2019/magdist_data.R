####################################################
# Eastern Shore Profile
####################################################
# Acquire Magisterial District geometry data
# Last updated: 03/01/2021
# From Tigris
#
# Geography: Accomack, Northhampton
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
library(ggthemes)

ccode <- read_csv("datacode/county_codes.csv")
region <- str_pad(as.character(ccode$code), width = 3, pad = "0") # list of desired counties


# ....................................................
# 2. Download data ----

# pull county subdivisions and reduce to region
mcd <- county_subdivisions(state = "VA", county = region, cb = TRUE, year = 2019)


# ....................................................
# 3. Check and tranform ----

# check
mcd_df <- fortify(mcd)

p <- ggplot() + 
  geom_map(data=mcd_df, map=mcd_df,
           aes(x=long, y=lat, map_id=id),
           color="black", fill="white", size=0.25)
p + coord_map() + theme_map() # with correct mercator projection

# change to sf format
mcd_sf <- st_as_sf(mcd)
st_crs(mcd_sf)

# transform crs
mcd_sf <- st_transform(mcd_sf, 4326)
plot(mcd_sf[6])


# ....................................................
# 4. Save as geojson ----

st_write(mcd_sf, "data/mcd_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE) 
# mcd_sf <- st_read("data/mcd_sf.geojson") 

