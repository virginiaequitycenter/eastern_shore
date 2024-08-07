# Generating common CBG files
# claibourn
# 2024-04-18

# .............................................
# Tools ----
library(tidyverse)
library(sf)
library(tigris)


# .............................................
# TIGER file version ----
sf <- block_groups(state = "51", cb = TRUE, year = 2022)

esva_sf <- sf %>% 
  filter(COUNTYFP %in% c("001", "131"))

ggplot(esva_sf) +
  geom_sf()
st_crs(esva_sf)

# write to share
st_write(esva_sf, "cbg/esva_2020blkgrp.geojson")
st_write(esva_sf, "cbg/esva_2020blkgrp.shp")


# .............................................
# NHGIS version (clipped) ----
# NHGIS adjusts the TIGER files to (among a few other things)
# "erase coastal water areas to produce polygons that terminate at the U.S. coasts and Great Lakes shores
# https://www.nhgis.org/gis-files#derivation

# Requested/downloaded Virginia Block Groups from data request site
# https://data2.nhgis.org/main
nh <- st_read("cbg/nhgis0009_shapefile_tl2022_510_blck_grp_2022/VA_blck_grp_2022.shp")

nh_esva <- nh %>% 
  filter(COUNTYFP %in% c("001", "131"))

ggplot(nh_esva) +
  geom_sf()
st_crs(nh_esva)

nh_4326 <- nh_esva %>% 
  st_transform(crs = st_crs(4326))

# write to share 
st_write(nh_4326, "cbg/esva_2020blkgrp_clipped.geojson")
st_write(nh_4326, "cbg/esva_2020blkgrp_clipped.shp")


# .............................................
# Test/Compare ----
rm(list = ls())

tf <- st_read("cbg/esva_2020blkgrp.geojson")
nh <- st_read("cbg/esva_2020blkgrp_clipped.geojson")

tf <- st_read("cbg/esva_2020blkgrp.shp")
nh <- st_read("cbg/esva_2020blkgrp_clipped.shp")

ggplot(tf) +
  geom_sf()

ggplot(nh) +
  geom_sf()

nh_4326 <- nh %>% 
  st_transform(crs = st_crs(4326))

ggplot() +
  geom_sf(data = tf, linewidth = 1) +
  geom_sf(data = nh_4326, alpha = 0, color = "blue")

## nh has 2 additional block groups
sum(tf$GEOID %in% nh$GEOID)

nh %>% 
  filter(!(GEOID %in% tf$GEOID)) %>% 
  view()

nh <- nh %>% 
  mutate(comp = ifelse(GEOID %in% tf$GEOID, 
                       "both", "only_nh"))

ggplot(nh) +
  geom_sf(aes(fill = comp)) +
  scale_fill_manual(values = c("grey", "red"))

nh %>% 
  filter(comp == "only_nh") %>% 
  ggplot() +
  geom_sf()

# nh contains the essentially water-only/non-populated tracts
# 990100 in accomack
# 990100 in northhampton