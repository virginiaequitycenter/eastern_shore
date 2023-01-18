# Eastern Shore Geometries
# 2023-01-18

library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(ggmap)

# counties ----
county <- counties(state = "51", cb = TRUE, year = 2021)
es_county <- county %>% filter(COUNTYFP %in% c("001", "131"))

ggplot(es_county) + 
  geom_sf() +
  theme_void()

# tracts ----
es_tracts <- tracts(state = "51", cb = TRUE, year = 2021, 
                    county = c("001", "131"))

ggplot(es_tracts) +
  geom_sf() +
  theme_void()

# places (no?) ----
places <- places(state = "51", cb = TRUE, year = 2021)
# spatial intersect to reduce
es_places <- places %>%
  st_filter(es_county, .predicate = st_within)

ggplot(es_places) +
  geom_sf() +
  theme_void()

# area water (yes) ----
es_awater <- area_water(state = "51", year = 2021,
                     county = c("001", "131"))

ggplot(es_awater) +
  geom_sf() +
  theme_void()

# this is more what I want; how to clip off the ocean/bay water?

# linear water ----
es_lwater <- linear_water(state = "51", year = "2021",
                       county = c("001", "131"))

ggplot(es_lwater) +
  geom_sf() + 
  theme_void()

# primary and secondary roads ----
roads2 <- primary_secondary_roads(state = "51", year = 2021)
# spatial intersect to reduce
es_roads2 <- roads2 %>%
  st_filter(es_county, .predicate = st_within)

ggplot(es_roads2) +
  geom_sf() +
  theme_void()

# rails ----
rails <- rails(year = 2021)
# spatial intersect to reduce
es_rail <- rails %>%
  st_filter(es_county, .predicate = st_within)

ggplot(es_rail) +
  geom_sf() +
  theme_void()

# landmarks ----
landmarks <- landmarks(state = "51", year = 2021)
# spatial intersect to reduce
es_lmark <- landmarks %>%
  st_filter(es_county, .predicate = st_within)

es_lmark <- es_lmark %>% 
  filter(!is.na(FULLNAME))

ggplot(es_lmark) +
  geom_sf() +
  theme_void()

rm(county, landmarks, mil, places, rails, roads1, roads2, schools,
   es_mil, es_roads1)

# together?
ggplot() +
  geom_sf(data = es_county, color = "black") +
  geom_sf(data = es_roads2, color = "grey") +
  geom_sf(data = es_rail, color = "orange") +
  theme_void()

# better to use a basemap, i think...
# https://cran.r-project.org/web/packages/ggmap/readme/README.html
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/ggmapCheatsheet.pdf
# https://appsilon.com/r-ggmap/
# https://stackoverflow.com/questions/49626233/plotting-static-base-map-underneath-a-sf-object
# https://www.littlemissdata.com/blog/maps

# start over with ggmap/basemaps and census tracts
# tracts ----
es_tracts <- tracts(state = "51", cb = TRUE, year = 2021, 
                    county = c("001", "131"))


