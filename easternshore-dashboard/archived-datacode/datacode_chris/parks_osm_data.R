####################################################
# Eastern Shore Virginia Equity Atlas
####################################################
# Original script written by Lee LeBoeuf
# adapted for Eastern Shore by Chris Barber
# Acquire Park geometry data
# Last updated: 01/23/2023
# Data from Open Street Map using the osm package
## Open street map amenity key: https://wiki.openstreetmap.org/wiki/Key:amenity
####################################################
# 1. Load libraries
# 2. Download data
# 3. Reduce data, add county FIPS
# 4. Combine data file and save as geojson
####################################################

# ....................................................
# 1. Load libraries ----
library(tidyverse)
library(osmdata)
library(tigris)
library(sf)
library(leaflet)

# ....................................................
# 2. Download data ----
# define bounding box via county shape files
ccode <- read_csv("data/county_codes.csv")
ccode <- ccode[1:2]
region <- ccode$code # list of desired counties

es_bounds <- tracts(state = 'VA', county = region, year = 2022) 

# align crs
es_bounds <- sf::st_transform(es_bounds, 4326)

es_bbox <- st_bbox(es_bounds)

### 2. Parks ----------------------------------------------------------------------------------------
# Eastern Shore parks map
region_parks <- opq(es_bbox) %>% 
  add_osm_feature(key = "leisure", value = "park") %>% 
  osmdata_sf()

parks1 <- region_parks$osm_points[!is.na(region_parks$osm_points$name),] 
parks2 <- region_parks$osm_polygons[!is.na(region_parks$osm_polygons$name),] 
parks3 <- region_parks$osm_multipolygons[!is.na(region_parks$osm_multipolygons$name),] 

# remove entities outside of county bounds
parks1_bounds <- st_intersection(parks1, es_bounds)
parks2_bounds <- st_intersection(parks2, es_bounds)
parks3_bounds <- st_intersection(st_make_valid(parks3), es_bounds)

# Need to combine the three data frames 
## WARNING: The exact column names and numbers of columns appear to change over time for each
# of the different data frames created above with st_intersection. This means that the next section that
# selects different columns so that the three data frames can be bound together with rbind will likely
# need to change when this chunk of code is run in the future. 
# for now, I filter based on the column names of the data frame with the 
# fewest columns 
parkvar1 <- intersect(names(parks1_bounds), names(parks2_bounds))
parkvar2 <- names(parks3_bounds)[colnames(parks3_bounds) %in% parkvar1] 
parkvars <- intersect(parkvar1, parkvar2)

parks1_bounds <- parks1_bounds[,colnames(parks1_bounds) %in% parkvars]
parks2_bounds <- parks2_bounds[,colnames(parks2_bounds) %in% parkvars]
parks3_bounds <- parks3_bounds[,colnames(parks3_bounds) %in% parkvars]

parks <- rbind(parks1_bounds, parks2_bounds, parks3_bounds)

# view
leaflet(es_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(parks, "POINT"), color = "blue",
             popup = st_collection_extract(parks, "POINT")$name) %>% 
  addPolygons(data = st_collection_extract(parks, "POLYGON"), color = "orange",
              popup = st_collection_extract(parks, "POLYGON")$name)


# ....................................................
# 3. Reduce data, add county FIPS ----

# In spot checking the data frame, there are many that have historic district in the name, and 
# might not be worth including
# there are also several points within larger polygons that appear to be redundant. 

View(parks[str_detect(parks$name, pattern = "Historic"),])
# As of 1/25/2023, these "parks" include
# Cape Charles Historic District
# Accomac Historic District
# Northampton County Courthouse Historic District 

## Forming a subset of just the historic districts to map them 
historic <- parks[str_detect(parks$name, pattern = "Historic"),]

leaflet(es_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(data = st_collection_extract(historic, "POINT"), color = "blue",
             popup = st_collection_extract(historic, "POINT")$name)

## None of these appear to capture any green space, except for "University of Virginia Historic District"
# which really just includes the lawn. Other data sources we considered for the parks data 
# also included the lawn; It could potentially be worth including in the future because certainly other people 
# outside the UVA community access the lawn, but those often seem to be tourists. 
# but for now, I'm excluding it along with the other historic districts. 

parks <- parks[parks$name %in% historic$name == F,]

## Spot checking for redundancies and point/polygon inconsistencies:
### As of 12/09/2022
# there are a lot of parks with repeated names, but R doesn't recognize them as duplicated rows because the 
# geometries are ever-so-slightly different. 
# Getting rid of repeated names 
parks <- parks[order(parks$name),]
parks <- parks[!duplicated(parks$name),]


# CB: Need to review historic parks in ES
### From previous run ###########################################################################
# Polygon for Fry Spring's Park and point for Fry's Spring Beach Club (only keeping polygon)
# Polygon for Northeast Park and a point for "Northeast Park Southbound" (only keeping polygon)
# 
# Point for Pen Park, but no polygon (Cville/Albemarle)
# Point for Brook Hill River Park, but no polygon (Cville/Albemarle)
# Point for Carysbrook Sports Complex, but no polygon (Buckingham)
# Point for Carys Creek Wayside, but no polygon (Buckingham)

# Filtering out the redundant points mentioned above and 
# getting rid of the osm id column because it's unnecessary
# parks <- parks %>%
#   filter(parks$name != "Fry's Spring Beach Club" & parks$name != "Northeast Park Southbound") %>%
#   select(-osm_id)
#################################################################################################

# Need to perserve the column with park names--it keeps getting chopped after using the st_write function 
# And getting rid of osm_id column 
parks <- parks %>% 
  rename(ParkName = name) %>%
  select(-osm_id)

# Getting rid of row names 
rownames(parks) <- NULL

# ....................................................
# 4. Save date as geojson  
# Save as geojson
st_write(parks, "data/parks_OSM_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE, factorsAsCharacter = TRUE) 

st_crs(parks)



