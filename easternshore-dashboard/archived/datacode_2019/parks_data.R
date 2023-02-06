####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire Park geometry data
# Last updated: 10/02/2019
# From Charlottesville Open Data Portal 
# * https://opendata.charlottesville.org/
# From Albemarle  
# * http://www.albemarle.org/department.asp?department=gds&relpage=3914
# From Virginia Department of Conservation
# * https://www.dcr.virginia.gov/recreational-planning/vop-gis-data 
# Geography: Accomack, Northhampton
####################################################
# 1. Load libraries
# 2. Download data
# 3. Reduce data, add county FIPS
# 4. Combine data file and save as geojson
# 5. Possible statewide source - vdcr (not complete)
####################################################

# ....................................................
# 1. Load libraries ----
library(tidyverse)
library(sf)


# ....................................................
# 2. Download data ----

# a. Charlottesville parks
cvl_parks_sf <- st_read("https://opendata.arcgis.com/datasets/a13bdf43fff04168b724a64f7dca234d_19.geojson")
# geometry type:  MULTIPOLYGON
# dimension:      XY
# epsg (SRID):    4326
# proj4string:    +proj=longlat +datum=WGS84 +no_defs


# b. Albemarle Parks
download.file(url = "http://www.albemarle.org/gds/gisdata/ParksRec/park_parcels.zip",
              destfile = "tempdata/park_parcels.zip")
unzip(zipfile = "tempdata/park_parcels.zip", exdir = "tempdata/park_parcels")
alb_parks_sf = st_read(dsn = "tempdata/park_parcels/park_parcels.shp")
# geometry type:  MULTIPOLYGON
# dimension:      XY
# epsg (SRID):    2284
# proj4string:    +proj=lcc +lat_1=37.96666666666667 +lat_2=36.76666666666667 +lat_0=36.33333333333334 +lon_0=-78.5 +x_0=3500000.0001016 +y_0=999999.9998983998 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs


# c. Ragged mountain natural area
download.file(url = "http://www.albemarle.org/gds/gisdata/ParksRec/ragged_mtn_natural_area.zip",
              destfile = "tempdata/ragged_mtn_natural_area.zip")
unzip(zipfile = "tempdata/ragged_mtn_natural_area.zip", exdir = "tempdata/ragged_mtn_natural_area")
ragged_mtn_sf = st_read(dsn = "tempdata/ragged_mtn_natural_area/ragged_mtn_natural_area.shp")


# d. ivy creek
download.file(url = "http://www.albemarle.org/gds/gisdata/ParksRec/ivy_creek_natural_area.zip",
              destfile = "tempdata/ivy_creek_natural_area.zip")
unzip(zipfile = "tempdata/ivy_creek_natural_area.zip", exdir = "tempdata/ivy_creek_natural_area")
ivy_creek_sf = st_read(dsn = "tempdata/ivy_creek_natural_area/ivy_creek_natural_area.shp")


# ....................................................
# 3. Reduce data, add county FIPS ----
# Charlottesville
# Remove cemeteries, civic spaces, possible, and missing types from Cvl
# Remove other variables (objectid, park_type, weburl)
# Change PARKNAME to NAME
# Add county fips
cvl_parks_sf <- cvl_parks_sf %>% 
  filter(!PARK_TYPE %in% c("CEMETERY", "CIVIC SPACES", "Possible", " ")) %>% 
  select(NAME = PARKNAME) %>% 
  mutate(FIPS = "540")

# Albemarle
# Add county fips
alb_parks_sf <- alb_parks_sf %>% 
  mutate(FIPS = "003")

ivy_creek_sf <- ivy_creek_sf %>% 
  mutate(FIPS = "003")

ragged_mtn_sf <- ragged_mtn_sf %>% 
  mutate(FIPS = "003")


# ....................................................
# 4. Combine data file and save as geojson
# combine parks in Alb
parks_sf <- rbind(alb_parks_sf, ivy_creek_sf, ragged_mtn_sf)

# Alb and Cvl have different coordinate systems; change Alb
parks_sf <- st_transform(parks_sf, 4326)

# Combine Alb and Cvl
parks_sf <- rbind(cvl_parks_sf, parks_sf)

# Save as geojson
st_write(parks_sf, "data/parks_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE) 
st_crs(parks_sf)



# ....................................................
# 5. Alternative source - not complete/incorporated
# parks from https://www.dcr.virginia.gov/recreational-planning/vop-gis-data
library(rgdal)
# download.file(url = "https://www.dcr.virginia.gov/recreational-planning/document/vop-gis-sr-locparks-wtrtrails-lwcf.zip",
#               destfile = "vdcr_parks.zip")
# unzip(zipfile = "vdcr_parks.zip")
# unzip(zipfile = "vdcr_parks.zip")
ogrListLayers("tempdata/locparks/LocalParkInventory.gdb")
vcr_parks <- readOGR(dsn = "tempdata/locparks/LocalParkInventory.gdb", layer = "Local_Park_Inventory_2018")
vcr_parks2 <- readOGR(dsn = "tempdata/locparks/LocalParkInventory.gdb", layer = "Local_Park_Inventory")

summary(vcr_parks)
vcr_parks_sf = st_as_sf(vcr_parks, coords == vcr_parks@coords, crs = GRS80)

vcr_parks_sf2 = st_as_sf(vcr_parks, crs = GRS80)

# filter for local region
ccode <- read_csv("code/county_codes.csv")
region <- ccode$code # list of desired counties
vcr_parks_sf <- vcr_parks_sf %>% filter(COUNTY_FIPS %in% region)

summary(vcr_parks_sf)
plot(vcr_parks_sf["OBJECTID_1"])

# These are points, not polygons as expected...needs more investigation/understanding
# but could use to summarize things like: total acres, number of parks with playground, number ada accessible, etc.

vcr_point = st_multipoint(c(vcr_parks_sf$LONGITUDE, vcr_parks_sf$LATITUDE), dim = "XY") # sfg object
vcr_geom = st_sfc(vcr_point, crs = 4326)           # sfc object
vcr_attrib = vcr_parks_sf
vcr_sf = st_sf(vcr_attrib, geometry = vcr_geom)    # sf object
