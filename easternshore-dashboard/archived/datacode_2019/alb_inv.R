# Reading/Exploring Invetory_1.gdb (2018)
# "zip file containing the geodatabase for the first phase of the Albemarle County Community Inventory (Pilot Program 2018)"
# file sent via email by Nick Morrison (TJPDC), at Siri's request, on 2020-01-22

library(tidyverse)
library(sf)
library(rgdal)


# read in file sent by Nick Morrison
ogrListLayers("tempdata/Invetory_1.gdb")
# 14 layers

# 1. Schools: 25 (just school names, polygons, type)
alb_inv_schools <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Schools")
summary(alb_inv_schools)

alb_inv_schools_sf <- st_as_sf(alb_inv_schools)
names(alb_inv_schools_sf)

# 2. BikePedSurveyPoint: 154 (points checked and when, needed maintenance, impediments; stuff on curbs/ada unclear to me)
alb_inv_bikesvy <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "BikePedSurveyPoint")
summary(alb_inv_bikesvy)

alb_inv_bikesvy_sf <- st_as_sf(alb_inv_bikesvy)
names(alb_inv_bikesvy_sf)

# 3. Bike_Ped_Fac: 3186 (trail length, type. material, date surveyed, condition, width, curbs)
alb_inv_bikefac <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Bike_Ped_Fac")
summary(alb_inv_bikefac)

alb_inv_bikefac_sf <- st_as_sf(alb_inv_bikefac)
names(alb_inv_bikefac_sf)

# 4. Traffic_Signals: 59 (major street, minor street, pedestrian?)
alb_inv_trafsig <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Traffic_Signals")
summary(alb_inv_trafsig)

alb_inv_trafsig_sf <- st_as_sf(alb_inv_trafsig)
names(alb_inv_trafsig_sf)

# 5. Signs_AC: 3870 (names, routes, stuff I don't get, size - condition and type all missing)
alb_inv_signs <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Signs_AC")
summary(alb_inv_signs)

alb_inv_signs_sf <- st_as_sf(alb_inv_signs)
names(alb_inv_signs_sf)

# 6. StreetLights: 441 (id, power com, lumens, watts, type, pole-type missing)
alb_inv_strlght <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "StreetLights")
summary(alb_inv_strlght)

alb_inv_strlght_sf <- st_as_sf(alb_inv_strlght)
names(alb_inv_strlght_sf)

# 7. VDOT_Sign: 238 (id, date, direction, stuff I don't get, type, text, support, etc.)
alb_inv_vdotsigns <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "VDOT_Sign")
summary(alb_inv_vdotsigns)

alb_inv_vdotsigns_sf <- st_as_sf(alb_inv_vdotsigns)
names(alb_inv_vdotsigns_sf)

# 8. Parcels_1: 1616 (PIN, condition, land use, notes, parcel id, hs district, zoneing, comp plan, ZMAsRealte?, stuff I don't know)
alb_inv_parcel <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Parcels_1")
summary(alb_inv_parcel)

alb_inv_parcel_sf <- st_as_sf(alb_inv_parcel)
names(alb_inv_parcel_sf)

# 9. Park_amenities: no features found
alb_inv_parkamn <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Park_amenities")

# 10. Crosswalks: no features found
alb_inv_crswlk <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Crosswalks")

# 11. Poles: no features found
alb_inv_poles <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Poles")

# 12. Stormdrain: no features found
alb_inv_strmdrn <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Stormdrain")

# 13. Street_Trees: no features found
alb_inv_strttree <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Street_Trees")

# 14. Sidewalk_Imp: no features found
alb_inv_sidewalk <- readOGR(dsn = "tempdata/Invetory_1.gdb", layer = "Sidewalk_Imp")
