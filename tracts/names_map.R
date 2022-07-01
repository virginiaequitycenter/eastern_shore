# Map of potential tract names
# For community advisory review and refinement
# Michele Claibourn
# 2022-06-23

# Setup ----
library(tidyverse)
library(readxl)
library(tigris)
library(sf)
library(leaflet)

# Data sources ----
names <- read_excel("tract_names.xlsx")
names <- names %>% 
  mutate(tract = as.character(tract),
         tract = str_pad(tract, 6, side = "left", pad = "0"))
tracts <- tracts(state = "51", county = c("001", "131"), cb = TRUE, year = 2020)
# tracts2 <- tracts(state = "51", county = c("001", "131"), cb = FALSE, year = 2020)

# names$tract
# tracts$TRACTCE
# tracts2$TRACTCE

# which(!names$tract %in% tracts2$TRACTCE)
# which(!tracts2$TRACTCE %in% names$tract)

ggplot(tracts) + geom_sf(aes(fill = TRACTCE))
# ggplot(tracts2 %>% filter(!str_detect(TRACTCE, "99"))) +
#   geom_sf(aes(fill = TRACTCE))

tracts <- tracts %>% 
  left_join(names, by = c("TRACTCE" = "tract"))

# Map ----
tracts <- st_transform(tracts, crs = 4326)

leaflet(tracts) %>% 
  #addTiles() %>% 
  #addProviderTiles("CartoDB.Positron") %>% 
  addProviderTiles("Esri.NatGeoWorldMap") %>% 
  addPolygons(color = "black",
              weight = 1,
              popup = paste0("Tract Number: ", tracts$TRACTCE, "<br>",
                             "Notes/Names: ", tracts$names)) 

saveRDS(tracts, file = "tractnames.RDS")
