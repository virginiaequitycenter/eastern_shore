# Map of potential tract names
# For community advisory review and refinement
# Michele Claibourn
# Created: 2022-07-01
# Last updated: 2022-09-17

# Setup ----
library(tidyverse)
library(readxl)
library(tigris)
library(sf)
library(leaflet)

# Data sources: Tracts ----
names <- read_excel("tract_names.xlsx")

names <- names %>% 
  mutate(tract = as.character(tract),
         tract = str_pad(tract, 6, side = "left", pad = "0"))

tracts <- tracts(state = "51", county = c("001", "131"), cb = TRUE, year = 2020)
tracts <- st_transform(tracts, crs = 4326)

# Check
# which(!names$tract %in% tracts2$TRACTCE)
# which(!tracts2$TRACTCE %in% names$tract)

ggplot(tracts) + geom_sf(aes(fill = TRACTCE))

# Join community names
tracts <- tracts %>% 
  left_join(names, by = c("TRACTCE" = "tract"))

# save for use
saveRDS(tracts, file = "tractnames.RDS")


# Data sources: Block Groups ----
blkgps <- block_groups(state = "51", county = c("001", "131"), cb = TRUE, year = 2020)
blkgps <- st_transform(blkgps, crs = 4326)

group_vector <- rep(seq(1,20,1),3)[1:46] # create 20-group variable for distinguishing color

blkgps <- blkgps %>% 
  mutate(tract_blkgp = paste0(TRACTCE, ".", BLKGRPCE),
         blkgp_color = group_vector)

# Check
ggplot(blkgps) + geom_sf()

# save for use
saveRDS(blkgps, file = "blkgpnames.RDS")


# Example map: Tracts ----

pal <- colorFactor(palette = tableau_color_pal(palette = "Tableau 20")(17), tracts$TRACTCE)

leaflet(tracts) %>% 
  addTiles() %>% 
  #addProviderTiles("CartoDB.Positron") %>% 
  #addProviderTiles("Esri.WorldStreetMap") %>% 
  #addProviderTiles("Esri.WorldImagery") %>% 
  addPolygons(fillColor = ~ pal(TRACTCE),
              fillOpacity = 0.3,
              color = ~ pal(TRACTCE),
              weight = 1,
              popup = paste0("Tract Number: ", tracts$TRACTCE, "<br>",
                             "Notes/Names: ", tracts$names)) 


# Example map: Block groups ----
pal <- colorFactor(palette = tableau_color_pal(palette = "Tableau 20")(20), blkgps$blkgp_color)

leaflet(blkgps) %>% 
  addTiles() %>% 
  #addProviderTiles("CartoDB.Positron") %>% 
  #addProviderTiles("Esri.WorldStreetMap") %>% 
  #addProviderTiles("Esri.WorldImagery") %>% 
  addPolygons(fillColor = ~ pal(blkgp_color),
              fillOpacity = 0.3,
              color = ~ pal(blkgp_color),
              weight = 1,
              popup = paste0("Tract Number.Block Group: ", blkgps$tract_blkgp))

