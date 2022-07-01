
library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(ggthemes)
library(patchwork)

# eastern shore tracts 2020
acc_tract_2020 <- tracts(state = "51", county = "001", cb = TRUE, year = 2020)
nor_tract_2020 <- tracts(state = "51", county = "131", cb = TRUE, year = 2020)

acc_tract_2020_noncb <- tracts(state = "51", county = "001", cb = FALSE, year = 2021)
nor_tract_2020_noncb <- tracts(state = "51", county = "131", cb = FALSE, year = 2021)

# accomack tracts 2019
acc_tract_2019 <- tracts(state = "51", county = "001", cb = TRUE, year = 2019)
nor_tract_2019 <- tracts(state = "51", county = "131", cb = TRUE, year = 2019)

# map
ac19 <- ggplot(acc_tract_2019) + geom_sf(aes(fill = TRACTCE)) + 
  scale_fill_tableau(palette = "Tableau 20", guide = "none") +
  labs(title = "2019") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
ac20 <- ggplot(acc_tract_2020) + geom_sf(aes(fill = TRACTCE)) +
  scale_fill_tableau(palette = "Tableau 20", , guide = "none") +
  labs(title = "2020") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

no19 <- ggplot(nor_tract_2019) + geom_sf(aes(fill = TRACTCE)) + 
  scale_fill_tableau(palette = "Tableau 10", guide = "none") +
  labs(title = "2019") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
no20 <- ggplot(nor_tract_2020) + geom_sf(aes(fill = TRACTCE)) + 
  scale_fill_tableau(palette = "Tableau 10", guide = "none") +
  labs(title = "2020") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


acc <- ac19 + ac20 
ggsave("accomack_tracts.jpg")

nor <- no19 + no20
ggsave("northampton_tracts.jpg")


library(leaflet)
acc_tract_2020 <- st_transform(acc_tract_2020, 4326)

leaflet(data = acc_tract_2020) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(popup = paste0("tract: ", acc_tract_2020$TRACTCE)) 

leaflet(data = acc_tract_2020_noncb) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(popup = paste0("tract: ", acc_tract_2020_noncb$TRACTCE)) 

leaflet(data = nor_tract_2020) %>% 
  addPolygons(popup = paste0("tract: ", nor_tract_2020$TRACTCE)) 
