# https://community.esri.com/t5/gis-life-blog/accessing-arcgis-rest-services-using-r/ba-p/898451
library(httr)
library(sf)
library(tmap)
library(leaflet)

url <- parse_url("https://coast.noaa.gov/arcgis/rest/services")
url$path <- paste(url$path, "FloodExposureMapper/CFEM_CoastalFloodHazardComposite/MapServer/0/query", sep = "/")
url$query <- list(where = "STATE = '51'",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geoJSON")
request <- build_url(url)

VA_Hazards <- st_read(request)

tmap_mode(mode = "view")
tm_shape(VA_Hazards)+tm_polygons(col="HazardLayers", palette = "Set1", lwd = 5)


leaflet(VA_Hazards) %>%
  addTiles() %>%
  addPolygons()

leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 4) %>%
  addWMSTiles(
    "https://coast.noaa.gov:443/arcgis/services/FloodExposureMapper/CFEM_CoastalFloodHazardComposite/MapServer/WMSServer?",
    layers = "0",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Weather data © 2012 IEM Nexrad"
  )

leaflet() %>%
  addTiles() %>%
  addTiles("https://coast.noaa.gov/arcgis/rest/services/FloodExposureMapper/CFEM_CoastalFloodHazardComposite/MapServer")

library(dplyr)
library(leaflet)
library(leaflet.esri)

leaflet() %>% setView(-96.8, 38.5, 4) %>%
  addEsriBasemapLayer(esriBasemapLayers$Gray, autoLabels = TRUE) %>%
  addEsriFeatureLayer(
    url = paste0("https://coast.noaa.gov/arcgis/rest/services/FloodExposureMapper/CFEM_CoastalFloodHazardComposite/MapServer/0"),
    useServiceSymbology = TRUE)

url <- parse_url("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services")
url$path <- paste(url$path, "USA_Railroads_1/FeatureServer/0/query", sep = "/")
url$query <- list(where = "STATE = 'FL'",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)

Florida_Railroads <- st_read(request)

tmap_mode(mode = "view")
tm_shape(Florida_Railroads)+tm_lines(col="NET_DESC", palette = "Set1", lwd = 5)
