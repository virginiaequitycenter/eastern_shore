# Libraries
library(here)
library(tidyverse)
library(httr2)
library(jsonlite)
library(boxr)
library(sf)

library(htmltools)
library(leaflet)
library(RColorBrewer)

# Set WD
setwd(here("esva-livability-tool"))

# Refresh Box token
# boxr::box_fresh_auth()

# Get data from Box
readRenviron("~/.Renviron")
box_auth(client_id = Sys.getenv('BOX_CLIENT_ID'), client_secret = Sys.getenv('BOX_CLIENT_SECRET'))

dir_id <- "305971996036" # EA_Export
box_setwd(dir_id)

# box_ls()

box_fetch(
  dir_id = box_getwd(),
  local_dir = "app_data/app_data_updated",
  recursive = TRUE,
  overwrite = TRUE,
  delete = TRUE
)

# # Read setup file
# setup <- readLines('setup.txt')
# 
# # Separate setup file components into list
# setup <- list('updates' = setup[(which(grepl('updates:', setup)) + 1):length(setup)])
# setup$updates <- setup$updates
# 
# # Get data from box folders
# get_folders <- function(file_name) {
#   box_fetch(
#     dir_id = sapply(box_ls(), \(x) x$id[x$name == file_name]) |> unlist(),
#     local_dir = "app_data/EA_Export",
#     recursive = TRUE,
#     overwrite = TRUE,
#     delete = FALSE
#   )
# }
# 
# sapply(setup$updates, get_folders)

# Data prep ----
eafile <- list.files("app_data/app_data_updated")[32]

# Read in ea_export
ea_export <- fromJSON(paste0("app_data/app_data_updated/", eafile))

# Get Box ID function
get_boxid <- function(path) {
  boxid <- str_split(path, "https://virginia.app.box.com/file/", simplify = TRUE)[2]
  boxid
}

# Get CSV-JSON
json_path <- ea_export$data$path

# json_id <- str_split(json_path, "https://virginia.app.box.com/file/", simplify = TRUE)[2]
json_id <- get_boxid(json_path)
json <- box_read_json(file_id = json_id)

# Get CSV
csv_id <- get_boxid(json$path)
csv_id <- get_boxid('https://virginia.app.box.com/file/1917303195994')
csv <- box_read_csv(csv_id)

# Join geometries 
# shape_path <- json$regionShapeSet$path
# shape_id <- get_boxid(shape_path)
# shape_geo <- box_read(shape_id) # cant read geojson

block_geo <- st_read("data/blocks/esva_2020block_clipped.geojson")
block_geo <- st_transform(block_geo, 4326)

csv_geo <- csv %>%
  mutate(GEOID20 = as.character(GEOID20)) %>%
  left_join(block_geo, by = join_by(GEOID20 == GEOID20)) %>%
  st_as_sf()

# For inlandflooding data:
# csv_geo <- csv %>% 
#   mutate(GISJOIN = paste0("G", as.character(GEOID20))) %>% 
#   left_join(block_geo, by = join_by(GISJOIN == GISJOIN)) %>% 
#   st_as_sf()



# Gather data for app
region_bbox <- ea_export$regionBoundingBox
bbox <- c(region_bbox$lonMin, region_bbox$latMin, region_bbox$lonMax, region_bbox$latMax)

legend_breaks <- as.vector(ea_export$dataColumns$bins[[1]])
legend_labels <- as.list(ea_export$dataColumns$labels[[1]])

descriptionTitle <- ea_export$descriptionTitle

var1_name <- ea_export$dataColumns$name[1]
var1_name_index <- match(var1_name, json$schema$fields$name)
var1_title <- json$schema$fields$title[var1_name_index]

var2_name <- ea_export$dataColumns$name[2]
var2_name_index <- match(var2_name, json$schema$fields$name)
var2_title <- json$schema$fields$title[var2_name_index]

var3_name <- ea_export$dataColumns$name[3]
var3_name_index <- match(var3_name, json$schema$fields$name)
var3_title <- json$schema$fields$title[var3_name_index]

var4_name <- ea_export$dataColumns$name[4]
var4_name_index <- match(var4_name, json$schema$fields$name)
var4_title <- json$schema$fields$title[var4_name_index]
# 
# var5_name <- ea_export$dataColumns$name[5]
# var5_name_index <- match(var5_name, json$schema$fields$name)
# var5_title <- json$schema$fields$title[var5_name_index]

# var6_name <- ea_export$dataColumns$name[6]
# var6_name_index <- match(var6_name, json$schema$fields$name)
# var6_title <- json$schema$fields$title[var6_name_index]
# 
# var7_name <- ea_export$dataColumns$name[7]
# var7_name_index <- match(var7_name, json$schema$fields$name)
# var7_title <- json$schema$fields$title[var7_name_index]

# Make leaflet map
m <- csv_geo

var1 <- m[[var1_name]]
var2 <- m[[var2_name]]
var3 <- m[[var3_name]]
var4 <- m[[var4_name]]
# var5 <- m[[var5_name]]
# var6 <- m[[var6_name]]
# var7 <- m[[var7_name]]

breaks <- legend_breaks
sel_range <- c(min(breaks), max(breaks))

pal <- colorBin(c("#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#B30000"), 
                sel_range, 
                bins = breaks, 
                right = TRUE, 
                # reverse = TRUE,
                na.color = "#808080", 
                pretty = FALSE )

# labs <- as.list(var1)

map <- leaflet(m) %>%
  addProviderTiles('CartoDB.Positron') %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
  clearShapes() %>%
  addPolygons(data = m,
              weight = 0.5,
              color = "#FFFFFF",
              smoothFactor = 0.2,
              fillColor = ~pal(var1),
              fillOpacity = 0.7,
              label = lapply(as.list(var1), HTML),
              group = var1_title) %>%
  addPolygons(data = m,
              weight = 0.5,
              color = "#FFFFFF",
              smoothFactor = 0.2,
              fillColor = ~pal(var2),
              fillOpacity = 0.7,
              label = lapply(as.list(var2), HTML),
              group = var2_title) %>%
  addPolygons(data = m,
              weight = 0.5,
              color = "#FFFFFF",
              smoothFactor = 0.2,
              fillColor = ~pal(var3),
              fillOpacity = 0.7,
              label = lapply(as.list(var3), HTML),
              group = var3_title) %>%
  addPolygons(data = m,
              weight = 0.5,
              color = "#FFFFFF",
              smoothFactor = 0.2,
              fillColor = ~pal(var4),
              fillOpacity = 0.7,
              label = lapply(as.list(var4), HTML),
              group = var4_title) %>%
  # addPolygons(data = m,
  #             weight = 0.5,
  #             color = "#FFFFFF",
  #             smoothFactor = 0.2,
  #             fillColor = ~pal(var5),
  #             fillOpacity = 0.7,
  #             label = lapply(as.list(var5), HTML),
  #             group = var5_title) %>%
  # addPolygons(data = m,
  #             weight = 0.5,
  #             color = "#FFFFFF",
  #             smoothFactor = 0.2,
  #             fillColor = ~pal(var6),
  #             fillOpacity = 0.7,
  #             label = lapply(as.list(var6), HTML),
  #             group = var6_title) %>%
  # addPolygons(data = m,
  #             weight = 0.5,
  #             color = "#FFFFFF",
  #             smoothFactor = 0.2,
  #             fillColor = ~pal(var7),
  #             fillOpacity = 0.7,
  #             label = lapply(as.list(var7), HTML),
  #             group = var7_title) %>%
  addLegend(position = 'bottomright', 
            pal = pal,
            values = sel_range,
            labFormat = function(type, breaks) {
              return(legend_labels)
            },
            title = ~gsub("\n", "<br>",
                          stringr::str_wrap(descriptionTitle,
                                            width = 20,
                                            whitespace_only = FALSE)),
            opacity = 0.7) %>% 
  addLayersControl(
    baseGroups = c(var1_title, var2_title, var3_title, var4_title), #, var2_title, var3_title, var4_title, var5_title, var6_title, var7_title
    options = layersControlOptions(collapsed = FALSE)
  ) 

map
 