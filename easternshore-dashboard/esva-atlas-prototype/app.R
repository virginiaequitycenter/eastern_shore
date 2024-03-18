## Eastern Shore of VA Climate Equity Atlas Prototype

library(shiny)
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(rmapshaper)
library(tigris)
options(tigris_use_cache = TRUE)

es_blkgrp <- readRDS("www/flood_composite_blkgrp.RDS")
blkgrp_names <- read_excel("www/tract_names.xlsx", sheet = "blkgrp2020")
pop <- read_csv("www/population_blkgrp.csv")
# data <- read_csv("blkgrp_pop_data.csv")

counties_geo <- counties(state = 'VA', year = 2022, cb = TRUE) # from tigris / used 2021 bc 2022 caused error
counties_geo <- counties_geo %>% subset(COUNTYFP %in% c("001", "131"))
counties_geo <- st_transform(counties_geo, 4326)

# below variables for leaflet map boundary settings
bbox <- st_bbox(counties_geo) %>% as.vector()

blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp))

es_blkgrp <- es_blkgrp %>% 
  left_join(blkgrp_names)

input_choices <- es_blkgrp %>% 
  select(starts_with("mean_"))%>% 
  st_drop_geometry %>% 
  colnames() 

es_blkgrp <- st_transform(es_blkgrp, 4326)
# es_blkgrp <- rmapshaper::ms_simplify(es_blkgrp)

datasets <- c("economics", "faithfuld", "seals")

ui <- fluidPage(
  # selectInput("dataset", label = "Dataset", choices = input_choices),
  radioButtons("dataset", "Datasets", choices = input_choices),
  leafletOutput("map", width = "600px"),
  textOutput("geoid"),
  # leafletOutput("dem-map"),
  # verbatimTextOutput("summary"),
  # plotOutput("plot"),
  # tableOutput("table")
  
)

server <- function(input, output, session) {
  # Create a reactive expression
  # md <- reactive({
  #   get(input$dataset, es_blkgrp)
  # })
  
  # md <- reactive({
  #   es_blkgrp[input$dataset]
  # })
  
  md <- reactive({
    es_blkgrp <- es_blkgrp %>%
      dplyr::select(GEOID, names, locality, geometry,
                    mean_SeaLevelRise, mean_FEMAFloodZones, mean_StormSurge, mean_HazardNumber,
                    mean_HighTideFlooding) %>%
      drop_na()
  })
  
  listen_indicator <- reactive({
    list(input$dataset)
  })
  
  # output$floodmap <- renderLeaflet({
  #   leaflet() %>% 
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     fitBounds(-76.26, 37.05, -75.11, 38.07) %>%
  #     addPolygons(data = md())
  # })
  # 
  
  ## Map Functions -------------------------------------------------------
  
  ### Leaflet base map function ----
  renderLeafletFunction <- function(map) {
    renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    })
  }
  
  ### leafletProxy Map Function ----
  mapFunction <- function(mapData, mapId, fillColor, ind, strokeColor){
    
    ### map proxy
    proxy <- leafletProxy(mapId, data = mapData)
    
    ### observe
    observe({
      proxy %>%
        clearShapes() %>%
        addPolygons(data = mapData,
                    fillColor = fillColor,
                    fillOpacity = 0.6,
                    color = strokeColor,
                    weight = 1,
                    smoothFactor = 0.2,
                    # popup = paste0(input$dataset, ": ",round(ind, digits = 2)),
                    highlight = highlightOptions(
                      weight = 3,
                      fillOpacity = 0.8,
                      bringToFront = FALSE),
                    layerId = mapData$GEOID,
                    group = "B") %>%
        clearControls() %>%
        addLegend(pal = colorNumeric("YlOrRd", domain = ind),
                  values = ind,
                  position = "topright",
                  opacity = 0.6,
                  title = input$dataset) 
    })
  }
  
  ### leafletProxy Select Polygon Function ----
  selectMapFunction <- function(mapData, mapId, fillColor, ind, strokeColor){
    
    ### map proxy
    proxy <- leafletProxy(mapId, data = mapData)
    
    ### observe
    observe({
      proxy %>%
        clearGroup('A') %>% 
        addPolygons(data = mapData,
                    fillColor = fillColor,
                    fillOpacity = 0.0,
                    color = strokeColor,
                    weight = 1,
                    smoothFactor = 0.2,
                    popup = paste0(input$dataset, ": ",round(ind, digits = 2)),
                    # highlight = highlightOptions(
                    #   weight = 3,
                    #   fillOpacity = 0.8,
                    #   bringToFront = FALSE),
                    layerId = mapData$GEOID,
                    group = 'A') 
      # %>%
      #   clearControls() %>%
      #   addLegend(pal = colorNumeric("YlOrRd", domain = ind),
      #             values = ind,
      #             position = "topright",
      #             opacity = 0.6,
      #             title = input$dataset) 
    })
  }
  
  # Build floodmap -------------------------------------------------------
  
  output$map <- renderLeafletFunction()
  
  observeEvent(listen_indicator(), {
    ind1 <- md() %>% 
      filter(!is.na(.data[[input$dataset]])) %>% 
      pull(input$dataset)
    
    mapFunction(md(), "map", colorNumeric("YlOrRd", ind1)(ind1), ind1, "#FFFFFF")
    print(md()$GEOID)
  })
  
  ## end map
  
  observe({ 
    event <- input$map_shape_click
    
    # output$geoid <- renderText(md()$names[md()$GEOID == event$id])
    
    selected <- es_blkgrp$names[es_blkgrp$GEOID == event$id]
    
    # selectPoly <- 

    if(is.null(event$id)){
      output$geoid <- renderText("null")
      
      
    } else {
      output$geoid <- renderText(selected)
      
      observeEvent(listen_indicator(), {
        ind1 <- md() %>% 
          filter(!is.na(.data[[input$dataset]])) %>%
          pull(input$dataset)
        
        selectPoly <- md() %>% 
          # filter(!is.na(.data[[input$dataset]])) %>% 
          filter(GEOID == event$id)
        
        mapFunction(md(), "map", colorNumeric("YlOrRd", ind1)(ind1), ind1, "#FFFFFF")
        
        selectMapFunction(selectPoly, "map", colorNumeric("YlOrRd", ind1)(ind1), ind1, "blue")

      })
    }
    
    print(event$id)
    
    
  })
  
  # output$summary <- renderPrint({
  #   # Use a reactive expression by calling it like a function
  #   summary(dataset())
  # })
  # 
  # output$plot <- renderPlot({
  #   plot(dataset())
  # }, res = 96)
  # 
  # output$table <- renderTable(dataset())
  
}

shinyApp(ui, server)