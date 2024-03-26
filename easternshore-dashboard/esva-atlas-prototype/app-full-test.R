# https://walkerke.shinyapps.io/neighborhood_diversity/
# https://github.com/walkerke/neighborhood_diversity/blob/master/neighborhood_diversity.Rmd

library(shiny)
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(plotly)
library(RColorBrewer)
# library(rmapshaper)
library(tigris)
options(tigris_use_cache = TRUE)
library(highcharter)

source("functions/utils.R")

es_blkgrp <- readRDS("www/flood_composite_blkgrp.RDS")
blkgrp_names <- read_excel("www/tract_names.xlsx", sheet = "blkgrp2020")
pop <- read_csv("www/population_blkgrp.csv")
# data <- read_csv("blkgrp_pop_data.csv")
schools_sf <- st_read("../data/schools_sf.geojson")

blkgrp_geo <- es_blkgrp %>% 
  select(GEOID, geometry)

# make 10 percent columns
flood_blkgrp <- es_blkgrp %>% 
  mutate(across(starts_with("sum"), ~round(.x/cells *100, 2) , .names = "per_{.col}"))

# counties_geo <- counties(state = 'VA', year = 2022, cb = TRUE) # from tigris / used 2021 bc 2022 caused error
# counties_geo <- counties_geo %>% subset(COUNTYFP %in% c("001", "131"))
# counties_geo <- st_transform(counties_geo, 4326)
# 
# # below variables for leaflet map boundary settings
# bbox <- st_bbox(counties_geo) %>% as.vector()

blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp))

flood_blkgrp <- flood_blkgrp %>% 
  left_join(blkgrp_names)

input_choices <- flood_blkgrp %>%
  select(starts_with("per_sum"))%>%
  st_drop_geometry %>%
  colnames()

# view(input_choices)

flood_blkgrp <- st_transform(flood_blkgrp, 4326)
# es_blkgrp <- rmapshaper::ms_simplify(es_blkgrp)

full_tracts <- flood_blkgrp %>%
  mutate(tract_id = GEOID) %>% 
  dplyr::select(GEOID, tract_id, names, locality, geometry,
                starts_with("per_sum")
                # mean_SeaLevelRise, mean_FEMAFloodZones, mean_StormSurge, mean_HazardNumber,
                # mean_HighTideFlooding
                ) 


## add demographic/population/housing data ----
pop <- pop %>% 
  mutate(tract_id = as.character(GEOID),
         GEOID = as.character(GEOID)) 

pop_est <- pop %>% 
  select(GEOID, tract_id, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est) %>% 
  mutate(GEOID = as.character(GEOID),
         whiteper_est = round(whiteper_est,0),
         blackper_est = round(blackper_est,0),
         ltnxper_est = round(ltnxper_est,0),
         remainper_est = round(remainper_est,0))

pop_est <- pop_est %>% 
  right_join(blkgrp_names)

pop_est <- pop_est %>% 
  left_join(blkgrp_geo)

pop_est <- sf::st_as_sf(pop_est)



es_blkgrp_pop <- full_tracts %>%
  left_join(pop)

es_blkgrp_pop <- es_blkgrp_pop %>% 
  mutate(GEOID = as.character(GEOID),
         whiteper_est = round(whiteper_est,0),
         blackper_est = round(blackper_est,0),
         ltnxper_est = round(ltnxper_est,0),
         remainper_est = round(remainper_est,0),
         age17per_est = round(age17per_est,0), 
         age18to64per_est = round(age18to64per_est,0), 
         age65per_est = round(age65per_est,0))

# per_sum_slr1.ft
# per_sum_slr2.ft
# per_sum_slr3.ft
# per_sum_vzone.1percent
# per_sum_azone.1percent
# per_sum_azone.2percent
# per_sum_ss.cat1
# per_sum_ss.cat2
# per_sum_ss.cat3
# per_sum_HighTideFlooding

per_sum_slr1.ft <- es_blkgrp_pop %>% 
  mutate(entropy = per_sum_slr1.ft,
         group = "SeaLevelRise") %>% 
  select(tract_id, names, locality, geometry,group,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est)
  
per_sum_slr2.ft <- es_blkgrp_pop %>% 
  mutate(entropy = per_sum_slr2.ft,
         group = "SeaLevelRise") %>% 
  select(tract_id, names, locality, geometry, group,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est)

per_sum_slr3.ft <- es_blkgrp_pop %>% 
  mutate(entropy = per_sum_slr3.ft,
         group = "SeaLevelRise") %>% 
  select(tract_id, names, locality, geometry, group,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est)

per_sum_ss.cat1 <- es_blkgrp_pop %>% 
  mutate(entropy = per_sum_ss.cat1,
         group = "StormSurge") %>% 
  select(tract_id, names, locality, geometry, group,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est)

per_sum_ss.cat2 <- es_blkgrp_pop %>% 
  mutate(entropy = per_sum_ss.cat2,
         group = "StormSurge") %>% 
  select(tract_id, names, locality, geometry, group,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est)

data_test <- list(per_sum_slr1.ft=per_sum_slr1.ft, per_sum_slr2.ft=per_sum_slr2.ft, per_sum_slr3.ft=per_sum_slr3.ft, per_sum_ss.cat1=per_sum_ss.cat1, per_sum_ss.cat2=per_sum_ss.cat2)

whiteper_est <- pop_est %>% 
  mutate(entropy = whiteper_est) %>% 
  select(tract_id, names, locality, geometry,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est)

blackper_est <- pop_est %>% 
  mutate(entropy = blackper_est) %>% 
  select(tract_id, names, locality, geometry,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est)

ltnxper_est <- pop_est %>% 
  mutate(entropy = ltnxper_est) %>% 
  select(tract_id, names, locality, geometry,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est)

data_pop <- list(whiteper_est=whiteper_est, blackper_est=blackper_est, ltnxper_est=ltnxper_est)


## reshape for heatmap ----
flood_blkgrp_long <- flood_blkgrp %>% 
  select(GEOID, COUNTYFP, names, starts_with("per")) %>% 
  pivot_longer(starts_with("per"), names_to = "variable", values_to = "risk") %>% 
  mutate(group = case_when(str_detect(variable, "HighTide") ~ "HighTideFlooding",
                           str_detect(variable, "ss.cat") ~ "StormSurge",
                           str_detect(variable, "slr") ~ "SeaLevelRise",
                           str_detect(variable, "vzone|azone") ~ "FEMACat"),
    # variable = str_remove(variable, "per_sum_"),
         variable = factor(variable, levels = c("per_sum_HighTideFlooding", "per_sum_vzone.1percent", "per_sum_azone.1percent", "per_sum_azone.2percent",
                                                "per_sum_ss.cat1", "per_sum_ss.cat2", "per_sum_ss.cat3", "per_sum_slr1.ft", "per_sum_slr2.ft", "per_sum_slr3.ft"),
                           labels = c("HighTideFlooding", "vzone.1percent", "azone.1percent", "azone.2percent",
                                      "ss.cat1", "ss.cat2", "ss.cat3", "slr1.ft", "slr2.ft", "slr3.ft")
                           # labels = c("High Tide", "FEMA V (1%)", "FEMA A (1%)", "FEMA A (2%)", 
                           #            "Surge Category 1", "Surge Category 2", "Surge Category 3", "Sea Level 1 Foot",
                           #            "Sea Level 2 Foot", "Sea Level 3 Foot")
                           )
         
         )

# brewer.pal(n=9,"OrRd")
OrRdPal <- c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000")

ui <- fluidPage(
  fluidRow(
    column(2, 
           prettyRadioButtons(
             inputId = "metro_name",
             label = NULL,
             selected = NULL,
             status = "primary",
             shape = c("round"),
             width = NULL,
             choices = c("per_sum_slr1.ft", "per_sum_slr2.ft", "per_sum_slr3.ft", "per_sum_ss.cat1","per_sum_ss.cat2")
             )
           ),
    column(10,
           fluidRow(
             column(12, 
                    selectInput('scenario', label = 'Select Scenario', choices = c("Scenario 1", "Scenario 2"), selected = "Scenario 1")
                    ),
             fluidRow(
               column(3, highchartOutput('heatmap', height = '600px')),
               column(6,leafletOutput('map', height = '550px')),
               column(3, "")
               )
             )
           )
  ), # end fluidRow
  fluidRow(
    column(2,
           radioButtons("pop_name", 
                        "Population", 
                        choices = c("blackper_est", "ltnxper_est", "whiteper_est"), 
                        selected = "blackper_est")
           ),
    column(6, 
           tabsetPanel(
             tabPanel("Relationship", 
                      highchartOutput('scatter', height = '550px')
                      # fluidRow(
                      #   column(8,
                      # plotlyOutput('scatter', height = '550px')
                      #          ),
                      #   column(4, "")
                      #   )
                      # )
                      
             ),
             tabPanel("Demographics Map",
                      # fluidRow(
                      #   column(6, 
                               leafletOutput('map2', height = '550px')
                      #          ),
                      #   column(6, highchartOutput('raceplot'))
                      # )
             )
           )
    ),
    column(4,
           fluidRow(
             column(12, highchartOutput('raceplot'))
             ),
           fluidRow(
             column(12, highchartOutput('ageplot'))
           )
    )

  ) # end fluidRow
) # end fluidPage

server <- function(input, output, session){
  
  metro <- reactive({

    data_test[[input$metro_name]]

  })
  
  
  demo <- reactive({
    data_pop[[input$pop_name]]
  })
  
  
  listen_indicator1 <- reactive({
    list(input$metro_name)
  })
  
  listen_indicator2 <- reactive({
    list(input$pop_name)
  })
  
  
  # Draw the map without selected tracts
  
  output$map <- renderLeaflet({
    
    m <- metro()
 
    pal <- colorNumeric('OrRd', c(0,100))
    
    m <- m %>% 
      mutate(label = paste0('Area: ', names, '<br/>',input$metro_name, ": ",round(m$entropy, digits = 2)))
    
    labs <- as.list(m$label)
    
    map <- leaflet(m) %>%
      addProviderTiles('CartoDB.Positron') %>%
      # fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
      clearShapes() %>%
      addPolygons(weight = 1,
                  color = "#FFFFFF",
                  # stroke = FALSE, 
                  smoothFactor = 0.2, 
                  fillColor = ~pal(entropy), fillOpacity = 0.7,
                  label = lapply(labs, HTML),
                  highlight = highlightOptions(
                    weight = 3,
                    fillOpacity = 1,
                    bringToFront = FALSE),
                  layerId = ~tract_id) %>%
      addLegend(position = 'bottomright', pal = pal, 
                # values = m$entropy, 
                values = c(0,100),
                labFormat = labelFormat(suffix = "%"),
                title = input$metro_name) %>%
      addCircles(data =  filter(schools_sf),
                 group="Schools",
                 label = ~str_to_title(NAME)) %>%
      addLayersControl(overlayGroups = c("Schools"),
                       options = layersControlOptions(collapsed = FALSE), 
                       position = "topright") %>% 
      hideGroup("Schools") %>% 
      addResetMapButton()
    
    map
    
  })
  
  ## Add Map Reset button function ----
  addResetMapButton <- function(leaf) {
    leaf %>%
      addEasyButton(
        easyButton(
          icon = "ion-arrow-expand", 
          title = "Reset View", 
          onClick = JS(
            "function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"
          )
        )
      ) %>% 
      htmlwidgets::onRender(
        JS(
          "function(el, x){ 
            var map = this; 
            map.whenReady(function(){
              map._initialCenter = map.getCenter(); 
              map._initialZoom = map.getZoom();
            });
          }"
        )
      )
  }
  
  # Draw the map2 without selected tracts
  
  output$map2 <- renderLeaflet({
    req(input$pop_name)
    
    d <- demo()
    
    pal <- colorNumeric('GnBu', c(0,100))
    
    d <- d %>%
      mutate(label = paste0('Area: ', names, '<br/>',input$pop_name, ": ",round(d$entropy, digits = 2)))

    labs <- as.list(d$label)
    
    map2 <- leaflet(d) %>%
      addProviderTiles('CartoDB.Positron') %>%
      # fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
      clearShapes() %>%
      addPolygons(weight = 1,
                  color = "#FFFFFF",
                  # stroke = FALSE, 
                  smoothFactor = 0.2, 
                  fillColor = ~pal(entropy), fillOpacity = 0.7,
                  label = lapply(labs, HTML),
                  highlight = highlightOptions(
                    weight = 3,
                    fillOpacity = 1,
                    bringToFront = FALSE),
                  layerId = ~tract_id) %>%
      addLegend(position = 'bottomright', pal = pal, 
                # values = d$entropy, 
                values = c(0,100),
                labFormat = labelFormat(suffix = "%"),
                title = input$pop_name) %>%
      addCircles(data =  filter(schools_sf),
                 group="Schools",
                 label = ~str_to_title(NAME)) %>%
      addLayersControl(overlayGroups = c("Schools"),
                       options = layersControlOptions(collapsed = FALSE),
                       position = "topright") %>%
      hideGroup("Schools") %>%
      addResetMapButton()
    
    map2
    
  })
  
  # outputOptions(output, "map2", suspendWhenHidden = FALSE)
  

  # Click event for the map (will use to generate chart)
  click_tract <- eventReactive(input$map_shape_click, {

    x <- input$map_shape_click

    y <- x$id

    return(y)

  })
  
  click_tract2 <- eventReactive(input$map2_shape_click, {
    
    x <- input$map2_shape_click
    
    y <- x$id
    
    return(y)
    
  })
  
  
  # tract_ids <- reactive({
  #   eventdata <- event_data("plotly_selected", source = "source")
  #   if (is.null(eventdata)) {
  # 
  #     return(NULL) # do nothing
  # 
  #   } else {
  # 
  #     tracts <- eventdata$key
  # 
  #     return(tracts)
  #   }
  # })



  # observe({
  # 
  #   req(tract_ids())
  # 
  #   proxy <- leafletProxy('map')
  # 
  #   sub <- filter(metro(), tract_id %in% tract_ids())
  # 
  #   box <- st_bbox(sub) %>% as.vector()
  # 
  #   # Clear old selection on map, and add new selection
  #   proxy %>%
  #     clearGroup(group = 'sub') %>%
  #     addPolygons(data = sub, fill = FALSE, color = '#FFFF00',
  #                 opacity = 1, group = 'sub', weight = 1.5)
  #   # %>%
  #   #   fitBounds(lng1 = box[1],
  #   #             lat1 = box[2],
  #   #             lng2 = box[3],
  #   #             lat2 = box[4])
  # 
  # })
  

  observeEvent(list(click_tract(), listen_indicator1(), listen_indicator2()), {

    # Add the clicked tract to the map in aqua, and remove when a new one is clicked
    map <- leafletProxy('map') %>%
      removeShape('htract') %>%
      addPolygons(data = filter(demo(), tract_id == click_tract()), fill = FALSE,
                  color = '#00FFFF', opacity = 1, layerId = 'htract',
                  weight = 1.6)
    
    map2 <- leafletProxy('map2') %>%
      removeShape('htract') %>%
      addPolygons(data = filter(demo(), tract_id == click_tract()), fill = FALSE,
                  color = '#00FFFF', opacity = 1, layerId = 'htract',
                  weight = 1.6)
    
    # Add clicket tract point to scatter, remove when new one is clicked 
    d <- filter(metro(), tract_id == click_tract()) %>% 
      st_drop_geometry()
    d$xname <- input$pop_name
    d$ylabel <- input$metro_name

    scatter <- highchartProxy("scatter") %>%
      hcpxy_remove_series(id = "addpoint") %>% 
      hcpxy_add_series(
        data = d, 
        "scatter",
        hcaes(
          id=tract_id,
          x=.data[[input$pop_name]],
          y = entropy
        ),
        marker = list(
          symbol = "circle",
          radius = 8,
          lineWidth = 2,
          lineColor = "#00FFFF"
        ),
        id = "addpoint"
      )
    

  })
  
  # observeEvent(list(click_tract2(), listen_indicator1(), listen_indicator2()), {
  #   
  #   # Add the clicked tract to the map in aqua, and remove when a new one is clicked
  #   map <- leafletProxy('map') %>%
  #     removeShape('htract') %>%
  #     addPolygons(data = filter(demo(), tract_id == click_tract2()), fill = FALSE,
  #                 color = '#00FFFF', opacity = 1, layerId = 'htract',
  #                 weight = 1.6)
  #   
  #   map2 <- leafletProxy('map2') %>%
  #     removeShape('htract') %>%
  #     addPolygons(data = filter(demo(), tract_id == click_tract2()), fill = FALSE,
  #                 color = '#00FFFF', opacity = 1, layerId = 'htract',
  #                 weight = 1.6)
  #   
  # })
  
  
  
  tract_data <- reactive({

    # Fetch data for the clicked tract
    return(filter(metro(), tract_id == click_tract()))

  })
  
  # Here, we draw the scatterplot with highcharters
  
  output$scatter <- renderHighchart({
    d <- st_drop_geometry(metro())
    
    d$xname <- input$pop_name
    d$ylabel <- input$metro_name
    
    chart <- highchart() %>% 
      hc_legend(enabled = FALSE) %>%
      hc_add_series(
        data = d,
        type = "scatter",
        animation=FALSE,
        mapping = 
          hcaes(
            id=tract_id,
            x=.data[[input$pop_name]],
            y = entropy,
            names = names,
            xname = xname,
            ylabel = ylabel
          ),
        id="scatter1",
        marker = list(
          radius = 6
        )
      ) %>% 
      hc_xAxis(min = 0,
               max = 100,
               title = list(text = input$pop_name)) %>%
      hc_yAxis(title = list(text = input$metro_name)) %>% 
      hc_add_theme(hc_theme_smpl()) %>% 
      hc_colorAxis(
        min = 0,
        max = 100,
        stops = color_stops(9, OrRdPal)
      ) %>% 
      hc_tooltip(
        formatter = JS("function(){
                            return ('<strong>Area:</strong> ' + this.point.names +  ' <br>' + this.point.ylabel + ': ' + this.y + ' <br>' + this.point.xname + ': '  + this.x)
                            }")
      ) %>% 
      hc_plotOptions(
        series = list(states = list(inactive = list(opacity = 1)))
      )
      
    chart
  })
  
  ## Race/ethnicity, selected tract (click on the map to show chart)
  output$raceplot <- renderHighchart({

    td <- tract_data()

    chart <- highchart() %>%
      hc_chart(type = 'column') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = c('White', 'Black', 'Hispanic', 'All Others'), title = list(text = 'Race/ethnicity')) %>%
      hc_yAxis(min = 0,
               max = 100,
               title = list(text = 'Percent of Population'),
               labels = list(format = '{text}%')) %>%
      hc_plotOptions(series = list(dataLabels = list(format = '{y}%', enabled = TRUE))) %>%
      hc_add_series(name = 'Population, 2020', data = c(td$whiteper_est,
                                                        td$blackper_est,
                                                        td$ltnxper_est,
                                                        td$remainper_est)) %>%
      hc_title(text = paste0(td$names, ' (Block group ', td$tract_id, '), ', td$locality, ', Virginia'),
               align = 'left') %>%
      hc_subtitle(text = paste0('Race & Ethnicity in ', input$pop_name, ': ', as.character(round(td$entropy, 2)), '<br>Total Population:', as.character(td$totpop_est)),
                  align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c('#0073c1', '#0073c1')) %>%
      hc_tooltip(enabled = FALSE)


    chart

  })
  
  output$ageplot <- renderHighchart({
    
    td <- tract_data()
    
    chart <- highchart() %>%
      hc_chart(type = 'column') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = c('Under 18', '18-64 yrs', '65+'), title = list(text = 'Age')) %>%
      hc_yAxis(min = 0,
               max = 100,
               title = list(text = 'Percent of Population'),
               labels = list(format = '{text}%')) %>%
      hc_plotOptions(series = list(dataLabels = list(format = '{y}%', enabled = TRUE))) %>%
      hc_add_series(name = 'Population, 2020', data = c(td$age17per_est,
                                                        td$age18to64per_est,
                                                        td$age65per_est)) %>%
      hc_title(text = paste0('Age of Population in ', td$names, ' (Block group ', td$tract_id, '), ', td$locality, ', Virginia'),
               align = 'left') %>%
      # hc_subtitle(text = paste0(input$pop_name, ': ', as.character(round(td$entropy, 2)), '<br>Total Population:', as.character(td$totpop_est)),
      #             align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c('#7f50ca', '#7f50ca')) %>%
      hc_tooltip(enabled = FALSE)
    
    
    chart
    
  })
  
  output$heatmap <- renderHighchart({
    sel <- input$metro_name

    vars <- flood_blkgrp_long %>% 
      group_by(variable, names)

    vars <- if(str_detect(sel, "ss.cat")){
         vars %>% filter(group == "StormSurge")
       } else if (str_detect(sel, "slr")) {
         vars %>% filter(group == "SeaLevelRise")
       } else { vars }
       
    chart <- hchart(vars, 
                    "heatmap", 
                    hcaes(x = variable, 
                          y = names, 
                          value = risk), 
                    name = "Risk",
                    showInLegend = c(FALSE)
                    ) %>% 
      hc_colorAxis(
        min = 0,
        max = 100,
        stops = color_stops(9, OrRdPal)
      ) %>% 
      hc_xAxis(title = list(text = ""),
               opposite = TRUE) %>% 
      hc_yAxis(
        title = list(text = ""),
        reversed = TRUE, 
        # offset = 20,
        # tickLength = 0,
        # gridLineWidth = 0,
        # minorGridLineWidth = 0,
        labels = list(style = list(fontSize = "9px"),
                      align = "right",
                      padding = 0,
                      step = 1)
      ) %>% 
      hc_legend(
        layout = "horizontal",
        verticalAlign = "bottom",
        align = "center",
        valueDecimals = 0,
        margin = 8
      ) %>% 
      hc_size(height = 600)
      # hc_chart(type = 'heatmap') %>%
      # hc_legend(enabled = FALSE) %>%
      # hc_add_series(
      #   data = vars,
      #   type = "heatmap",
      #   animation=FALSE,
      #   mapping = 
      #     hcaes(
      #       x=variable,
      #       y = names,
      #       value = risk
      #     ),
      #   id="heat1"
      # )
    
    chart
  })
  
  
 #  output$heatmap <- renderPlotly({
 #    
 #    sel <- input$metro_name
 #    
 #    vars <- if(str_detect(sel, "ss.cat")){
 #      flood_blkgrp_long %>% filter(group == "StormSurge")
 #    } else if (str_detect(sel, "slr")) {
 #      flood_blkgrp_long %>% filter(group == "SeaLevelRise")
 #    } else { flood_blkgrp_long }
 #    
 #    vars_acc <- vars %>% 
 #      filter(COUNTYFP == "001")
 #    
 #    vars_nor <- vars %>% 
 #      filter(COUNTYFP == "131")
 #    
 #    p <- ggplot(vars, aes(x=variable,y=names, key = GEOID, group = COUNTYFP, fill = risk)) +
 #      # geom_tile(aes(fill = risk)) +
 #      geom_tile(colour="gray90", linewidth=1.5, stat="identity") +
 #      # scale_fill_gradient(low = "white", high = "dodgerblue", space = "Lab", na.value = "gray90", guide = "colourbar") +
 #      scale_x_discrete(expand = c(0, 0)) +
 #      scale_y_discrete(expand = c(0, 0)) +
 #      scale_fill_distiller(palette = "OrRd", direction = 1)+
 #      facet_grid(COUNTYFP ~ ., 
 #                 scales = "free_y", 
 #                 space = "free_y",
 #                 # axes = "all", axis.labels = "all",
 #                 switch = "both",
 #                 labeller = as_labeller(c("001"='Accomack County', "131"='Northhampton'))) +
 #      # facet_wrap(~COUNTYFP, ncol=1, strip.position = "bottom", scales = "free_x")
 #      # facet_wrap(~COUNTYFP, ncol=1, switch='y') +
 #      theme(legend.position="none",
 #            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
 #            axis.text = element_text(size = 8),
 #            panel.spacing = unit(0, "lines"), 
 #            strip.background = element_blank(),
 #            strip.placement = "outside")
 # 
 #    ggplotly(p) %>%
 #      layout(xaxis = list(side = "top", title=""),
 #                      yaxis = list(title = ""),
 #                      showlegend = FALSE) %>%
 #      config(displayModeBar = FALSE)
 # 
 #    g1 <- ggplotly(p, source = 'source2', tooltip = "text") %>%
 #      layout(clickmode = 'event+select',
 #             xaxis = list(side = "top", title=""),
 #             yaxis = list(title = ""),
 #             showlegend = FALSE,
 #             margin = list(l = 100)) %>%
 #      config(displayModeBar = FALSE) %>%
 #      event_register("plotly_selecting")
 #    
 #    # p <- ggplot(vars, aes(x=variable,y=names, key = GEOID)) + 
 #    #   geom_tile(aes(fill = risk)) +
 #    #   scale_fill_distiller(palette = "OrRd", direction = 1)+
 #    #   theme(legend.position="none",
 #    #         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
 #    #         axis.text = element_text(size = 8))
 #    # 
 #    # ggplotly(p) %>% 
 #    #   layout(xaxis = list(side = "top", title=""), 
 #    #                   yaxis = list(title = ""),
 #    #                   showlegend = FALSE) %>% 
 #    #   config(displayModeBar = FALSE)
 #    # 
 #    # g1 <- ggplotly(p, source = 'source2', tooltip = "text") %>% 
 #    #   layout(clickmode = 'event+select', 
 #    #          xaxis = list(side = "top", title=""), 
 #    #          yaxis = list(title = ""),
 #    #          showlegend = FALSE,
 #    #          margin = list(l = 100)) %>% 
 #    #   config(displayModeBar = FALSE) %>%
 #    #   event_register("plotly_selecting")
 #    
 #    
 #    # new version with split counties
 #    
 #    # gg1 <- ggplotly((
 #    #   ggplot(vars_acc, 
 #    #          aes(x=variable,y=names
 #    #              # label = Tree, 
 #    #              # text = stringr::str_wrap(
 #    #              #   string = tooltip,
 #    #              #   width = 25,
 #    #              #   indent = 1, # let's add extra space from the margins
 #    #              #   exdent = 1  # let's add extra space from the margins
 #    #              # )
 #    #              )
 #    #   ) + 
 #    #     geom_tile(aes(fill = risk)) +
 #    #     scale_fill_distiller(palette = "OrRd", direction = 1)+
 #    #     theme_minimal()+
 #    #       theme(legend.position="none",
 #    #             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
 #    #             axis.text = element_text(size = 8)))
 #    # ) %>%
 #    #   layout(xaxis = list(side = "top", title=""), 
 #    #                            yaxis = list(title = ""),
 #    #                            showlegend = FALSE) %>%
 #    #            config(displayModeBar = FALSE) %>% 
 #    #   add_annotations(
 #    #       text = "Accomack County",
 #    #       x = 0,
 #    #       y = 1,
 #    #       yref = "paper",
 #    #       xref = "paper",
 #    #       xanchor = "center",
 #    #       yanchor = "bottom",
 #    #       yshift = 0,
 #    #       xshift = -70,
 #    #       showarrow = FALSE,
 #    #       font = list(size = 12)
 #    #     )
 #    # 
 #    # gg2 <- ggplotly((
 #    #   ggplot(vars_nor, 
 #    #          aes(x=variable,y=names
 #    #              # label = Tree, 
 #    #              # text = stringr::str_wrap(
 #    #              #   string = tooltip,
 #    #              #   width = 25,
 #    #              #   indent = 1, # let's add extra space from the margins
 #    #              #   exdent = 1  # let's add extra space from the margins
 #    #              # )
 #    #          )
 #    #   ) + 
 #    #     geom_tile(aes(fill = risk)) +
 #    #     scale_fill_distiller(palette = "OrRd", direction = 1)+
 #    #     theme_minimal()+
 #    #     theme(legend.position="none",
 #    #           axis.text.x = element_blank(),
 #    #           # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
 #    #           axis.text = element_text(size = 8))
 #    #   )
 #    # ) %>%
 #    #   layout(xaxis = list(side = "top", title=""), 
 #    #          yaxis = list(title = ""),
 #    #          showlegend = FALSE) %>%
 #    #   config(displayModeBar = FALSE) %>% 
 #    #   add_annotations(
 #    #     text = "Northampton County",
 #    #     x = 0,
 #    #     y = 1,
 #    #     yref = "paper",
 #    #     xref = "paper",
 #    #     xanchor = "center",
 #    #     yanchor = "bottom",
 #    #     yshift = 0,
 #    #     xshift = -70,
 #    #     showarrow = FALSE,
 #    #     font = list(size = 12)
 #    #   )
 #    # 
 #    # subplot(gg1, gg2, nrows = 2, 
 #    #         margin = c(0.02, 0.02, 0.05, 0.01), # c(left, right, top, bottom )
 #    #         # shareX = TRUE,
 #    #         titleX = FALSE)
 # 
 # 
 # } )
  
  
}

shinyApp(ui, server)