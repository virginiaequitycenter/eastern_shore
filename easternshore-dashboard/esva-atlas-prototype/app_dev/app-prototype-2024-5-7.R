# Eastern Shore of Virginia Climate Equity Atlas Prototype

library(shiny)
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(RColorBrewer)
library(tigris)
options(tigris_use_cache = TRUE)
library(highcharter)
library(bslib)

source("functions/utils.R")

blkgrp_names <- read_excel("www/tract_names.xlsx", sheet = "blkgrp2020")
pop <- read_csv("www/population_blkgrp.csv")
# data <- read_csv("blkgrp_pop_data.csv")
schools_sf <- st_read("../data/schools_sf.geojson")
storm_surge <- read_delim("IsabelStormOutput_upd.txt")

storm_blkgrp <- storm_surge %>% 
  mutate(GEOID = as.character(GEOID))

blkgrp_geo <- block_groups(state = "VA", year = 2023, cb = TRUE)
blkgrp_geo <- blkgrp_geo %>% 
  subset(COUNTYFP %in% c("001", "131"))
blkgrp_geo <- st_transform(blkgrp_geo, 4326)


blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp))

storm_blkgrp <- storm_blkgrp %>% 
  left_join(blkgrp_names) %>% 
  left_join(blkgrp_geo)

# Find max/mins for scale
PeakSurge_m <- storm_surge %>% 
  select(PeakSurge_m) %>% 
  rename(surge = PeakSurge_m)
PeakSurge2050_m <- storm_surge %>% 
  select(PeakSurge2050_m) %>% 
  rename(surge = PeakSurge2050_m)
MeanSurge_m <- storm_surge %>% 
  select(MeanSurge_m) %>% 
  rename(surge = MeanSurge_m)
MeanSurge2050_m <- storm_surge %>% 
  select(MeanSurge2050_m) %>% 
  rename(surge = MeanSurge2050_m)

surge_range <- PeakSurge_m %>% 
  rbind(PeakSurge2050_m) %>% 
  rbind(MeanSurge_m) %>% 
  rbind(MeanSurge2050_m)
# surgemax <- max(surge_range, na.rm=TRUE)

InundationAreaFraction <- storm_surge %>% 
  select(InundationAreaFraction) %>% 
  rename(surge = InundationAreaFraction)
InundationAreaFraction2050 <- storm_surge %>% 
  select(InundationAreaFraction2050) %>% 
  rename(surge = InundationAreaFraction2050)

inund_range <- InundationAreaFraction %>% 
  rbind(InundationAreaFraction2050)

peak_s_max = max(storm_blkgrp$PeakSurge_m, na.rm=TRUE)
peak_s2050_max = max(storm_blkgrp$PeakSurge2050_m, na.rm=TRUE)

storm_blkgrp <- storm_blkgrp %>% 
  mutate(SurgeMax = max(surge_range, na.rm=TRUE),
         SurgeMin = min(surge_range, na.rm=TRUE),
         InundationMax = max(inund_range, na.rm=TRUE),
         InundationMin = min(inund_range, na.rm=TRUE))

storm_blkgrp <- sf::st_as_sf(storm_blkgrp)

storm_dat <- storm_blkgrp %>% 
  mutate(tract_id = GEOID) %>% 
  select(GEOID, tract_id, locality, localityfips, tract, blkgrp, names, 
         PeakSurge_m, MeanSurge_m, InundationAreaFraction, 
         SurgeMax, SurgeMin, InundationMax, InundationMin,
         geometry)

storm_2050_dat <- storm_blkgrp %>% 
  mutate(tract_id = GEOID,
         PeakSurge_m = PeakSurge2050_m, 
         MeanSurge_m = MeanSurge2050_m, 
         InundationAreaFraction = InundationAreaFraction2050) %>% 
  select(GEOID, tract_id, locality, localityfips, tract, blkgrp, names, 
         PeakSurge_m, MeanSurge_m, InundationAreaFraction, 
         SurgeMax, SurgeMin, InundationMax, InundationMin,
         geometry)  


## reshape for heatmap ----
storm_blkgrp_long <- storm_blkgrp %>%
  rename_with(.fn = ~ paste0("var_", .x), .cols = c("PeakSurge_m", "MeanSurge_m", "InundationAreaFraction", "PeakSurge2050_m", "MeanSurge2050_m", "InundationAreaFraction2050")) %>% 
  select(GEOID, COUNTYFP, names, starts_with("var_")) %>%
  pivot_longer(starts_with("var_"), names_to = "variable", values_to = "risk") %>%
  mutate(group = case_when(str_detect(variable, "2050") ~ "Scenario2050",
                           .default = "Scenario1"),
         # variable = str_remove(variable, "var_"),
         variable = factor(variable, levels = c("var_PeakSurge_m", "var_MeanSurge_m", "var_InundationAreaFraction", "var_PeakSurge2050_m", "var_MeanSurge2050_m", "var_InundationAreaFraction2050"),
                           labels = c("Peak Surge", "Mean Surge", "Inundation Area Fraction", "Peak Surge", "Mean Surge", "Inundation Area Fraction")
         ),
         locality = case_when(COUNTYFP == "001" ~ "Accomack",
                              COUNTYFP == "131" ~ "Northhampton")
  )  

# brewer.pal(n=9,"OrRd")
OrRdPal <- c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000")

ui <- page_fluid(
  h2("ESVA Climate Equity Dashboard Prototype"),
  card(
    height = 800,
    # full_screen = TRUE, # adds expand button
    card_header("Map"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        radioButtons(
          "variable",
          "Storm Surge",
          choices = c("PeakSurge_m", "MeanSurge_m", "InundationAreaFraction"),
          selected = "PeakSurge_m"
        )
      ),
      layout_columns(
        col_widths = c(12,3,9),
        row_heights = c(1, 6),
        selectInput('scenario', label = 'Select Scenario', choices = c("Scenario 1", "Scenario 2050"), selected = "Scenario 1"),
        highchartOutput('heatmap'),
        leafletOutput('map')
      )
    )
  )

) # end fluidPage

server <- function(input, output, session){
  
  df <- reactive({
    d <- switch(input$scenario,
                "Scenario 1" = storm_dat,
                "Scenario 2050" = storm_2050_dat)
    d[[input$variable]]
    
    print(d)
    
  })
  
  # listen_indicator1 <- reactive({
  #   list(input$variable)
  # })
  
  # Draw the map without selected tracts
  
  output$map <- renderLeaflet({
    
    m <- df()
    
    sel <- input$variable
    
    sel_range <- if(str_detect(sel, "Surge")){
      # c(min(m$SurgeMin), max(m$SurgeMax))
      c(0, max(m$SurgeMax))
    } else if(str_detect(sel, "Inundation")){
      # c(min(m$InundationMin), max(m$InundationMax))
      c(0, max(m$InundationMax))
    }

    pal <- colorNumeric('OrRd', sel_range)
    
    m <- m %>% 
      mutate(label = paste0('Area: ', names, '<br/>',input$variable, ": ",round(m[[input$variable]], digits = 2)))
    
    labs <- as.list(m$label)
    
    map <- leaflet(m) %>%
      addProviderTiles('CartoDB.Positron') %>%
      clearShapes() %>%
      addPolygons(weight = 1,
                  color = "#FFFFFF",
                  # stroke = FALSE, 
                  smoothFactor = 0.2, 
                  fillColor = ~pal(m[[input$variable]]),
                  fillOpacity = 0.7,
                  label = lapply(labs, HTML),
                  highlight = highlightOptions(
                    weight = 3,
                    fillOpacity = 1,
                    bringToFront = FALSE),
                  layerId = ~tract_id) %>% 
      addLegend(position = 'bottomright', pal = pal,
                # values = m[[input$variable]],
                values = sel_range,
                title = input$variable) %>%
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
  
  
  output$heatmap <- renderHighchart({
    sel <- input$scenario
    
    risk_max <- max(storm_blkgrp_long$risk, na.rm=TRUE)
    
    ind <- storm_blkgrp_long %>% filter(variable == "Inundation Area Fraction")
    max_ind <- max(ind$risk, na.rm=TRUE)
    
    vars_acc <- storm_blkgrp_long %>%
      mutate(hex = colorize(risk, colors = OrRdPal)
               ) %>% 
      # filter(locality == "Accomack") %>%
      group_by(variable, names)
    
    vars_acc <- vars_acc[order(vars_acc$locality, vars_acc$names),]
    
    vars_acc <- if(sel == "Scenario 2050"){
      vars_acc %>% filter(group == "Scenario2050")
    } else if(sel == "Scenario 1"){
      vars_acc %>% filter(group == "Scenario1")
    }
    
    vars_1 <- vars_acc %>% 
      filter(variable == "Peak Surge")
    
    vars_2 <- vars_acc %>% 
      filter(variable == "Mean Surge")
    
    vars_3 <- vars_acc %>% 
      filter(variable == "Inundation Area Fraction")
    
    vars_nor <- storm_blkgrp_long %>%
      filter(locality == "Northhampton") %>% 
      group_by(variable, names)
    
    vars_nor <- if(sel == "Scenario 2050"){
      vars_nor %>% filter(group == "Scenario2050")
    } else if(sel == "Scenario 1"){
      vars_nor %>% filter(group == "Scenario1")
    }
    
    # vars_acc <- vars_acc[order(vars_acc$locality, vars_acc$names),]
    # cats <- as.list(vars$names) %>% unique()
    # print(vars)
    
    # categories_grouped <- vars_acc  %>% 
    #   select(locality, names) %>% 
    #   unique() %>% 
    #   group_by(name = locality)%>% 
    #   summarise(categories = list(names))  %>%  
    #   list_parse()
    
    chart_acc <- highchart() %>% 
      hc_chart(type = "heatmap") %>%
      hc_xAxis(
        list(
          type="category",
          title = list(text = ""),
          opposite = TRUE,
          labels = list(rotation = -90),
          width = '33%',
          offset = 0
        ),
        list(
          type="category",
          title = list(text = ""),
          opposite = TRUE,
          labels = list(rotation = -90),
          width = '33%',
          left = '33.33%',
          offset = 0
        ),
        list(
          type="category",
          title = list(text = ""),
          opposite = TRUE,
          labels = list(rotation = -90),
          width = '33%',
          left = '66.66%',
          offset = 0
        )
        ) %>%
      hc_yAxis(
        title = list(text = "",
                     fontSize = "10px"),
        reversed = TRUE,
        offset = -10,
        labels = list(style = list(fontSize = "11px",
                                   width = 120,
                                   textOverflow = 'ellipsis'
                                   # whiteSpace = 'nowrap'
                                   ),
        rotation = 0,
        align = "right",
        padding = 0,
        step = 1)) %>%
      hc_plotOptions(series = list(states = list(inactive = list(opacity = 1)),
                                   pointWidth=1),
                     column = list(cropThreshold = 1000,
                                   stacking = "normal")) %>%
      hc_tooltip(
        formatter = JS("function(){
                            return ('<strong>Area:</strong> ' + this.point.names +  ' <br><strong>' + this.point.variable + ':</strong> '  + this.point.value)
                            }")
        ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_add_series(
        data = vars_1,
        type = "heatmap",
        animation=FALSE,
        mapping =
          hcaes(
            x=variable,
            y = names,
            value = round(risk, digits = 2)),
        id = "max",
        xAxis = 0) %>%
      hc_add_series(
        data = vars_2,
        type = "heatmap",
        animation=FALSE,
        mapping =
          hcaes(
            x=variable,
            y = names,
            value = round(risk, digits = 2)
          ),
        id= "mean",
        className = "mean",
        colorAxis = 1,
        xAxis = 1
      ) %>%
      hc_add_series(
        data = vars_3,
        type = "heatmap",
        animation=FALSE,
        mapping =
          hcaes(
            x=variable,
            y = names,
            value = round(risk, digits = 2)
          ),
        id= "fraction",
        className = "fract",
        colorAxis = 2,
        xAxis = 2
      )  %>%
      hc_colorAxis(
        list(
          min = 0,
          max = risk_max,
          stops = color_stops(9, OrRdPal)
        ),
        list(
          min = 0,
          max = risk_max,
          stops = color_stops(9, OrRdPal)
        ),
        list(
          min = 0,
          max = max_ind,
          stops = color_stops(9, OrRdPal)
        )
      )
    
    
    chart_acc
    
    
    # chart_acc <- highchart() %>% 
    #   hc_chart(type = "heatmap") %>%
    # #   hc_chart(vars_1,
    # #                 "heatmap",
    # #                 hcaes(x = variable,
    # #                       y = names,
    # #                       value = round(risk, digits = 2)),
    # #                 name = "Risk",
    # #                 showInLegend = c(FALSE)
    # # ) %>%
    #   hc_add_series(
    #     data = vars_1,
    #     type = "heatmap",
    #     animation=FALSE,
    #     mapping =
    #       hcaes(
    #         x=variable,
    #         y = names,
    #         value = round(risk, digits = 2)
    #       )
    #   ) %>%
    #   # hc_colorAxis(
    #   #   min = 0,
    #   #   # max = function()if(this.point.variable == "Peak Surge" | this.point.variable == "Mean Surge"){
    #   #   #   risk_max
    #   #   # } else if(this.point.variable == "Inundation Area Fraction"){
    #   #   #   max_ind
    #   #   # },
    #   #   max = risk_max,
    #   #   stops = color_stops(9, OrRdPal)
    #   # ) %>%
    #   hc_xAxis(type="category",
    #     title = list(text = ""),
    #            opposite = TRUE,
    #     labels = list(rotation = -90)) %>%
    #   hc_yAxis(
    #     title = list(text = "",
    #                  fontSize = "10px"),
    #     # categories = categories_grouped,
    #     reversed = TRUE,
    #     offset = -10,
    #     # tickWidth = 1,
    #     # tickLength = 0,
    #     # gridLineWidth = 0,
    #     # minorGridLineWidth = 0,
    #     labels = list(style = list(fontSize = "11px",
    #                                width = 120,
    #                                textOverflow = 'ellipsis'
    #                                # whiteSpace = 'nowrap'
    #                                ),
    #                   rotation = 0,
    #                   align = "right",
    #                   padding = 0,
    #                   step = 1
    #                   )
    #   ) %>%
    #   hc_plotOptions(series = list(states = list(inactive = list(opacity = 1)),
    #                                pointWidth=1),
    #                  column = list(cropThreshold = 1000,
    #                                stacking = "normal")) %>%
    #   # hc_responsive(
    #   #   rules = list(condition = list(minWidth = 200))
    #   # ) %>% 
    #   hc_tooltip(
    #     formatter = JS("function(){
    #                         return ('<strong>Area:</strong> ' + this.point.names +  ' <br><strong>' + this.point.variable + ':</strong> '  + this.point.value)
    #                         }")
    #   ) %>%
    #   # hc_size(height = 650) %>%
    # hc_legend(enabled = FALSE)   %>%
    # hc_add_series(
    #  data = vars_2,
    #     type = "heatmap",
    #     animation=FALSE,
    #     mapping =
    #       hcaes(
    #         x=variable,
    #         y = names,
    #         value = round(risk, digits = 2)
    #       ),
    #     id= "mean",
    #  className = "mean",
    #  colorAxis = 1
    #   ) %>%
    # hc_add_series(
    #   data = vars_3,
    #   type = "heatmap",
    #   animation=FALSE,
    #   mapping =
    #     hcaes(
    #       x=variable,
    #       y = names,
    #       value = round(risk, digits = 2)
    #     ),
    #   id= "fraction",
    #   className = "fract",
    #   colorAxis = 2
    # )  %>%
    #   hc_colorAxis(
    #      list(
    #         min = 0,
    #         max = risk_max,
    #         stops = color_stops(9, OrRdPal)
    #       ),
    #       list(
    #         min = 0,
    #         max = risk_max,
    #         stops = color_stops(9, OrRdPal)
    #       ),
    #       list(
    #         min = 0,
    #         max = max_ind,
    #         stops = color_stops(9, OrRdPal)
    #       )
    #   )
    # 
    # 
    # chart_acc
    


    
    
    
  })
  
}

shinyApp(ui, server)