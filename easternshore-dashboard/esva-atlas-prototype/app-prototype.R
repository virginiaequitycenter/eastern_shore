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

counties_geo <- counties(state = 'VA', year = 2022, cb = TRUE) # from tigris / used 2021 bc 2022 caused error
counties_geo <- counties_geo %>% subset(COUNTYFP %in% c("001"))
counties_geo <- st_transform(counties_geo, 4326)

# below variables for leaflet map boundary settings
bbox <- st_bbox(counties_geo) %>% as.vector()


blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp))

## add demographic/population/housing data ----
pop <- pop %>% 
  mutate(tract_id = as.character(GEOID),
         GEOID = as.character(GEOID)) 

pop_est <- pop %>% 
  select(GEOID, tract_id, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est, medhhinc_est
         ) %>% 
  mutate(GEOID = as.character(GEOID),
         whiteper_est = round(whiteper_est,0),
         blackper_est = round(blackper_est,0),
         ltnxper_est = round(ltnxper_est,0),
         remainper_est = round(remainper_est,0),
         age17per_est = round(age17per_est,0),
         age18to64per_est = round(age18to64per_est,0),
         age65per_est = round(age65per_est,0))

storm_blkgrp <- storm_blkgrp %>% 
  left_join(blkgrp_names) %>% 
  left_join(blkgrp_geo) %>% 
  left_join(pop_est)

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
         totpop_est, whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est, medhhinc_est,
         geometry)

names(storm_dat) = c(
  "GEOID", "tract_id", "locality", "localityfips", "tract", "blkgrp", "names", 
  "Peak Surge", "Mean Surge", "Inundation Area Fraction", 
  "SurgeMax", "SurgeMin", "InundationMax", "InundationMin", "totpop_est", 
  "Percent White Population", "Percent Black Population", "Percent Hispanic Population", 
  "All Others", "Population under 18 yrs", "Population 18-64 yrs", "Population over 65 yrs", 
  "Median Household Income",
  "geometry"
)

storm_2050_dat <- storm_blkgrp %>% 
  mutate(tract_id = GEOID,
         PeakSurge_m = PeakSurge2050_m, 
         MeanSurge_m = MeanSurge2050_m, 
         InundationAreaFraction = InundationAreaFraction2050) %>% 
  select(GEOID, tract_id, locality, localityfips, tract, blkgrp, names, 
         PeakSurge_m, MeanSurge_m, InundationAreaFraction, 
         SurgeMax, SurgeMin, InundationMax, InundationMin,
         totpop_est, whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est, medhhinc_est,
         geometry)  

names(storm_2050_dat) = c(
  "GEOID", "tract_id", "locality", "localityfips", "tract", "blkgrp", "names", 
  "Peak Surge", "Mean Surge", "Inundation Area Fraction", 
  "SurgeMax", "SurgeMin", "InundationMax", "InundationMin", "totpop_est", 
  "Percent White Population", "Percent Black Population", "Percent Hispanic Population", 
  "All Others", "Population under 18 yrs", "Population 18-64 yrs", "Population over 65 yrs", 
  "Median Household Income",
  "geometry"
)


## reshape for heatmap ----
storm_dat_long <- storm_blkgrp %>% 
  mutate(tract_id = GEOID) %>% 
  select(GEOID, tract_id, locality, localityfips, tract, blkgrp, names, 
         PeakSurge_m, MeanSurge_m, InundationAreaFraction) %>% 
  st_drop_geometry() %>% 
  rename_with(.fn = ~ paste0("var_", .x), .cols = c("PeakSurge_m", "MeanSurge_m", "InundationAreaFraction")) %>% 
  pivot_longer(starts_with("var_"), names_to = "variable", values_to = "risk") %>% 
  mutate(variable = case_when(variable == "var_PeakSurge_m" ~ "Peak Surge",
                              variable == "var_MeanSurge_m" ~ "Mean Surge",
                              variable == "var_InundationAreaFraction" ~ "Inundation Area Fraction"))

storm_2050_dat_long <- storm_blkgrp %>% 
  mutate(tract_id = GEOID) %>% 
  select(GEOID, tract_id, locality, localityfips, tract, blkgrp, names, 
         PeakSurge2050_m, MeanSurge2050_m, InundationAreaFraction2050) %>% 
  st_drop_geometry() %>% 
  rename_with(.fn = ~ paste0("var_", .x), .cols = c("PeakSurge2050_m", "MeanSurge2050_m", "InundationAreaFraction2050")) %>% 
  pivot_longer(starts_with("var_"), names_to = "variable", values_to = "risk") %>% 
  mutate(variable = case_when(variable == "var_PeakSurge2050_m" ~ "Peak Surge",
                              variable == "var_MeanSurge2050_m" ~ "Mean Surge",
                              variable == "var_InundationAreaFraction2050" ~ "Inundation Area Fraction"))

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
                              COUNTYFP == "131" ~ "Northampton")
  )  

# brewer.pal(n=9,"OrRd")
OrRdPal <- c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000")

ui <- page_navbar(
  title = "ESVA Climate Equity Dashboard Prototype",
  fillable = FALSE,
  nav_panel(
    "Peninsula Scale",
    card(
      height = 700,
      # full_screen = TRUE, # adds expand button
      # card_header("Map"),
      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          sliderInput("scenario", "Scenario Year:",
                      min = 2003, max = 2050,
                      value = 2003, step = 47,
                      sep = ""),
          # selectInput(
          #   'scenario',
          #   label = 'Select Scenario',
          #   choices = c("Hurricane Isabel 2003", "Hurricane Isabel 2050 Projection"),
          #   selected = "Hurricane Isabel 2003"
          # ),
          radioButtons(
            "variable",
            "Storm Surge",
            choices = c("Peak Surge", "Mean Surge", "Inundation Area Fraction"),
            selected = "Peak Surge"
          ),
          checkboxGroupInput(
            inputId = "locality",
            label = "Select Counties:",
            choices = c("Accomack" = "Accomack",
                        "Northampton" = "Northampton"),
            selected = c("Accomack", "Northampton"),
            inline = TRUE)
        ),
        layout_columns(
          col_widths = c(3,6,3),
          row_heights = c(1),
          highchartOutput('heatmap'),
          leafletOutput('map'),
          "Info Panel TBD"
        )
      )
    ), # end card
    card(
      height = 650,
      # card_header("Population Characteristics"),
      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          selectInput('pop_name', 
                      label = 'Population Characteristics', 
                      choices = c("Percent Black Population", "Percent Hispanic Population", "Percent White Population",
                                  "Population under 18 yrs", "Population over 65 yrs", "Median Household Income"), 
                      selected = "Percent Black Population"
          )
        ),
        layout_columns(
          col_widths = c(8,4),
          row_heights = c(1),
          highchartOutput('scatter'),
          layout_columns(
            col_widths = 12,
            row_heights = c(3,2,2),
            highchartOutput('raceplot'),
            highchartOutput('ageplot'),
            highchartOutput('incomeplot')
          )
          
        )
      )
    ) # end card
    
  ), # end nav_panel
  nav_panel(title = "Other Scale",
            layout_sidebar(
              sidebar = "Selection panel",
              leafletOutput('map2', height = '780px'),
              )
  )

) # end page_navbar

server <- function(input, output, session){
  
  
  df <- reactive({
    var <- as.character(input$scenario)
    
    d <- switch(var,
                "2003" = storm_dat,
                "2050" = storm_2050_dat)
    
    # d <- switch(input$scenario,
    #             "Hurricane Isabel 2003" = storm_dat,
    #             "Hurricane Isabel 2050 Projection" = storm_2050_dat)
      
    d <- d %>% filter(locality %in% input$locality)
    d[[input$variable]]
    
    print(d)
    
  })
  
  dl <- reactive({
    # d <- switch(input$scenario,
    #             "Hurricane Isabel 2003" = storm_dat_long,
    #             "Hurricane Isabel 2050 Projection" = storm_2050_dat_long)
    d <- storm_blkgrp_long %>%
      st_drop_geometry()
      
    d <- d %>% filter(locality %in% input$locality)
  })
  
  # reactive function to detect when variable 1, variable 2, or locality selection changes
  listen_closely <- reactive({
    list(input$variable, input$scenario)
  })
  
  listen_pop <- reactive(input$pop_name)
  
  listen_local <- reactive(input$locality)
  

  
  # Map ----
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
      mutate(label = paste0('Block Group: ', names, '<br/>',input$variable, ": ",round(m[[input$variable]], digits = 2)))
    
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
  
  # Add Map Reset button function
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
  
  # Click event for the map (will use to generate chart)
  click_tract <- eventReactive(input$map_shape_click, {
    
    x <- input$map_shape_click
    
    y <- x$id
    
    return(y)
    
  })
  
  tract_data <- reactive({
    
    # Fetch data for the clicked tract
    return(filter(df(), tract_id == click_tract()))
    
  })
  
  
  observeEvent(list(click_tract(), listen_closely()), {
    
      # Add the clicked tract to the map in aqua, and remove when a new one is clicked
      map <- leafletProxy('map') %>%
        removeShape('htract') %>%
        addPolygons(data = filter(df(), tract_id == click_tract()), fill = FALSE,
                    color = '#00FFFF', opacity = 1, layerId = 'htract',
                    group = 'tract',
                    weight = 1.6)
      
      # Add clicket tract point to scatter, remove when new one is clicked 
      d <- filter(df(), tract_id == click_tract()) %>% 
        st_drop_geometry()
      
      d$xname <- input$pop_name
      d$ylabel <- input$variable
      
      scatter <- highchartProxy("scatter") %>%
        hcpxy_remove_series(id = "addpoint") %>% 
        hcpxy_add_series(
          data = d, 
          "scatter",
          hcaes(
            id=tract_id,
            x=round(.data[[input$pop_name]], 2),
            y = round(.data[[input$variable]], 2),
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
  
  observeEvent(list(click_tract(), listen_pop()), {
    # Add clicked tract point to scatter, remove when new one is clicked 
    # d <- filter(df(), tract_id == click_tract()) %>% 
    #   st_drop_geometry()
    
    d <- tract_data()
    
    d$xname <- input$pop_name
    d$ylabel <- input$variable
    
    scatter <- highchartProxy("scatter") %>%
      hcpxy_remove_series(id = "addpoint") %>% 
      hcpxy_add_series(
        data = d, 
        "scatter",
        hcaes(
          id=tract_id,
          x=round(.data[[input$pop_name]], 2),
          y = round(.data[[input$variable]], 2),
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
  
  observeEvent(listen_local(), {
    map <- leafletProxy('map') %>%
      clearGroup('tract')
    
    scatter <- highchartProxy("scatter") %>%
      hcpxy_remove_series(id = "addpoint")
  })
  

  # Heatmap ----
  
  output$heatmap <- renderHighchart({
    d <- st_drop_geometry(dl())
    
    sel <- as.character(input$scenario)
    
    risk_max <- max(d$risk, na.rm=TRUE)
    
    ind <- d %>% filter(variable == "Inundation Area Fraction")
    max_ind <- max(ind$risk, na.rm=TRUE)
    
    heat_dat <- d %>%
      group_by(variable, names)
    
    heat_dat <- if(sel == "2050"){
      heat_dat %>% filter(group == "Scenario2050")
    } else if(sel == "2003"){
      heat_dat %>% filter(group == "Scenario1")
    }
    
    
    # highchart heatmap using the hc_add_series for each storm surge variable (separate series)
    # axis labels not working - labels showing index value instead
    # chart_add_series <- highchart() %>%
    #   hc_chart(type = "heatmap") %>%
    #   hc_add_series(
    #     data = heat_dat %>% filter(variable == "Peak Surge"),
    #     type = "heatmap",
    #     animation=FALSE,
    #     mapping =
    #       hcaes(
    #         x=variable,
    #         y = names,
    #         value = round(risk, digits = 2)),
    #     id = "peak",
    #     xAxis = 0,
    #     yAxis = 0,
    #     name = "Peak Surge"
    #   ) %>%
    #   hc_add_series(
    #     data = heat_dat %>% filter(variable == "Mean Surge"),
    #     type = "heatmap",
    #     animation=FALSE,
    #     mapping =
    #       hcaes(
    #         x=variable,
    #         y = names,
    #         value = round(risk, digits = 2)
    #       ),
    #     id= "mean",
    #     className = "mean",
    #     colorAxis = 1,
    #     xAxis = 1,
    #     yAxis = 0
    #   ) %>%
    #   hc_add_series(
    #     data = heat_dat %>% filter(variable == "Inundation Area Fraction"),
    #     type = "heatmap",
    #     animation=FALSE,
    #     mapping =
    #       hcaes(
    #         x=variable,
    #         y = names,
    #         value = round(risk, digits = 2)
    #       ),
    #     id= "fraction",
    #     className = "fract",
    #     colorAxis = 2,
    #     xAxis = 2,
    #     yAxis = 0
    #   ) %>%
    #   hc_colorAxis(
    #     list(
    #       min = 0,
    #       max = risk_max,
    #       stops = color_stops(9, OrRdPal)
    #     ),
    #     list(
    #       min = 0,
    #       max = risk_max,
    #       stops = color_stops(9, OrRdPal)
    #     ),
    #     list(
    #       min = 0,
    #       max = max_ind,
    #       stops = color_stops(9, OrRdPal)
    #     )
    #   ) %>%
    #   hc_xAxis(
    #     list(
    #       type="category",
    #       # categories = list("Peak Surge"),
    #       title = list(enabled = FALSE),
    #       opposite = TRUE,
    #       labels = list(rotation = -90
    #                     # format = '{text}'
    #                     # formatter = JS("function(){
    #                     #     return (series.name)
    #                     #     }")
    #                     ),
    #       width = '33%',
    #       offset = 0
    #     ),
    #     list(
    #       type="category",
    #       title = list(text = ""),
    #       opposite = TRUE,
    #       labels = list(rotation = -90),
    #       width = '33%',
    #       left = '33.33%',
    #       offset = 0
    #     ),
    #     list(
    #       type="category",
    #       title = list(text = ""),
    #       opposite = TRUE,
    #       labels = list(rotation = -90),
    #       width = '33%',
    #       left = '66.66%',
    #       offset = 0
    #     )
    #     ) %>%
    #   hc_yAxis(
    #     title = list(text = "",
    #                  fontSize = "10px"),
    #     reversed = TRUE,
    #     offset = -10,
    #     labels = list(style = list(fontSize = "11px",
    #                                width = 120,
    #                                textOverflow = 'ellipsis'
    #                                # whiteSpace = 'nowrap'
    #                                ),
    #     rotation = 0,
    #     align = "right",
    #     padding = 0,
    #     step = 1)) %>%
    #   hc_plotOptions(series = list(states = list(inactive = list(opacity = 1)),
    #                                pointWidth=1),
    #                  column = list(cropThreshold = 1000,
    #                                stacking = "normal")) %>%
    #   hc_tooltip(
    #     formatter = JS("function(){
    #                         return ('<strong>Block Group:</strong> ' + this.point.names +  ' <br><strong>' + this.point.variable + ':</strong> '  + this.point.value)
    #                         }")
    #     ) %>%
    #   hc_legend(enabled = FALSE)
    # 
    # chart_add_series
    
    # highchart heatmap using hchart, all data as one series
    # axis labels working
    
    chart_heat <- hchart(heat_dat,
                    "heatmap",
                    hcaes(x = variable,
                          y = names,
                          value = round(risk, digits = 2)),
                    name = "Risk",
                    showInLegend = c(FALSE)) %>%
      hc_colorAxis(
        min = 0,
        max = risk_max,
        stops = color_stops(9, OrRdPal)
      ) %>%
      hc_xAxis(type="category",
        title = list(text = ""),
               opposite = TRUE,
        labels = list(rotation = -90)) %>%
      hc_yAxis(
        title = list(text = "",
                     fontSize = "10px"),
        reversed = TRUE,
        offset = -10,
        # tickWidth = 1,
        # tickLength = 0,
        # gridLineWidth = 0,
        # minorGridLineWidth = 0,
        labels = list(style = list(fontSize = "11px",
                                   width = 120,
                                   textOverflow = 'ellipsis'
                                   # whiteSpace = 'nowrap'
                                   ),
                      rotation = 0,
                      align = "right",
                      padding = 0,
                      step = 1
                      )
      ) %>%
      hc_plotOptions(series = list(states = list(inactive = list(opacity = 1)),
                                   pointWidth=1),
                     column = list(cropThreshold = 1000,
                                   stacking = "normal")) %>%
      hc_tooltip(
        formatter = JS("function(){
                            return ('<strong>Area:</strong> ' + this.point.names +  ' <br><strong>' + this.point.variable + ':</strong> '  + this.point.value)
                            }")
      ) %>%
      hc_legend(enabled = FALSE)


    chart_heat
    
    
  })
  
  
  # Scatterplot ----
  
  output$scatter <- renderHighchart({
    d <- st_drop_geometry(df())
    
    d$xname <- input$pop_name
    d$ylabel <- input$variable
    
    sel <- input$variable
    
    sel_max <- if(str_detect(sel, "Surge")){
      max(d$SurgeMax)
    } else if(str_detect(sel, "Inundation")){
      max(d$InundationMax)
    }
    
    xmean <- mean(d[[input$pop_name]], na.rm = TRUE)
    # print(xmean)
    
    xmax <- max(d[[input$pop_name]], na.rm = TRUE)
    # print(xmax)
    
    ymean <- mean(d[[input$variable]], na.rm = TRUE)
    # print(ymean)
    
    plotlineX <- list(
      # color = "black", 
      dashStyle = "LongDash",
      value = xmean, 
      width = 2, 
      zIndex = 1,
      label = list(
        text = paste0("Average ", input$pop_name), verticalAlign = "bottom",
        style = list(color = "#606060"), textAlign = "left",
        rotation = -90, y = 0, x = -3
      )
    )
    
    plotlineY <- list(
      dashStyle = "LongDash",
      value = ymean, 
      width = 2, 
      zIndex = 1,
      label = list(
        text = paste0("Average ", input$variable), verticalAlign = "top",
        style = list(color = "#606060"), textAlign = "left",
        rotation = 0, y = -3, x = 0
      )
    )
    
    chart <- highchart() %>% 
      hc_legend(enabled = FALSE) %>%
      hc_add_series(
        data = d,
        type = "scatter",
        animation=FALSE,
        mapping = 
          hcaes(
            id=tract_id,
            x=round(.data[[input$pop_name]], 2),
            y = round(.data[[input$variable]], 2),
            names = names,
            xname = xname,
            ylabel = ylabel
          ),
        id="scatter1",
        marker = list(
          radius = 6
        )
      ) %>% 
      hc_xAxis(
        min = 0,
               # max = 100,
               title = list(text = input$pop_name),
               plotLines = list(plotlineX)
               ) %>%
      hc_yAxis(min = 0,
               max = sel_max,
               title = list(text = input$variable),
               plotLines = list(plotlineY)) %>% 
      hc_annotations(
        list(
          zIndex = 1,
          draggable = "",
          labelOptions = list(
            backgroundColor = "white",
            borderWidth = 0,
            # padding = 0,
            allowOverlap = TRUE
            # x = 0,
            # y = 0
          ),
          labels = list(
            list(
              verticalAlign = 'top',
              align = "left",
              point = list(x = xmean+1, y = 0, xAxis= 0),
              text = paste0("Higher ", input$pop_name, " →")
              # overflow = 'allow'
              # crop = FALSE
            ),
            list(
              verticalAlign = 'top',
              align = "right",
              point = list(x = xmean-1, y = 0, xAxis= 0),
              text = paste0("← Lower ", input$pop_name)
            ),
            list(
              verticalAlign = 'bottom',
              align = 'right',
              point = list(x = xmax, y = ymean, xAxis= 0, yAxis= 0),
              text = paste0("↑ Higher ", input$variable),
              distance = 5
            ),
            list(
              verticalAlign = 'top',
              align = 'right',
              point = list(x = xmax, y = ymean, xAxis= 0, yAxis= 0),
              text = paste0("↓ Lower ", input$variable),
              distance = -25
            )
          )
        )
      ) %>% 
      hc_add_theme(hc_theme_smpl()) %>% 
      hc_colorAxis(
        # min = 0,
        # max = 100,
        stops = color_stops(9, OrRdPal)
      ) %>% 
      hc_tooltip(
        formatter = JS("function(){
                            return ('<strong>Block Group:</strong> ' + this.point.names +  ' <br>' + this.point.ylabel + ': ' + this.y + ' <br>' + this.point.xname + ': '  + this.x)
                            }")
      ) %>% 
      hc_plotOptions(
        series = list(states = list(inactive = list(opacity = 1)))
      ) %>% 
      hc_title(text = paste0('Relationship: ', input$pop_name, ' and ', input$variable),
               align = 'left')
    
    chart
  })
  
  # Race plot ----
  ## Race/ethnicity, selected tract (click on the map to show chart)
  output$raceplot <- renderHighchart({
    
    td <- tract_data()
    
    chart <- highchart() %>%
      hc_chart(type = 'column') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = c('White', 'Black', 'Hispanic', 'All Others'), title = list(text = 'Race/ethnicity')) %>%
      hc_yAxis(min = 0,
               max = 100,
               title = list(enabled = FALSE),
               labels = list(format = '{text}%')) %>%
      hc_plotOptions(series = list(dataLabels = list(format = '{y}%', enabled = TRUE))) %>%
      hc_add_series(name = 'Population, 2020', data = c(td$`Percent White Population`,
                                                        td$`Percent Black Population`,
                                                        td$`Percent Hispanic Population`,
                                                        td$`All Others`)) %>%
      hc_title(text = paste0(td$names, ' (Block group ', td$tract, '), ', td$locality, ', Virginia', '<br>Population: ', as.character(td$totpop_est)),
               align = 'left') %>%
      # hc_subtitle(text = paste0('Race & Ethnicity in ', input$pop_name, ': ', as.character(round(td$entropy, 2)), '<br>Total Population:', as.character(td$totpop_est)),
      #             align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c('#0073c1', '#0073c1')) %>%
      hc_tooltip(enabled = FALSE)

    
    chart
    
  })
  
  # Age plot ----
  output$ageplot <- renderHighchart({
    
    td <- tract_data()
    
    chart <- highchart() %>%
      hc_chart(type = 'column') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = c('Under 18', '18-64 yrs', '65+'), title = list(text = 'Age')) %>%
      hc_yAxis(min = 0,
               max = 100,
               title = list(enabled = FALSE),
               labels = list(format = '{text}%')) %>%
      hc_plotOptions(series = list(dataLabels = list(format = '{y}%', enabled = TRUE))) %>%
      hc_add_series(name = 'Population, 2020', data = c(td$`Population under 18 yrs`,
                                                        td$`Population 18-64 yrs`,
                                                        td$`Population over 65 yrs`)) %>%
      # hc_title(text = paste0('Age of Population in ', td$names, ' (Block group ', td$tract, '), ', td$locality, ', Virginia'),
      #          align = 'left') %>%
      # hc_subtitle(text = paste0(input$pop_name, ': ', as.character(round(td$entropy, 2)), '<br>Total Population:', as.character(td$totpop_est)),
      #             align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c('#7f50ca', '#7f50ca')) %>%
      hc_tooltip(enabled = FALSE)
    
    chart
    
  })
  
  # Income plot ----
  
  output$incomeplot <- renderHighchart({
    
    d <- st_drop_geometry(df())
    td <- st_drop_geometry(tract_data())
    
    chart <- hchart(d,
                    "scatter",
                    hcaes(x = `Median Household Income`, y = 1, color = `Median Household Income`)) %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(title = list(text = 'Median Household Income'),
               opposite = TRUE,
               showFirstLabel = TRUE,
               startOnTick = TRUE,
               endOnTick = TRUE) %>%
      hc_yAxis(title = list(enabled = FALSE),
               labels = list(enabled = FALSE)) %>%
      hc_plotOptions(series = list(states = list(inactive = list(opacity = 1)),
                                   hover = list(enabled = FALSE))) %>% 
      hc_add_theme(hc_theme_smpl()) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_add_series(
        data = td, 
        "scatter",
        hcaes(
          id=tract_id,
          x = `Median Household Income`,
          y = 1,
        ),
        marker = list(
          symbol = "circle",
          radius = 8,
          lineWidth = 2,
          lineColor = "#00FFFF"
        ),
        id = "addpoint"
      ) %>% 
      hc_annotations(
        list(
          draggable = "",
          labels = list(
            list(point = list(x = td$`Median Household Income`, y = 1, xAxis = 0, yAxis = 0), text = paste0("$", td$`Median Household Income`))
          )
        )
      )
    
    # 
    chart
    
  })
  
  
  # Nav panel 2 ----
  
  output$map2 <- renderLeaflet({
    
    map2 <- leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
      addResetMapButton()
    
    map2
    
  })
  
  
}

shinyApp(ui, server)