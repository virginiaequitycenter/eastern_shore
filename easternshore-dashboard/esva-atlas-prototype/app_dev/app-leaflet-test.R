# https://walkerke.shinyapps.io/neighborhood_diversity/
# https://github.com/walkerke/neighborhood_diversity/blob/master/neighborhood_diversity.Rmd

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
library(highcharter)


library(qs)

source("functions/utils.R")

es_blkgrp <- readRDS("www/flood_composite_blkgrp.RDS")
blkgrp_names <- read_excel("www/tract_names.xlsx", sheet = "blkgrp2020")
pop <- read_csv("www/population_blkgrp.csv")
# data <- read_csv("blkgrp_pop_data.csv")

schools_sf <- st_read("../data/schools_sf.geojson")

# make 10 percent columns
flood_blkgrp <- es_blkgrp %>% 
  mutate(across(starts_with("sum"), ~round(.x/cells *100, 2) , .names = "per_{.col}"))

counties_geo <- counties(state = 'VA', year = 2022, cb = TRUE) # from tigris / used 2021 bc 2022 caused error
counties_geo <- counties_geo %>% subset(COUNTYFP %in% c("001", "131"))
counties_geo <- st_transform(counties_geo, 4326)

# below variables for leaflet map boundary settings
bbox <- st_bbox(counties_geo) %>% as.vector()

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
  select(GEOID, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est) %>% 
  mutate(GEOID = as.character(GEOID),
         whiteper_est = round(whiteper_est,0),
         blackper_est = round(blackper_est,0),
         ltnxper_est = round(ltnxper_est,0),
         remainper_est = round(remainper_est,0))

es_blkgrp_pop <- full_tracts %>% 
  left_join(pop)

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
         whiteper_est, blackper_est, ltnxper_est, remainper_est)
  
per_sum_slr2.ft <- es_blkgrp_pop %>% 
  mutate(entropy = per_sum_slr2.ft,
         group = "SeaLevelRise") %>% 
  select(tract_id, names, locality, geometry, group,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est)

per_sum_slr3.ft <- es_blkgrp_pop %>% 
  mutate(entropy = per_sum_slr3.ft,
         group = "SeaLevelRise") %>% 
  select(tract_id, names, locality, geometry, group,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est)

per_sum_ss.cat1 <- es_blkgrp_pop %>% 
  mutate(entropy = per_sum_ss.cat1,
         group = "StormSurge") %>% 
  select(tract_id, names, locality, geometry, group,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est)

per_sum_ss.cat2 <- es_blkgrp_pop %>% 
  mutate(entropy = per_sum_ss.cat2,
         group = "StormSurge") %>% 
  select(tract_id, names, locality, geometry, group,
         entropy, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est)

data_test <- list(per_sum_slr1.ft=per_sum_slr1.ft, per_sum_slr2.ft=per_sum_slr2.ft, per_sum_slr3.ft=per_sum_slr3.ft, per_sum_ss.cat1=per_sum_ss.cat1, per_sum_ss.cat2=per_sum_ss.cat2)


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

ui <- fluidPage(
  fluidRow(
    column(1, 
           prettyRadioButtons(
             inputId = "metro_name",
             label = NULL,
             selected = NULL,
             status = "primary",
             shape = c("round"),
             width = NULL,
             choices = c("per_sum_slr1.ft", "per_sum_slr2.ft", "per_sum_slr3.ft", "per_sum_ss.cat1","per_sum_ss.cat2")
           )
           # radioButtons("metro_name", "Datasets", choices = c("mean_SeaLevelRise", "mean_FEMAFloodZones", "mean_StormSurge", "mean_HighTideFlooding"), selected = "mean_SeaLevelRise")
           ),
    column(3, plotlyOutput('heatmap', height = '600px')),
    column(5,leafletOutput('map', height = '550px')),
    column(3, highchartOutput('raceplot'))
    
  ),
  fluidRow(
    column(4, ""),
    column(8, plotlyOutput('scatter', width = "100%"))
  )
  # selectInput('metro_name', label = 'Select flood risk', choices = input_choices, selected = "mean_HighTideFlooding"),

  
)

server <- function(input, output, session){
  
  metro <- reactive({

    data_test[[input$metro_name]]

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
                  popup = lapply(labs, HTML),
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
                 popup = ~str_to_title(NAME)) %>%
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
  
  
  # data <- reactiveValues(clickedShape=NULL)
  # # observe the marker click info and print to console when it is changed.
  # observeEvent(input$map_shape_click,{
  #   data$clickedShape <- input$map_shape_click
  #   print(data$clickedShape)}
  # )
  # observeEvent(input$map_click,{
  #   data$clickedShape <- NULL
  #   print(data$clickedShape)})
  
  # Click event for the map (will use to generate chart)
  click_tract <- eventReactive(input$map_shape_click, {
    
    x <- input$map_shape_click
    
    y <- x$id
    
    return(y)
    
  })
  
  tract_ids <- reactive({
    eventdata <- event_data("plotly_selected", source = "source")
    if (is.null(eventdata)) {
      
      return(NULL) # do nothing
      
    } else {
      
      tracts <- eventdata$key
      
      return(tracts)
    }
  })
  
  
  
  observe({
    
    req(tract_ids())
    
    proxy <- leafletProxy('map')
    
    sub <- filter(metro(), tract_id %in% tract_ids())
    
    box <- st_bbox(sub) %>% as.vector()
    
    # Clear old selection on map, and add new selection
    proxy %>%
      clearGroup(group = 'sub') %>%
      addPolygons(data = sub, fill = FALSE, color = '#FFFF00',
                  opacity = 1, group = 'sub', weight = 1.5) 
    # %>%
    #   fitBounds(lng1 = box[1],
    #             lat1 = box[2],
    #             lng2 = box[3],
    #             lat2 = box[4])
    
  })
  
  
  observeEvent(click_tract(), {
    
    # Add the clicked tract to the map in aqua, and remove when a new one is clicked
    map <- leafletProxy('map') %>%
      removeShape('htract') %>%
      addPolygons(data = filter(metro(), tract_id == click_tract()), fill = FALSE,
                  color = '#00FFFF', opacity = 1, layerId = 'htract', 
                  weight = 1.6)
    
  })
  
  
  
  tract_data <- reactive({
    
    # Fetch data for the clicked tract
    return(filter(metro(), tract_id == click_tract()))
    
  })
  
  # Here, we draw the diversity gradient with ggplotly
  output$scatter <- renderPlotly({
    
    m <- metro() %>%
      mutate(text = glue::glue(
        "Area Name: {names}<br>Population: {round(totpop_est, 0)}<br>Value: {round(entropy, 2)}"
      ))
    
    p1a <- ggplot(m) + 
      geom_point(aes(totpop_est, entropy, key = tract_id, 
                                  text = text, color = entropy),
                 alpha = 0.8) + 
      scale_color_distiller(palette = "OrRd",
                            limits = c(0,100),
                            direction = 1) +
      theme_minimal(base_size = 14) + 
      # stat_smooth(aes(totpop_est, entropy), color = 'red', method = 'loess', span = input$span, se = FALSE) +
      labs(x = "Population",
           y = "")+
      theme(legend.position = "none")
    
    g <- ggplotly(p1a, source = 'source', tooltip = "text") %>% 
      layout(dragmode = 'lasso', 
             # clickmode = "event+select",
             yaxis = list(title = input$metro_name), 
             margin = list(l = 100), 
             font = list(
               # family = 'Open Sans', 
               size = 16)) %>%
      event_register("plotly_selecting")
    
  })  
  
  
  ### Race/ethnicity, selected tract (click on the map to show chart)
  output$raceplot <- renderHighchart({
    
    td <- tract_data()
    
    # lookup <- tigris::fips_codes
    # state <- unique(filter(lookup, state_code == str_sub(td$tract_id, 1, 2))$state_name)
    # county <- unique(filter(lookup, state_code == str_sub(td$tract_id, 1, 2), 
    #                         county_code == str_sub(td$tract_id, 3, 5))$county)
    
    chart <- highchart() %>%
      hc_chart(type = 'column') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = c('White', 'Black', 'Hispanic', 'All Others'), title = list(text = 'Race/ethnicity')) %>%
      hc_yAxis(title = list(text = 'Percent of Population')) %>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      hc_add_series(name = 'Population, 2020', data = c(td$whiteper_est,
                                                        td$blackper_est,
                                                        td$ltnxper_est,
                                                        td$remainper_est)) %>%
      hc_title(text = paste0(td$names, ' (Block group ', td$tract_id, '), ', td$locality, ', Virginia'),
               align = 'left') %>%
      hc_subtitle(text = paste0(input$metro_name, ': ', as.character(round(td$entropy, 2)), '<br>Total Population:', as.character(td$totpop_est)),
                  align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c('#0073c1', '#0073c1')) %>%
      hc_tooltip(enabled = FALSE)
    
    
    chart
    
  })
  
  
  output$heatmap <- renderPlotly({
    
    sel <- input$metro_name
    
    vars <- if(str_detect(sel, "ss.cat")){
      flood_blkgrp_long %>% filter(group == "StormSurge")
    } else if (str_detect(sel, "slr")) {
      flood_blkgrp_long %>% filter(group == "SeaLevelRise")
    } else { flood_blkgrp_long }
    
    vars_acc <- vars %>% 
      filter(COUNTYFP == "001")
    
    vars_nor <- vars %>% 
      filter(COUNTYFP == "131")
    
    p <- ggplot(vars, aes(x=variable,y=names, key = GEOID, group = COUNTYFP, fill = risk)) +
      # geom_tile(aes(fill = risk)) +
      geom_tile(colour="gray90", size=1.5, stat="identity") +
      # scale_fill_gradient(low = "white", high = "dodgerblue", space = "Lab", na.value = "gray90", guide = "colourbar") +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      scale_fill_distiller(palette = "OrRd", direction = 1)+
      facet_grid(COUNTYFP ~ ., 
                 scales = "free_y", 
                 space = "free_y",
                 # axes = "all", axis.labels = "all",
                 switch = "both",
                 labeller = as_labeller(c("001"='Accomack County', "131"='Northhampton'))) +
      # facet_wrap(~COUNTYFP, ncol=1, strip.position = "bottom", scales = "free_x")
      # facet_wrap(~COUNTYFP, ncol=1, switch='y') +
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text = element_text(size = 8),
            panel.spacing = unit(0, "lines"), 
            strip.background = element_blank(),
            strip.placement = "outside")

    ggplotly(p) %>%
      layout(xaxis = list(side = "top", title=""),
                      yaxis = list(title = ""),
                      showlegend = FALSE) %>%
      config(displayModeBar = FALSE)

    g1 <- ggplotly(p, source = 'source2', tooltip = "text") %>%
      layout(clickmode = 'event+select',
             xaxis = list(side = "top", title=""),
             yaxis = list(title = ""),
             showlegend = FALSE,
             margin = list(l = 100)) %>%
      config(displayModeBar = FALSE) %>%
      event_register("plotly_selecting")
    
    # p <- ggplot(vars, aes(x=variable,y=names, key = GEOID)) + 
    #   geom_tile(aes(fill = risk)) +
    #   scale_fill_distiller(palette = "OrRd", direction = 1)+
    #   theme(legend.position="none",
    #         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    #         axis.text = element_text(size = 8))
    # 
    # ggplotly(p) %>% 
    #   layout(xaxis = list(side = "top", title=""), 
    #                   yaxis = list(title = ""),
    #                   showlegend = FALSE) %>% 
    #   config(displayModeBar = FALSE)
    # 
    # g1 <- ggplotly(p, source = 'source2', tooltip = "text") %>% 
    #   layout(clickmode = 'event+select', 
    #          xaxis = list(side = "top", title=""), 
    #          yaxis = list(title = ""),
    #          showlegend = FALSE,
    #          margin = list(l = 100)) %>% 
    #   config(displayModeBar = FALSE) %>%
    #   event_register("plotly_selecting")
    
    
    # new version with split counties
    
    # gg1 <- ggplotly((
    #   ggplot(vars_acc, 
    #          aes(x=variable,y=names
    #              # label = Tree, 
    #              # text = stringr::str_wrap(
    #              #   string = tooltip,
    #              #   width = 25,
    #              #   indent = 1, # let's add extra space from the margins
    #              #   exdent = 1  # let's add extra space from the margins
    #              # )
    #              )
    #   ) + 
    #     geom_tile(aes(fill = risk)) +
    #     scale_fill_distiller(palette = "OrRd", direction = 1)+
    #     theme_minimal()+
    #       theme(legend.position="none",
    #             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    #             axis.text = element_text(size = 8)))
    # ) %>%
    #   layout(xaxis = list(side = "top", title=""), 
    #                            yaxis = list(title = ""),
    #                            showlegend = FALSE) %>%
    #            config(displayModeBar = FALSE) %>% 
    #   add_annotations(
    #       text = "Accomack County",
    #       x = 0,
    #       y = 1,
    #       yref = "paper",
    #       xref = "paper",
    #       xanchor = "center",
    #       yanchor = "bottom",
    #       yshift = 0,
    #       xshift = -70,
    #       showarrow = FALSE,
    #       font = list(size = 12)
    #     )
    # 
    # gg2 <- ggplotly((
    #   ggplot(vars_nor, 
    #          aes(x=variable,y=names
    #              # label = Tree, 
    #              # text = stringr::str_wrap(
    #              #   string = tooltip,
    #              #   width = 25,
    #              #   indent = 1, # let's add extra space from the margins
    #              #   exdent = 1  # let's add extra space from the margins
    #              # )
    #          )
    #   ) + 
    #     geom_tile(aes(fill = risk)) +
    #     scale_fill_distiller(palette = "OrRd", direction = 1)+
    #     theme_minimal()+
    #     theme(legend.position="none",
    #           axis.text.x = element_blank(),
    #           # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    #           axis.text = element_text(size = 8))
    #   )
    # ) %>%
    #   layout(xaxis = list(side = "top", title=""), 
    #          yaxis = list(title = ""),
    #          showlegend = FALSE) %>%
    #   config(displayModeBar = FALSE) %>% 
    #   add_annotations(
    #     text = "Northampton County",
    #     x = 0,
    #     y = 1,
    #     yref = "paper",
    #     xref = "paper",
    #     xanchor = "center",
    #     yanchor = "bottom",
    #     yshift = 0,
    #     xshift = -70,
    #     showarrow = FALSE,
    #     font = list(size = 12)
    #   )
    # 
    # subplot(gg1, gg2, nrows = 2, 
    #         margin = c(0.02, 0.02, 0.05, 0.01), # c(left, right, top, bottom )
    #         # shareX = TRUE,
    #         titleX = FALSE)


 } )
  
  
}

shinyApp(ui, server)