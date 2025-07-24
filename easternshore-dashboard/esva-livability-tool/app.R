# Eastern Shore of Virginia Livability Tool

# Libraries
library(here)
library(shiny)
library(tidyverse)
library(boxr)
library(jsonlite) 
library(sf)
library(leaflet)
library(leaflet.extras2)
library(RColorBrewer)
library(bslib)

# Set WD
setwd(here("esva-livability-tool"))

# Read in data
app_dat <- qs::qread("app_data_test.qs")

input_choices = names(app_dat) 
input_years = names(app_dat[[1]])
input_measures = names(app_dat[[1]][[1]][["measures"]])

ui <- page_sidebar(
  title = "ESVA Livability Tool",
  fillable = FALSE,
  sidebar = sidebar(
    selectInput(
            'scenario_ww',
            label = 'Topic',
            choices = input_choices,
            selected = input_choices[1]
          ),
    selectInput(
      'scenario_yr',
      label = 'Year',
      choices = input_years
      ),
    selectInput(
      'scenario_m',
      label = 'Measure',
      choices = input_measures
      )
    ),
  layout_columns(
    # col_widths = 12,
    col_widths = c(8,4),
    row_heights = c(1),
    leafletOutput('map', width="100%", height = "600px"),
    htmlOutput("scenario_meta")
    
    )
  
) # end page_navbar

server <- function(input, output, session){
  
  scenario_ww <- reactive({
    for (i in seq_along(input_choices)) {
      if (input$scenario_ww == names(app_dat)[i]) {
        d <- app_dat[[i]]
      }
    }
    d
  })
  
  observeEvent(scenario_ww(), {
    choices <- names(scenario_ww())
    freezeReactiveValue(input, "scenario_yr")
    updateSelectInput(inputId = "scenario_yr", choices = choices)
  })
  
  scenario_yr <- reactive({
    req(input$scenario_yr)
    for (i in seq_along(scenario_ww())) {
      if (input$scenario_yr == names(scenario_ww())[i]) {
        d <- scenario_ww()[[i]][["measures"]]
      }
    }
    d
    # print(d)
  })
  
  observeEvent(scenario_yr(), {
    choices <- names(scenario_yr())
    freezeReactiveValue(input, "scenario_m")
    updateSelectInput(inputId = "scenario_m", choices = choices)
  })
  
 dw <- reactive({
    yr <- as.character(input$scenario_yr)
    d <- scenario_ww()[[`yr`]]
    d
  })
 
 dm <- reactive({
   ms <- as.character(input$scenario_m)
   d <- dw()[["measures"]][[`ms`]]
   d
   # print(d)
 })

 event_name <- reactive({
   d <- dw()
   d <- d$event
 })

 event_title <- reactive({
   d <- dw()
   d <- d$descriptionTitle
 })
 
 event_meas_descript <- reactive({
   d <- dm()
   d <- d$field_description
   # print(d)
 })
 
 event_year <- reactive({
   d <- dw()
   d <- d$year
 })

 event_description <- reactive({
   d <- dw()
   d <- d$description
 })

 output$scenario_meta <- renderUI({
   HTML(paste0('<span><small>Selected Measure:</small><br/><b>', event_title(), '</b></span><br/><br/>',
               '<span><small>Selected Measure Description:</small><br/>', event_meas_descript(), '</span><br/><br/>',
               '<span><small>Selected Year:</small><br/>', event_year(), '</span><br/><br/>',
               '<span><small>More Information:</small><br/>', event_description(), '</span><br/>'
   )
   )
 })
  

  # Map Functions -------------------------------------------------------
  ## Leaflet base map function ----
  
 esva_bbox <- c(-76.06697 , 37.04853, -75.16060,  38.04154)

  renderLeafletFunction <- function(map) {
    renderLeaflet({
      leaflet() %>%
        addProviderTiles('CartoDB.Positron') %>%
        fitBounds(esva_bbox[1], esva_bbox[2], esva_bbox[3], esva_bbox[4]) %>%
        addResetMapButton()
    })
  }

  ## leafletProxy Map Function ----
  mapProxyFunction <- function(mapData, mapId,
                               var, var_title,
                               # descriptionTitle, 
                               legend_labels,
                               # bg_list, 
                               pal, sel_range){

    # map proxy
    proxy <- leafletProxy(mapId, data = mapData)

    # observe
    observe({
      proxy %>%
        # fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
        clearShapes() %>%
        addPolygons(data = mapData,
                    weight = 0.5,
                    color = "#FFFFFF",
                    smoothFactor = 0.2,
                    fillOpacity = 0.6,
                    fillColor = ~pal(var),
                    label = lapply(as.list(var), HTML),
                    group = var_title) %>%
        clearControls() %>%
        addLegend(position = 'topright',
                  pal = pal,
                  values = sel_range,
                  labFormat = function(type, breaks) {
                    return(legend_labels)
                  },
                  title = ~gsub("\n", "<br>",
                                stringr::str_wrap(var_title,
                                                  width = 20,
                                                  whitespace_only = FALSE)),
                  opacity = 0.6)
        # addLayersControl(
        #   baseGroups = bg_list,
        #   options = layersControlOptions(collapsed = FALSE)
        # )
    })
  }

  listen_scen <- reactive(input$scenario_ww)

  listen_yr <- reactive(input$scenario_yr)
  
  listen_m <- reactive(input$scenario_m)


  # Build Map -------------------------------------------------------

  # render leaflet map
  output$map <- renderLeafletFunction()

  observeEvent(list(listen_scen(), listen_yr(), listen_m()), {

    p <- dm()
    
    # print(p)

    name <- as.character(p$name)
    var <- p$map_data[[`name`]]

    breaks <- p$legend_breaks

    sel_range <- c(min(breaks), max(breaks))

    pal <- colorBin(c("#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#B30000"),
                    sel_range,
                    bins = breaks,
                    right = TRUE,
                    # reverse = TRUE,
                    na.color = "#808080",
                    pretty = FALSE )
    
    mapProxyFunction(p$map_data, "map", var, p$title, p$legend_labels, pal, sel_range)
      
    })

  
  # Map Reset button function ----
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
  
}

shinyApp(ui, server)
