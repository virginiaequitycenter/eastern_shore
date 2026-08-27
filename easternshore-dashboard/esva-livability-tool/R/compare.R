# Module for Comparison Map Tab
# Not currently used

library(shiny)
library(leaflet)
library(leaflet.extras2)

# Read in data ----
esva_bbox <- c(-76.06697 , 37.04853, -75.16060,  38.04154)
app_dat <- qs::qread("esva_app_data_1_2026.qs")
blkgrp_dat <- qs::qread("esva_app_pop_dat_1_2026.qs")
combine_dat <- c(app_dat, blkgrp_dat)
combine_input_choices = names(combine_dat)

# compareUI ----
compareUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      "Map #1 Selections",
      selectInput(NS(id, 'compare_a'), label = 'Topic', choices = combine_input_choices, selected = combine_input_choices[1]),
      # # Only show this panel if Storm Surge is selected
      conditionalPanel(
        condition = "input.compare_a == 'Depth to Groundwater'|input.compare_a == 'Storm Surge'|input.compare_a == 'Seawater Intrusion'|input.compare_a == 'Small Area Case Study'",
        selectInput(NS(id, 'compare_a_name'), label = 'Name', choices = character(0)),
        ns=NS(id)
      ),
      selectInput(NS(id, 'compare_a_yr'), label = 'Year', choices = character(0)),
      selectInput(NS(id, 'compare_a_meas'), label = 'Measure', choices = character(0))
    ),
  layout_sidebar(
    sidebar = sidebar(
      "Map #2 Selections",
      selectInput(NS(id, 'compare_b'), label = 'Topic', choices = combine_input_choices, selected = combine_input_choices[1]),
      # # Only show this panel if Storm Surge is selected
      conditionalPanel(
        condition = "input.compare_b == 'Depth to Groundwater'|input.compare_b == 'Storm Surge'|input.compare_b == 'Seawater Intrusion'|input.compare_b == 'Small Area Case Study'",
        selectInput(NS(id, 'compare_b_name'), label = 'Name', choices = character(0)),
        ns=NS(id)
      ),
      selectInput(NS(id, 'compare_b_yr'), label = 'Year', choices = character(0)),
      selectInput(NS(id, 'compare_b_meas'), label = 'Measure', choices = character(0)),
      position = "right",
      open = TRUE
    ),
    splitLayout(cellWidths = rep("50%", 2),
                leafletOutput(NS(id, "mapcompare1"), height = 700),
                leafletOutput(NS(id, "mapcompare2"), height = 700)
    ),
    border = FALSE),
  border_radius = FALSE,
  fillable = TRUE,
  class = "p-0"
  )
  
}


# compareServer ----
compareServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    listen_compare <- reactive(input$compare_a)
    
    listen_name <- reactive(input$compare_a_name)
    
    listen_compare_yr <- reactive(input$compare_a_yr)
    
    listen_compare_meas <- reactive(input$compare_a_meas)
    
    compare_a <- reactive({
      req(input$compare_a)
      for (i in seq_along(names(combine_dat))) {
        if (input$compare_a == names(combine_dat)[i]) {
          d <- combine_dat[[i]]
        }
      }
      d
    })
    
    observeEvent(list(compare_a(), listen_compare()), {
      choices <- names(compare_a())
      # print(choices)
      freezeReactiveValue(input, "compare_a_name")
      freezeReactiveValue(input, "compare_a_yr")
      freezeReactiveValue(input, "compare_a_meas")
      if (input$compare_a %in% c('Depth to Groundwater', 'Storm Surge', 'Seawater Intrusion', 'Small Area Case Study')){
        updateSelectInput(session, inputId = "compare_a_name", choices = choices)
      } else {
        updateSelectInput(session, inputId = "compare_a_yr", choices = choices)
      }
    })
    
    
    compare_a_name <- reactive({
      if (input$compare_a %in% c('Depth to Groundwater', 'Storm Surge', 'Seawater Intrusion', 'Small Area Case Study')){
        req(compare_a(), input$compare_a_name)
        for (i in seq_along(compare_a())) {
          if (input$compare_a_name == names(compare_a())[i]) {
            d <- compare_a()[[i]]
          }
        }
        d
      }
    })

    observeEvent(list(compare_a_name(), listen_name(), listen_compare()), {
      choices <- names(compare_a_name())
      freezeReactiveValue(input, "compare_a_yr")
      freezeReactiveValue(input, "compare_a_meas")
      updateSelectInput(session, inputId = "compare_a_yr", choices = choices)
    })

    
    compare_a_yr <- reactive({
      req(compare_a(), input$compare_a_yr)
      if (input$compare_a %in% c('Depth to Groundwater', 'Storm Surge', 'Seawater Intrusion', 'Small Area Case Study')){
        for (i in seq_along(compare_a_name())) {
          if (input$compare_a_yr == names(compare_a_name())[i]) {
            d <- compare_a_name()[[i]][["measures"]]
          }
        }
        d
      } else {
        for (i in seq_along(compare_a())) {
          if (input$compare_a_yr == names(compare_a())[i]) {
            d <- compare_a()[[i]][["measures"]]
          }
        }
        d
      }

    })

    observeEvent(list(compare_a_yr(), listen_compare(), listen_compare_yr()), {
      choices <- names(compare_a_yr())
      freezeReactiveValue(input, "compare_a_meas")
      if (input$compare_a %in% c('Depth to Groundwater', 'Storm Surge', 'Seawater Intrusion', 'Small Area Case Study')){
        updateSelectInput(session, inputId = "compare_a_meas", choices = choices)
      } else {
        updateSelectInput(session, inputId = "compare_a_meas", choices = choices)
      }

    })

    cw <- reactive({
      req(input$compare_a_yr, compare_a())
      if (input$compare_a %in% c('Depth to Groundwater', 'Storm Surge', 'Seawater Intrusion', 'Small Area Case Study')){
        yr <- as.character(input$compare_a_yr)
        d <- compare_a_name()[[`yr`]]
      } else {
        yr <- as.character(input$compare_a_yr)
        d <- compare_a()[[`yr`]]
      }

    })

    cm <- reactive({
      req(input$compare_a_meas, cw())
      ms <- as.character(input$compare_a_meas)
      d <- cw()[["measures"]][[`ms`]]

    })
    
    
    ## Panel 2 selections ----
    listen_compare2 <- reactive(input$compare_a)
    
    listen_name2 <- reactive(input$compare_b_name)
    
    listen_compare_yr2 <- reactive(input$compare_b_yr)
    
    listen_compare_meas2 <- reactive(input$compare_b_meas)
    
    compare_b <- reactive({
      req(input$compare_b)
      for (i in seq_along(names(combine_dat))) {
        if (input$compare_b == names(combine_dat)[i]) {
          d <- combine_dat[[i]]
        }
      }
      d
    })
    
    observeEvent(list(compare_b(), listen_compare2()), {
      choices <- names(compare_b())
      # print(choices)
      freezeReactiveValue(input, "compare_b_name")
      freezeReactiveValue(input, "compare_b_yr")
      freezeReactiveValue(input, "compare_b_meas")
      if (input$compare_b %in% c('Depth to Groundwater', 'Storm Surge', 'Seawater Intrusion', 'Small Area Case Study')){
        updateSelectInput(session, inputId = "compare_b_name", choices = choices)
      } else {
        updateSelectInput(session, inputId = "compare_b_yr", choices = choices)
      }
    })
    
    
    compare_b_name <- reactive({
      if (input$compare_b %in% c('Depth to Groundwater', 'Storm Surge', 'Seawater Intrusion', 'Small Area Case Study')){
        req(compare_b(), input$compare_b_name)
        for (i in seq_along(compare_b())) {
          if (input$compare_b_name == names(compare_b())[i]) {
            d <- compare_b()[[i]]
          }
        }
        d
      }
    })
    
    observeEvent(list(compare_b_name(), listen_name2(), listen_compare2()), {
      choices <- names(compare_b_name())
      freezeReactiveValue(input, "compare_b_yr")
      freezeReactiveValue(input, "compare_b_meas")
      updateSelectInput(session, inputId = "compare_b_yr", choices = choices)
    })
    
    
    compare_b_yr <- reactive({
      req(compare_b(), input$compare_b_yr)
      if (input$compare_b %in% c('Depth to Groundwater', 'Storm Surge', 'Seawater Intrusion', 'Small Area Case Study')){
        for (i in seq_along(compare_b_name())) {
          if (input$compare_b_yr == names(compare_b_name())[i]) {
            d <- compare_b_name()[[i]][["measures"]]
          }
        }
        d
      } else {
        for (i in seq_along(compare_b())) {
          if (input$compare_b_yr == names(compare_b())[i]) {
            d <- compare_b()[[i]][["measures"]]
          }
        }
        d
      }
      
    })
    
    observeEvent(list(compare_b_yr(), listen_compare2(), listen_compare_yr2()), {
      choices <- names(compare_b_yr())
      freezeReactiveValue(input, "compare_b_meas")
      if (input$compare_b %in% c('Depth to Groundwater', 'Storm Surge', 'Seawater Intrusion', 'Small Area Case Study')){
        updateSelectInput(session, inputId = "compare_b_meas", choices = choices)
      } else {
        updateSelectInput(session, inputId = "compare_b_meas", choices = choices)
      }
      
    })
    
    cw2 <- reactive({
      req(input$compare_b_yr, compare_b())
      if (input$compare_b %in% c('Depth to Groundwater', 'Storm Surge', 'Seawater Intrusion', 'Small Area Case Study')){
        yr <- as.character(input$compare_b_yr)
        d <- compare_b_name()[[`yr`]]
      } else {
        yr <- as.character(input$compare_b_yr)
        d <- compare_b()[[`yr`]]
      }
      
    })
    
    cm2 <- reactive({
      req(input$compare_b_meas, cw2())
      ms <- as.character(input$compare_b_meas)
      d <- cw2()[["measures"]][[`ms`]]
      
    })
    
    ## Leaflet base map function ----
    
    renderLeafletFunction <- function(map) {
      renderLeaflet({
        leaflet() %>%
          addProviderTiles('CartoDB.Positron',
                           options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>%
          fitBounds(esva_bbox[1], esva_bbox[2], esva_bbox[3], esva_bbox[4]) %>%
          addResetMapButton()
      })
    }
    
    ## leafletProxy Map1 Function ----
    mapProxyFunctionCompare <- function(mapData, mapId,
                                 var, var_title,
                                 legend_labels,
                                 pal, sel_range, unit){
      
      # map proxy
      proxy <- leafletProxy(mapId, data = mapData)
      
      lab <- if(var_title %in% c("Median Household Income", "Median Rent", "Median house value")){
        paste0(var_title, ": $", prettyNum(round(var,0),big.mark=","))
      } else if(var_title %in% c("Estimated Population", "Housing Units")){
        paste0(var_title, ": ", prettyNum(round(var,0),big.mark=",") )
      } else if(var_title %in% c("Population under 18", "Population 65 and over","Percent White Population",
                                 "Percent Black Population","Percent Hispanic Population","Population below poverty",
                                 "High Income Households", "Vacant Housing", "Renters", "High Rent Households", "Homeowners", "High House Value"
                                 )){
        paste0(paste0(var_title, ": ", round(var,0), "%") )
      # } else if(max(sel_range) == 100){
      #   as.list(paste0(var_title, ": ", round(var,1), "%"))
      } else {paste0(var_title, ": ", round(var,2), " ", unit)}

      
      # observe
      observe({
        proxy %>%
          clearShapes() %>%
          clearControls() %>%
          addPolygons(data = mapData,
                      weight = 0.5,
                      color = "#FFFFFF",
                      smoothFactor = 0.2,
                      fillOpacity = 0.6,
                      fillColor = ~pal(var),
                      popup = ~gsub("\n", "<br>",
                                    stringr::str_wrap(lab,
                                                      width = 20,
                                                      whitespace_only = FALSE)),
                      group = var_title) %>%
          addLegend(position = 'bottomright',
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
      })
      
    }
    
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
    
    
    # Build Environmental Impact Map -------------------------------------------------------
    
    # render leaflet map
    output$mapcompare1 <- renderLeafletFunction()
    
    outputOptions(output, "mapcompare1", suspendWhenHidden = FALSE)
    
    observeEvent(list(listen_compare(), listen_compare_yr(), listen_compare_meas()), {
      p <- cm()
      
      name <- as.character(p$name)
      # var <- p$map_data[[`name`]]
      
      pal <- colorBin(p$col_pal,
                      p$sel_range,
                      bins = p$legend_breaks,
                      right = TRUE,
                      pretty = FALSE )
      
      mapProxyFunctionCompare(p$map_data, "mapcompare1", p$map_data[[`name`]], p$title, p$legend_labels, pal, p$sel_range, p$unit)
      
    })

    
    # render leaflet map
    output$mapcompare2 <- renderLeafletFunction()
    
    outputOptions(output, "mapcompare2", suspendWhenHidden = FALSE)
    
    observeEvent(list(listen_compare2(), listen_compare_yr2(), listen_compare_meas2()), {
      p <- cm2()
      
      name <- as.character(p$name)

      pal <- colorBin(p$col_pal,
                      p$sel_range,
                      bins = p$legend_breaks,
                      right = TRUE,
                      pretty = FALSE )
      
      mapProxyFunctionCompare(p$map_data, "mapcompare2", p$map_data[[`name`]], p$title, p$legend_labels, pal, p$sel_range, p$unit)
      
    })
    
    # leafletProxy("mapcompare1")  %>% 
    #   addLeafletsync(c("mapcompare1","mapcompare2"))

    observeEvent(input$pages, {
      leafletProxy("mapcompare1")  %>%
        addLeafletsync(c("mapcompare1","mapcompare2"))
    })
    
    # observe({
    #   coords <- input$mapcompare1_bounds
    #   if (!is.null(coords)) {
    #     leafletProxy("mapcompare2") %>%
    #       addLeafletsync(c("mapcompare1","mapcompare2"),
    #                      options = leafletsyncOptions(noInitialSync = FALSE,
    #                                                   syncCursor = TRUE))
    #       # fitBounds(coords$west,
    #       #           coords$south,
    #       #           coords$east,
    #       #           coords$north)
    #   }
    # 
    # })
    
    # leafletProxy("mapcompare1") %>%
    #   addLeafletsync(
    #     ids = c("mapcompare1","mapcompare2"),
    #     options = leafletsyncOptions(noInitialSync = FALSE,
    #                                  syncCursor = TRUE))

    # observe({
    # leafletProxy("mapcompare1")  %>%
    #   addLeafletsync(c("mapcompare1","mapcompare2"),
    #                  options = leafletsyncOptions(noInitialSync = FALSE,
    #                                               syncCursor = TRUE))
    # })
    
    # observeEvent(input$sync, {
    #   leafletProxy("mapcompare1") %>%
    #     addLeafletsync(c("mapcompare2", "mapcompare2"))
    #   
    # })
    
  })
}