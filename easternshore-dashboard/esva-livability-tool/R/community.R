# Module for Community Characteristics Tab

# Read in data ----
esva_bbox <- c(-76.06697 , 37.04853, -75.16060,  38.04154)
blkgrp_dat <- qs2::qs_read("esva_app_pop_dat_8_2026.qs2")
blkgrp_input_choices = names(blkgrp_dat)
carto_url <- readRDS("esva_app_carto_url_8_2026.rds")

# communityUI ----
communityUI <- function(id) {
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      selectInput(NS(id, "community"), 
                  label = 'Topic',
                  choices = blkgrp_input_choices,
                  selected = blkgrp_input_choices[1]
      ),
      # selectInput(
      #   NS(id, 'community_yr'),
      #   label = 'Year',
      #   choices = character(0)
      # ),
      selectInput(
        NS(id, 'community_m'),
        label = 'Measure',
        choices = character(0)
      ),
      checkboxGroupInput(
        NS(id, 'community_local'),
        label = 'County',
        choices = c("Accomack County", "Northampton County"),
        selected = c("Accomack County", "Northampton County")
      )
    ),
    layout_columns(
      col_widths = 12,
      layout_columns(
        col_widths = c(8,4),
        leafletOutput(NS(id, 'map2'), width="100%", height = "650px"),
        layout_columns(
          col_widths = 12,
          fill = FALSE,
          card(
            class = "shadow-none",
            card_header(class= "p-1 m-0",
                        "Selected Measure"),
            card_body(class= "p-1 m-0",
                      htmlOutput(NS(id,"pop_measure")))
          ),
          card(
            class = "shadow-none",
            card_header(class= "p-1 m-0",
                        "Selected Measure Description"),
            card_body(class= "p-1 m-0",
                      htmlOutput(NS(id,"pop_descr")))
          ),
          card(
            class = "shadow-none",
            card_header(class= "p-1 m-0",
                        "More Information"),
            card_body(class= "p-1 m-0",
                      htmlOutput(NS(id,"pop_meta_info")))
          )
        )
      )
    )
  ) # end layout_sidebar
  
}


# communityServer ----

communityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    listen_scen <- reactive(input$community)

    # listen_yr <- reactive(input$community_yr)

    listen_m <- reactive(input$community_m)

    listen_local <- reactive(input$community_local)
    
    community <- reactive({
      req(input$community)
      for (i in seq_along(names(blkgrp_dat))) {
        if (input$community == names(blkgrp_dat)[i]) {
          d <- blkgrp_dat[[i]]
        }
      }
      d
    })
    
    # observeEvent(list(community(), listen_scen()), {
    #   choices <- names(community())
    #   # print(choices)
    #   freezeReactiveValue(input, "community_yr")
    #   freezeReactiveValue(input, "community_m")
    #   updateSelectInput(session, inputId = "community_yr", choices = choices)
    # })
    
    community_yr <- reactive({
      req(community())
        for (i in seq_along(community())) {
          # if (input$community_yr == names(community())[i]) {
            d <- community()[[i]][["measures"]]
          # }
        }
        d


    })

    observeEvent(list(community_yr(), listen_scen()), {
      choices <- names(community_yr())
      freezeReactiveValue(input, "community_m")
      updateSelectInput(session, inputId = "community_m", choices = choices)

    })

    mw <- reactive({
      req(community())
        yr <- as.character(names(community())[1])
        # print(yr)
        d <- community()[[`yr`]]

    })

    mm <- reactive({
      req(input$community_m, mw())
      ms <- as.character(input$community_m)
      d <- mw()[["measures"]][[`ms`]]

    })
    
# Metadata fields ----
    
    meta_title <- reactive({
      d <- mm()
      d <- d$title
    })
    
    output$pop_measure <- renderUI({meta_title()})
    
    pop_meas_descript <- reactive({
      d <- mm()
      d <- d$field_description
    })
    
    output$pop_descr <- renderUI({pop_meas_descript()})
    
    pop_event_description <- reactive({
      d <- mw()
      d <- d$description
    })
    
    output$pop_meta_info <- renderUI({pop_event_description()})
    
# Leaflet base map function ----
    
    renderLeafletFunction <- function(map) {
      renderLeaflet({
        leaflet() %>%
          addTiles(urlTemplate = carto_url,
                   options = providerTileOptions(minZoom = 8, maxZoom = 18)) |>
          # addProviderTiles('CartoDB.Positron',
          #                  options = providerTileOptions(minZoom = 8, maxZoom = 18)) %>%
          fitBounds(esva_bbox[1], esva_bbox[2], esva_bbox[3], esva_bbox[4]) %>%
          addResetMapButton()
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
    
## leafletProxy Map Function ---- 
    mapFunction2 <- function(mapData, mapId,
                             var, var_title,
                             legend_labels,
                             pal, sel_range){
      
      lab <- if(var_title %in% c("Median Household Income", "Median Rent", "Median house value")){
        as.list(paste0(var_title, ": $", prettyNum(round(var,0),big.mark=",")))
      } else if(var_title %in% c("Estimated Population", "Housing Units")){
        as.list(paste0(var_title, ": ", prettyNum(round(var,0),big.mark=",") ))
      } else {as.list(paste0(var_title, ": ", round(var,0), "%"))}
      
      ### map proxy
      proxy <- leafletProxy(mapId, data = mapData)
      
      ### observe
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
                      # popup = lapply(lab, HTML),
                      label = lapply(lab, HTML),
                      highlight = highlightOptions(
                        weight = 2,
                        fillOpacity = 0.9,
                        bringToFront = F)) %>%
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
    
    # render leaflet map
    output$map2 <- renderLeafletFunction()

    outputOptions(output, "map2", suspendWhenHidden = FALSE)

    observeEvent(list(listen_scen(), listen_m(), listen_local()), {
      
      p <- mm()
      
      name <- as.character(p$name)
      
      pal <- colorBin(p$col_pal,
                      p$sel_range,
                      bins = p$legend_breaks,
                      right = TRUE,
                      pretty = FALSE )
      
      p$map_data <- p$map_data %>% filter(locality %in% input$community_local)

      mapFunction2(p$map_data, "map2", p$map_data[[`name`]], p$title, p$legend_labels, pal, p$sel_range)

    })
    
  })
}