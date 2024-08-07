# Eastern Shore of Virginia Climate Equity Atlas Prototype

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(highcharter)
library(bslib)

# Read in/wrangle data ----
load("www/app_data.Rdata")

# brewer.pal(n=5,"OrRd")
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
          selectInput(
            'scenario',
            label = 'Select Scenario:',
            choices = c("Hurricane Isabel 2003", "Hurricane Isabel 2050 Projection", 
                        "King Tide", "King Tide 2050 Projection"),
            selected = "Hurricane Isabel 2003"
          ),
          radioButtons(
            "variable",
            "Select Measure:",
            choices = c("Peak Surge", "Mean Surge", "Percent of Area Inundated"),
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
          # col_widths = c(4,8),
          col_widths = c(3,6,3),
          row_heights = c(1),
          highchartOutput('heatmap'),
          leafletOutput('map'),
          layout_columns(
            col_widths = 12,
            row_heights = c(3,2),
            htmlOutput("scenario_meta"),
            htmlOutput("blk_grp_name_map")
          )
        )
      )
    ), # end card
    navset_card_underline(
      height = 650,
      nav_panel(title = "Climate Risk + Community Details",
                layout_sidebar(
                  fillable = TRUE,
                  sidebar = sidebar(
                    selectInput('pop_name', 
                                label = 'Social/Economic Indicator:', 
                                choices = c("Percent Black Population", "Percent Hispanic Population", "Percent White Population",
                                            "Population under 18 yrs", "Population over 65 yrs", "Median Household Income"), 
                                selected = "Percent Black Population"
                    )
                  ),
                  layout_columns(
                    col_widths = c(9,3),
                    row_heights = c(1),
                    highchartOutput('scatter'),
                    htmlOutput("blk_grp_name")
                  )
                )
              ),
        nav_panel(title = "Selected Area Characteristics",
                  htmlOutput("blk_grp_detail"),
                  layout_columns(
                    col_widths = c(4,4,4),
                    row_heights = c(1),
                    highchartOutput('raceplot'),
                    highchartOutput('ageplot'),
                    highchartOutput('incomeplot')
                  )
                )
        
          

    ) # end card
    
  ), # end nav_panel
  nav_panel(title = "Willis Wharf Detail",
            layout_sidebar(
              sidebar = sidebar(
                selectInput(
                  'scenario_ww',
                  label = 'Select Scenario:',
                  choices = c("Hurricane Isabel 2003", "Hurricane Isabel 2050 Projection"),
                  selected = "Hurricane Isabel 2003"
                ),
                selectInput(
                  'variable_ww',
                  label = 'Select Measure:',
                  choices = c("Max Water Levels (m)", "Inundation Time"),
                  selected = "Max Water Levels (m)"
                )
              ),
              leafletOutput('map2', height = '700px'),
              )
  )

) # end page_navbar

server <- function(input, output, session){
  
  df <- reactive({
    var <- as.character(input$scenario)
    
    d <- switch(var,
                "Hurricane Isabel 2003" = storm_isabel, 
                "Hurricane Isabel 2050 Projection" = storm_isabel_2050, 
                "King Tide" = king_tide, 
                "King Tide 2050 Projection" = king_tide_2050)

    d <- d %>% filter(locality %in% input$locality)
    
    d[[input$variable]]
    
    print(d)
    
  })
  
  dl <- reactive({
    var <- as.character(input$scenario)
    
    d <- switch(var,
                "Hurricane Isabel 2003" = storm_isabel_hm, 
                "Hurricane Isabel 2050 Projection" = storm_isabel_2050_hm, 
                "King Tide" = king_tide_hm, 
                "King Tide 2050 Projection" = king_tide_2050_hm)
    
    d <- d %>% filter(locality %in% input$locality)
  })
  
  dw <- reactive({
    var <- as.character(input$scenario_ww)
    
    d <- switch(var,
                "Hurricane Isabel 2003" = willis_wharf, 
                "Hurricane Isabel 2050 Projection" = willis_wharf_2050)
    
    d[[input$variable_ww]]
    print(d)
  })
  
 def_scenario <- reactive({
    d <- scenario_meta
    
    d <- d %>% filter(scenario %in% input$scenario)
    
    d <- d$def
  })
 
 def_variable <- reactive({
   d <- variable_meta
   
   d <- d %>% filter(variable %in% input$variable)
   
   d <- d$def
 })
 
 unit <- reactive({
   sel <- input$variable
   
   d <- if(str_detect(sel, "Surge")){
     " m"
   } else if(str_detect(sel, "Inundated")){
     "%"
   }
   
   d
 })
 
  
  # reactive function to detect when variable 1, variable 2, or locality selection changes
  listen_closely <- reactive({
    list(input$variable, input$scenario)
  })
  
  listen_pop <- reactive(input$pop_name)
  
  listen_local <- reactive(input$locality)
  
  listen_ww <- reactive({
    list(input$variable_ww, input$scenario_ww)
  })
  
  
  output$scenario_meta <- renderUI({
    HTML(paste0('<span style="color: rgb(51, 51, 51); font-size: 18px; font-family: Roboto Condensed; font-weight: bold; fill: rgb(51, 51, 51);">Selected Scenario: ', input$scenario, '</span><br/>',
                '<span style="color: rgb(51, 51, 51); font-size: 16px; font-family: Roboto Condensed; fill: rgb(51, 51, 51);">Scenario Description: ', def_scenario(), '</span><br/><br/>',
                '<span style="color: rgb(51, 51, 51); font-size: 18px; font-family: Roboto Condensed; font-weight: bold; fill: rgb(51, 51, 51);">Mapped Climate Measure: ', input$variable, '</span><br/>',
                '<span style="color: rgb(51, 51, 51); font-size: 16px; font-family: Roboto Condensed; fill: rgb(51, 51, 51);">Definition: ', def_variable(), '</span><br/>'
    )
    )
  })
  

  
  # Map ----
  # Draw the map without selected tracts
  
  output$map <- renderLeaflet({
    
    m <- df()
    
    sel <- input$variable
    
    sel_unit <- if(str_detect(sel, "Surge")){
      " m"
    } else if(str_detect(sel, "Inundated")){
      "%"
    }
    
    sel_range <- c(min(m[[input$variable]], na.rm=TRUE), max(m[[input$variable]], na.rm=TRUE))


    # pal <- colorNumeric('OrRd', sel_range)
    # pal <- c("#FEF0D9", "#FDCC8A", "#FC8D59", "#D7301F")
    pal <- colorBin(c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"), sel_range, bins = 5, reverse = FALSE, na.color = "#808080", pretty = FALSE )
    # pal_rev <- colorQuantile(c("#FEF0D9", "#FDCC8A", "#FC8D59", "#D7301F"), sel_range, reverse = TRUE )
    pal_rev <- colorBin(c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"), sel_range, bins = 5, reverse = TRUE, na.color = "#808080", pretty = FALSE )
    
    
    m <- m %>% 
      mutate(label = paste0('Block Group: ', names, '<br/>',input$variable, ": ",round(m[[input$variable]], digits = 2), sel_unit))
    
    labs <- as.list(m$label)
    
    map <- leaflet(m) %>%
      addProviderTiles('CartoDB.Positron',
                       options = providerTileOptions(minZoom = 8, maxZoom = 14)) %>%
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
      addLegend(position = 'bottomright', pal = pal_rev,
                values = m[[input$variable]],
                # values = sel_range,
                # title = input$variable,
                title = paste0(
                  '<span style="color: rgb(51, 51, 51); font-size: 16px; font-weight: bold; fill: rgb(51, 51, 51);">',
                  input$variable, '</span><br/>', 
                  '<span style="color: rgb(51, 51, 51); font-size: 14px; font-weight: bold; fill: rgb(51, 51, 51);">',
                  'Risk Level </span>'),
                opacity = 0.7,
                # labFormat = function(type, breaks) {
                #   return(c("Higher", "", "", "", "Lower"))
                # }
                labFormat = labelFormat(transform = function(x) sort(round(x, 1), decreasing = TRUE), suffix = sel_unit)
                # labels = c("Higher Risk", "", "", "", "Lower Risk")
                ) %>%
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
  
  
  observeEvent(list(click_tract(), listen_closely(), listen_pop()), {
    
    df <- df()
    
    xmean <- mean(df[[input$pop_name]], na.rm = TRUE)
    # print(xmean)
    
    ymean <- mean(df[[input$variable]], na.rm = TRUE)
    
    d <- tract_data()
    
    xname <- input$pop_name
    ylabel <- input$variable
    
    prettyNumber <- case_when(
      input$pop_name == "Median Household Income" ~ paste0("$",prettyNum(d[[input$pop_name]], big.mark=",", preserve.width="none") ),
      .default = paste0(prettyNum(d[[input$pop_name]], big.mark=",", preserve.width="none"), "%" )
    )
    
    
    d <- d %>% 
      mutate(
        diff_pop = case_when(
          d[[input$pop_name]] <= xmean ~ "lower than the average",
          d[[input$pop_name]] > xmean ~ "higher than the average"
        ),
        diff_var = case_when(
          d[[input$variable]] <= ymean ~ "lower than the average",
          d[[input$variable]] > ymean ~ "higher than the average"
        )
      )
    
    
    
    output$blk_grp_detail <- renderUI({
      HTML(paste0('<span style="color: rgb(51, 51, 51); font-size: 18px; font-family: Roboto Condensed; font-weight: bold; fill: rgb(51, 51, 51);">Selected Area: ', d$names, '</span><br/>',
                  '<span style="color: rgb(51, 51, 51); font-size: 16px; font-family: Roboto Condensed; font-weight: bold; fill: rgb(51, 51, 51);">', d$locality, ', Virginia<br/>Block group: ', d$tract, '<br/>Population: ', as.character(d$`Total Population`), '</span>')
      )
    })
    
    output$blk_grp_name <- renderUI({
      HTML(paste0('<span style="color: rgb(51, 51, 51); font-size: 18px; font-family: Roboto Condensed; font-weight: bold; fill: rgb(51, 51, 51);">Selected Area: ', d$names, '</span><br/>',
                  '<span style="color: rgb(51, 51, 51); font-size: 16px; font-family: Roboto Condensed; fill: rgb(51, 51, 51);">', d$locality, ', Virginia<br/>Block group: ', d$tract, '<br/><strong>Population: ', as.character(d$`Total Population`), '</strong></span>',
                  '<br/><br/>',
                  '<span style="color: rgb(51, 51, 51); font-size: 16px; font-family: Roboto Condensed; fill: rgb(51, 51, 51);">', 
                  xname, ': ', prettyNumber, '<br/>',
                  ylabel, ': ', round(d[[input$variable]],2), '<br/><br/>',
                  'This block group has a <strong>', d$diff_pop, '</strong> ', tolower(input$pop_name), " and a <strong>",
                  d$diff_var, "</strong> ", tolower(input$variable), ' value.',
                  '</span>')
      )
    })
    
    
    output$blk_grp_name_map <- renderUI({
      HTML(paste0('<span style="color: rgb(51, 51, 51); font-size: 18px; font-family: Roboto Condensed; font-weight: bold; fill: rgb(51, 51, 51);">Selected Area: ', d$names, '</span><br/>',
                  '<span style="color: rgb(51, 51, 51); font-size: 16px; font-family: Roboto Condensed; fill: rgb(51, 51, 51);">', d$locality, ', Virginia<br/>Block group: ', d$tract, '<br/><strong>Population: ', as.character(d$`Total Population`),
                  '<br/>',
                  ylabel, ': ', round(d[[input$variable]],2), unit(), '</strong></span>')
      )
    })
    
  })
 
  
  observeEvent(list(click_tract(), listen_closely()), {
    
      # Add the clicked tract to the map in aqua, and remove when a new one is clicked
      map <- leafletProxy('map') %>%
        removeShape('htract') %>%
        addPolygons(data = filter(df(), tract_id == click_tract()), fill = FALSE,
                    color = '#00FFFF', opacity = 1, layerId = 'htract',
                    group = 'tract',
                    weight = 1)
      
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
      
      # dl <- filter(dl(), GEOID == click_tract()) %>% 
      #   st_drop_geometry()
      # 
      # sel <- as.character(input$scenario)
      # 
      # heat_dat <- dl %>%
      #   group_by(variable, names)
      # 
      # heat_dat <- if(sel == "2050"){
      #   heat_dat %>% filter(group == "Scenario2050")
      # } else if(sel == "2003"){
      #   heat_dat %>% filter(group == "Scenario1")
      # }
      # 
      # print(heat_dat)
      # # return ('<strong>' +
      # #           this.value + '</strong>';
      # 
      # heatmap <- highchartProxy("heatmap") %>%
      #   hcpxy_update(
      #     yAxis = list(
      #       # type="category",
      #       # categories = ylabels,
      #       # title = list(text = "",
      #       #              fontSize = "10px"),
      #       # reversed = TRUE,
      #       # offset = -10,
      #       # labels = list(
      #       #   format = '<strong>{text}</strong>'
      #       #   # formatter = list(JS("function () {
      #       #   #   console.log(this.value)
      #       #   # }"))
      #       # )
      #       # labels = list(style = list(fontSize = "11px",
      #       #                            width = 120,
      #       #                            textOverflow = 'ellipsis'
      #       #                            # whiteSpace = 'nowrap'
      #       #                         ),
      #       # rotation = 0,
      #       # align = "right",
      #       # padding = 0,
      #       # step = 1,
      #       # formatter = JS("function () {
      #       #           console.log('this text: ', this.names);
      #       #           
      #       #         }"
      #       #         )
      #       # )
      #     )
      #   )
      #   # hcpxy_remove_series(id = "addbox") %>%
      #   # hcpxy_add_point(
      #   #   point = list(x= heat_dat$variable, y =heat_dat$names, color = '#00FFFF' ), 
      #   #   # "heatmap",
      #   #   # mapping =
      #   #   #   hcaes(
      #   #   #     x=variable,
      #   #   #     y = names,
      #   #   #     value = round(risk, digits = 2)),
      #   #   # borderRadius = 2,
      #   #   # borderColor = '#00FFFF',
      #   #   # xAxis = 0,
      #   #   # yAxis = 0,
      #   #   id = "peak"
      #   # )
     
  })
  
  observeEvent(list(click_tract(), listen_pop()), {
    # Add the clicked tract to the map in aqua, and remove when a new one is clicked
    map <- leafletProxy('map') %>%
      removeShape('htract') %>%
      addPolygons(data = filter(df(), tract_id == click_tract()), fill = FALSE,
                  color = '#00FFFF', opacity = 1, layerId = 'htract',
                  group = 'tract',
                  weight = 1.6)
    
    # Add clicked tract point to scatter, remove when new one is clicked
    d <- filter(df(), tract_id == click_tract()) %>%
      st_drop_geometry()
    
    # d <- tract_data()
    
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
    # %>% 
    #   hcpxy_update(
    #     annotations = list(
    #       labels = list(
    #         list(point = list(x = round(d[[input$pop_name]], 2), y = round(d[[input$variable]], 2), xAxis = 0, yAxis = 0), text = d$`names`)
    #       )
    #     )
    #   )
    
    
  })
  
  observeEvent(listen_local(), {
    map <- leafletProxy('map') %>%
      clearGroup('tract')
    
    scatter <- highchartProxy("scatter") %>%
      hcpxy_remove_series(id = "addpoint")
  })
  
  tract_data <- reactive({
    
    # Fetch data for the clicked tract
    return(filter(df(), tract_id == click_tract()))
    
  })
  
  # Heatmap ----
  
  output$heatmap <- renderHighchart({
    # d <- st_drop_geometry(dl())
    d <- dl()
    
    sel <- as.character(input$scenario)
    
    risk_max <- max(d$risk, na.rm=TRUE)
    risk_min <- min(d$risk, na.rm=TRUE)
    
    ind <- d %>% filter(variable == "Percent of Area Inundated")
    max_ind <- max(ind$risk, na.rm=TRUE)
    min_ind <- min(ind$risk, na.rm=TRUE)
    
    
    heat_dat <- d %>%
      group_by(variable, names)
    
    # heat_dat <- if(sel == "2050"){
    #   heat_dat %>% filter(group == "Scenario2050")
    # } else if(sel == "2003"){
    #   heat_dat %>% filter(group == "Scenario1")
    # }
    # 
    # ylabels <- as.list(heat_dat$names) %>% unique()
    ylabels <- heat_dat$names[order(heat_dat$names, na.last = NA)]
    ylabels <- ylabels %>% unique()

    # print(ylabels)

    # highchart heatmap using the hc_add_series for each storm surge variable (separate series)
    # axis labels not working - labels showing index value instead
    chart_add_series <- highchart() %>%
      hc_chart(type = "heatmap") %>%
      hc_add_series(
        data = heat_dat %>% filter(variable == "Peak Surge"),
        type = "heatmap",
        animation=FALSE,
        mapping =
          hcaes(
            x=variable,
            y = names,
            value = round(risk, digits = 2)),
        id = "peak",
        xAxis = 0,
        yAxis = 0,
        name = "Peak Surge"
      ) %>%
      hc_add_series(
        data = heat_dat %>% filter(variable == "Mean Surge"),
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
        xAxis = 1,
        yAxis = 0
      ) %>%
      hc_add_series(
        data = heat_dat %>% filter(variable == "Percent of Area Inundated"),
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
        xAxis = 2,
        yAxis = 0
      ) %>%
      hc_colorAxis(
        list(
          # min = risk_min,
          # max = risk_max,
          stops = color_stops(5, c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))
        ),
        list(
          # min = risk_min,
          # max = risk_max,
          stops = color_stops(5, c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))
        ),
        list(
          # min = min_ind,
          # max = max_ind,
          stops = color_stops(5, c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))
        )
      ) %>%
      hc_xAxis(
        list(
          type="category",
          # categories = list("Peak Surge"),
          title = list(enabled = FALSE),
          opposite = TRUE,
          labels = list(rotation = -90,
                        # format = '{text}'
                        formatter = JS("function(){
                            return (`Peak Surge`)
                            }")
                        ),
          width = '33%',
          offset = 0
        ),
        list(
          type="category",
          title = list(text = ""),
          opposite = TRUE,
          labels = list(rotation = -90,
                        formatter = JS("function(){
                            return (`Mean Surge`)
                            }")
                        ),
          width = '33%',
          left = '33.33%',
          offset = 0
        ),
        list(
          type="category",
          title = list(text = ""),
          opposite = TRUE,
          labels = list(rotation = -90,
                        formatter = JS("function(){
                            return (`% Area Inundated`)
                            }")
                        ),
          width = '33%',
          left = '66.66%',
          offset = 0
        )
        ) %>%
      hc_yAxis(
        type="category",
        categories = ylabels,
        title = list(text = "",
                     fontSize = "10px"),
        reversed = TRUE,
        offset = -10,
        # labels = list(
        #   format = '{text}'
        # )
        labels = list(style = list(fontSize = "11px",
                                   width = 120,
                                   textOverflow = 'ellipsis'
                                   # whiteSpace = 'nowrap'
                                   ),
        rotation = 0,
        align = "right",
        padding = 0,
        step = 1
        # formatter = JS("function(){
        #                     return (this.point.names)
        #                     }")
        )
        ) %>%
      hc_plotOptions(series = list(states = list(inactive = list(opacity = 0.8)),
                                   pointWidth=1,
                                   opacity = 0.8),
                     column = list(cropThreshold = 1000,
                                   stacking = "normal")) %>%
      hc_tooltip(
        formatter = JS("function(){
                            return ('<strong>Block Group:</strong> ' + this.point.names +  ' <br><strong>' + this.point.variable + ':</strong> '  + this.point.value)
                            }")
        ) %>%
      hc_legend(enabled = FALSE)

    chart_add_series
    
    # highchart heatmap using hchart, all data as one series
    # axis labels working
    
    # chart_heat <- hchart(heat_dat,
    #                 "heatmap",
    #                 hcaes(x = variable,
    #                       y = names,
    #                       value = round(risk, digits = 2)),
    #                 name = "Risk",
    #                 showInLegend = c(FALSE)) %>%
    #   hc_colorAxis(
    #     # min = 0,
    #     # max = risk_max,
    #     stops = color_stops(5, c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))
    #   ) %>%
    #   hc_xAxis(type="category",
    #     title = list(text = ""),
    #            opposite = TRUE,
    #     labels = list(rotation = -90)) %>%
    #   hc_yAxis(
    #     title = list(text = "",
    #                  fontSize = "10px"),
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
    #   hc_tooltip(
    #     formatter = JS("function(){
    #                         return ('<strong>Area:</strong> ' + this.point.names +  ' <br><strong>' + this.point.variable + ':</strong> '  + this.point.value)
    #                         }")
    #   ) %>%
    #   hc_legend(enabled = FALSE)
    # 
    # 
    # chart_heat
    
    
  })
  
  
  # Scatterplot ----
  
  output$scatter <- renderHighchart({
    d <- st_drop_geometry(df())
    
    d$xname <- input$pop_name
    d$ylabel <- input$variable
    
    sel <- input$variable
    
    # sel_max <- if(str_detect(sel, "Surge")){
    #   max(d$SurgeMax)
    # } else if(str_detect(sel, "Inundation")){
    #   max(d$InundationMax)
    # }
    
    # sel_min <- if(str_detect(sel, "Surge")){
    #   min(d$SurgeMax)
    # } else if(str_detect(sel, "Inundation")){
    #   min(d$InundationMax)
    # }
    
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
               # max = sel_max,
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
        stops = color_stops(5, c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))
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
      # hc_title(text = paste0(td$names, ' (Block group ', td$tract, '), ', td$locality, ', Virginia', '<br>Population: ', as.character(td$`Total Population`)),
      #          align = 'left') %>%
      hc_title(text = paste0('Race & Ethnicity'),
                  align = 'left') %>%
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
      hc_title(text = paste0('Age'),
                  align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c('#7f50ca', '#7f50ca')) %>%
      hc_tooltip(enabled = FALSE)
    
    chart
    
  })
  
  # Income plot ----
  
  output$incomeplot <- renderHighchart({
    
    d <- st_drop_geometry(df())
    td <- st_drop_geometry(tract_data())
    
    # td <- td %>%
    #   mutate(`Median Household Income` = case_when(
    #     is.na(`Median Household Income`) ~ NULL,
    #     .default = `Median Household Income`
    #   ))

    print(td$`Median Household Income`)

    chart <- hchart(d,
                    "scatter",
                    hcaes(y = `Median Household Income`, x = 1, color = `Median Household Income`),
                    ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_yAxis(title = list(text = 'Median Household Income'),
               # opposite = TRUE,
               showFirstLabel = TRUE,
               startOnTick = TRUE,
               endOnTick = TRUE) %>%
      hc_xAxis(title = list(enabled = FALSE),
               labels = list(enabled = FALSE)) %>%
      hc_plotOptions(series = list(states = list(inactive = list(opacity = 1)),
                                   hover = list(enabled = FALSE),
                                   events = list(afterAnimate = JS("function () {
                                                  console.log('this y: ', this.points[0].y);
                                                if(this.points[0].y === null){
                                                  this.chart.renderer.label(
                                                      'Data not available <br>for the selected area', 100, 50
                                                    )
                                                      .attr({
                                                        padding: 10,
                                                        r: 5,
                                                        zIndex: 8,
                                                        fill: 'grey'
                                                      })
                                                      .css({
                                                        color: 'white'
                                                      })
                                                      .add();
                                                }
                                                }"
                                     )
                                   ))
                     ) %>% 
      hc_add_theme(hc_theme_smpl()) %>%
      # hc_tooltip(enabled = FALSE) %>% 
      hc_tooltip(
        formatter = JS("function(){
                            return ('<strong>Block Group:</strong> ' + this.point.names +  ' <br>Median Household Income: ' + this.y)
                            }")
      ) %>% 
      hc_add_series(
        data = td, 
        "scatter",
        hcaes(
          id=tract_id,
          y = `Median Household Income`,
          x = 1,
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
            list(align = "left",
                 verticalAlign = "center",
                 # x = 10,
                 point = list(y = td$`Median Household Income`, 
                              x = 1, 
                              xAxis = 0, yAxis = 0), 
                 text = paste0("Selected Area: $", prettyNum(td$`Median Household Income`, big.mark=",", preserve.width="none"))
                 # formatter = JS("function(){
                 #              if (this.y == 0){
                 #                return('Data not available <br>for the selected area');
                 #              } 
                 #              return ('$' + this.y)
                 #            
                 #            }")
                   )
          )
        )
      ) %>% 
      hc_title(text = paste0('Income'),
                  align = 'left')
    
    chart
    
  })
  
  
  # Willis Warf panel ----
  
  output$map2 <- renderLeaflet({
    
    m <- dw()
    
    sel_range <- c(min(m[[input$variable_ww]], na.rm=TRUE), max(m[[input$variable_ww]], na.rm=TRUE))
    
    pal <- colorNumeric('GnBu', sel_range)
    
    map2 <- leaflet(m) %>%
      addProviderTiles('OpenStreetMap',
                       options = providerTileOptions(minZoom = 9, maxZoom = 17)) %>%
      setView(lng = -75.800288, lat = 37.522531, zoom = 14) %>%
      clearShapes() %>%
      addPolygons(weight = 1,
                  color = "#FFFFFF",
                  stroke = FALSE,
                  # smoothFactor = 0.2, 
                  fillColor = ~pal(m[[input$variable_ww]]),
                  fillOpacity = 0.5,
                  # label = lapply(labs, HTML),
                  # highlight = highlightOptions(
                  #   weight = 3,
                  #   fillOpacity = 1,
                  #   bringToFront = FALSE),
                  layerId = ~PatchID) %>% 
      addLegend(position = 'bottomright', 
                pal = pal,
                values = m[[input$variable_ww]],
                # values = sel_range,
                title = input$variable_ww,
                opacity = 0.5) %>%
      addResetMapButton()
    
    map2
    
  })
  
  
}

shinyApp(ui, server)