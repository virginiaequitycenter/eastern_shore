# Eastern Shore of Virginia Livability Tool
# Last updated: 8/26/2026
# Last deployed: 8/26/2026

# Libraries ----
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(bslib)
library(qs2)
library(highcharter)
library(rcartocolor)
library(leaflet.extras2)

# Read in data ----
app_dat <- qs2::qs_read("esva_app_data_8_2026.qs2")
input_choices = names(app_dat)
esva_bbox <- c(-76.06697 , 37.04853, -75.16060,  38.04154)
carto_url <- readRDS("esva_app_carto_url_8_2026.rds")

blkgrp_dat <- qs2::qs_read("esva_app_pop_dat_8_2026.qs2")
combine_dat <- c(app_dat, blkgrp_dat)
combine_input_choices = names(combine_dat)


ui <- page_navbar(
  tags$head(includeHTML("google-analytics.html")),
  id = "pages",
  title = span(img(src = 'esva-logo.png',
              height = 50),
    "Eastern Shore of Virginia Livability Tool"),
  window_title = "Eastern Shore of Virginia Livability Tool",
  fillable = FALSE,
  nav_panel(
    "Environmental Impacts",
    layout_sidebar(
      sidebar = sidebar(
        selectInput('scenario_ww', label = 'Topic', choices = input_choices, selected = input_choices[1]),
        # # Only show this panel if Storm Surge is selected
        conditionalPanel(
          condition = "input.scenario_ww == 'Depth to Groundwater'|input.scenario_ww == 'Historic Event Storm Surge'|input.scenario_ww == 'Future Storm Surge Flooding'|input.scenario_ww == 'Seawater Intrusion'|input.scenario_ww == 'Small Area Case Study'",
          selectInput('storm', label = 'Name', choices = character(0))
        ),
        selectInput('scenario_yr', label = 'Year', choices = character(0)),
        selectInput('scenario_m', label = 'Measure', choices = character(0)),
        checkboxGroupInput('locality', label = 'County', choices = c("Accomack County", "Northampton County"), selected = c("Accomack County", "Northampton County"))
      ),
      layout_columns(
        col_widths = 12,
        layout_columns(
          col_widths = c(8,4),
          leafletOutput('map1', width="100%", height = "650px"),
          layout_columns(
            col_widths = 12,
            fill = FALSE,
            card(
              class = "shadow-none",
              card_header(class= "p-1 m-0",
                          "Selected Measure"),
              card_body(class= "p-1 m-0",
                        htmlOutput("meta_measure"))
            ),
            card(
              class = "shadow-none",
              card_header(class= "p-1 m-0",
                          "Selected Measure Description"),
              card_body(class= "p-1 m-0",
                        htmlOutput("meta_descrip"))
            ),
            card(
              class = "shadow-none",
              card_header(class= "p-1 m-0",
                          "Selected Year"),
              card_body(class= "p-1 m-0",
                        htmlOutput("meta_year"))
            ),
            card(
              class = "shadow-none",
              card_header(class= "p-1 m-0",
                          "More Information"),
              card_body(class= "p-1 m-0",
                        htmlOutput("meta_info"))
            )
          )
        ),
        card(class= "bg-light fs-5 shadow-none",
             htmlOutput("population_title")
             ),
        layout_column_wrap(
          highchartOutput('houseplot'),
          highchartOutput('totalplot'),
          width = 1/2),
        card(class= "bg-light fs-5 shadow-none",
             htmlOutput("population_subtitle"),
             
             card_body(
               class = "fs-6",
               "Values above the dashed line mean a group is overrepresented for the outcome relative to their presence in the overall population. Values below the dashed line mean a group is underrepresented for the outcome relative to their presence in the overall population."
             )
        ),
        layout_column_wrap(
          highchartOutput('blackplot'),
          highchartOutput('whiteplot'),
          highchartOutput('hispplot'),
          highchartOutput('ageplot'),
          highchartOutput('wagehomeplot'),
          highchartOutput('wageplot'),
          width = 1/3),
        card(class = "shadow-none small",
             "Housing and Demographics Data Source: U.S. Census Bureau, Demographic and Housing Characteristics, Decennial Census, 2020.",
             "Low-Wage Jobs Data Source: U.S. Census Bureau, LEHD Origin-Destination Employment Statistics (LODES), 2022."
        )
      ) # end layout_columns
    ) # end layout_sidebar
  ), # end nav_panel
  nav_panel(
    "Community Characteristics",
    communityUI("community1")
  ), # end nav_panel
 
  nav_panel(
    "Compare",
    # compareUI("compare1")
    layout_sidebar(
      sidebar = sidebar(
        "Map #1 Selections",
        selectInput('compare_a', label = 'Topic', choices = combine_input_choices, selected = combine_input_choices[1]),
        # # Only show this panel if Storm Surge is selected
        conditionalPanel(
          condition = "input.compare_a == 'Depth to Groundwater'|input.compare_a == 'Historic Event Storm Surge'|input.compare_a == 'Future Storm Surge Flooding'|input.compare_a == 'Seawater Intrusion'|input.compare_a == 'Small Area Case Study'",
          selectInput('compare_a_name', label = 'Name', choices = character(0)),
        ),
        selectInput('compare_a_yr', label = 'Year', choices = character(0)),
        selectInput('compare_a_meas', label = 'Measure', choices = character(0))
      ),
      layout_sidebar(
        sidebar = sidebar(
          "Map #2 Selections",
          selectInput('compare_b', label = 'Topic', choices = combine_input_choices, selected = combine_input_choices[1]),
          # # Only show this panel if Storm Surge is selected
          conditionalPanel(
            condition = "input.compare_b == 'Depth to Groundwater'|input.compare_b == 'Historic Event Storm Surge'|input.compare_b == 'Future Storm Surge Flooding'|input.compare_b == 'Seawater Intrusion'|input.compare_b == 'Small Area Case Study'",
            selectInput('compare_b_name', label = 'Name', choices = character(0)),
          ),
          selectInput('compare_b_yr', label = 'Year', choices = character(0)),
          selectInput('compare_b_meas', label = 'Measure', choices = character(0)),
          position = "right",
          open = TRUE
        ),
        splitLayout(cellWidths = rep("50%", 2),
                    leafletOutput("mapcompare1", height = 700),
                    leafletOutput("mapcompare2", height = 700)
        ),
        border = FALSE),
      border_radius = FALSE,
      fillable = TRUE,
      class = "p-0"
    )
    
  ), # end nav_panel
  nav_spacer(),
  nav_panel(
    "About the Project",
    "Coming Soon!"
  ) # end nav_panel
) # end page_navbar
  

server <- function(input, output, session){
  
  listen_scen <- reactive(input$scenario_ww)
  
  listen_storm <- reactive(input$storm)
  
  listen_yr <- reactive(input$scenario_yr)
  
  listen_m <- reactive(input$scenario_m)
  
  listen_local <- reactive(input$locality)
  
  scenario_ww <- reactive({
    req(input$scenario_ww)
    for (i in seq_along(names(app_dat))) {
      if (input$scenario_ww == names(app_dat)[i]) {
        d <- app_dat[[i]]
      }
    }
    d
  })
  
  observeEvent(list(listen_scen()), {
    choices <- names(scenario_ww())
    freezeReactiveValue(input, "storm")
    freezeReactiveValue(input, "scenario_yr")
    freezeReactiveValue(input, "scenario_m")
    if (input$scenario_ww %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      
      updateSelectInput(session, inputId = "storm", choices = choices)
    } else {
      
      updateSelectInput(session, inputId = "scenario_yr", choices = choices)
    }
  }, priority = 3)
  
  
    storm <- reactive({
      req(input$scenario_ww)
      if (input$scenario_ww %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      for (i in seq_along(scenario_ww())) {
        req(input$storm)
        if (input$storm == names(scenario_ww())[i]) {
          d <- scenario_ww()[[i]]
        }
      }
        d
      }
    })
    
    
    observeEvent(list(listen_storm(), input$storm), {
        choices <- names(storm())
        freezeReactiveValue(input, "scenario_yr")
        freezeReactiveValue(input, "scenario_m")
        updateSelectInput(session, inputId = "scenario_yr", choices = choices)
    }, priority = 2)

  
  scenario_yr <- reactive({
    req(scenario_ww(), input$scenario_yr)
    if (input$scenario_ww %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      req(storm())
      for (i in seq_along(storm())) {
        if (input$scenario_yr == names(storm())[i]) {
          d <- storm()[[i]][["measures"]]
        }
      }
      d
    } else {
      for (i in seq_along(scenario_ww())) {
        if (input$scenario_yr == names(scenario_ww())[i]) {
          d <- scenario_ww()[[i]][["measures"]]
        }
      }
      d
    }
    
  })
  
  observeEvent(list(scenario_yr(), listen_scen(), listen_yr()), {
    choices <- names(scenario_yr())
    freezeReactiveValue(input, "scenario_m")
    if (input$scenario_ww %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      updateSelectInput(session, inputId = "scenario_m", choices = choices)
    } else {
      updateSelectInput(session, inputId = "scenario_m", choices = choices)
    }
    
  }, priority = 1)

 dw <- reactive({
   req(input$scenario_yr, scenario_ww())
   if (input$scenario_ww %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
     yr <- as.character(input$scenario_yr)
     d <- storm()[[`yr`]]
   } else {
     yr <- as.character(input$scenario_yr)
     d <- scenario_ww()[[`yr`]]
   }
    
  })

 dm <- reactive({
  req(input$scenario_m, dw())
  ms <- as.character(input$scenario_m)
  d <- dw()[["measures"]][[`ms`]]

 })
 
 dp <- reactive({
   req(dm())
  d <-  if (length(input$locality) == 2){
    dm()$pop_data
  } else if("Accomack County" == input$locality){
    dm()$pop_data_acc
   }else if("Northampton County" == input$locality){
     dm()$pop_data_north
   }
   
 })


 event_name <- reactive({
   d <- dw()
   d <- d$event
 })

 event_title <- reactive({
   d <- dw()
   d <- d$descriptionTitle
 })

 output$meta_measure <- renderUI({event_title()})
 
 event_meas_descript <- reactive({
   req(dm())
   d <- dm()
   d <- d$field_description
 })
 
 output$meta_descrip <- renderUI({event_meas_descript()})
 
 event_year <- reactive({
   d <- dw()
   d <- d$year
 })

 output$meta_year <- renderUI({event_year()})

 event_description <- reactive({
   d <- dw()
   d <- d$description
 })

 output$meta_info <- renderUI({event_description()})
 
 pop_title <- reactive({
   d <- if (length(input$locality) == 2){
     "on the Eastern Shore of VA"
   } else if (length(input$locality) == 1){
     paste0("in ", input$locality)
   } else {""}
 })
 
 output$population_title <- renderUI({paste0("What percent of housing or people ", pop_title(), " are in areas estimated to experience each outcome?")})
 output$population_subtitle <- renderUI({paste0("What percent of each group are in areas estimated to experience each outcome? Are these groups more or less impacted relative to their presence in the overall population ", pop_title(), "?")})

  # Map Functions -------------------------------------------------------
  ## Leaflet base map function ----

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

  ## leafletProxy Map1 Function ----
  mapProxyFunction <- function(mapData, mapId,
                               var, var_title,
                               legend_labels,
                               pal, sel_range, unit, total_housing){

    # map proxy
    proxy <- leafletProxy(mapId, data = mapData)

    lab <- if (var_title=="Total Housing Units"){
      as.list(paste0("Number of Housing Units: ", total_housing))
    } else {
      as.list(paste0(var_title, ": <b>", round(var,2), " ", unit, "</b><br/>Number of Housing Units: ", total_housing))
    }
    
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
                    label = lapply(lab, HTML),
                    group = var_title) %>%
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
    })
  }


  # Build Environmental Impact Map -------------------------------------------------------
  
  # render leaflet map
  output$map1 <- renderLeafletFunction()
 
  outputOptions(output, "map1", priority = 0)

  observeEvent(list(listen_scen(), listen_yr(), listen_m(), listen_local()), {
    p <- dm()
    
    name <- as.character(p$name)
    # var <- p$map_data[[`name`]]
    
    pal <- colorBin(p$col_pal,
                    p$sel_range,
                    bins = p$legend_breaks,
                    right = TRUE,
                    pretty = FALSE )
    
    p$map_data <- p$map_data %>% filter(locality %in% input$locality)
    
    p$map_data <- p$map_data %>% 
      mutate(housing_num = case_when(total_housing > 0 & total_housing < 10 ~ "<10",
                                     .default = as.character(total_housing)))

    
    mapProxyFunction(p$map_data, "map1", p$map_data[[`name`]], p$title, p$legend_labels, pal, p$sel_range, p$unit, p$map_data$housing_num)
      
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

  
# chart function ----
  chart_func <- function(chart_dat, x, y, col, title, chart_tot, chart_title) {
    x <- enexpr(x)
    y <- enexpr(y)
    col <- enexpr(col)
    
    chart <- chart_dat %>%
      filter(event != "none") %>%
      hchart('column', hcaes(x = !!x, y = !!y, color = !!col)) %>% 
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(title = list(text = title), 
               labels = list(style = list(fontSize = '1.2em')),
               categories = chart_dat$bin) %>%
      hc_yAxis(min = 0,
               max = 100,
               title = list(text = "Percent Impacted", style = list(fontSize = '1.2em')),
               labels = list(format = '{text}%'),
               plotLines = list(
                 list(
                   label = list(text = "ESVA Total"),
                   width = 2,
                   value = chart_tot,
                   zIndex = 1,
                   dashStyle = "LongDash"
                 )
               )) %>%
      hc_plotOptions(series = list(dataLabels = list(format = '{y}%', enabled = TRUE),
                                   colorByPoint = TRUE,
                                   states = list(hover = list(enabled = FALSE)))) %>%
      hc_title(text = chart_title,
               align = 'left') %>%
      hc_add_theme(hc_theme_smpl())
    
    chart
  }
  
  # Hispanic plot ----
  output$hispplot <- renderHighchart({
    
    shiny::validate(
      need(input$locality, "Please select at least one county to display the chart.")
    )
    req(dm())
    td <- dm()
    
    cat_labels <- td$legend_labels
    
    pal <- td$col_pal
    
    chart_dat <- dp()
  
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_hisp = round(per_hisp,0),
             num_label = prettyNum(hisp, big.mark = ","),
             outcome = td$title,
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_hisp)
    
    
    cat_num <- chart_dat %>% 
      filter(event != "none") 
    
    chart_func(chart_dat, bin, per_hisp, color_bin, td$title, chart_tot, 'Hispanic Residents') %>% 
      hc_tooltip(formatter = JS("function(){
  return 'Hispanic Residents make up <b>' + this.y + '%' + '</b> of the population in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/>' + 'Estimated Number of Hispanic Residents Experiencing this Outcome: <b>' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Black plot ----
  output$blackplot <- renderHighchart({
    
    shiny::validate(
      need(input$locality, "Please select at least one county to display the chart.")
    )
    req(dm())
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- dp()
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_black = round(per_black,0),
             num_label = prettyNum(black, big.mark = ","),
             outcome = td$title,
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_black)
    
    chart_func(chart_dat, bin, per_black, color_bin, td$title, chart_tot, 'Black Residents') %>% 
      hc_tooltip(formatter = JS("function(){
  return 'Black Residents make up <b>' + this.y + '%' + '</b> of the population in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/>' + 'Estimated Number of Black Residents Experiencing this Outcome: <b>' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # White plot ----
  output$whiteplot <- renderHighchart({
    
    shiny::validate(
      need(input$locality, "Please select at least one county to display the chart.")
    )
    req(dm())
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- dp()
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_white = round(per_white,0),
             num_label = prettyNum(white, big.mark = ","),
             outcome = td$title,
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_white)
    
    chart_func(chart_dat, bin, per_white, color_bin, td$title, chart_tot, 'White Residents') %>% 
      hc_tooltip(formatter = JS("function(){
  return 'White Residents make up <b>' + this.y + '%' + '</b> of the population in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/>' + 'Estimated Number of White Residents Experiencing this Outcome: <b>' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Age plot ----
  output$ageplot <- renderHighchart({
    
    shiny::validate(
      need(input$locality, "Please select at least one county to display the chart.")
    )
    req(dm())
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- dp()
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_pop_under18 = round(per_pop_under18,0),
             num_label = prettyNum(pop_under18, big.mark = ","),
             outcome = td$title,
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_pop_under18)
    
    chart_func(chart_dat, bin, per_pop_under18, color_bin, td$title, chart_tot, 'Residents Under 18 yrs') %>% 
      hc_tooltip(formatter = JS("function(){
  return 'Residents under 18 yrs old make up <b>' + this.y + '%' + '</b> of the population in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/>' + 'Estimated Number of Residents Under 18 Experiencing this Outcome: <b>' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Low wage workers by residence plot ----
  output$wagehomeplot <- renderHighchart({
    
    shiny::validate(
      need(input$locality, "Please select at least one county to display the chart.")
    )
    req(dm())
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- dp()
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_jobs_rac_low = round(per_jobs_rac_low,0),
             num_label = prettyNum(jobs_rac_low, big.mark = ","),
             outcome = td$title,
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index]))
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_jobs_rac_low)
    
    chart_func(chart_dat, bin, per_jobs_rac_low, color_bin, td$title, chart_tot, 'Low Wage Workers by Location of Residence') %>% 
      hc_tooltip(formatter = JS("function(){
  return 'Workers in low wage jobs make up <b>' + this.y + '%' + '</b> of the working population with homes in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/>' + 'Estimated Number of Workers by Residence Experiencing this Outcome: <b>' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Low wage workers plot ----
  output$wageplot <- renderHighchart({
    
    shiny::validate(
      need(input$locality, "Please select at least one county to display the chart.")
    )
    req(dm())
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- dp()
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_jobs_wac_low = round(per_jobs_wac_low,0),
             num_label = prettyNum(jobs_wac_low, big.mark = ","),
             outcome = td$title,
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index]))
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_jobs_wac_low)
    
    chart_func(chart_dat, bin, per_jobs_wac_low, color_bin, td$title, chart_tot, 'Low Wage Workers by Location of Workplace') %>% 
      hc_tooltip(formatter = JS("function(){
  return 'Workers in low wage jobs make up <b>' + this.y + '%' + '</b> of the working population with workplaces in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/>' + 'Estimated Number of Workers by Workplace Experiencing this Outcome: <b>' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Total population plot ----
  output$totalplot <- renderHighchart({
    
    shiny::validate(
      need(input$locality, "Please select at least one county to display the chart.")
    )
    req(dm())
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- dp()
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_total = round(per_total,0),
             num_label = prettyNum(total, big.mark = ","),
             outcome = td$title,
             locality = paste(unlist(input$locality), collapse=' and '),
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_total)
    
    chart_func(chart_dat, bin, per_total, color_bin, td$title, chart_tot, 'Total Population') %>% 
      hc_tooltip(formatter = JS("function(){
  return '<b>' + this.y + '%' + '</b> of the population of ' + this.point.locality + ' live in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/>' + 'Estimated Population Experiencing this Outcome: <b>' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Total Housing Units plot ----
  output$houseplot <- renderHighchart({
    
    shiny::validate(
      need(input$locality, "Please select at least one county to display the chart.")
    )
    req(dm())
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- dp()
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_housing = round(per_housing,0),
             num_label = prettyNum(total_housing, big.mark = ","),
             outcome = td$title,
             locality = paste(unlist(input$locality), collapse=' and '),
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_housing)
    
    chart_func(chart_dat, bin, per_housing, color_bin, td$title, chart_tot, 'Total Housing Units') %>% 
      hc_tooltip(formatter = JS("function(){
  return '<b>' + this.y + '%' + '</b> of housing units in ' + this.point.locality + ' are in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/>' + 'Estimated Number of Housing Units in Areas with this Outcome: <b>' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Build Community Characteristics Map -------------------------------------------------------
  communityServer("community1")
  
  # Build Comparison Maps -------------------------------------------------------

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
    req(d)
    d
  })

  observeEvent(list(compare_a(), listen_compare()), {
    choices <- names(compare_a())
    freezeReactiveValue(input, "compare_a_name")
    freezeReactiveValue(input, "compare_a_yr")
    freezeReactiveValue(input, "compare_a_meas")
    if (input$compare_a %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      updateSelectInput(session, inputId = "compare_a_name", choices = choices)
    } else {
      updateSelectInput(session, inputId = "compare_a_yr", choices = choices)
    }
  }, priority = 3)


  compare_a_name <- reactive({
    req(compare_a())
    req(input$compare_a_name)
    if (input$compare_a %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      for (i in seq_along(compare_a())) {
        req(compare_a())
        if (input$compare_a_name == names(compare_a())[i]) {
          d <- compare_a()[[i]]
        }
      }
      req(d)
      d
    }
  })

  observeEvent(list(compare_a_name(), listen_name(), listen_compare()), {
    choices <- names(compare_a_name())
    freezeReactiveValue(input, "compare_a_yr")
    freezeReactiveValue(input, "compare_a_meas")
    updateSelectInput(session, inputId = "compare_a_yr", choices = choices)
  }, priority = 2)


  compare_a_yr <- reactive({
    req(compare_a(), input$compare_a_yr)
    if (input$compare_a %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      for (i in seq_along(compare_a_name())) {
        if (input$compare_a_yr == names(compare_a_name())[i]) {
          req(compare_a_name())
          d <- compare_a_name()[[i]][["measures"]]
        }
      }
      req(d)
      d
    } else {
      for (i in seq_along(compare_a())) {
        if (input$compare_a_yr == names(compare_a())[i]) {
          d <- compare_a()[[i]][["measures"]]
        }
      }
      req(d)
      d
    }

  })

  observeEvent(list(compare_a_yr(), listen_compare(), listen_compare_yr()), {
    choices <- names(compare_a_yr())
    freezeReactiveValue(input, "compare_a_meas")
    if (input$compare_a %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      updateSelectInput(session, inputId = "compare_a_meas", choices = choices)
    } else {
      updateSelectInput(session, inputId = "compare_a_meas", choices = choices)
    }

  }, priority = 1)

  cw <- reactive({
    req(input$compare_a_yr, compare_a())
    if (input$compare_a %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
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
    req(d)
    d
  })

  observeEvent(list(compare_b(), listen_compare2()), {
    choices <- names(compare_b())
    freezeReactiveValue(input, "compare_b_name")
    freezeReactiveValue(input, "compare_b_yr")
    freezeReactiveValue(input, "compare_b_meas")
    if (input$compare_b %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      updateSelectInput(session, inputId = "compare_b_name", choices = choices)
    } else {
      updateSelectInput(session, inputId = "compare_b_yr", choices = choices)
    }
  }, priority = 3)


  compare_b_name <- reactive({
    req(compare_b(), input$compare_b_name)
    if (input$compare_b %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      for (i in seq_along(compare_b())) {
        req(compare_b(), input$compare_b_name)
        if (input$compare_b_name == names(compare_b())[i]) {
          d <- compare_b()[[i]]
        }
      }
      req(d)
      d
    }
  })

  observeEvent(list(compare_b_name(), listen_name2(), listen_compare2()), {
    choices <- names(compare_b_name())
    freezeReactiveValue(input, "compare_b_yr")
    freezeReactiveValue(input, "compare_b_meas")
    updateSelectInput(session, inputId = "compare_b_yr", choices = choices)
  }, priority = 2)


  compare_b_yr <- reactive({
    req(compare_b(), input$compare_b_yr)
    if (input$compare_b %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      for (i in seq_along(compare_b_name())) {
        if (input$compare_b_yr == names(compare_b_name())[i]) {
          req(compare_b_name())
          d <- compare_b_name()[[i]][["measures"]]
        }
      }
      req(d)
      d
    } else {
      for (i in seq_along(compare_b())) {
        if (input$compare_b_yr == names(compare_b())[i]) {
          d <- compare_b()[[i]][["measures"]]
        }
      }
      req(d)
      d
    }

  })

  observeEvent(list(compare_b_yr(), listen_compare2(), listen_compare_yr2()), {
    choices <- names(compare_b_yr())
    freezeReactiveValue(input, "compare_b_meas")
    if (input$compare_b %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
      updateSelectInput(session, inputId = "compare_b_meas", choices = choices)
    } else {
      updateSelectInput(session, inputId = "compare_b_meas", choices = choices)
    }

  }, priority = 1)

  cw2 <- reactive({
    req(input$compare_b_yr, compare_b())
    if (input$compare_b %in% c('Depth to Groundwater', 'Historic Event Storm Surge', 'Future Storm Surge Flooding', 'Seawater Intrusion', 'Small Area Case Study')){
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

  renderLeafletFunctionCompare <- function(map) {
    renderLeaflet({
      leaflet() %>%
        addTiles(urlTemplate = carto_url,
                 options = providerTileOptions(minZoom = 8, maxZoom = 18)) |>
        # addProviderTiles('CartoDB.Positron',
        #                  options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>%
        fitBounds(esva_bbox[1], esva_bbox[2], esva_bbox[3], esva_bbox[4]) %>%
        addResetMapButton()
    })
  }

  ## leafletProxy Compare Function ----
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


  # Build Environmental Impact Map -------------------------------------------------------

  # render leaflet map
  output$mapcompare1 <- renderLeafletFunctionCompare()

  outputOptions(output, "mapcompare1", suspendWhenHidden = FALSE)

  observeEvent(list(listen_compare(), listen_compare_yr(), listen_compare_meas()), {
    req(cm())
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
  output$mapcompare2 <- renderLeafletFunctionCompare()

  outputOptions(output, "mapcompare2", suspendWhenHidden = FALSE)

  observeEvent(list(listen_compare2(), listen_compare_yr2(), listen_compare_meas2()), {
    req(cm2)
    p <- cm2()

    name <- as.character(p$name)

    pal <- colorBin(p$col_pal,
                    p$sel_range,
                    bins = p$legend_breaks,
                    right = TRUE,
                    pretty = FALSE )

    mapProxyFunctionCompare(p$map_data, "mapcompare2", p$map_data[[`name`]], p$title, p$legend_labels, pal, p$sel_range, p$unit)

  })
  
  sync_observer <- observe({
    req(input$pages == "Compare")
    leafletProxy("mapcompare1")  %>%
      addLeafletsync(c("mapcompare1","mapcompare2"))
    sync_observer$destroy()
  })

  
}

shinyApp(ui, server)
