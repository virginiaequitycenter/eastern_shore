# Eastern Shore of Virginia Livability Tool

# Libraries
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(bslib)
library(qs)
library(highcharter)
library(rcartocolor)

colors <- carto_pal(6, "RedOr")
# Read in data
app_dat <- qs::qread("app_data_test.qs")

input_choices = names(app_dat) 
# input_years = names(app_dat[[1]])
# input_measures = names(app_dat[[1]][[1]][["measures"]])

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
      choices = NULL
      ),
    selectInput(
      'scenario_m',
      label = 'Measure',
      choices = NULL
      )
    ),
  layout_columns(
    col_widths = 12,
    row_heights = c(1,2),
    layout_columns(
      col_widths = c(8,4),
      # row_heights = c(1),
      leafletOutput('map', width="100%", height = "600px"),
      htmlOutput("scenario_meta")
    ),
    card(card_header("Population Impacts"),
         layout_column_wrap(
             highchartOutput('totalplot'),
             highchartOutput('houseplot'),
             highchartOutput('blackplot'),
             highchartOutput('whiteplot'),
             highchartOutput('hispplot'),
             highchartOutput('ageplot'),
             highchartOutput('wageplot'),
             width = 1/3
           )
           
         )
    )
  
) # end page_navbar

server <- function(input, output, session){
  
  scenario_ww <- reactive({
    for (i in seq_along(names(app_dat))) {
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
  })
  
  observeEvent(scenario_yr(), {
    choices <- names(scenario_yr())
    freezeReactiveValue(input, "scenario_m")
    updateSelectInput(inputId = "scenario_m", choices = choices)
  })
  
 dw <- reactive({
    yr <- as.character(input$scenario_yr)
    d <- scenario_ww()[[`yr`]]

  })
 
 dm <- reactive({
   ms <- as.character(input$scenario_m)
   d <- dw()[["measures"]][[`ms`]]

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
                               legend_labels,
                               pal, sel_range){

    # map proxy
    proxy <- leafletProxy(mapId, data = mapData)

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
                    label = lapply(as.list(var), HTML),
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

  listen_scen <- reactive(input$scenario_ww)

  listen_yr <- reactive(input$scenario_yr)
  
  listen_m <- reactive(input$scenario_m)


  # Build Map -------------------------------------------------------

  # render leaflet map
  output$map <- renderLeafletFunction()

  observeEvent(list(listen_scen(), listen_yr(), listen_m()), {

    p <- dm()

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

# chart function ----
  chart_func <- function(chart_dat, x, y, col, title, chart_tot, chart_title) {
    x <- enexpr(x)
    y <- enexpr(y)
    col <- enexpr(col)
    
    # chart_pal <- carto_pal(num, "RedOr")
    
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
      # hc_colorAxis(stops = 5) %>%
      # hc_colors(c("#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#B30000")) %>%
      # hc_colors(chart_pal) %>% 
      hc_plotOptions(series = list(dataLabels = list(format = '{y}%', enabled = TRUE),
                                   colorByPoint = TRUE)) %>%
      hc_tooltip(formatter = JS("function(){
  return '<b>' + this.key + '</b></br>Percent Impacted: <b>' + this.y + '%' + '</b>'
  }")) %>%
      hc_title(text = chart_title,
               align = 'left') %>%
      hc_add_theme(hc_theme_smpl())
    
    chart
  }
  
  # Hispanic plot ----
  output$hispplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    # print(length(cat_labels))
    # 
    pal <- carto_pal(length(cat_labels), "RedOr")
    # 
    # names(pal) <- cat_labels
    # 
    # pal <- c(pal, "#808080")
    # 
    # 
    # cat_labels <- factor(levels=cat_labels)
    # null <- factor(levels = c("N/A"))
    # names(pal) <- levels(factor(c(levels(cat_labels), levels(null))))
    # # pal <- c("#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#B30000")
    # print(pal)
    
    chart_dat <- td$pop_data
  
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_hisp = round(per_hisp,0),
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    # print(chart_dat$color_bin)

    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_hisp)
    
    cat_num <- chart_dat %>% 
      filter(event != "none") 
    
    chart_func(chart_dat, bin, per_hisp, color_bin, td$title, chart_tot, 'Hispanic Residents')
    
  #   chart <- chart_dat %>%
  #     filter(event != "none") %>%
  #     hchart('column', hcaes(x = bin, y = round(per_hisp,0))) %>% 
  #     hc_legend(enabled = FALSE) %>%
  #     hc_xAxis(title = list(text = td$title), 
  #              labels = list(style = list(fontSize = '1.2em'))) %>%
  #     hc_yAxis(min = 0,
  #              max = 40,
  #              title = list(text = "Percent Impacted", style = list(fontSize = '1.2em')),
  #              labels = list(format = '{text}%'),
  #              plotLines = list(
  #                list(
  #                  label = list(text = "ESVA Total"),
  #                  width = 2,
  #                  value = chart_tot,
  #                  zIndex = 1,
  #                  dashStyle = "LongDash"
  #                )
  #              )) %>%
  #     # hc_colors(c('#3B8EA5')) %>%
  #     hc_plotOptions(series = list(dataLabels = list(format = '{y}%', enabled = TRUE))) %>%
  #     hc_tooltip(formatter = JS("function(){
  # return '<b>' + this.key + '</b></br>Percent Impacted: <b>' + this.y + '%' + '</b>'
  # }")) %>%
  #     hc_title(text = paste0('Hispanic Residents Impacted'),
  #              align = 'left') %>%
  #     hc_add_theme(hc_theme_smpl())
  #   
  #   chart
    
  })
  
  # Black plot ----
  output$blackplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- carto_pal(length(cat_labels), "RedOr")
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_black = round(per_black,0),
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_black)
    
    chart_func(chart_dat, bin, per_black, color_bin, td$title, chart_tot, 'Black Residents')
    
  })
  
  # White plot ----
  output$whiteplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- carto_pal(length(cat_labels), "RedOr")
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_white = round(per_white,0),
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_white)
    
    chart_func(chart_dat, bin, per_white, color_bin, td$title, chart_tot, 'White Residents')
    
  })
  
  # Age plot ----
  output$ageplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- carto_pal(length(cat_labels), "RedOr")
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_pop_under18 = round(per_pop_under18,0),
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_pop_under18)
    
    chart_func(chart_dat, bin, per_pop_under18, color_bin, td$title, chart_tot, 'Residents Under 18 yrs')
    
  })
  
  # Low wage workers plot ----
  output$wageplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- carto_pal(length(cat_labels), "RedOr")
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_jobs_wac_low = round(per_jobs_wac_low,0),
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index]))
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_jobs_wac_low)
    
    chart_func(chart_dat, bin, per_jobs_wac_low, color_bin, td$title, chart_tot, 'Residents in Low Wage Jobs by Workplace')
    
  })
  
  # Total population plot ----
  output$totalplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- carto_pal(length(cat_labels), "RedOr")
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_total = round(per_total,0),
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_total)
    
    chart_func(chart_dat, bin, per_total, color_bin, td$title, chart_tot, 'Total Population')
    
  })
  
  # Total housing plot ----
  output$houseplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- carto_pal(length(cat_labels), "RedOr")
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_housing = round(per_housing,0),
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_housing)
    
    chart_func(chart_dat, bin, per_housing, color_bin, td$title, chart_tot, 'Total Housing')
    
  })
  
}

shinyApp(ui, server)
