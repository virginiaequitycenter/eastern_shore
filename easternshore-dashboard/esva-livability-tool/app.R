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

# Read in data
app_dat <- qs::qread("app_data_test.qs")
input_choices = names(app_dat)
esva_bbox <- c(-76.06697 , 37.04853, -75.16060,  38.04154)

ui <- page_sidebar(
  title = "Eastern Shore of Virginia Livability Tool",
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
      choices = character(0)
      ),
    selectInput(
      'scenario_m',
      label = 'Measure',
      choices = character(0)
      )
    ),
  layout_columns(
    col_widths = 12,
    layout_columns(
      col_widths = c(8,4),
      leafletOutput('map', width="100%", height = "650px"),
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
              "What percent of households or people on the Eastern Shore of VA are in areas estimated to experience each outcome?"),
         layout_column_wrap(
           highchartOutput('houseplot'),
           highchartOutput('totalplot'),
           width = 1/2),
    card(class= "bg-light fs-5 shadow-none",
         "What percent of each group are in areas estimated to experience each outcome? Are these groups more or less impacted relative to their presence in the overall population on the Eastern Shore of VA?",
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
    )
  
) # end page_navbar

server <- function(input, output, session){
  
  listen_scen <- reactive(input$scenario_ww)
  
  listen_yr <- reactive(input$scenario_yr)
  
  listen_m <- reactive(input$scenario_m)
  
  scenario_ww <- reactive({
    req(input$scenario_ww)
    for (i in seq_along(names(app_dat))) {
      if (input$scenario_ww == names(app_dat)[i]) {
        d <- app_dat[[i]]
      }
    }
    d
  })
  
  observeEvent(list(scenario_ww(), listen_scen()), {
    choices <- names(scenario_ww())
    freezeReactiveValue(input, "scenario_yr")
    freezeReactiveValue(input, "scenario_m")
    updateSelectInput(session, inputId = "scenario_yr", choices = choices)
  })
  
  scenario_yr <- reactive({
    req(scenario_ww(), input$scenario_yr)
    for (i in seq_along(scenario_ww())) {
      if (input$scenario_yr == names(scenario_ww())[i]) {
        d <- scenario_ww()[[i]][["measures"]]
      }
    }
    d
  })
  
  observeEvent(list(scenario_yr(), listen_scen(), listen_yr()), {
    choices <- names(scenario_yr())
    freezeReactiveValue(input, "scenario_m")
    updateSelectInput(session, inputId = "scenario_m", choices = choices)
  })

 dw <- reactive({
   req(input$scenario_yr, scenario_ww())
    yr <- as.character(input$scenario_yr)
    d <- scenario_ww()[[`yr`]]
  })

 dm <- reactive({
  req(input$scenario_m, dw())
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

 output$meta_measure <- renderUI({event_title()})
 
 event_meas_descript <- reactive({
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

  # Map Functions -------------------------------------------------------
  ## Leaflet base map function ----

  renderLeafletFunction <- function(map) {
    renderLeaflet({
      leaflet() %>%
        addProviderTiles('CartoDB.Positron',
                         options = providerTileOptions(minZoom = 8, maxZoom = 18)) %>%
        fitBounds(esva_bbox[1], esva_bbox[2], esva_bbox[3], esva_bbox[4]) %>%
        addResetMapButton()
    })
  }

  ## leafletProxy Map Function ----
  mapProxyFunction <- function(mapData, mapId,
                               var, var_title,
                               legend_labels,
                               pal, sel_range, unit, total_housing){

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
                    label = lapply(as.list(paste0(var_title, ": <b>", round(var,2), " ", unit, "</b><br/>Number of Households: ", total_housing)), HTML),
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


  # Build Map -------------------------------------------------------
  
  # render leaflet map
  output$map <- renderLeafletFunction()

  observeEvent(list(listen_scen(), listen_yr(), listen_m()), {
    p <- dm()
    
    name <- as.character(p$name)
    # var <- p$map_data[[`name`]]
    
    pal <- colorBin(p$col_pal,
                    p$sel_range,
                    bins = p$legend_breaks,
                    right = TRUE,
                    # reverse = FALSE,
                    # na.color = "#808080",
                    pretty = FALSE )

    
    mapProxyFunction(p$map_data, "map", p$map_data[[`name`]], p$title, p$legend_labels, pal, p$sel_range, p$unit, p$map_data$total_housing)
      
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
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    
    pal <- td$col_pal
    
    chart_dat <- td$pop_data
  
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_hisp = round(per_hisp,0),
             num_label = prettyNum(round(hisp, -1), big.mark = ","),
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
  return 'Hispanic Residents make up <b>' + this.y + '%' + '</b> of the population<br/>in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/><b>' + 'Estimated Number of Hispanic Residents: ' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Black plot ----
  output$blackplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_black = round(per_black,0),
             num_label = prettyNum(round(black, -1), big.mark = ","),
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
  return 'Black Residents make up <b>' + this.y + '%' + '</b> of the population<br/>in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/><b>' + 'Estimated Number of Black Residents: ' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # White plot ----
  output$whiteplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_white = round(per_white,0),
             num_label = prettyNum(round(white, -1), big.mark = ","),
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
  return 'White Residents make up <b>' + this.y + '%' + '</b> of the population<br/>in areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/><b>' + 'Estimated Number of White Residents: ' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Age plot ----
  output$ageplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_pop_under18 = round(per_pop_under18,0),
             num_label = prettyNum(round(pop_under18, -1), big.mark = ","),
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
  '<br/><b>' + 'Estimated Number of Residents Under 18: ' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Low wage workers by residence plot ----
  output$wagehomeplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_jobs_rac_low = round(per_jobs_rac_low,0),
             num_label = prettyNum(round(jobs_rac_low, -1), big.mark = ","),
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
  '<br/><b>' + 'Estimated Number of Workers by Residence: ' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Low wage workers plot ----
  output$wageplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_jobs_wac_low = round(per_jobs_wac_low,0),
             num_label = prettyNum(round(jobs_wac_low, -1), big.mark = ","),
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
  '<br/><b>' + 'Estimated Number of Workers by Workplace: ' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Total population plot ----
  output$totalplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_total = round(per_total,0),
             num_label = prettyNum(round(total, -1), big.mark = ","),
             outcome = td$title,
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_total)
    
    chart_func(chart_dat, bin, per_total, color_bin, td$title, chart_tot, 'Total Population') %>% 
      hc_tooltip(formatter = JS("function(){
  return '<b>' + this.y + '%' + '</b> of the population of the ESVA live in<br/>areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/><b>' + 'Estimated Population: ' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
  # Total households plot ----
  output$houseplot <- renderHighchart({
    
    td <- dm()
    
    cat_labels <- td$legend_labels
    pal <- td$col_pal
    
    chart_dat <- td$pop_data
    
    chart_dat <- chart_dat %>% 
      mutate(bin = case_when(is.na(bin) ~ "N/A",
                             .default = bin),
             per_housing = round(per_housing,0),
             num_label = prettyNum(round(total_housing, -1), big.mark = ","),
             outcome = td$title,
             color_index = case_when(bin %in% cat_labels ~ match(bin, cat_labels)),
             color_bin = case_when(is.na(color_index) ~ "#808080",
                                   .default = pal[color_index])
             )
    
    chart_tot <- chart_dat %>% 
      filter(event == "none") %>% 
      pull(per_housing)
    
    chart_func(chart_dat, bin, per_housing, color_bin, td$title, chart_tot, 'Total Households') %>% 
      hc_tooltip(formatter = JS("function(){
  return '<b>' + this.y + '%' + '</b> of households on the ESVA are in<br/>areas experiencing the outcome: ' + this.point.outcome + ': <b>' + this.key + '</b><br/>' + 
  '<br/><b>' + 'Estimated Number of Households: ' + this.point.num_label + '</b><br/>'
  }"))
    
  })
  
}

shinyApp(ui, server)
