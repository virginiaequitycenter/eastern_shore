# Published version
# VA Eastern Shore Atlas
# Updated 8-23-2023
# Last Deployed: 8-23-2023

library(shiny)
library(bslib)
library(shinyhelper)
library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(DT)
library(biscale)
library(stringi)
library(leafem)

source("functions/utils.R")

# Load Data ---------------------------------------------------------------

load("www/app_data_2022.Rdata")
load("www/eastern_dat.RData")

# was not deploying on shinyapps:
# https://stackoverflow.com/questions/61286108/error-in-cpl-transformx-crs-aoi-pipeline-reverse-ogrcreatecoordinatetrans
all_data <- st_transform(all_data, 4326)
all_data$pop <- as.character(all_data$totalpopE)
counties_geo <- st_transform(counties_geo, 4326)

# create palette for use in bichoropleth palette function
bipal <- c("#e8e8e8", "#dfd0d6", "#be64ac", # A-1, A-2, A-3,
           "#ace4e4", "#a5add3", "#8c62aa", # B-1, B-2, B-3
           "#5ac8c8", "#5698b9", "#3b4994") # C-1, C-2, C-3

fewpal <- c("#7DC462", "#0D95D0", "#E72F52", "#774FA0", "#EFB743", "#D44627")
# fewpal <- c("#265dab", "#df5c24", "#c7b42e", "#059748",
#             "#cb2027", "#9d722a")
            

# no-go variables for mapping
cant_map <- c('indigE', 'othraceE', 'bbmax_up', 'HWAV_AFREQ', 'RFLD_AFREQ', 'avg_prox_transit_rank')
cant_map_message <- c("One of your selected variables cannot be rendered in the map or in the median plot. This is usually because there isn't enough variation in the variable to break its values up into meaningful categories.")

# Define UI ---------------------------------------------------------------

ui <- htmlTemplate(filename = "esva-template.html", main = 
      navbarPage(title = img(src='logo-horizontal-lg.png',
                         height = 70,
                         alt = "Eastern Shore of Virginia Climate Equity Project logo"),
                 windowTitle = "Eastern Shore of Virginia Climate Equity Project Atlas",
                 collapsible = TRUE,
                 fluid = TRUE,
                 theme = bs_theme(version = 5),
        tabPanel(HTML("<center>CLIMATE EQUITY<br>ATLAS PROTOTYPE</center>"), # tab panel Climate App
          fluidRow(
            column(
              width = 12,
              HTML("<h1><center>Climate Equity Atlas Prototype</center></h1>")
            )
          ), br(), # end fluidRow
          fluidRow(
            column(
              width = 4,
              cardComponent(
                accordianComponent("intro-climate", "Dashboard Instructions",
                                    "<p>Select any two measures from the dropdown menus below to see the relationship between the measures in census tracts across the Eastern Shore of Virginia.</p>
                                    <p>Select the tabs to the right to see the relationship between the two selected measures in different ways: on a map, correlation plot, and median plot.</p>",
                                    "intro-cli-1", "intro-cli-2")
              ),
              cardComponentSelect(
                selectInput(inputId = "variable1",
                            label = "Select Variable 1:",
                            choices = ind_demfirst_ct,
                            selected = ind_demfirst_ct$`Demographic & Social`["Estimated Population"]),
                accordianComponentSource("ind1", "Show Selected Variable Definition & Source", textOutput("ind1_defn", inline = TRUE), textOutput("ind1_source", inline = TRUE),"var-def-1", "map-ind-1")
              ),
              cardComponentSelect(
                selectInput(inputId = "variable2",
                            label = "Select Variable 2:",
                            choices = ind_climfirst_ct,
                            selected = ind_climfirst_ct$`Climate Measures`["Average Land Surface Temperature"]),
                accordianComponentSource("ind2", "Show Selected Variable Definition & Source", textOutput("ind2_defn", inline = TRUE),textOutput("ind2_source", inline = TRUE),"var-def-2", "map-ind-2")
              ),
              cardComponentSelectOne(
                checkboxGroupInput(inputId = "locality",
                                  label = "Select Localities:",
                                  choices = c("Accomack" = "001",
                                              "Northampton" = "131"),
                                  selected = c("001", "131"),
                                  inline = TRUE)
              )
            ), # end column width=4
            column(
              width = 8,
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  title = 'Map',
                  icon = icon('map'),
                  h2(textOutput("maptitleclimate", inline = TRUE)),
                  cardComponent(
                  accordianComponent("map-descr", "Map Instructions",
                      "<p>This map shows how each census tract is ranked from Low to High on the measures selected above (Variable 1 & Variable 2). The legend in the upper left corner of the map provides a key for what each color represents.</p>
                      <p>For example, if the values of both variables are Low, the census tract appears gray. When the value of Variable 1 is Low and the value of Variable 2 is High the track will be pink; when Variable 1 is High and Variable 2 is Low the tract will be green; When both variables have a High value, the tract appears dark blue.</p>
                      <p>Click on the tracts to see the values for each measure and how the tract ranks (Low, Medium, High) relative to others in the selected region. Zoom in to see specific areas more closely; click the reset button on the map to see the full region.</p>",
                      "mapdescr-1", "mapdescr-2")
                  ), 
                  leafletOutput(outputId = 'leaf', width = '100%', height = 600)
                ),
                tabPanel(
                  title = "Correlation",
                  icon = icon('chart-line'),
                  h2(textOutput("comptitleclimate", inline = TRUE)),
                  cardComponent(
                  accordianComponent("comp-cli-desc", "Correlation Plot Instructions",
                      "<p>This plot shows the correlation, or relationship, between the two selected variables for the localities selected.</p>
                      <p>Each census tract is represented by a circle, plotted by the values of the measures selected above. The size of each circle is based on the population of that tract so that tracts with more people appear larger and those with less people appear smaller. The color of the circle is based on the locality.</p>
                      <p>The gray figures on the top and right show how frequently high and low values of the selected variables occur in the region; taller bars mean that range of values is more common.</p>",
                      "compdesc-cli-1", "compdesc-cli-2")
                  ), 
                  plotlyOutput(outputId = "scatterplotclimate", width = '100%', height = 500)
                ),
                tabPanel(
                  title = "Differences",
                  icon = icon('chart-simple'),
                  h2(textOutput("difftitleclimate", inline = TRUE)),
                  cardComponent(
                  accordianComponent("diff-desc-cli", "Median Plot Instructions",
                      "<p>This plot divides the census tracts in the selected localities into two groups, representing the values above and below the median of the measure selected for Variable 1.</p>
                      <p>The height of the bar shows the average value of the measure selected for Variable 2 within that group of tracts. Hover over each bar to see the average value of Variable 2.</p>
                      <p>For Example, when Estimated Population (Variable 1) and Average Land Surface Temperature (Variable 2) are selected, this median plot shows that the tracts below the median population have an average value of the Average Land Surface Temperature of 89.9, and the tracts below the median population have an average value of the Average Land Surface Temperature of 90.2.</p>",
                      "diffdesc-cli-1", "diffdesc-cli-2")
                  ), 
                  plotlyOutput(outputId = 'median_plot_climate', width = '100%', height = 500)
                )
              )
            ) # end column width=8
          ), br(), hr(),  # end fluidRow
          fluidRow(
            column(
              width = 12,
              h3("Download Data"),
              p("Data in this Atlas is provided as a compressed folder, which includes CSVs for data at the census tract level and a data dictionary."),
              downloadButton("downloadBtnCli", "Download"),
              br(), hr(), br() 
              # h4("Citation"),
              # p("The Equity Center, Democratization of Data Initiative; \"Eastern Shore Climate Equity Dashboard Prototype\"; An Initiative of the UVA Karsh Institute of Democracy Center for the Redress of Inequity Through Community-Engaged Scholarship; Accessed ", Sys.Date(), "; https://equityatlas.virginiaequitycenter.org/dashboards/climate-dashboard/.")
              
            )
          ) # end fluidRow
        ), # end tabPanel Climate App
        singleton(tags$head(tags$script(src = "message-handler.js"))),
        tabPanel(HTML("<center>SOCIAL & ECONOMIC<br>EQUITY ATLAS</center>"), # start tabPanel S&E App
          fluidRow(
            column(
              width = 12,
              HTML("<h1><center>Social & Economic Equity Atlas</center></h1>")
            )
          ), br(), # end fluidRow
          fluidRow(
            column(
              width = 12,
              cardComponent(
                accordianComponent("intro", "Dashboard Instructions",
                                    "Make selections in the boxes below to show demographic, economic and social data on the maps and plots in tabs below.
                                    Indicators include data related to Health, Housing, People, Youth & Education, Jobs, Wages & Income, and various Indices. 
                                    Data is available for download at the bottom of the page.",
                                    "intro-1", "intro-2")
              )
            )
          ), br(), # end fluidRow
          fluidRow(
            column(
              width = 4,
                cardComponentSelectGeo(
                selectInput("indicator1",
                    "Select First Equity Indicator:",
                    choices = ind_choices,
                    selected = ind_choices$People['Estimated Population']) %>% 
                helper(type = "inline",
                  icon = "question-circle",
                  content = helpers$indicator,
                  size = "m"),
                textOutput("ind_geo1", inline = TRUE),
                accordianComponent("ind1", "Show Selected Indicator Definition", textOutput("ind1_abt", inline = TRUE),"var-def-1", "map-ind-1")
                )
            ) %>% tagAppendAttributes(class="mb-3 mb-sm-0"),
            column(
              width = 4,
              cardComponentSelectGeo(
              selectInput("indicator2",
                  "Select Second Equity Indicator:",
                  choices = ind_choices,
                  selected = ind_choices$Housing['Total Housing Units']) %>% 
              helper(type = "inline",
                  icon = "question-circle",
                  content = helpers$indicator2,
                  size = "m"),
              textOutput("ind_geo2", inline = TRUE),
              accordianComponent("ind2", "Show Selected Indicator Definition", textOutput("ind2_abt", inline = TRUE),"var-def-2", "map-ind-2")
              )
            ) %>% tagAppendAttributes(class="mb-3 mb-sm-0"),
            column(
              width = 4,
              cardComponentSelect(
              checkboxGroupInput(
                  inputId = "geo",
                  label = "Localities",
                  choices = counties,
                  selected = counties,
                  inline = TRUE) %>%
                  helper(type = "inline",
                          icon = "question-circle",
                          content = helpers$counties,
                          size = "m"),
              radioButtons(inputId = "geo_df",
                  label = "Select Geographic Level:",
                  choices = c("County", "Census Tract"),
                  selected = "Census Tract",
                  inline = TRUE) %>%
                  helper(type = "inline",
                            title = "Geographic Level",
                            icon = "question-circle",
                            content = helpers$geo,
                            size = "m")
              )
            ) %>% tagAppendAttributes(class="mb-3 mb-sm-0")
          ), br(), 
          fluidRow(
            column(
              width = 12,
              tabsetPanel(
                id = "tabs",
                tabPanel(
                    "First Indicator Map",
                    value = "tab1",
                    h2(textOutput("ind1_name", inline = TRUE)),
                    p("Click on areas below to view names and indicator values."),
                    leafletOutput("map1", height=600),
                    br(),
                    textOutput("source")
                  ),
                tabPanel("Second Indicator Map", 
                    value = "tab2",
                    h2(textOutput("ind2_name", inline = TRUE)),
                    p("Click on areas below to view names and indicator values."),
                    leafletOutput("map2", height=600),
                    br(),
                    textOutput("source2")
                  ),
                # tabPanel(title = "Differences",
                #          h2(""),
                #          p("Each census tract in the selected localities are ranked into three groups representing tracts with Low, Middle, or High values on the measure you select on the left (Indicator 1). The height of the bar shows the average value of the measure you select on the right (Indicator 2) within that group of tracts. Hover over each bar to see the average value on Indicator 2."),
                #          plotlyOutput(outputId = 'tercile_plot'), br()
                # ),
                tabPanel("Selection Relationship",
                    h2(textOutput("comptitle")) %>% 
                      helper(type = "inline",
                              title = "Distribution",
                              icon = "question-circle",
                              content = helpers$correlation,
                              size = "m"),
                    p("Each circle represents a county or census tract (depending on the selected geographic level), plotted by the values of the two selected Equity Indicators. The size of each circle is based on the estimated population of the county or census tract."),
                    plotlyOutput("compare"),
                    br(),
                    textOutput("source_c")
                  )
              )
            )
          ), br(), hr(),# end fluidRow
          fluidRow(
            column(
              width = 12,
              h2("Download Data"),
              p("Data in this Atlas is provided as a compressed folder, which includes CSVs for data at the county, census tract, and block group levels and a data dictionary."),
              downloadButton("downloadBtn", "Download"),
              br(), hr(), br()
            )
          ) # end fluidRow
        ) # end tabPanel S&E App
)) # end navbarPage / end HTML template

# Define server logic -----------------------------------------------------

server <- function(input, output, session) {

# Climate App ------

  geo_data <- reactive({
    if (input$variable1 == input$variable2) {
      session$sendCustomMessage(type = 'testmessage',
                                message = paste0("Please make sure that you've selected two different variables."))
    } else if (length(input$locality) == 0) {
      session$sendCustomMessage(type = 'testmessage',
                                message = paste0("At least one locality must be selected."))
    } else {
      geo <- geo %>%
        dplyr::select(x = !!sym(input$variable1),
                      y = !!sym(input$variable2),
                      locality, countyname, tract, geoid,
                      pop = pop, tractnames) %>%
        dplyr::filter(locality %in% input$locality) %>%
        drop_na()
    }
  })

    ## output map ----

    # map tab title
  output$maptitleclimate <- renderText({
    if (input$variable1 == input$variable2 | length(input$locality) == 0) {
      paste0("Please make sure that you've selected two different variables and/or at least one locality.")
    } else {
      paste(attr(geo_data()$x, "goodname"), " and ", 
            attr(geo_data()$y, "goodname"))
    }
  })

  #build static parts of map, and display initial outline of region
  output$leaf <- renderLeaflet({
    leaflet() %>% addProviderTiles('CartoDB.Positron') %>%
      addPolygons(data = geo, color = 'grey', opacity = 0) %>%
      fitBounds(-76.26, 37.05, -75.11, 38.07) %>%
      addLogo('bivariate_legend_static.svg', src = "remote",
              position = "topleft", width = 100, height = 100, alpha = 0.8) %>%
      addResetMapButton()
  })
  # reactive function to detect when variable 1, variable 2, or locality selection changes
  listen_closely <- reactive({
    list(input$variable1,input$variable2, input$locality)
  })
  # when a variable or locality selection is changed, render the appropriate bichoropleth without losing the legend
  observeEvent(listen_closely(), {
    if (input$variable1 == input$variable2 | length(input$locality) == 0) {
      leafletProxy('leaf') %>% clearShapes()
    } else if (input$variable1 %in% cant_map | input$variable2 %in% cant_map) {
      session$sendCustomMessage(type = 'testmessage', message = cant_map_message)
      leafletProxy('leaf') %>% clearShapes()
    } else {
      to_map <- bi_class(geo_data(), x = x, y = y, style = "quantile", dim = 3) %>% 
        mutate(var1_tercile = stri_extract(bi_class, regex = '^\\d{1}(?=-\\d)'),
               var1_tercile_cat = case_when(var1_tercile == 1 ~ 'Low',
                                       var1_tercile == 2 ~ 'Medium',
                                       var1_tercile == 3 ~ 'High'),
               var2_tercile = stri_extract(bi_class, regex = '(?<=\\d-)\\d{1}$'),
               var2_tercile_cat = case_when(var2_tercile == 1 ~ 'Low',
                                            var2_tercile == 2 ~ 'Medium',
                                            var2_tercile == 3 ~ 'High'),
               goodname_x = attr(geo_data()$x, "goodname"),
               goodname_y = attr(geo_data()$y, "goodname")) 
      factpal <- colorFactor(bipal, domain = to_map$bi_class)
      leafletProxy('leaf', data = to_map) %>% 
        clearShapes() %>%
        addPolygons(data = to_map,
                    fillColor = ~factpal(bi_class),
                    weight = 1,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.8,
                    highlight = highlightOptions(
                      weight = 2,
                      fillOpacity = 0.8,
                      bringToFront = T),
                    popup = paste0("<b>Locality:</b> ", to_map$countyname, ", tract ", to_map$tract, "<br><b>Tract Name(s): </b>",
                                   to_map$tractnames, "<br><b>",
                                   to_map$goodname_x, ":</b> ", round(to_map$x, 2),  "<br>",
                                   "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Relative to other tracts: <b>", to_map$var1_tercile_cat, "</b><br><b>",
                                   to_map$goodname_y, ":</b> ", round(to_map$y, 2), "<br>",
                                   "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Relative to other tracts: <b>", to_map$var2_tercile_cat, "</b>"))
    }
  })

  ## Build scatterplot ----

  ## scatterplot title
  output$comptitleclimate <- renderText({
    if (input$variable1 == input$variable2 | length(input$locality) == 0) {
      paste0("Please make sure that you've selected two different variables and/or at least one locality.")
    } else {
      paste(attr(geo_data()$x, "goodname"), " vs. ", 
            attr(geo_data()$y, "goodname"))
    }
  })

  output$scatterplotclimate <- renderPlotly({
    if (input$variable1 == input$variable2 | length(input$locality) == 0) {
      plotly_empty()
    } else {
      d <- st_drop_geometry(geo_data())
      xhist <- plot_ly(data = d, x = ~x,
                       type = "histogram", nbinsx = 20,
                       alpha =.75, color = I("grey")) %>%
        layout(yaxis = list(showgrid = FALSE,
                            showticklabels = FALSE,
                            fixedrange = T),
               xaxis = list(showticklabels = FALSE,
                            fixedrange = T))

      yhist <- plot_ly(data = d, y = ~y,
                       type = "histogram", nbinsx = 20,
                       alpha = .75, color = I("grey")) %>%
        layout(xaxis = list(showgrid = FALSE,
                            showticklabels = TRUE,
                            fixedrange = T),
               yaxis = list(showticklabels = FALSE,
                            fixedrange = T))

      xyscatter <- plot_ly(data = d, x = ~x, y = ~y,
                           type = "scatter",
                           mode = "markers", # to remove mode warning
                           fill = ~"", # to remove line.width error
                           size = ~pop,
                           sizes = c(50, 500),
                           color = ~countyname,
                           colors = fewpal,
                           alpha = .75,
                           text = paste0("Locality: ", d$countyname, "<br>",
                                         "Census tract: ", d$tract, "<br>",
                                         "Population: ", d$pop, "<br>",
                                         attr(d$x, "goodname"), ": ", round(d$x, 2), "<br>",
                                         attr(d$y, "goodname"), ": ", round(d$y, 2), "<br>"),
                           hoverinfo = "text") %>%
        layout(xaxis = list(title = attr(d$x, "goodname"), showticklabels = TRUE, fixedrange = T),
               yaxis = list(title = attr(d$y, "goodname"), showticklabels = TRUE, fixedrange = T),
               legend = list(orientation = "h", x = 0, y = -0.2))

      # note: in the legend, we hide trace 1 (the xhist) and trace (3 + length(input$locality)), which is the yhist;
      #       the yhist's trace # changes as a user selects different localities to map, but it can be dynamically
      #       referenced as... yhist trace number = 1 (xhist) + 1 (plotly_empty) + n_localities + 1 (to reach the yhist)
      subplot(xhist, plotly_empty(), xyscatter, yhist,
              nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0,
              shareX = TRUE, shareY = TRUE) %>%
        style(showlegend = FALSE, traces = c(1, sum(3 + length(input$locality)))) %>%
        layout(xaxis = list(showgrid = TRUE),
               yaxis2 = list(showgrid = TRUE))
    }
  })

  ## Build median plot ----
  
  output$difftitleclimate <- renderText({
    if (input$variable1 == input$variable2 | length(input$locality) == 0) {
      paste0("Please make sure that you've selected two different variables and/or at least one locality.")
    } else {
      paste("Median Plot:", attr(geo_data()$x, "goodname"), "rank by ", 
            attr(geo_data()$y, "goodname"), "averages")
    }
  })

  output$median_plot_climate <- renderPlotly({
    if (input$variable1 %in% cant_map | input$variable2 %in% cant_map | input$variable1 == input$variable2 | length(input$locality) == 0) {
      plotly_empty()
    } else {
      to_tercile <- bi_class(geo_data(), x = x, y = y, style = "quantile", dim = 2) %>%
        mutate(var1_tercile = stri_extract(bi_class, regex = '^\\d{1}(?=-\\d)'),
                var_1_group = case_when(var1_tercile == 1 ~ 'Low',
                                        var1_tercile == 2 ~ 'Medium',
                                        var1_tercile == 3 ~ 'High'),
                goodname_x = attr(geo_data()$x, "goodname"),
                goodname_y = attr(geo_data()$y, "goodname")) %>% 
        group_by(var1_tercile) %>% 
        mutate(var_2_mean = mean(y, na.rm = T)) %>% 
        slice(1)
      to_tercile <- st_drop_geometry(to_tercile)
      t <- ggplot(to_tercile, aes(x = var1_tercile, y = var_2_mean,
                                  fill = var1_tercile, label = var_1_group,
                                  text = paste0('Mean of ', goodname_y, ': ', round(var_2_mean, digits = 2)))) +
        geom_bar(stat = 'identity', width = 0.66) +
        scale_fill_manual(values = c('#dfb0d6', '#a5add3', '#569ab9')) +
        scale_x_discrete(labels = c('Tracts below the median', 'Tracts above the median')) +
        theme_minimal()

      ggplotly(t, tooltip = c('text')) %>%
        layout(showlegend = FALSE, 
                xaxis = list(title = list(text = attr(geo_data()$x, "goodname"), font = list(size=14)), showticklabels = TRUE, fixedrange = TRUE),
               yaxis = list(title = list(text = attr(geo_data()$y, "goodname"), font = list(size=14)),showticklabels = TRUE, fixedrange = TRUE)
                )
    }
  })


## output variable information ----

  # by selector
  # indicator 1
  output$ind1_defn <- renderText({
    attr(geo_data()$x, "description")
  })

  output$ind1_source <- renderText({
    paste("Source: ", attr(geo_data()$x, "source"))
  })

  # indicator 2
  output$ind2_defn <- renderText({
    attr(geo_data()$y, "description")
  })

  # indicator 2 description by selector
  output$ind2_source <- renderText({
    paste("Source: ", attr(geo_data()$y, "source"))
  })

  ## data download button function ----
  output$downloadBtnCli <- downloadHandler(
    filename = paste0("data_download.csv"),
    content = function(file) {
      write.csv(st_drop_geometry(geo_data()), file)
    }
  )
  ## data download button function ----
  output$downloadBtnCli <- downloadHandler(
    filename = "data_download.zip",
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("climate_atlas_census_tract.csv", "variable_dictionary_1.csv", "variable_dictionary_2.csv")
      write.csv(st_drop_geometry(geo), file = "climate_atlas_census_tract.csv", sep = ",")
      write.csv(group_df_clim, file = "variable_dictionary_1.csv", sep = ",")
      write.csv(group_df_dem, file = "variable_dictionary_2.csv", sep = ",")
      print (fs)
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )


# Economic and Social Atlas -------
  
  # to make helper() info render
  observe_helpers()
  
  # output available indicator geographic levels
  output$ind_geo1 <- renderText({
    paste0("Available Geographic Levels: ", attr(md()[[input$indicator1]], "geo_level"))
  })
  
  output$ind_geo2 <- renderText({
    paste0("Available Geographic Levels: ", attr(md()[[input$indicator2]], "geo_level"))
  })
  
  # output indicator 1 name, for Source & Definition box
  output$ind1_name <- renderText({
    attr(md()[[input$indicator1]], "goodname")
  })
  
  # output indicator 1 description, for Source & Definition box
  output$ind1_abt <- renderText({
    attr(md()[[input$indicator1]], "about") 
  })
  
  # output indicator 2 name, for Source & Definition box
  output$ind2_name <- renderText({
    req(input$indicator2)
    attr(md()[[input$indicator2]], "goodname")
  })
  
  # output indicator 2 description, for Source & Definition box
  output$ind2_abt <- renderText({
    req(input$indicator2)
    attr(md()[[input$indicator2]], "about") 
  })
  
  # get map data
  md <- reactive({
    all_data %>% filter(county.nice %in% input$geo &
                          GEO_LEVEL == input$geo_df &
                          year == "2021") 
  })
  
  listen_indicator1 <- reactive({
    list(input$indicator1, input$geo, input$geo_df)
  })

  listen_indicator2 <- reactive({
    list(input$indicator2, input$geo, input$geo_df)
  })


## Map Functions -------------------------------------------------------

  ### Leaflet base map function ----
  renderLeafletFunction <- function(map) {
    renderLeaflet({
      counties_geo %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(-76.26, 37.05, -75.11, 38.07) %>%
        addPolygons(color = "#969997",
                    fill = FALSE,
                    weight = 2,
                    group = 'baseCounties') %>% 
        addResetMapButton()
    }) 
  }

  
  ### leafletProxy Map Function ---- 
  mapFunction <- function(mapData, mapId, fillColor, ind, popupText){
    
    # popup content
    popupContent <- if (input$geo_df == "County"){
      paste0(attr(ind, "goodname"), ": ",
             popupText, "<br>",
             mapData[["NAME"]])
    } else {
      paste0(attr(ind, "goodname"), ": ",
             popupText, "<br>",
             mapData[["NAME"]], "<br>",
             "Tract Name(s): ", mapData[["tractnames"]])
    }
    
    ### map proxy
    proxy <- leafletProxy(mapId, data = mapData)

    ### observe
    observe({
      proxy %>%
        clearShapes() %>%
        mapGroupFunction(mapData) %>%
        addPolygons(data = mapData, fillColor = fillColor,
                      fillOpacity = 0.4,
                      color = "#969997",
                      weight = 2,
                      smoothFactor = 0.2,
                      popup = popupContent,
                      highlight = highlightOptions(
                        weight = 3,
                        fillOpacity = 0.7,
                        bringToFront = F)) %>% 
        clearControls() %>%
        addLegend(pal = colorNumeric(mycolors, domain = ind),
                    values = ind,
                    position = "topright",
                    opacity = 0.4,
                    title = attr(ind, "goodname")) 
    })
  }
  
  ## Parks/Schools/District Map Components Function ---- 
  mapGroupFunction <- function(map, mapData){
    addPolygons(map, data = mapData, color = "#969997",
                fill = FALSE,
                weight = 2) %>%
      addCircles(data = st_collection_extract(parks_sf, "POINT"), color = "green",
                 group="Parks",
                 popup = ~ParkName) %>% 
      addPolygons(data = st_collection_extract(parks_sf, "POLYGON"), color = "green",
                  group="Parks",
                  popup = ~ParkName) %>%
      addCircles(data =  filter(schools_sf),
                 group="Schools",
                 popup = ~NAME) %>%
      addPolygons(data = filter(sabselem_sf), 
                  group="Elem School Zone",
                  color = "blue", fill = FALSE, weight = 2,
                  popup = ~schnam,
                  highlight = highlightOptions(weight = 3,
                                               color = "blue",
                                               bringToFront = TRUE)) %>%
      addPolygons(data = filter(mcd_sf), 
                  group="Magisterial Districts",
                  color = "purple", fill = FALSE, weight = 2,
                  popup = ~NAMELSAD,
                  highlight = highlightOptions(weight = 3,
                                               color = "purple",
                                               bringToFront = TRUE)) %>% 
      addLayersControl(overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magisterial Districts"),
                       options = layersControlOptions(collapsed = FALSE), 
                       position = "bottomright") %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
      hideGroup("Elem School Zone") %>% 
      hideGroup("Magisterial Districts") 
  }
  
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

# Build Map 1 -------------------------------------------------------
  
  output$map1 <- renderLeafletFunction()

  outputOptions(output, "map1", suspendWhenHidden = FALSE)

  observeEvent(listen_indicator1(), {
    ind1 <- md() %>% 
      filter(!is.na(.data[[input$indicator1]])) %>% 
      pull(input$indicator1)

    if (all(is.na(md()[[input$indicator1]]))){
      # showModal(
      #   modalDialog(
      #     title = "Data not available",
      #     "Data not available for the current Geographic Level"
      #   ))
        mapFunction(md(), "map1", "#969997", ind1, "<b>Data not available for the current Geographic Level</b>")
    } else {
      mapFunction(md(), "map1", colorNumeric(mycolors, domain = ind1)(ind1), ind1, ind1)
     
    }
  })

  output$maptitle <- renderText({paste0(attr(md()[[input$indicator1]], "goodname"))})
  output$source <- renderText({paste0("Source: ", attr(md()[[input$indicator1]], "source"))})

 
# Build Map 2 -------------------------------------------------------
 
  output$map2 <- renderLeafletFunction()

  outputOptions(output, "map2", suspendWhenHidden = FALSE)
  
  observeEvent(listen_indicator2(), {
    ind2 <- md() %>%
      filter(!is.na(.data[[input$indicator2]])) %>%
      pull(input$indicator2)

    if (all(is.na(md()[[input$indicator2]]))){
      # showModal(
      #   modalDialog(
      #     title = "Data not available",
      #     "Data not available for the current Geographic Level"
      #   ))
        mapFunction(md(), "map2", "#969997", ind2, "<b>Data not available for the current Geographic Level</b>")

    } else {
      mapFunction(md(), "map2", colorNumeric(mycolors, domain = ind2)(ind2), ind2, ind2)

    }
  })

  
  output$maptitle2 <- renderText({
    if (input$indicator2 != "None") paste0(attr(md()[[input$indicator2]], "goodname")) })
  output$source2 <- renderText({
    if (input$indicator2 != "None") paste0("Source: ", attr(md()[[input$indicator2]], "source"))})
  
# Build Scatterplot -------------------------------------------------------
  
  output$comptitle <- renderText({
    if (input$indicator2=="None") {
      paste("You have not selected a second indicator. To compare, select a second indicator from the control panel.")
    } else { 
      paste(attr(md()[[input$indicator1]], "goodname"), " vs. ", 
            attr(md()[[input$indicator2]], "goodname"))
    }
  })
  
  output$compare <- renderPlotly({ # add loess line to this?
    d <- st_drop_geometry(md())
      plot_ly(data=d,  
              x = ~get(input$indicator1),
              y=  ~get(input$indicator2),
              color = ~county.nice,
              type = "scatter", 
              mode = "markers",
              fill = ~"", # to remove line.width error
              size = ~as.numeric(d$pop),
              sizes = if (input$geo_df == "County"){
                        c(900,2000)
                      } else {
                        c(50, 500)
                      },
              marker = list(line = list(color = 'rgba(0, 0, 0, .4)',
                                        width = 1)),
              colors = "Set2",
              text = if (input$geo_df == "County"){
                      ~paste0(
                        md()[["NAME"]], "<br>",
                        "Estimated Population: ", d$pop, "<br>",
                        "<b>Indicator Selections:</b><br>",
                        attr(d[[input$indicator1]], "goodname"), ": ", 
                        d[[input$indicator1]], "<br>",
                        attr(d[[input$indicator2]], "goodname"), ": ", 
                        d[[input$indicator2]])
                    } else if (attr(d[[input$indicator1]], "goodname") == "Estimated Population"){
                      ~paste0(
                        md()[["NAME"]], "<br>",
                        "Tract Name(s): ", md()[["tractnames"]], "<br>",
                        "<b>Indicator Selections:</b><br>",
                        attr(d[[input$indicator1]], "goodname"), ": ", 
                        d[[input$indicator1]], "<br>",
                        attr(d[[input$indicator2]], "goodname"), ": ", 
                        d[[input$indicator2]])
                    } else {
                      ~paste0(
                        md()[["NAME"]], "<br>",
                        "Tract Name(s): ", md()[["tractnames"]], "<br>",
                        "Estimated Population: ", d$pop, "<br>",
                        "<b>Indicator Selections:</b><br>",
                        attr(d[[input$indicator1]], "goodname"), ": ", 
                        d[[input$indicator1]], "<br>",
                        attr(d[[input$indicator2]], "goodname"), ": ", 
                        d[[input$indicator2]])
                    }, 
              hoverinfo='text') %>% 
        layout(xaxis=list(title=attr(d[[input$indicator1]], "goodname")),
               yaxis=list(title=attr(d[[input$indicator2]], "goodname")))
  })
  
  # scatterplot source caption, if present
  output$source_c <- renderText({
    if (input$indicator2=="None") {
      paste("")
    } else {
      paste0(
        "Sources: ",
        attr(md()[[input$indicator1]], "goodname"), ": ", 
        attr(md()[[input$indicator1]], "source"), " ",
        attr(md()[[input$indicator2]], "goodname"), ": ", 
        attr(md()[[input$indicator2]], "source")
      )
    }
  })

# Build Tercile Plot -----------------------------------------------------
## NOT WORKING
  output$tercile_plot <- renderPlotly({
    
    if (!input$indicator2=="None") {
      to_tercile <- bi_class(md(), 
                             x = ~get(input$indicator1), 
                             y = ~get(input$indicator2), 
                             style = "quantile", 
                             dim = 3)
      to_tercile$var1_tercile <- stri_extract(to_tercile$bi_class, regex = '^\\d{1}(?=-\\d)')
      
      to_tercile$`Var 1 Group` <- ifelse(to_tercile$var1_tercile == 1, 'Low', ifelse(to_tercile$var1_tercile == 2, 'Medium', ifelse(to_tercile$var1_tercile == 3, 'High', '')))
      to_tercile <- to_tercile %>% group_by(var1_tercile) %>% mutate(`Var 2 Mean` = mean(y, na.rm = T)) %>% slice(1)
      t <- ggplot(to_tercile, aes(x = var1_tercile, y = `Var 2 Mean`,
                                  fill = var1_tercile, label = `Var 1 Group`,
                                  text = paste0('Mean of ', attr(to_tercile$y, "goodname"), ': ', round(`Var 2 Mean`, digits = 2)))) +
        geom_bar(stat = 'identity', width = 0.66) +
        scale_fill_manual(values = c('#dfb0d6', '#a5add3', '#569ab9')) +
        scale_x_discrete(labels = paste0(c('Lowest ', 'Middle ', 'Highest '), 'third of tracts')) +
        labs(x = attr(to_tercile$x, "goodname"),
             y = attr(to_tercile$y, "goodname")) +
        theme_minimal()
      
      ggplotly(t, tooltip = c('text')) %>%
        layout(showlegend = FALSE, yaxis = list(side = "right", fixedrange = T), xaxis = list(fixedrange = T))
      
    }
  })
# Data Download -------------------------------------------------------
  
  ## data download button function ----
  output$downloadBtn <- downloadHandler(
    filename = "esva_data_download.zip",
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("esva_county.csv", "esva_census_tract.csv", "esva_block_groups.csv", "variable_dictionary.csv")
      write.csv(all_data %>% filter(GEO_LEVEL == "County" &
                                      year == "2021") %>% st_drop_geometry(), 
                file = "esva_county.csv", sep =",")
      write.csv(all_data %>% filter(GEO_LEVEL == "Census Tract" &
                                      year == "2021") %>% st_drop_geometry(), 
                file = "esva_census_tract.csv", sep =",")
      write.csv(all_data %>% filter(GEO_LEVEL == "Block Group" &
                                      year == "2021") %>% st_drop_geometry(), 
                file = "esva_block_groups.csv", sep =",")
      write.csv(data_dict, file = "variable_dictionary.csv", sep = ",")
      print (fs)
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )

}

# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
