# Published version
# VA Eastern Shore Atlas
# Updated 3/15/2023
# Last Deployed: -

library(shiny)
library(bslib)
library(shinyhelper)
library(tidyverse)
library(leaflet)
library(plotly)
library(biscale)
library(stringi)
library(sf)
library(DT)

source("functions/utils.R")

# Load Data ---------------------------------------------------------------

load("www/app_data_2022.Rdata")

# was not deploying on shinyapps:
# https://stackoverflow.com/questions/61286108/error-in-cpl-transformx-crs-aoi-pipeline-reverse-ogrcreatecoordinatetrans
all_data <- st_transform(all_data, 4326)
counties_geo <- st_transform(counties_geo, 4326)


# Define UI ---------------------------------------------------------------

ui <- htmlTemplate(filename = "esva-template.html", main = 
      navbarPage(title = img(src='logo-horizontal-lg.png',
                         height = 70,
                         alt = "Eastern Shore of Virginia Climate Equity Project logo"),
                 windowTitle = "Eastern Shore of Virginia Climate Equity Project Atlas",
                 collapsible = TRUE,
                 fluid = TRUE,
                 theme = bs_theme(version = 5),
                 tabPanel(HTML("<center>SOCIAL & ECONOMIC<br>EQUITY ATLAS</center>"),
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
                                                   "Make selections in the boxes below to show demographic, economic and social data on the maps and correlation plot in tabs to the right.
                                                    Variables include data related to Health, Housing, People, Youth & Education, Jobs, Wages & Income, and various Indices.",
                                                   "intro-1", "intro-2")
                              )
                            )
                          ), br(), # end fluidRow
                          fluidRow(
                            column(
                              width = 4,
                                cardComponentSelect(
                                selectInput("indicator1",
                                    "Select First Equity Indicator:",
                                    choices = ind_choices_county,
                                    selected = ind_choices_county$People['Estimated Population']) %>% 
                                helper(type = "inline",
                                  icon = "question-circle",
                                  content = helpers$indicator,
                                  size = "m"),
                                accordianComponent("ind1", "Show Selected Indicator Definition", textOutput("ind1_abt", inline = TRUE),"var-def-1", "map-ind-1")
                                )
                            ) %>% tagAppendAttributes(class="mb-3 mb-sm-0"),
                            column(
                              width = 4,
                              cardComponentSelect(
                              selectInput("indicator2",
                                 "Select Second Equity Indicator:",
                                 choices = ind_choices_county,
                                 selected = ind_choices_county$Housing['Total Housing Units']) %>% 
                              helper(type = "inline",
                                  icon = "question-circle",
                                  content = helpers$indicator2,
                                  size = "m"),
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
                              # actionButton(inputId = "refresh", "Refresh Atlas")
                              )
                            ) %>% tagAppendAttributes(class="mb-3 mb-sm-0")
                          ), br(), 
                          fluidRow(
                            column(
                              width = 12,
                              tabsetPanel(
                                id = "tabs",
                                tabPanel(
                                    "First Map Selection",
                                    value = "tab1",
                                    h2(textOutput("ind1_name", inline = TRUE)),
                                    p("Click on areas below to view names and indicator values."),
                                    leafletOutput("map1", height=600),
                                    br(),
                                    textOutput("source")
                                  ),
                                tabPanel("Second Map Selection", 
                                    value = "tab2",
                                    h2(textOutput("ind2_name", inline = TRUE)),
                                    p("Click on areas below to view names and indicator values."),
                                    leafletOutput("map2", height=600),
                                    br(),
                                    textOutput("source2")
                                  ),
                                tabPanel("Selection Relationship",
                                    h2(textOutput("comptitle")) %>% 
                                      helper(type = "inline",
                                              title = "Distribution",
                                              icon = "question-circle",
                                              content = helpers$correlation,
                                              size = "m"),
                                    plotlyOutput("compare"),
                                    br(),
                                    textOutput("source_c")
                                  ),
                                tabPanel(title = "Differences",
                                         tags$div(style="font-size:13px", br(), tags$p("Each census tract in the selected Charlottesville region is ranked into three groups representing tracts with Low, Middle, or High values on the measure you select on the left (Variable 1). The height of the bar shows the average value of the measure you select on the right (Variable 2) within that group of tracts. Hover over each bar to see the average value on Variable 2.")),
                                         plotlyOutput(outputId = 'tercile_plot'), br()
                                ) 
                                # tabPanel("Data Table",
                                #   h2(textOutput("tbltitle", inline = TRUE)),
                                #   p("Variables ending in E are estimates; variables ending in M are margins of error."),
                                #   DTOutput("tbl"),
                                #   h3(textOutput("dltitle", inline = TRUE)),
                                #   downloadButton("downloadBtn", "Download")
                                # )
                              )
                            )
                          ), br(), hr(),# end fluidRow
                          fluidRow(
                            column(
                              width = 12,
                              h2("Download Data"),
                              p("Data in this Atlas is provided as a compressed folder, which includes CSVs for data at the county, census tract, and block group levels as well as a data dictionary and README."),
                              downloadButton("downloadBtn", "Download"),
                              br(), hr(), br()
                            )
                          ) # end fluidRow
                 ) # end tabPanel
)) # end navbarPage / end HTML template

# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  
  # to make helper() info render
  observe_helpers()
  
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


# Map Functions -------------------------------------------------------

  ## Leaflet base map function ----
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

  
  ## leafletProxy Map Function ---- 
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
    
    # map proxy
    proxy <- leafletProxy(mapId, data = mapData)

    # observe
    observe({
      proxy %>%
        clearShapes() %>%
        mapGroupFunction(mapData) %>%
        addPolygons(data = mapData, fillColor = fillColor,
                      fillOpacity = 0.5,
                      color = "#969997",
                      weight = 2,
                      smoothFactor = 0.2,
                      popup = popupContent,
                      highlight = highlightOptions(
                        weight = 5,
                        fillOpacity = 0.7,
                        bringToFront = F)) %>% 
        clearControls() %>%
        addLegend(pal = colorNumeric(mycolors, domain = ind),
                    values = ind,
                    position = "topright",
                    opacity = 0.25,
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
  output$source <- renderText({attr(md()[[input$indicator1]], "source")})

 
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
    if (input$indicator2 != "None") attr(md()[[input$indicator2]], "source")})
  
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
    # if (!input$indicator2=="None") {
      plot_ly(data=d,  
              x = ~get(input$indicator1),
              y=  ~get(input$indicator2),
              color = ~county.nice,
              type = "scatter", 
              mode = "markers",
              marker = list(size = 10,
                            # colors = ~county.nice,
                            line = list(color = 'rgba(0, 0, 0, .4)',
                                        width = 1)),
              # Changed to Set2 for more visible colors for 2 counties
              colors = "Set2",
              text = paste0(
                            # d$county.nice, "<br>",
                            md()[["NAME"]], "<br>",
                            "Tract Name(s): ", md()[["tractnames"]], "<br>",
                            attr(d[[input$indicator1]], "goodname"), ": ", 
                            d[[input$indicator1]], "<br>",
                            attr(d[[input$indicator2]], "goodname"), ": ", 
                            d[[input$indicator2]]
              ), 
              hoverinfo='text') %>% 
        layout(xaxis=list(title=attr(d[[input$indicator1]], "goodname")),
               yaxis=list(title=attr(d[[input$indicator2]], "goodname")))
    # }
  })
  
  # scatterplot source caption, if present
  output$source_c <- renderText({
    if (input$indicator2=="None") {
      paste("")
    } else {
      paste(
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
# Build Data Table -------------------------------------------------------
  
  ## output data table ----
  output$tbl <-  renderDT({
    datatable(st_drop_geometry(md()),
              options = list(scrollX = TRUE))
  })
  
  ## data table title ----
  # output$tbltitle <- renderText({
  #   paste("Data by", input$geo_df)
  # })
  
  ## data download button function ----
  output$downloadBtn <- downloadHandler(
    filename = paste0("esva_data_download.zip"),
    # filename = paste0("esva_", sub(" ", "_", tolower(input$geo_df)), "_data.csv"),
    # content = function(file) {
    #   write.csv(st_drop_geometry(md()), file)
    # }
    
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

  ## data download title ----
  # output$dltitle <- renderText({
  #   paste0("Download Table Data (Current Geographic Level: ", input$geo_df, ")")
  # })
  
}

# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
