# Published version
# VA Eastern Shore Atlas
# Updated 3/3/2023
# Last Deployed: -

library(shiny)
library(bslib)
library(shinyhelper)
library(tidyverse)
library(leaflet)
library(plotly)
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
                              width = 4,
                                cardComponentSelect(
                                selectInput("indicator1",
                                 "Select First Map Variable:",
                                 choices = ind_choices_county,
                                 selected = ind_choices_bg$People['Estimated Population']
                                ) %>% 
                                helper(type = "inline",
                                  icon = "question-circle",
                                  content = helpers$indicator,
                                  size = "m"),
                                accordianComponent("ind1", "Show Selected Variable Definition", textOutput("ind1_abt", inline = TRUE),"var-def-1", "map-ind-1")
                                )
                            ) %>% tagAppendAttributes(class="mb-3 mb-sm-0"),
                            column(
                              width = 4,
                              cardComponentSelect(
                              selectInput("indicator2",
                                 "Select Second Map Variable:",
                                 choices = ind_choices_county,
                                 selected = ind_choices_county$Housing['Total Housing Units']) %>% 
                              helper(type = "inline",
                                  icon = "question-circle",
                                  content = helpers$indicator2,
                                  size = "m"),
                              accordianComponent("ind2", "Show Selected Variable Definition", textOutput("ind2_abt", inline = TRUE),"var-def-2", "map-ind-2")
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
                                  choices = c("County", "Census Tract", "Block Group"),
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
                                tabPanel("Data Table",
                                  h2(textOutput("tbltitle", inline = TRUE)),
                                  p("Variables ending in E are estimates; variables ending in M are margins of error."),
                                  DTOutput("tbl"),
                                  h3(textOutput("dltitle", inline = TRUE)),
                                  downloadButton("downloadBtn", "Download")
                                )
                              )
                            )
                          ), # end fluidRow
                          fluidRow(
                            column(
                              width = 12,
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

  ## leafletProxy Map Function ---- 
  mapFunction <- function(mapData, mapId, fillColor, ind, popupText){
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
                      popup = paste0(attr(ind, "goodname"), ": ",
                                     popupText, "<br>",
                                     mapData[["NAME"]], "<br>",
                                     "Tract Name(s): ", mapData[["tractnames"]]),
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
  
  ## Add Map Reset button function ----
  addResetMapButton <- function(leaf) {
    leaf %>%
      addEasyButton(
        easyButton(
          icon = "ion-arrow-shrink", 
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

# Build Map 1 -------------------------------------------------------
  
  output$map1 <- renderLeafletFunction()

  observeEvent(listen_indicator1(), {
    ind1 <- md() %>% 
      filter(!is.na(.data[[input$indicator1]])) %>% 
      pull(input$indicator1)

    if (all(is.na(md()[[input$indicator1]]))){
      showModal(
        modalDialog(
          title = "Data not available",
          "Data not available for the current Geographic Level"
        ))
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
      showModal(
        modalDialog(
          title = "Data not available",
          "Data not available for the current Geographic Level"
        ))
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
    if (!input$indicator2=="None") {
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
    }
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
  
# Build Data Table -------------------------------------------------------
  
  ## output data table ----
  output$tbl <-  renderDT({
    datatable(st_drop_geometry(md()),
              options = list(scrollX = TRUE))
  })
  
  ## data table title ----
  output$tbltitle <- renderText({
    paste("Data by", input$geo_df)
  })
  
  ## data download button function ----
  output$downloadBtn <- downloadHandler(
    filename = paste0("esva_data_download.csv"),
    # filename = paste0("esva_", sub(" ", "_", tolower(input$geo_df)), "_data.csv"),
    content = function(file) {
      write.csv(st_drop_geometry(md()), file)
    }
  )

  ## data download title ----
  output$dltitle <- renderText({
    paste0("Download Table Data (Current Geographic Level: ", input$geo_df, ")")
  })
  
}

# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
