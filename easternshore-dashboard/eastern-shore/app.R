# VA Eastern Shore Atlas
# Updated 1/26/2023
# Last Deployed: 1/26/2023

library(shiny)
library(shinydashboard)
library(shinyhelper)
library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(DT)
# library(RColorBrewer)

# Load Data ---------------------------------------------------------------

load("www/app_data_2022.Rdata")

# was not deploying on shinyapps:
# https://stackoverflow.com/questions/61286108/error-in-cpl-transformx-crs-aoi-pipeline-reverse-ogrcreatecoordinatetrans
all_data <- st_transform(all_data, 4326)
counties_geo <- st_transform(counties_geo, 4326)


# Define UI ---------------------------------------------------------------

ui <- dashboardPage(skin = "purple",
        
        # Dashboard Header
        dashboardHeader(title = "Virginia Eastern Shore Atlas", 
                        tags$li(class = "dropdown", actionButton(inputId = "refresh", "Refresh Atlas", class = "btn-lg")), 
                        titleWidth = 300),
        
        # Dashboard Sidebar - currently disabled
        dashboardSidebar(disable = TRUE),
        
        # Dashboard Main Content
        dashboardBody(
          tags$head(tags$style(HTML('
          .skin-purple .main-header .navbar, .skin-purple .main-header .logo, .skin-purple .main-header .logo:hover  {
              background-color: #605ca8;
          }

          .nav-tabs-custom > .nav-tabs > li.active {
              border-top-color: #605ca8;
          }
          .nav > li > a {
              padding: 4px 15px;
          }
          h2 {
              margin-top: 5px;
              font-size: 24px;
          }

          # .box-header {
          #     color: #425775;
          #     # background: #425775;
          #     # background-color: #425775;
          # }
          # .box.box-solid {
          #     border: 1px solid #425775;
          # }
                              '))),
          
          fluidRow(
            column(width = 4,
                   # Sidebar layout
                   box(
                     title = "Instructions", width = NULL, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                     p("Make selections in the boxes below to show demographic, economic and social data on the maps and correlation plot in tabs to the right.
                       Variables include data related to Health, Housing, People, Youth & Education, Jobs, Wages & Income, and various Indices.")
                   ),
                   box(
                     title = "Select First Map Variable:", width = NULL, solidHeader = TRUE,
                     # pick indicator 1
                     selectInput("indicator1",
                                 NULL,
                                 choices = ind_choices_county,
                                 selected = ind_choices_county$People['Estimated Population']),
                     strong("Selected Variable Definition: "), textOutput("ind1_abt", inline = TRUE)
                     # %>% 
                     #   helper(type = "inline",
                     #          icon = "info-circle",
                     #          content = helpers$indicator,
                     #          size = "m")
                   ),
                   box(
                     title = "Select Second Map Variable:", width = NULL, solidHeader = TRUE,
                     # pick indicator 2
                     selectInput("indicator2",
                                 NULL,
                                 choices = ind_choices_county,
                                 selected = ind_choices_county$Housing['Total Housing Units']),
                     strong("Selected Variable Definition: "), textOutput("ind2_abt", inline = TRUE)
                     # %>% 
                     #   helper(type = "inline",
                     #          icon = "info-circle",
                     #          content = helpers$indicator2,
                     #          size = "m")
                   ),
                   box(
                     title = "Additional Map Options", width = NULL, solidHeader = TRUE,
                     # pick counties
                     checkboxGroupInput(
                       inputId = "geo", 
                       label = "Localities", 
                       choices = counties, 
                       selected = counties,
                       inline = TRUE)
                     # %>% # checkboxGroupInput ends, pipe to helper()
                     #   helper(type = "inline",
                     #          icon = "info-circle",
                     #          content = helpers$counties,
                     #          size = "m")
                     ,
                     
                     # pick geographic level
                     radioButtons(inputId = "geo_df",
                                  label = "Select Geographic Level:", 
                                  choices = c("County", "Census Tract", "Block Group"),
                                  selected = "Census Tract", 
                                  inline = TRUE) 
                     # %>% # radioButtons ends, pipe to helper()
                     #   helper(type = "inline",
                     #          title = "Geographic Level",
                     #          icon = "info-circle",
                     #          content = helpers$geo,
                     #          size = "m")
                     # Select a base map
                     # radioButtons(inputId = "map_geo",
                     #              label = "Select Base Map:",
                     #              choices = c("Minimal" = "CartoDB.Positron",
                     #                          "Detailed" = "OpenStreetMap.Mapnik"),
                     #              inline = TRUE) %>% 
                     #   helper(type = "inline",
                     #          inputId = "map_geo",
                     #          icon = "info-circle",
                     #          content = helpers$map_geo,
                     #          size = "m")
                   )
            ), # end column 1
            column(width = 8,
              tabBox(id = "tabs", width = NULL,
                tabPanel(h4("Map: First Selection"), 
                         value = "tab1",
                         h2(textOutput("ind1_name", inline = TRUE)),
                         p("Click on areas below to view names and indicator values."),
                         leafletOutput("map", height=600),
                         br(),
                         textOutput("source")
                         ),
                tabPanel(h4("Map: Second Selection"), 
                         value = "tab2",
                         h2(textOutput("ind2_name", inline = TRUE)),
                         p("Click on areas below to view names and indicator values."),
                         leafletOutput("map2", height=600),
                         br(),
                         textOutput("source2")
                         ),
                tabPanel(h4("Correlation"),
                         h2(textOutput("comptitle")) %>% 
                           helper(type = "inline",
                                  title = "Distribution",
                                  icon = "question-circle",
                                  content = helpers$correlation,
                                  size = "m"),
                         plotlyOutput("compare"),
                         br(),
                         textOutput("source_c")
                         )
              )
            )
           
          ) # end fluidRow
        ) # end dashboardBody
) # end dashboardPage

# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  
  # to make helper() info render
  observe_helpers()
  
  ## refresh app ----
  observeEvent(input$refresh, session$reload())
  
  # geography selections - update selection of indicators based on geo level
  observeEvent(input$geo_df, {
    if (input$geo_df == "Block Group"){
      updateSelectInput(session, "indicator1", choices = ind_choices_bg, 
                        selected = ind_choices_bg$People['Estimated Population']
      )
      updateSelectInput(session, "indicator2", choices = ind_choices_bg,
                        selected = ind_choices_bg$Housing['Total Housing Units']
      )
    } else if (input$geo_df == "Census Tract"){
      updateSelectInput(session, "indicator1", choices = ind_choices_ct,
                        selected = ind_choices_ct$People['Estimated Population']
      )
      updateSelectInput(session, "indicator2", choices = ind_choices_ct,
                        selected = ind_choices_ct$Housing['Total Housing Units']
      )
    } else {
      updateSelectInput(session, "indicator1", choices = ind_choices_county,
                        selected = ind_choices_county$People['Estimated Population']
      )
      updateSelectInput(session, "indicator2", choices = ind_choices_county,
                        selected = ind_choices_county$Housing['Total Housing Units']
      )
    }
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
  
  ################
  #### BASE MAP
  ################
  
  output$map <- output$map2 <- renderLeaflet({
    
    # filter for "Parks", "Schools", "Elem School Zone", "Magisterial Districts"
    f <- md()[["COUNTYFP"]]
    
    counties_geo %>%
      #sf::st_transform(4326) %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(color = "grey",
                  fill = FALSE,
                  weight = 3) %>% 
      addCircles(data = st_collection_extract(parks_sf, "POINT"), color = "green",
                 group="Parks",
                 popup = ~ParkName) %>% 
      addPolygons(data = st_collection_extract(parks_sf, "POLYGON"), color = "green",
                  group="Parks",
                  popup = ~ParkName) %>%
      addCircles(data =  filter(schools_sf, county %in% f),
                 group="Schools",
                 popup = ~NAME) %>%
      addPolygons(data = filter(sabselem_sf, county %in% f), 
                  group="Elem School Zone",
                  color = "blue", fill = FALSE, weight = 2,
                  popup = ~schnam,
                  highlight = highlightOptions(weight = 3,
                                               color = "blue",
                                               bringToFront = TRUE)) %>%
      addPolygons(data = filter(mcd_sf, COUNTYFP %in% f), 
                  group="Magisterial Districts",
                  color = "purple", fill = FALSE, weight = 2,
                  popup = ~NAMELSAD,
                  highlight = highlightOptions(weight = 3,
                                               color = "purple",
                                               bringToFront = TRUE)) %>% 
      addLayersControl(
        overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magisterial Districts"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "bottomright"
      ) %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
      hideGroup("Elem School Zone") %>% 
      hideGroup("Magisterial Districts") 
    
    
  })
  
  ################
  #### BEGIN MAP 1
  ################
  
  observe({
    if(input$tabs == "tab1"){
      
      # vector of values
      ind1 <- md() %>% 
        filter(!is.na(.data[[input$indicator1]])) %>% 
        pull(input$indicator1)
      
      if (all(is.na(ind1))){
        showModal(modalDialog(
          title = "Data not available",
          "Data not available for the selected Geographic Level." ))
      } else {
        leafletProxy("map", data = md()
                     # data = sf::st_transform(md(), 4326)
        ) %>%
          clearControls() %>%
          # addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(fillColor = colorNumeric(mycolors, domain = ind1)(ind1),
                      fillOpacity = 0.5,
                      color = "#969997",
                      weight = 2,
                      smoothFactor = 0.2,
                      popup = paste0(attr(ind1, "goodname"), ": ",
                                     ind1, "<br>",
                                     md()[["NAME"]], "<br>",
                                     "Tract Name(s): ", md()[["tractnames"]]),
                      highlight = highlightOptions(
                        weight = 5,
                        fillOpacity = 0.7,
                        bringToFront = F)) %>%
          addLegend(pal = colorNumeric(mycolors, domain = ind1),
                    values = ind1,
                    position = "topright",
                    opacity = 0.25,
                    title = attr(ind1, "goodname"))
      }
    }
  })
  
  output$maptitle <- renderText({paste0(attr(md()[[input$indicator1]], "goodname"),
                                        ", ", input$time) })
  output$source <- renderText({attr(md()[[input$indicator1]], "source")})
  
  
  ################
  #### BEGIN MAP 2
  ################
  
  observe({
    
    if (input$tabs == "tab2"){
      
      # redraw a basic map if None selected again
      if (input$indicator2 == "None"){
        leafletProxy("map2", data = md()
                     # data = sf::st_transform(md(), 4326)
        ) %>%
          clearControls() %>% 
          clearShapes() %>%
          addPolygons(color = "grey",
                      fill = FALSE,
                      weight = 3)
        
      } else {  
        
        # vector of values
        ind2 <- md() %>% 
          filter(!is.na(.data[[input$indicator2]])) %>% 
          pull(input$indicator2)
        
        if (all(is.na(ind2))){
          showModal(modalDialog(
            title = "Data not available",
            "Data not available for the selected Geographic Level." ))
        } else {
          leafletProxy("map2", data = md()
                       # data = sf::st_transform(md(), 4326)
          ) %>%
            clearControls() %>% 
            # addProviderTiles(providers$CartoDB.Positron) %>% 
            addPolygons(fillColor = colorNumeric(mycolors, domain = ind2)(ind2),
                        fillOpacity = 0.5,
                        color = "#969997",
                        weight = 2,
                        smoothFactor = 0.2,
                        popup = paste0(attr(ind2, "goodname"), ": ",
                                       ind2, "<br>",
                                       md()[["NAME"]], "<br>",
                                       "Tract Name(s): ", md()[["tractnames"]]),
                        highlight = highlightOptions(
                          weight = 5,
                          fillOpacity = 0.7,
                          bringToFront = F)) %>%
            addLegend(pal = colorNumeric(mycolors, domain = ind2),
                      values = ind2,
                      position = "topright",
                      opacity = 0.25,
                      title = attr(ind2, "goodname"))
        }
      } 
    }
  })
  
  output$maptitle2 <- renderText({
    if (input$indicator2 != "None") paste0(attr(md()[[input$indicator2]], "goodname"),
                                           ", ", input$time) })
  output$source2 <- renderText({
    if (input$indicator2 != "None") attr(md()[[input$indicator2]], "source")})
  
###############################
# BEGIN Scatterplot
###############################  
  
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
  
  
  
}

# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
