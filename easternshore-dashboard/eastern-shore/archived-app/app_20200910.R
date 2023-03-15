# Published version
# Equity Indicators

# Load libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(shinyhelper)

library(tidyverse)
library(RColorBrewer)
library(scales)
library(leaflet)
library(plotly)
library(sp)
library(sf)
library(DT)



# Load data
load("www/app_data.Rdata")

# arrange for plotting by race: 
race_comp <- select(county_data, GEOID:year, hhinc_blackE:hhinc_ltnxM, lifeexp_blackE:lifeexp_asianM) %>% 
  pivot_longer(hhinc_blackE:lifeexp_asianM, names_to = "var", values_to = "value") %>% 
  separate(var, into=c("var", "type"), sep=-1) %>% 
  separate(var, into=c("var", "race"), sep="_") %>% 
  separate(NAME, into=c("NAME", "state"), sep=",") %>% 
  mutate(race = fct_recode(race,
                           "Black"="black", 
                           "Asian"="asian", 
                           "Hispanic"="ltnx", 
                           "Multi-racial"="multi",
                           "White"="white"))


# to ease creation of select/deselect all action
counties <- levels(factor(tract_data_geo$county.nice))

# Define color palettes
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(8, "YlGnBu"))(nb.cols)


# .....................................................................................

# create ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Regional Equity Dashboard Prototype", titleWidth = 400),
  dashboardSidebar(collapsed = TRUE, 
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Example Stories", tabName = "storywidgets", icon = icon("bar-chart-o"),
                              menuSubItem("BA Attainment: Albemarle", tabName = "story1"),
                              menuSubItem("Home Sales: Albemarle", tabName = "story2")),
                     menuItem("Relevant Links", tabName = "linkwidget", icon = icon("external-link"))
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              
              fluidRow(
                box(tags$h3("Regional Indicators"),
                    tags$p("Visualizing the greater Charlottesville Region"),
                    
                    checkboxGroupInput(
                      inputId = "geo", 
                      label = "Counties", 
                      choices = counties, 
                      selected = counties,
                      inline = TRUE) %>%  #checkboxGroup ends
                      helper(type = "inline",
                             icon = "info-circle",
                             content = c("<b>Counties:</b> select one or more counties of interest to view. The localities selected appear on the map and as observations in the figures below. Use the Select/Deselect All button below to make many changes quickly."),
                             size = "m"),
                    actionButton(inputId = "selectall_geo", 
                                 label = "Select/Unselect All"),  #selectall end
                    
                    tags$br(),
                    tags$br(),
                    radioButtons(inputId = "df_geo",
                                 label = "Select a Geographic Level:",
                                 choices = c("County", "Census Tract", "Block Group"),
                                 selected = "County",
                                 inline = TRUE) %>% 
                      helper(type = "inline",
                             title = "Geographic Level",
                             icon = "info-circle",
                             content = c("View indicators at the county level, the census tract level, or the block group level. Some indicators are only available at the tract or locality level.",
                                         "<b>County:</b> The counties and cities within the region.", "",
                                         "<b>Census Tract:</b> The Census Tract is an area roughly equivalent to a neighborhood generally encompassing between 2,500 to 8,000 people. They are designed to be relatively homogeneous with respect to population characteristics, economic status, and living conditions.", "",
                                         "<b>Block Group:</b> The block group is a cluster of adjacent census blocks within a census tract. Block groups generally contain between 600 and 3,000 people. This is the smallest geographical unit for which the census publishes sample data."),
                             size = "m"),
                    
                    htmlOutput("category") %>% 
                      helper(type = "inline",
                             inputId = "category",
                             icon = "info-circle",
                             content = c("There are 30+ indicators currently available, with more in the works. You may find filtering the available indicators to a specific category helpful to view included measures or locate a measure of interest. Selecting a category will limit the choice of indicators in the Primary and Secondary Indicator fields.",
                                         "<b>Health:</b> Health insurance rates and life expectancy.", "",
                                         "<b>Housing:</b> Number and rates of vacant housing, home ownership rates, percent rent burdened.", "",
                                         "<b>Indices:</b> Measures derived from combined indicators including human development index, gini inequality index, and measures of residential dissimilarity.", "",
                                         "<b>Jobs, Wages & Income:</b> Median household income and personal earnings, poverty and unemployment rates.", "",
                                         "<b>People:</b> Basic demographics including total population, racial and ethnic populations, populations by age group.", "",
                                         "<b>Youth & Education:</b> Educational attainment, school enrollment, child poverty rates."),
                             size = "m"),
                    
                    htmlOutput("indicator") %>% 
                      helper(type = "inline",
                             inputId = "indicator",
                             icon = "info-circle",
                             content = c("Select an indicator to view on the Primary map. The distribution of the selected indicator will also be plotted below."),
                             size = "m"),
                    htmlOutput("indicator2") %>% 
                      helper(type = "inline",
                             inputId = "indicator2",
                             icon = "info-circle",
                             content = c("Select an indicator to view on the Secondary map. The distribution of the selected indicator will also be plotted below, including a plot showing the correlation with the Primary indicator."),
                             size = "m"),
                    htmlOutput("time") %>% 
                      helper(type = "inline",
                             inputId = "time",
                             icon = "info-circle",
                             content = c("<b>Under Development.</b> Some indicators are available for multiple years. Select the year you wish to view. Currently, over time data is only present for total population and population by race or ethnicity."),
                             size = "m"),
                    radioButtons(inputId = "map_geo",
                                 label = "Select a Base Map:",
                                 choices = c("Minimal", "Detailed"),
                                 selected = "Minimal",
                                 inline = TRUE) %>% 
                      helper(type = "inline",
                             inputId = "map_geo",
                             icon = "info-circle",
                             content = c("Select a base map. The detailed map will show roads and other features more clearly, but viewers may find the detail distracting."),
                             size = "m"),
                    width=4), 
                
                tabBox(tabPanel("Map of Primary Indicator", 
                                textOutput("maptitle"),
                                leafletOutput("map", height=600),
                                textOutput("source")),
                       tabPanel("Map of Secondary Indicator",
                                textOutput("map2title"),
                                leafletOutput("map_compare", height=600),
                                textOutput("source2")),
                       tabPanel("Data Table",
                                textOutput("tbltitle"),
                                tags$p("Variables ending in E are estimates; variables ending in M are margins of error."),
                                DTOutput("tbl")),
                       width=8)), # fluid row
              
              fluidRow(
                box(title = "Indicator Source & Definition",
                    strong(textOutput("ind1_name")), 
                    textOutput("ind1_abt"), tags$br(),
                    strong(textOutput("ind2_name")), 
                    textOutput("ind2_abt"),
                    width=4),
                tabBox(width=8,
                       tabPanel("Distribution",
                                textOutput("histtitle") %>% 
                                  helper(type = "inline",
                                         title = "Distribution",
                                         icon = "question-circle",
                                         content = c("The distribution, or histogram, shows how often each value of an indicator occurs in the selected data (defined by locality and geography level). The distribution quickly shows the minimum and maximum values as well as the range of the most common values of an indicator."),
                                         size = "m"),
                                plotlyOutput("hist"),
                                textOutput("source_b")), 
                       tabPanel("Correlation", 
                                textOutput("comptitle") %>% 
                                  helper(type = "inline",
                                         title = "Correlation",
                                         icon = "question-circle",
                                         content = c("The correlation, or scatterplot, shows the relation between the primary and secondary indicators across geographic units; this helps us see if two indicators are associated and, if so, how -- e.g., as the value of the primary indicator increases, does the value of the secondary indicator notably increase or decrease. It can also help us locate geographic units that have extreme values on both measures."),
                                         size = "m"),
                                plotlyOutput("compare"),
                                textOutput("source_c")), 
                       tabPanel("By Race", 
                                textOutput("racetitle") %>% 
                                  helper(type = "inline",
                                         title = "By Race",
                                         icon = "question-circle",
                                         content = c("<b>Under Development</b>. The comparison by race is intended to show, where available, how an indicator like household income differs by racial and ethnic populations. Currently only available when Median Household Income is selected as the primary indicator."),
                                         size = "m"),
                                plotlyOutput("byrace"))
                )
                
              ), # fluid row
              fluidRow(
                box(tags$h2(tags$a(href = "http://virginiaequitycenter.org/", tags$img(height = 80, src = "UVAEQUITYCENTER.jpeg"), "Equity Center")),
                    tags$p("The Equity Center works to tangibly redress racial and socioeconomic inequality in 
                   university communities by advancing a transformative approach to the fundamental 
                   research mission, which will, in turn, reform institutional values, pedagogy, 
                   and operations."),
                    tags$p("The Democratization of Data Initiative is working with the UVA Library, the CommPAS Lab,
                   and the broader regional community to co-create a 
                   comprehensive Equity Atlas, which will aim to be inclusive of an open-source 
                   platform that makes data and information that the community wants to know about 
                   itself broadly available for use in the pursuit of equity throughout the region.")),
                
                box(tags$a(href = "http://commpas-lab.mystrikingly.com",
                           tags$img(height = 80, src = "three-line-bw.png")),
                    tags$p("The Community Politics, Analytics and Strategy Lab (CommPAS) 
                               sponsors the community-oriented work and collaboration between 
                               the Batten School of Leadership and Public Policy and the UVA 
                               Library's StatLab. Through courses and research projects, the 
                               CommPAS Lab works in partnership with local agencies, nonprofits, 
                               and citizen groups to produce actionable research and resources. 
                               The CommPAS Lab brings students into community-engaged research 
                               where they learn about local challenges while developing and 
                               applying their policy and data science skills in the service of 
                               our community partners."), 
                    tags$p("Dashboard Creators: Michele Claibourn, Charlotte McClintock"),
                    tags$p("Contributors: Clay Ford, Hannah Lewis"),
                    tags$div(style="display: inline-block;vertical-align:top;", tags$p("We welcome feedback and suggestions at")),
                    tags$div(style="display: inline-block;vertical-align:top;", tags$a(href="mailto:commpaslab@virginia.edu", tags$p("commpaslab@virginia.edu"))))
                
              ) # row close
      ), # tab item close
      
      tabItem(tabName = "storywidgets"),
      tabItem(tabName = "story1", 
              uiOutput("disparity")),  # created in alb_homesales_school.Rmd
      tabItem(tabName = "story2",
              uiOutput("homes")),  # created in BAed_003.Rmd
      tabItem(tabName = "linkwidget",
              uiOutput("links")) # created in equity_links.Rmd
    ) # tab items
  ) # dashboard body
) # dashboard page


# .....................................................................................

# create server ----
server <- function(input, output, session) {
  
  # .....................................................................................
  
  # uses 'helpfiles' directory by default
  # in this example, we use the withMathJax parameter to render formulae
  observe_helpers()
  
  # select data sets ----
  # selected geo dataset
  data_geo <- reactive({
    d1 <- switch(input$df_geo,
                 "County" = county_data_geo,
                 "Census Tract" = tract_data_geo,
                 "Block Group" = blkgrp_data_geo)
    #    d1 <- filter(d1, locality %in% input$geo & major_group2 %in% input$group)
  })
  
  # select non-geo dataset
  df2 <- reactive({
    d2 <- if (input$df_geo == "Census Tract") {
      tract_data
    }
    else if (input$df_geo == "County") {
      county_data
    }
    else {
      blkgrp_data
    }
    #    d2 <- filter(d2, locality %in% input$geo & major_group2 %in% input$group)
  })
  
  # select pretty table
  prettytab <- reactive({
    pt <- if (input$df_geo == "Census Tract"){
      pretty
    }
    else if (input$df_geo == "County") {
      pretty2
    }
    else {
      pretty3
    }
  })
  
  
  # .....................................................................................
  
  # generate UI ----
  
  # Select/Deselect All
  observe({
    if(input$selectall_geo == 0) return(NULL)
    else if (input$selectall_geo%%2 == 0){
      updateCheckboxGroupInput(session, inputId = "geo", "Counties",
                               choices = counties, selected = counties,
                               inline = TRUE)    }else{
                                 updateCheckboxGroupInput(session, "geo", "Counties",
                                                          choices = counties, inline = TRUE)
                               }
  })
  
  # categories of indicators
  output$category <- renderUI({
    selectInput(
      inputId = "category", 
      label = "Limit Indicator by Category",
      choices = levels(factor(prettytab()$group)), 
      selected = levels(factor(prettytab()$group)),
      multiple = T)
  })
  
  # primary indicator
  output$indicator <- renderUI({
    arb <- input$category
    available <- prettytab()[prettytab()$group %in% arb, "goodname"]
    names(available) <- "Indicator 1"
    selectInput(
      inputId = "indicator", 
      label = "Primary Indicator:",
      choices = unique(available),
      selected = unique(available)[1],
      multiple = F)
  })
  
  # secondary indicator
  output$indicator2 <- renderUI({
    arb <- input$category
    available <- prettytab()[prettytab()$group %in% arb, "goodname"]
    names(available) <- "Indicator 2"
    selectInput(
      inputId = "indicator2",
      label = "Secondary Indicator:",
      choices = c("None", unique(available)),
      selected = "None",
      multiple = F)
  })
  
  years_avail <- reactive({    
    req(input$indicator)
    df <- st_drop_geometry(data_geo()) %>% filter(county.nice %in% input$geo) # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
    col <- paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])
    df <- na.omit(df[,c(col, "year")])
    df$year <- as.numeric(df$year)
    sort(unique(df$year))
  })
  
  output$time <- renderUI({
    sliderTextInput(inputId = "time", 
                    label = "Select a Year",
                    choices = years_avail(),
                    selected = years_avail()[length(years_avail())],
                    animate=F,
                    grid=T )})
  
  
  # .....................................................................................
  
  # titles & source ----
  
  # output indicator 1 source
  output$source <- renderText({
    req(input$indicator)
    paste(prettytab()[prettytab()$goodname==input$indicator, "source"])
  })
  
  # output indicator 1 name
  output$ind1_name <- renderText({
    req(input$indicator)
    paste(input$indicator)
  })
  
  # output indicator 1 description
  output$ind1_abt <- renderText({
    req(input$indicator)
    paste(prettytab()[prettytab()$goodname==input$indicator, "about"]) 
  })
  
  # output indicator 2 source
  src2 <- reactive({
    req(input$indicator2)
    if (input$indicator2=="None") {
      paste("")
    }
    else {
      paste(prettytab()[prettytab()$goodname==input$indicator2, "source"])
    }
  })
  
  output$source2 <- renderText({src2()})
  
  # output indicator 2 name
  output$ind2_name <- renderText({
    if (input$indicator2=="None") {
      paste("")
    }
    else {
      paste(input$indicator2)
    }
  })
  
  # output indicator 2 description
  output$ind2_abt <- renderText({
    if (input$indicator2=="None") {
      paste("")
    }
    else {
      req(input$indicator2)
      paste(prettytab()[prettytab()$goodname==input$indicator2, "about"]) }
  })
  
  
  # primary map title
  output$maptitle <- renderText({input$indicator})
  
  
  # secondary map title
  map2.title <- reactive({
    req(input$indicator2)
    if (input$indicator2=="None") {
      paste("You have not selected a second indicator.")
    }
    else {paste(input$indicator2)}
  })
  
  output$map2title <- renderText({map2.title()})
  
  # Table title
  output$tbltitle <- renderText({
    paste("Data by", input$df_geo)
  })
  
  # comparison title (plotly/scatterplot)
  comp.title <- reactive({
    req(input$indicator2)
    if (input$indicator2=="None") {
      paste("You have not selected a second indicator. To compare, select a second indicator from the control panel.")
    }
    else {paste(input$indicator, " vs. ", input$indicator2)}
  })
  
  # comparison title
  output$comptitle <- renderText({comp.title()})
  
  
  # comparison source (plotly/scatterplot)
  src_c <- reactive({
    req(input$indicator2)
    if (input$indicator2=="None") {
      paste("")
    }
    else {
      paste(input$indicator, ":", prettytab()[prettytab()$goodname==input$indicator, "source"], input$indicator2, ":", prettytab()[prettytab()$goodname==input$indicator2, "source"])
    }
  })
  
  output$source_c <- renderText({src_c()})
  
  
  # histogram title
  hist.title <- reactive({
    req(input$indicator2)
    if (input$indicator2=="None") {
      paste("Histogram of", input$indicator, "by", input$df_geo)
    }
    else {
      paste("Histograms of", input$indicator, " and ", input$indicator2, "by Census Tract")}
  })
  
  # histogram title
  output$histtitle <- renderText({hist.title()})
  
  
  # comparison source: source 1 or source 1 and 2 (plotly/histogram)
  src_b <- reactive({
    if (input$indicator2=="None") {
      paste(prettytab()[prettytab()$goodname==input$indicator, "source"])
    }
    else {
      paste(input$indicator, ":", prettytab()[prettytab()$goodname==input$indicator, "source"], input$indicator2, ":", prettytab()[prettytab()$goodname==input$indicator2, "source"])
    }
  })
  
  output$source_b <- renderText({src_b()})
  
  
  # by race title
  race.title <- reactive({
    req(input$indicator)
    req(input$indicator2)
    if (!input$indicator2 %in% c("Median Household Income", "Life Expectancy at Birth")&!input$indicator %in% c("Median Household Income", "Life Expectancy at Birth")) {
      paste("Breakouts by race are not yet available for the indicator you selected.")
    }
    else {paste(input$indicator, "by Race by Locality")}
  })
  
  output$racetitle <- renderText({race.title()})
  
  
  # .....................................................................................
  
  # create indicator 1 data frame ----
  d <- reactive({
    req(input$indicator)
    df <- st_drop_geometry(data_geo()) %>% filter(county.nice %in% input$geo &  # change data_geo()@data to data_geo(); nope, change to st_drop_geometry(data_geo())
                                                    year %in% input$time)
    col <- paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])
    df[,col]
  })
  
  # create indicator 1 palette function
  pal <- reactive({
    req(input$indicator)
    df <- st_drop_geometry(data_geo()) %>% filter(county.nice %in% input$geo &  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                                    year %in% input$time)
    col <- paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])
    colorNumeric(palette = mycolors,
                 domain = df[,col]) })
  
  # create indicator 2 data frame 
  d2 <- reactive({
    req(input$indicator2)
    df <- st_drop_geometry(data_geo()) %>% filter(county.nice %in% input$geo &  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                                    year %in% input$time)
    col <- paste(prettytab()[prettytab()$goodname==input$indicator2, "varname"])
    df[,col]
  })
  
  # create indicator 2 palette function
  pal2 <- reactive({
    req(input$indicator2)
    df <- st_drop_geometry(data_geo()) %>% filter(county.nice %in% input$geo &  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                                    year %in% input$time)
    col2 <- paste(prettytab()[prettytab()$goodname==input$indicator2, "varname"])
    colorNumeric(palette = mycolors,
                 domain = df[,col2])
  })
  
  # create (filtered) polygon/point files
  parks <- reactive({
    parks_sf %>% filter(FIPS %in% data_geo()$COUNTYFP[data_geo()$county.nice %in% input$geo])
  })
  
  schools <- reactive({
    schools_sf %>% filter(county %in% data_geo()$COUNTYFP[data_geo()$county.nice %in% input$geo])
  })
  
  sabselem <- reactive({
    sabselem_sf %>% filter(county %in% data_geo()$COUNTYFP[data_geo()$county.nice %in% input$geo])
  })
  
  mag <- reactive({
    mcd_sf %>% filter(COUNTYFP %in% data_geo()$COUNTYFP[data_geo()$county.nice %in% input$geo])
  })
  
  # create map tile layer
  tile_geo <- reactive({
    tile <- switch(input$map_geo,
                   "Minimal" = "CartoDB.Positron",
                   "Detailed" = "OpenStreetMap.Mapnik")
  })
  
  # create var selector for by race plot, if aailable
  race_var <- reactive({
    if (input$indicator == "Median Household Income" | input$indicator2 == "Median Household Income") {
      var = "hhinc"
    } else if (input$indicator == "Life Expectancy at Birth" | input$indicator2 == "Life Expectancy at Birth") {
      var = "lifeexp"
    } else {
      is.null(var)
    }
  })
  
  # .....................................................................................
  
  # output map 1 ----
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(tile_geo()) %>% 
      addPolygons(data = subset(data_geo(), county.nice %in% input$geo & year %in% input$time),
                  fillColor = ~pal()(d()),
                  fillOpacity = 0.5,
                  color = "white",
                  weight = 2,
                  smoothFactor = 0.2,
                  popup = paste0(input$indicator, ": ",d(),  "<br>",
                                 data_geo()$NAME[data_geo()$county.nice %in% input$geo & data_geo()$year %in% input$time], "<br>"
                  ),
                  highlight = highlightOptions(
                    weight = 5,
                    fillOpacity = 0.7,
                    bringToFront = F)) %>% 
      addPolygons(data = subset(counties_geo, NAMELSAD %in% input$geo),
                  color = "grey",
                  fill = FALSE,
                  weight = 3) %>% 
      addPolygons(data =  parks(), group="Parks", 
                  color = "green",
                  popup = paste(parks()$NAME)) %>% 
      addCircles(data =  schools(), group="Schools", 
                 color = "blue",
                 popup = paste(schools()$NAME)) %>% 
      addPolygons(data = sabselem(), group="Elem School Zone",
                  color = "blue", fill = FALSE, weight = 2,
                  popup = paste(sabselem()$schnam),
                  highlight = highlightOptions(weight = 3,
                                               color = "blue",
                                               bringToFront = TRUE)) %>% 
      addPolygons(data = mag(), group="Magesterial Districts",
                  color = "purple", fill = FALSE, weight = 2,
                  popup = paste(mag()$NAMELSAD),
                  highlight = highlightOptions(weight = 3,
                                               color = "purple",
                                               bringToFront = TRUE)) %>% 
      addLayersControl(
        overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magesterial Districts"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "bottomright"
      ) %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
      hideGroup("Elem School Zone") %>% 
      hideGroup("Magesterial Districts") %>% 
      addLegend(pal = pal(),
                values = as.numeric(d()),
                position = "topright",
                opacity = 0.25,
                title = input$indicator)  
  })
  
  
  # output map 2 ----
  output$map_compare <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(tile_geo()) %>% 
      addPolygons(data = subset(data_geo(), county.nice %in% input$geo & year %in% input$time),
                  fillColor = ~pal2()(d2()),
                  fillOpacity = 0.5,
                  color = "white",
                  weight = 2,
                  smoothFactor = 0.2,
                  popup = paste0(input$indicator2, ": ",d2(),  "<br>",
                                 data_geo()$NAME[data_geo()$county.nice %in% input$geo & data_geo()$year %in% input$time], "<br>"
                  ),
                  highlight = highlightOptions(
                    weight = 5,
                    fillOpacity = 0.7,
                    bringToFront = F)) %>% 
      addPolygons(data = subset(counties_geo, NAMELSAD %in% input$geo),
                  color = "grey",
                  fill = FALSE,
                  weight = 3) %>% 
      addPolygons(data =  parks(), group="Parks", 
                  popup = paste(parks()$NAME)) %>% 
      addCircles(data =  schools(), group="Schools", 
                 popup = paste(schools()$NAME)) %>% 
      addPolygons(data = sabselem(), group="Elem School Zone",
                  color = "blue", fill = FALSE, weight = 2,
                  popup = paste(sabselem()$schnam),
                  highlight = highlightOptions(weight = 3,
                                               color = "blue",
                                               bringToFront = TRUE)) %>% 
      addPolygons(data = mag(), group="Magesterial Districts",
                  color = "purple", fill = FALSE, weight = 2,
                  popup = paste(mag()$NAMELSAD),
                  highlight = highlightOptions(weight = 3,
                                               color = "purple",
                                               bringToFront = TRUE)) %>% 
      addLayersControl(
        overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magesterial Districts"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "bottomright"
      ) %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
      hideGroup("Elem School Zone") %>% 
      hideGroup("Magesterial Districts") %>% 
      addLegend(pal = pal2(),
                values = as.numeric(d2()),
                position = "topright",
                opacity = 0.25,
                title = input$indicator2)  
  })
  
  
  # .....................................................................................
  # output data table ----
  output$tbl <-  renderDT({
    datatable(df2(),
              options = list(scrollX = TRUE))
  })
  
  
  # .....................................................................................
  # output comparison plot ----
  output$compare <- renderPlotly({ # add loess line to this
    req(input$indicator2)
    if (!input$indicator2=="None") {
      plot_ly(data=st_drop_geometry(data_geo()),  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
              x=st_drop_geometry(data_geo())[data_geo()$county.nice %in% input$geo,  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                             paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])],
              y=st_drop_geometry(data_geo())[data_geo()$county.nice %in% input$geo,  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                             paste(prettytab()[prettytab()$goodname==input$indicator2, "varname"])],
              color=st_drop_geometry(data_geo())[data_geo()$county.nice %in% input$geo,  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                                 "county.nice"],
              type = "scatter", mode = "markers", 
              # Set3 has 12 values, which matches the 12 cities/counties
              colors = "Set3", 
              text=paste0(data_geo()$NAME[data_geo()$county.nice %in% input$geo], "<br>",
                          input$indicator, ": ", st_drop_geometry(data_geo())[data_geo()$county.nice %in% input$geo,  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                                                              paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])], "<br>",
                          input$indicator2, ": ", st_drop_geometry(data_geo())[data_geo()$county.nice %in% input$geo,  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                                                               paste(prettytab()[prettytab()$goodname==input$indicator2, "varname"])]
              ), 
              hoverinfo='text') %>% 
        layout(xaxis=list(title=input$indicator),
               yaxis=list(title=input$indicator2))
    }
  })
  
  
  # .....................................................................................
  # output histogram/s ----
  output$hist <- renderPlotly({ 
    req(input$indicator2)
    if (!input$indicator2=="None") {
      p1 <- plot_ly(data=st_drop_geometry(data_geo()),  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                    x=st_drop_geometry(data_geo())[data_geo()$county.nice %in% input$geo,  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                                   paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])],
                    type="histogram", marker = list(color = '#2c7fb8')) %>%
        layout(xaxis=list(title=input$indicator))
      p2 <- plot_ly(data=st_drop_geometry(data_geo()),  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                    x=st_drop_geometry(data_geo())[data_geo()$county.nice %in% input$geo,  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                                   paste(prettytab()[prettytab()$goodname==input$indicator2, "varname"])],
                    type="histogram", marker = list(color = '#41b6c4')) %>%
        layout(xaxis=list(title=input$indicator2))
      subplot(p1, p2, titleX = T, titleY = T) %>%
        layout(showlegend = FALSE)
    }
    else {
      plot_ly(data=st_drop_geometry(data_geo()),  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
              x=st_drop_geometry(data_geo())[data_geo()$county.nice %in% input$geo,  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                                             paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])],
              type="histogram", marker = list(color = '#253494')) %>%
        layout(xaxis=list(title=input$indicator))
    }
  })
  
  
  # .....................................................................................
  # output by race visual ----
  output$byrace <- renderPlotly({
    req(input$indicator2)
    req(input$indicator)
    if (input$indicator %in% c("Median Household Income", "Life Expectancy at Birth") | input$indicator2 %in% c("Median Household Income", "Life Expectancy at Birth")) {
      ggplotly(
        ggplot(subset(race_comp, NAME %in% input$geo & var == race_var()),
               aes(x=race, y=value, fill=race)) + 
          stat_summary(fun.y="sum", geom="bar", 
                       aes(text=paste0(NAME,"<br>", race, 
                                       input$indicator,
                                       prettyNum(value, big.mark=",",scientific=F)))) + 
          scale_fill_manual(values=brewer.pal(6, "YlGnBu")[2:6]) +
          facet_wrap(~NAME, scales="free_y") +
          labs(x="", y="") +
          theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0)),
        tooltip="text") %>%
        layout(showlegend = FALSE)
    }
  })
  
  
  # .....................................................................................
  # output sidebar stories ----
  output$tabs <- renderText("Story Examples")
  output$disparity <- renderUI(includeHTML("BAed_003.html"))
  output$homes <- renderUI(includeHTML("alb_homesales_school.html"))
  output$links <- renderUI(includeHTML("equity_links.html"))
}

# run the application 
shinyApp(ui = ui, server = server)
