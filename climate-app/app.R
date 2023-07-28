# ....................................
# Begin Climate Equity Atlas Development
# Authors: Michele Claibourn, Jacob Goldstein-Greenwood, Lee LeBoeuf
# Last updated: 2022-04-13 mpc, ll
# ....................................

# Phase 1a: added select indicators, output plotly scatterplot, add sample header
#    to do: look into line.width warnings (and no trace specified warnings)
# Phase 1b: added select localities to plot, add tabs for other components, add navbar

# Phase 2a: integrated bichoropleth; add base map selector;
# Phase 2b: added popup info, fixed legend! added setview to force zoom;
#    replaced locality fips with names, replace varnames with good names,
#    added variable descriptions to sidebar
#    updated meta (short description on front, notes on new tab)
# initial output for tercile graph!

# Phase 3:
#    fixed data documentation (into iframe)
#    updated scatterplot colors with ggthemes::few palette
#    updated plotly to get rid of warnings (line.widths, etc.)
#    updated plotly and leaflet hover info to round indicator values
#    update data documentation (reduced)
#    moved logo into navbar
#    reduced tercile bar width
#    made leaflet and plotly output plots taller (height = '450')
#    removed detailed base map, 
#    make variable description text slightly smaller, all other text slightly bigger
#    add space to variable details, add plot orientation language, add global instructions
#    add refresh button

# April update
#   add walkability measures
# May
#   add foreign born and snap variables

# To do?
#    after exhibit, add links to stories and sonification; 
#     and add links on story page to app?
#    add layering points (parks, schools, food retailers)

# ....................................
# Load libraries and data
library(shiny)
library(tidyverse)
library(plotly)
library(ggthemes)
library(leaflet)
library(biscale)
library(sf)
library(leafem)
library(stringi)
library(knitr)
library(htmltools)

load("data/eastern_dat.RData")
# data prepared in combine_data.R

# create palette for use in bichoropleth palette function
bipal <- c("#e8e8e8", "#dfd0d6", "#be64ac", # A-1, A-2, A-3,
           "#ace4e4", "#a5add3", "#8c62aa", # B-1, B-2, B-3
           "#5ac8c8", "#5698b9", "#3b4994") # C-1, C-2, C-3

fewpal <- c("#265dab", "#df5c24", "#c7b42e", "#059748",
            "#cb2027", "#9d722a")

# no-go variables for mapping
cant_map <- c('indigE', 'othraceE', 'bbmax_up', 'HWAV_AFREQ', 'RFLD_AFREQ', 'avg_prox_transit_rank')
cant_map_message <- c("One of your selected variables cannot be rendered in the map or in the tercile plot. This is usually because there isn't enough variation in the variable to break its values up into meaningful categories.")


# ....................................
# Define User Interface ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  navbarPage(title = div(
    div(
      id = "img-id",
      img(src = "ec_climate_logo.png",
          height = 50)
    ),
    "Eastern Shore Climate Equity Atlas"
  ),

                 ## indicator selectors and plots ----
                 tabPanel("Atlas",
                          # global instructions
                          tags$div(style='font-size:15px; font-weight:bold; text-align:center;', tags$p("Select any two measures from the dropdown menus on the left and right side of the screen to see the relationship between the measures in census tracts across the Eastern Shore Region.")),
                          # tags$div(style='text-align:center', tags$a(href="https://virginiaequitycenter.github.io/picturing-climate-stories/", "And visit our Picturing Climate Justice-Data Stories site to view more explanation and analysis using a selection of these data.")),
                          tags$br(),

                          fluidRow(
                            # Sidebar for indicator 1
                            column(2,
                                   selectInput(inputId = "indicator1",
                                               label = h4("Select Variable 1"),
                                               choices = ind_demfirst_ct,
                                               selected = ind_demfirst_ct$`Demographic & Social`["Estimated Population"]),
                                   # variable definitions
                                   span(textOutput("ind1_defn"), style='font-size:13px'),
                                   tags$br(),
                                   span(textOutput("ind1_source"), style='font-size:13px'),
                            ),

                            # Place figures
                            column(8,
                                   tabsetPanel(type = "tabs",
                                               tabPanel(title = 'Map', 
                                                        tags$div(style="font-size:13px", tags$p("Each census tract is ranked from Low to High on the measures you select from the left (Variable 1) and the right (Variable 2). The legend in the upper left corner of the map provides a key for what each color represents. Zoom in to see specific areas more closely or zoom out to see the full region. Click on specific tracts to see the values for each measure, as well as how the tract ranks (Low, Medium, High) relative to others in the selected region.")),
                                                        leafletOutput(outputId = 'leaf', width = '100%', height = '450')
                                               ),
                                               tabPanel(title = "Correlation",
                                                        tags$div(style="font-size:13px", tags$p("Each census tract in the Charlottesville region is represented with a dot, plotted by the value of the tract on the measures you select on the left (Variable 1) and the right (Variable 2). The size of each dot is based on the population of the tract so that tracts with more people appear larger and the color is based on the locality of the tract. The gray figures on the top and right show how frequently high and low values of the selected variables occur in the region; taller bars mean that range of values is more common.")),
                                                        plotlyOutput(outputId = "scatterplot", width = '100%', height = '450')
                                               ),
                                               # tabPanel(title = "Differences",
                                               #          tags$div(style="font-size:13px", tags$p("Each census tract in the selected Charlottesville region is ranked into three groups representing tracts with Low, Middle, or High values on the measure you select on the left (Variable 1). The height of the bar shows the average value of the measure you select on the right (Variable 2) within that group of tracts. Hover over each bar to see the average value on Variable 2.")),
                                               #          plotlyOutput(outputId = 'tercile_plot')
                                               # ),
                                               tabPanel(title = "Variable Details",
                                                        tags$br(),
                                                        strong(textOutput("var1_name")),
                                                        textOutput("var1_abt"),
                                                        textOutput("var1_source"),
                                                        tags$br(),
                                                        strong(textOutput("var2_name")),
                                                        textOutput("var2_abt"),
                                                        textOutput("var2_source"))
                                   )
                            ),

                            # Sidebar for indicator 2
                            column(2,
                                   selectInput(inputId = "indicator2",
                                               label = h4("Select Variable 2"),
                                               choices = ind_climfirst_ct,
                                               selected = ind_climfirst_ct$`Climate Measures`["Average Land Surface Temperature"]),
                                   # variable definitions
                                   span(textOutput("ind2_defn"), style='font-size:13px'),
                                   tags$br(),
                                   span(textOutput("ind2_source"), style='font-size:13px')
                            )
                          ),

                          tags$hr(),

                          ## county/map/geography selector ----
                          fluidRow(

                            # refresh button
                            column(3,
                                   actionButton(inputId = "refresh", "Refresh Atlas")
                                   # base map selector (needs troubleshooting)
                                   # , radioButtons(inputId = "base_map",
                                   #              label = h4("Select a Base Map"),
                                   #              choices = c("Minimal" = "CartoDB.Positron",
                                   #                          "Detailed" = "OpenStreetMap.Mapnik"),
                                   #              inline = TRUE)
                            ),

                            # locality selector
                            column(6,
                                   checkboxGroupInput(inputId = "locality",
                                                      label = h4("Select Localities"),
                                                      choices = c("Accomack" = "001",
                                                                  "Northampton" = "131"),
                                                      selected = c("001", "131"),
                                                      inline = TRUE)
                            )
                          )
                 ),

                 ## information navbars ----
                 tabPanel("Data Documentation",
                          # includeHTML("cville_climate_update.html") # table not displaying
                          # uiOutput("documentation") # table not displaying
                          htmltools::tags$iframe(src = 'cville_climate_update.html',
                                                 style='width:100vw;height:100vh;',
                                                 frameBorder = "0")
                 ),

                   tabPanel("About",
                            #includeHTML("www/about_work.html")
                            htmltools::tags$iframe(src = 'about_work.html',
                                                   style='width:100vw;height:100vh;',
                                                   frameBorder = "0")
                            ),

                 singleton(tags$head(tags$script(src = "message-handler.js")))
))



# ....................................
# Define Server Logic ----
server <- function(input, output, session) {

  geo_data <- reactive({
    if (input$indicator1 == input$indicator2) {
      session$sendCustomMessage(type = 'testmessage',
                                message = paste0("Please make sure that you've selected two different variables."))
    } else if (length(input$locality) == 0) {
      session$sendCustomMessage(type = 'testmessage',
                                message = paste0("At least one locality must be selected."))
    } else {
      geo <- geo %>%
        dplyr::select(x = !!sym(input$indicator1),
                      y = !!sym(input$indicator2),
                      locality, countyname, tract, geoid,
                      pop = pop) %>%
        dplyr::filter(locality %in% input$locality) %>%
        drop_na()
    }
  })

  ## output scatterplot ----
  output$scatterplot <- renderPlotly({
    if (input$indicator1 == input$indicator2 | length(input$locality) == 0) {
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
                           mode = 'markers', # to remove mode warning
                           fill = ~'', # to remove line.width error
                           size = ~pop,
                           sizes = c(1, 500),
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

  ## output map ----
  #build static parts of map, and display initial outline of region
  output$leaf <- renderLeaflet({
    leaflet() %>% addProviderTiles('CartoDB.Positron') %>%
      addPolygons(data = geo, color = 'grey', opacity = 0) %>%
      # setView(lng = -78.47668, lat = 38.02931, zoom = 9) %>% will need to change lat and long
      addLogo('bivariate_legend_static.svg', src = "remote",
              position = "topleft", width = 100, height = 100, alpha = 0.8)
  })
  # reactive function to detect when variable 1, variable 2, or locality selection changes
  listen_closely <- reactive({
    list(input$indicator1,input$indicator2, input$locality)
  })
  # when a variable or locality selection is changed, render the appropriate bichoropleth without losing the legend
  observeEvent(listen_closely(), {
    if (input$indicator1 == input$indicator2 | length(input$locality) == 0) {
      leafletProxy('leaf') %>% clearShapes()
    } else if (input$indicator1 %in% cant_map | input$indicator2 %in% cant_map) {
      session$sendCustomMessage(type = 'testmessage', message = cant_map_message)
      leafletProxy('leaf') %>% clearShapes()
    } else {
      to_map <- bi_class(geo_data(), x = x, y = y, style = "quantile", dim = 3)
      to_map$var1_tercile <- stri_extract(to_map$bi_class, regex = '^\\d{1}(?=-\\d)')
      to_map$var1_tercile_cat <- ifelse(to_map$var1_tercile == 1, 'Low', ifelse(to_map$var1_tercile == 2, 'Medium', ifelse(to_map$var1_tercile == 3, 'High', '')))
      to_map$var2_tercile <- stri_extract(to_map$bi_class, regex = '(?<=\\d-)\\d{1}$')
      to_map$var2_tercile_cat <- ifelse(to_map$var2_tercile == 1, 'Low', ifelse(to_map$var2_tercile == 2, 'Medium', ifelse(to_map$var2_tercile == 3, 'High', '')))
      factpal <- colorFactor(bipal, domain = to_map$bi_class)
      leafletProxy('leaf', data = to_map) %>% clearShapes() %>%
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
                    popup = paste0("Locality: ", to_map$countyname, ", tract ", to_map$tract, "<br>",
                                   attr(to_map$x, "goodname"), ": ", round(to_map$x, 2),  "<br>",
                                   "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Relative to other tracts: ", to_map$var1_tercile_cat, "<br>",
                                   attr(to_map$y, "goodname"), ": ", round(to_map$y, 2), "<br>",
                                   "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Relative to other tracts: ", to_map$var2_tercile_cat))
    }
  })

  # ## output tercile plot ----
  # output$tercile_plot <- renderPlotly({
  #   if (input$indicator1 %in% cant_map | input$indicator2 %in% cant_map | input$indicator1 == input$indicator2 | length(input$locality) == 0) {
  #     plotly_empty()
  #   } else {
  #     to_tercile <- bi_class(geo_data(), x = x, y = y, style = "quantile", dim = 3)
  #     to_tercile$var1_tercile <- stri_extract(to_tercile$bi_class, regex = '^\\d{1}(?=-\\d)')
  # 
  #     to_tercile$`Var 1 Group` <- ifelse(to_tercile$var1_tercile == 1, 'Low', ifelse(to_tercile$var1_tercile == 2, 'Medium', ifelse(to_tercile$var1_tercile == 3, 'High', '')))
  #     to_tercile <- to_tercile %>% group_by(var1_tercile) %>% mutate(`Var 2 Mean` = mean(y, na.rm = T)) %>% slice(1)
  #     t <- ggplot(to_tercile, aes(x = var1_tercile, y = `Var 2 Mean`,
  #                                 fill = var1_tercile, label = `Var 1 Group`,
  #                                 text = paste0('Mean of ', attr(to_tercile$y, "goodname"), ': ', round(`Var 2 Mean`, digits = 2)))) +
  #       geom_bar(stat = 'identity', width = 0.66) +
  #       scale_fill_manual(values = c('#dfb0d6', '#a5add3', '#569ab9')) +
  #       scale_x_discrete(labels = paste0(c('Lowest ', 'Middle ', 'Highest '), 'third of tracts')) +
  #       labs(x = attr(to_tercile$x, "goodname"),
  #            y = attr(to_tercile$y, "goodname")) +
  #       theme_minimal()
  # 
  #     ggplotly(t, tooltip = c('text')) %>%
  #       layout(showlegend = FALSE, yaxis = list(side = "right", fixedrange = T), xaxis = list(fixedrange = T))
  # 
  #   }
  # })

  ## refresh app ----
  observeEvent(input$refresh, session$reload())
  
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

  # detailed var info on var info tab
  # indicator 1
  output$var1_name <- renderText({
    attr(geo_data()$x, "goodname")
  })

  output$var1_abt <- renderText({
    attr(geo_data()$x, "about")
  })

  output$var1_source <- renderText({
    paste("Source: ", attr(geo_data()$x, "source"))
  })

  # indicator 2
  output$var2_name <- renderText({
    attr(geo_data()$y, "goodname")
  })

  output$var2_abt <- renderText({
    attr(geo_data()$y, "about")
  })

  output$var2_source <- renderText({
    paste("Source: ", attr(geo_data()$y, "source"))
  })

  ## about page ----
  # output$documentation <- renderUI(htmltools::includeHTML("cville_climate_update.html"))

  }
# Run the application ----
shinyApp(ui = ui, server = server)
