################################################################################
# Charlottesville Regional Equity Atlas                                        
# Author: Michele Claibourn                           
# Author: Lee LeBoeuf
# Author: Elizabeth Mitchell                               
# Last revised: 2023-02-20                                                     
################################################################################

library(shiny)
library(tidyverse)
library(leaflet)
library(shinyhelper)
library(sf)
library(plotly)
library(biscale)
library(stringi)

# Load Data ---------------------------------------------------------------

load("www_2023/app_data_2022.Rdata")

# transform data 
all_data <- st_transform(all_data, 4326)
all_data$pop <- as.character(all_data$totalpopE)
counties_geo <- st_transform(counties_geo, 4326)

# no-go variables for mapping
cant_map <- c('indigE', 'othraceE', 'bbmax_up', 'HWAV_AFREQ', 'RFLD_AFREQ')
cant_map_message <- c("One of your selected variables cannot be rendered in the map or in the tercile plot. This is usually because there isn't enough variation in the variable to break its values up into meaningful categories.")

fewpal <- c("#265dab", "#df5c24", "#c7b42e", "#059748",
                     "#cb2027", "#9d722a")

# Define User Interface ---------------------------------------------------
ui <- navbarPage(title = "Charlottesville Regional Equity Atlas",
                 collapsible = TRUE,
                 fluid = TRUE,
                 tabPanel("Atlas",
                          fluidRow( # fluid row 1 
                            
                            # pick indicator 1 
                            column(width = 4,
                                     selectInput("indicator1",
                                                 "Map 1 Indicator:",
                                                 choices = ind_choices_county,
                                                 selected = ind_choices_county$People['Estimated Population']),
                                     # variable definitions
                                     span(textOutput("ind1_defn"), style='font-size:13px'),
                                     tags$br(),
                                     span(textOutput("ind1_source"), style='font-size:13px'),
                                     ), # end pick indicator 1 column
                            
                            # pick indicator 2 
                            column(width = 4,
                                   selectInput("indicator2",
                                               "Map 2 Indicator:",
                                               choices = ind_choices_county,
                                               selected = ind_choices_county$Housing['Total Housing Units']),
                                   # variable definitions
                                   span(textOutput("ind2_defn"), style='font-size:13px'),
                                   tags$br(),
                                   span(textOutput("ind2_source"), style='font-size:13px')
                            ), # end pick indicator 2 column 
                            
                            # other map selections 
                            column(width = 4,
                                   # pick counties
                                     checkboxGroupInput(
                                       inputId = "geo", 
                                       label = "Localities", 
                                       choices = counties,
                                       selected = counties,
                                       inline = TRUE) %>% # end checkboxGroupInput 
                                    # checkboxGroupInput ends, pipe to helper()
                                       helper(type = "inline",
                                              icon = "info-circle",
                                              content = helpers$counties,
                                              size = "m"), # end helper
                                   
                                   # add select all option
                                   actionButton(inputId = "selectall_geo", 
                                                label = "Select/Unselect All"),
                                   
                                   tags$br(),
                                   tags$br(),
                                   
                                   # pick geographic level
                                   radioButtons(inputId = "geo_df",
                                                label = "Select a Geographic Level:", 
                                                choices = c("County", "Census Tract"),
                                                selected = "Census Tract", 
                                                inline = TRUE) %>% 
                                   # radioButtons ends, pipe to helper()
                                     helper(type = "inline",
                                            title = "Geographic Level",
                                            icon = "info-circle",
                                            content = helpers$geo,
                                            size = "m"), # end helper
                                   
                                   tags$br(),
                                   
                                   # Select a base map
                                   radioButtons(inputId = "map_geo",
                                                label = "Select a Base Map:",
                                                choices = c("Minimal" = "CartoDB.Positron",
                                                            "Detailed" = "OpenStreetMap.Mapnik"),
                                                inline = TRUE),
                                   
                                   actionButton(inputId = "refresh", "Refresh Atlas")
                                   
                                   ) # end other map selections column 
                            
                              ), # end fluid row 1 
                          
                          fluidRow( # fluid row 2 
                              column(width = 12,
                                     tabsetPanel(id = "tabs",
                                       tabPanel(title = 'Map 1', value = 'tab1',
                                                tags$div(style="font-size:13px", br(), tags$p("Click on areas below to view names and indicator values.")),
                                                leafletOutput(outputId = 'map1', width = '100%', height = '500'), br(),
                                                ),
                                       tabPanel(title = 'Map 2', value = 'tab2',
                                                tags$div(style="font-size:13px", br(), tags$p("Click on areas below to view names and indicator values.")),
                                                leafletOutput(outputId = 'map2', width = '100%', height = '500'), br(),
                                                ),
                                       tabPanel(title = "Correlation",
                                                tags$div(style="font-size:13px", br(), tags$p("Each census tract in the Charlottesville region is represented with a dot, plotted by the value of the tract on the measures you select on the left (Variable 1) and the right (Variable 2). The size of each dot is based on the population of the tract so that tracts with more people appear larger and the color is based on the locality of the tract. The gray figures on the top and right show how frequently high and low values of the selected variables occur in the region; taller bars mean that range of values is more common.")),
                                                plotlyOutput(outputId = "scatterplot", width = '100%', height = '500'), br()
                                                ),
                                       tabPanel(title = "Differences",
                                                tags$div(style="font-size:13px", br(), tags$p("Each census tract in the selected Charlottesville region is ranked into three groups representing tracts with Low, Middle, or High values on the measure you select on the left (Variable 1). The height of the bar shows the average value of the measure you select on the right (Variable 2) within that group of tracts. Hover over each bar to see the average value on Variable 2.")),
                                                plotlyOutput(outputId = 'tercile_plot'), br()
                                                ) 
                                       ) # end tabsetPanel
                                     ), # end column width 10

                            ) # end fluidRow 2 
                   
                 ) # end tabPanel
  
) # end navbarPage

# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  
  # to make helper() info render
  observe_helpers()
  
  # get map data
    md <- reactive({ all_data %>%
        dplyr::filter(county.nice %in% input$geo &
                        GEO_LEVEL == input$geo_df) %>%
        dplyr::select(x = !!sym(input$indicator1),
                      y = !!sym(input$indicator2),
                      locality, county.nice, tract, GEOID, GEO_LEVEL,
                      pop
                      )
  })
  
  # county selections (select/deselect all)
  observe({
    if (input$selectall_geo == 0) return(NULL)
    else if (input$selectall_geo %% 2 == 0){
      updateCheckboxGroupInput(session, inputId = "geo", "Counties",
                               choices = counties, selected = counties,
                               inline = TRUE)
    } else {
      updateCheckboxGroupInput(session, "geo", "Counties",
                               choices = counties, inline = TRUE)
    }
  })
  
  # geography selections - update selection of indicators based on geo level
  observeEvent(input$geo_df, {
    if (input$geo_df == "Census Tract"){
      updateSelectInput(session, "indicator1", choices = ind_choices_ct,
                        selected = ind_choices_ct$People['Estimated Population'])
      updateSelectInput(session, "indicator2", choices = ind_choices_ct,
                        selected = ind_choices_ct$Housing['Total Housing Units'])} 
    else {
      updateSelectInput(session, "indicator1", choices = ind_choices_county,
                        selected = ind_choices_county$People['Estimated Population'])
      updateSelectInput(session, "indicator2", choices = ind_choices_county,
                        selected = ind_choices_county$Housing['Total Housing Units'])
    } # end else statement 
  }) # end observe event 
  
  ################
  #### MAP 1 
  ################
  
  # reactive function to detect when variable 1, variable 2, or locality selection changes
  listen_closely <- reactive({
    list(input$indicator1,input$indicator2, input$geo)
  }) # end listen closely function for input selection 
  
  # when a variable or locality selection is changed, render the map
  observeEvent(listen_closely(), {
    if (input$indicator1 == input$indicator2 | length(input$geo) == 0) { # if statement 1 
      leafletProxy('map1') %>% clearShapes()
    } # end if statement 1 
    else if (input$indicator1 %in% cant_map) { # if statement 2 
      session$sendCustomMessage(type = 'testmessage', message = cant_map_message)
      leafletProxy('map1') %>% clearShapes() } # end if statement 2 
    else if(input$tabs == "tab1"){ # if statement 3 
      
      # vector of values
      
      if (all(is.na(md()$x))){ # if statement 4 
        showModal(modalDialog(
          title = "Data not available",
          "Data not available for the current Geographic Level or selected Year." ))
      } else { 
        
        output$map1 <- renderLeaflet({
          
          # filter for "Parks", "Schools", "Elem School Zone", "Magisterial Districts"
          counties_geo %>%
            #sf::st_transform(4326) %>%
            leaflet() %>%
            addProviderTiles(input$map_geo) %>%
            addPolygons(color = "grey",
                        weight = 3) %>% 
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
            hideGroup("Magisterial Districts") %>%
            addPolygons(data = md(), fillColor = colorNumeric("viridis", domain = md()$x)(md()$x),
                        fillOpacity = 0.5,
                        color = "white",
                        weight = 2,
                        smoothFactor = 0.2,
                        popup = paste0(attr(md()$x, "goodname"), ": ",
                                       md()$x, "<br>",
                                       md()[["NAME.x"]], "<br>",
                                       md()[["tractnames"]])) %>%
            addLegend(pal = colorNumeric("viridis", domain = md()$x),
                      values = md()$x,
                      position = "topright",
                      opacity = 0.25,
                      title = attr(md()$x, "goodname")) 
          
        }) # end render leaflet map 1  
      } # end else statement 
    } # end if statement 3 
  }) # end observe event for map 1 
  
  ################
  #### MAP 2 
  ################
  
  output$map2 <- renderLeaflet({
    
    # filter for "Parks", "Schools", "Elem School Zone", "Magisterial Districts"
    counties_geo %>%
      leaflet() %>%
      addProviderTiles(input$map_geo) %>%
      addPolygons(color = "grey",
                  weight = 3) %>% 
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
                  highlight = highlightOptions(weight = 3, color = "blue", bringToFront = TRUE)) %>%
      addPolygons(data = filter(mcd_sf), 
                  group="Magisterial Districts",
                  color = "purple", fill = FALSE, weight = 2,
                  popup = ~NAMELSAD,
                  highlight = highlightOptions(weight = 3, color = "purple", bringToFront = TRUE)) %>% 
      addLayersControl(overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magisterial Districts"),
                       options = layersControlOptions(collapsed = FALSE), 
                       position = "bottomright") %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
      hideGroup("Elem School Zone") %>% 
      hideGroup("Magisterial Districts") %>% 
      addPolygons(data = md(), fillColor = colorNumeric("viridis", domain = md()$y)(md()$y),
                  fillOpacity = 0.5,
                  color = "white",
                  weight = 2,
                  smoothFactor = 0.2,
                  popup = paste0(attr(md()$y, "goodname"), ": ",
                                 md()$y, "<br>",
                                 md()[["NAME.x"]], "<br>",
                                 md()[["tractnames"]])) %>%
      addLegend(pal = colorNumeric("viridis", domain = md()$y),
                values = md()$y,
                position = "topright",
                opacity = 0.25,
                title = attr(md()$y, "goodname"))
    
  }) # end render leaflet for map two 
  
  ################
  #### Scatter plot 
  ################
  
  output$scatterplot <- renderPlotly({
    if (md()$x == md()$y | length(input$geo) == 0) {
      plotly_empty()
    } else {
      d <- st_drop_geometry(md())
      xhist <- plot_ly(data = d, x = ~d$x,
                       type = "histogram", nbinsx = 20,
                       alpha =.75, color = I("grey")) %>%
        layout(yaxis = list(showgrid = FALSE,
                            showticklabels = FALSE,
                            fixedrange = T),
               xaxis = list(showticklabels = FALSE,
                            fixedrange = T))
      
      yhist <- plot_ly(data = d, y = ~d$y,
                       type = "histogram", nbinsx = 20,
                       alpha = .75, color = I("grey")) %>%
        layout(xaxis = list(showgrid = FALSE,
                            showticklabels = TRUE,
                            fixedrange = T),
               yaxis = list(showticklabels = FALSE,
                            fixedrange = T))
      
      xyscatter <- plot_ly(data = d, x = ~d$x, y = ~d$y,
                           type = "scatter",
                           mode = 'markers', # to remove mode warning
                           fill = ~'', # to remove line.width error
                           size = ~as.numeric(d$pop),
                           sizes = c(1, 500),
                           color = ~d$county.nice,
                           colors = fewpal,
                           alpha = .75,
                           text = paste0("Locality: ", d$county.nice, "<br>",
                                         "Census tract: ", d$tract, "<br>",
                                         "Population: ", d$pop, "<br>",
                                         attr(d$x, "goodname"), ": ", round(d$x, 2), "<br>",
                                         attr(d$y, "goodname"), ": ", round(d$y, 2), "<br>"),
                           hoverinfo = "text") %>%
        layout(xaxis = list(title = attr(d$x, "goodname"), showticklabels = TRUE, fixedrange = T),
               yaxis = list(title = attr(d$y, "goodname"), showticklabels = TRUE, fixedrange = T),
               legend = list(orientation = "h", x = 0.1, y = 1.2))
      
      # note: in the legend, we hide trace 1 (the xhist) and trace (3 + length(input$locality)), which is the yhist;
      #       the yhist's trace # changes as a user selects different localities to map, but it can be dynamically
      #       referenced as... yhist trace number = 1 (xhist) + 1 (plotly_empty) + n_localities + 1 (to reach the yhist)
      subplot(xhist, plotly_empty(), xyscatter, yhist,
              nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0,
              shareX = TRUE, shareY = TRUE) %>%
        style(showlegend = FALSE, traces = c(1, sum(3 + length(input$geo)))) %>%
        layout(xaxis = list(showgrid = TRUE),
               yaxis2 = list(showgrid = TRUE))
    }
  })
  
  
  
  ################
  #### Tercile plot
  ################
  
  output$tercile_plot <- renderPlotly({
    if (md()$x %in% cant_map | md()$y %in% cant_map | md()$x == md()$y | length(input$geo) == 0) {
      plotly_empty()
    } else {
      to_tercile <- bi_class(md(), x = x, y = y, style = "quantile", dim = 3)
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
  
  ## refresh app ----
  observeEvent(input$refresh, session$reload())

  ## output variable information ----
  
  # by selector
  # indicator 1
  output$ind1_defn <- renderText({
    attr(md()$x, "about")
  })
  
  output$ind1_source <- renderText({
    paste("Source: ", attr(md()$x, "source"))
  })
  
  # indicator 2
  output$ind2_defn <- renderText({
    attr(md()$y, "about")
  })
  
  # indicator 2 description by selector
  output$ind2_source <- renderText({
    paste("Source: ", attr(md()$y, "source"))
  })
  
  # detailed var info on var info tab
  # indicator 1
  output$var1_name <- renderText({
    attr(md()$x, "goodname")
  })
  
  output$var1_abt <- renderText({
    attr(md()$x, "about")
  })
  
  output$var1_source <- renderText({
    paste("Source: ", attr(md()$x, "source"))
  })
  
  # indicator 2
  output$var2_name <- renderText({
    attr(md()$y, "goodname")
  })
  
  output$var2_abt <- renderText({
    attr(md()$y, "about")
  })
  
  output$var2_source <- renderText({
    paste("Source: ", attr(md()$y, "source"))
  })
  

} 

# Run the application 
shinyApp(ui = ui, server = server)
