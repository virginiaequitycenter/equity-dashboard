# Published version
# Equity Indicators
# Last updated/deployed: 2023-2-16 ll

library(shiny)
library(shinydashboard)
library(shinyhelper)
library(shinyWidgets) # for sliderTextInput()
library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(DT)
library(RColorBrewer)
library(biscale)
library(stringi)

# plotly version '4.9.2.1' causes dplyr to issue warning:
# Warning: `arrange_()` is deprecated as of dplyr 0.7.0.
# Please use `arrange()` instead.

# Load Data ---------------------------------------------------------------

load("www_2023/app_data_2022.Rdata")

# was not deploying on shinyapps:
# https://stackoverflow.com/questions/61286108/error-in-cpl-transformx-crs-aoi-pipeline-reverse-ogrcreatecoordinatetrans
all_data <- st_transform(all_data, 4326)
counties_geo <- st_transform(counties_geo, 4326)

fewpal <- c("#265dab", "#df5c24", "#c7b42e", "#059748",
                     "#cb2027", "#9d722a")

# no-go variables for mapping
cant_map <- c('indigE', 'othraceE', 'bbmax_up', 'HWAV_AFREQ', 'RFLD_AFREQ')
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
    "Charlottesville Regional Equity Atlas prototype"
  ),
  
  ## indicator selectors and plots ----
  tabPanel("Atlas",
           # global instructions
           tags$div(style='font-size:15px; font-weight:bold;', tags$p("Select any two measures from the dropdown menus on the left and right side of the screen to see the relationship between the measures in census tracts across the Greater Charlottesville Region.")),
           
           fluidRow(
             # Sidebar for indicator 1
             column(2,
                    selectInput("indicator1",
                                "Primary Indicator:",
                                choices = ind_choices_county,
                                selected = ind_choices_county$People['Estimated Population']),
                    # variable definitions
                    span(textOutput("ind1_defn"), style='font-size:13px'),
                    tags$br(),
                    span(textOutput("ind1_source"), style='font-size:13px'),
             ),
             
             # Place figures
             column(8,
                    tabsetPanel(id = "tabs",
                                tabPanel(title = 'Map 1', value = 'tab1',
                                         tags$div(style="font-size:13px", tags$p("Click on areas below to view names and indicator values.")),
                                         leafletOutput(outputId = 'map1', width = '100%', height = '450')
                                ),
                                tabPanel(title = 'Map 2', value = 'tab2',
                                         tags$div(style="font-size:13px", tags$p("Click on areas below to view names and indicator values.")),
                                         leafletOutput(outputId = 'map2', width = '100%', height = '450')
                                ),
                                tabPanel(title = "Correlation",
                                         tags$div(style="font-size:13px", tags$p("Each census tract in the Charlottesville region is represented with a dot, plotted by the value of the tract on the measures you select on the left (Variable 1) and the right (Variable 2). The size of each dot is based on the population of the tract so that tracts with more people appear larger and the color is based on the locality of the tract. The gray figures on the top and right show how frequently high and low values of the selected variables occur in the region; taller bars mean that range of values is more common.")),
                                         plotlyOutput(outputId = "scatterplot", width = '100%', height = '450')
                                ),
                                tabPanel(title = "Differences",
                                         tags$div(style="font-size:13px", tags$p("Each census tract in the selected Charlottesville region is ranked into three groups representing tracts with Low, Middle, or High values on the measure you select on the left (Variable 1). The height of the bar shows the average value of the measure you select on the right (Variable 2) within that group of tracts. Hover over each bar to see the average value on Variable 2.")),
                                         plotlyOutput(outputId = 'tercile_plot')
                                ),
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
                    selectInput("indicator2",
                                "Secondary Indicator:",
                                choices = ind_choices_county,
                                selected = ind_choices_county$Housing['Total Housing Units']),
                    # variable definitions
                    span(textOutput("ind2_defn"), style='font-size:13px'),
                    tags$br(),
                    span(textOutput("ind2_source"), style='font-size:13px')
             )
           ),
           
           tags$hr(),
           
           # Select a base map
           radioButtons(inputId = "map_geo",
                        label = "Select a Base Map:",
                        choices = c("Minimal" = "CartoDB.Positron",
                                    "Detailed" = "OpenStreetMap.Mapnik"),
                        inline = TRUE),
           
           ## county/map/geography selector ----
           fluidRow(
             
             # refresh button
             column(3,
                    actionButton(inputId = "refresh", "Refresh Atlas")
             ),
             
             # pick counties
             checkboxGroupInput(
               inputId = "geo", 
               label = "Localities", 
               choices = counties,
               selected = counties,
               inline = TRUE) %>% # checkboxGroupInput ends, pipe to helper()
               helper(type = "inline",
                      icon = "info-circle",
                      content = helpers$counties,
                      size = "m"),
             
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
                          inline = TRUE) %>% # radioButtons ends, pipe to helper()
               helper(type = "inline",
                      title = "Geographic Level",
                      icon = "info-circle",
                      content = helpers$geo,
                      size = "m"), 
             
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
           #includeHTML("www_2023/about_work.html")
           htmltools::tags$iframe(src = 'about_work.html',
                                  style='width:100vw;height:100vh;',
                                  frameBorder = "0")
  ),
  
  singleton(tags$head(tags$script(src = "message-handler.js")))
  ))


# ....................................
# Define Server Logic ----
server <- function(input, output, session) {
  
  # to make helper() info render
  observe_helpers()
  
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
  observeEvent(input$geo_df, {if (input$geo_df == "Census Tract"){
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
  
  # get map data
  md <- reactive({
    all_data %>%
      dplyr::filter(county.nice %in% input$geo &
                      GEO_LEVEL == input$geo_df) %>%
      dplyr::select(x = !!sym(input$indicator1),
                    y = !!sym(input$indicator2), totalpopE,
                    locality, county.nice, tract, GEOID,
                    NAME.x)
  })
  
  ################
  #### MAP 1 
  ################
  # reactive function to detect when variable 1, variable 2, or locality selection changes
  listen_closely <- reactive({
    list(input$indicator1,input$indicator2, input$geo)
  })
  
  # when a variable or locality selection is changed, render the map
  observeEvent(listen_closely(), {
    if (input$indicator1 == input$indicator2 | length(input$geo) == 0) {
      leafletProxy('map1') %>% clearShapes()
    } else if (input$indicator1 %in% cant_map) {
      session$sendCustomMessage(type = 'testmessage', message = cant_map_message)
      leafletProxy('map1') %>% clearShapes()
      # if (input$tabs == "tab2") {
      #   leafletProxy('map1') %>% clearShapes()
      # }
    } else if(input$tabs == "tab1"){
      
      # vector of values
      
      if (all(is.na(md()$x))){
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
      addLayersControl(
        overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magisterial Districts"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "bottomright"
      ) %>% 
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
                                       md()[["tractnames"]]),
                        # layerId = "map1layer"
            ) %>%
      addLegend(pal = colorNumeric("viridis", domain = md()$x),
                      values = md()$x,
                      position = "topright",
                      opacity = 0.25,
                      title = attr(md()$x, "goodname")) 
    
        }) 
      }
    }
    })
  
  ################
  #### MAP 2 
  ################
        
  # when a variable or locality selection is changed, render the map 
  # observeEvent(listen_closely(), {
  #   if (length(input$geo) == 0) {
  #     leafletProxy('map2') %>% clearShapes()
  #     } else if (input$indicator2 %in% cant_map) {
  #       session$sendCustomMessage(type = 'testmessage', message = cant_map_message)
  #       leafletProxy('map2') %>% clearShapes()
  #       # if (input$tabs == "tab1") {
  #       #   leafletProxy('map2') %>% clearShapes()
  #         # }
  #       } else if(input$tabs == "tab2"){
  #         if (all(is.na(md()$y))){
  #           showModal(modalDialog(
  #             title = "Data not available",
  #             "Data not available for the current Geographic Level or selected Year." ))
  #           } else {
  # ^ when all of the above are included, the map on the second tab does not render unless a new 
  # indicator variable is selected (even though the current default will render if you switch back to it) 
  # this page may have some relevant information as to why: https://github.com/rstudio/leaflet/issues/590
  # However, as far as I can tell, the app works as desired without all of the above anywway, so I'm not sure it's
  # necessary for both maps (the same chunk is above the first map)

              output$map2 <- renderLeaflet({
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
                  highlight = highlightOptions(weight = 3, color = "blue", bringToFront = TRUE)) %>%
      addPolygons(data = filter(mcd_sf), 
                  group="Magisterial Districts",
                  color = "purple", fill = FALSE, weight = 2,
                  popup = ~NAMELSAD,
                  highlight = highlightOptions(weight = 3, color = "purple", bringToFront = TRUE)) %>% 
      addLayersControl(
        overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magisterial Districts"),
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
              })
            # }
    #         }
    # })

  
  ## output scatterplot ----
  output$scatterplot <- renderPlotly({
    if (md()$x == md()$y | length(input$geo) == 0) {
      plotly_empty()
    } else {
      d <- st_drop_geometry(md())
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
                           # size = ~totalpopE,
                           sizes = c(1, 500),
                           color = ~county.nice,
                           colors = fewpal,
                           alpha = .75,
                           text = paste0("Locality: ", d$county.nice, "<br>",
                                         "Census tract: ", d$tract, "<br>",
                                         # "Population: ", d$totalpopE, "<br>",
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
        style(showlegend = FALSE, traces = c(1, sum(3 + length(input$geo)))) %>%
        layout(xaxis = list(showgrid = TRUE),
               yaxis2 = list(showgrid = TRUE))
    }
  })
  
  ## output tercile plot ----
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
  
  ## output variable information ----
  
  # by selector
  # indicator 1
  output$ind1_defn <- renderText({
    attr(md()$x, "goodname")
  })
  
  output$ind1_source <- renderText({
    paste("Source: ", attr(md()$x, "source"))
  })
  
  # indicator 2
  output$ind2_defn <- renderText({
    attr(md()$y, "goodname")
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
  
  ## about page ----
  # output$documentation <- renderUI(htmltools::includeHTML("cville_climate_update.html"))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
