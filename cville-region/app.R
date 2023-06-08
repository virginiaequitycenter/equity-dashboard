# Published version
# Cville Region Equity Atlas Dashboard
# Last Updated 6/8/2023
# Last Deployed: 6/8/2023

library(shiny)
library(bslib)
library(shinyhelper)
library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(DT)
library(biscale) # for tercile plots 
library(stringi) # for tercile plots 

source("functions/utils.R")

# Load Data ---------------------------------------------------------------

load("www/app_data.Rdata")

# was not deploying on shinyapps:
# https://stackoverflow.com/questions/61286108/error-in-cpl-transformx-crs-aoi-pipeline-reverse-ogrcreatecoordinatetrans
all_data <- st_transform(all_data, 4326)
all_data$pop <- as.character(all_data$totalpopE)
counties_geo <- st_transform(counties_geo, 4326)
bbox <- st_bbox(counties_geo) %>% as.vector()
cville_geo <- counties_geo %>% filter(NAME == "Charlottesville")

fewpal <- c("#7DC462", "#0D95D0", "#E72F52", "#774FA0", "#EFB743", "#D44627")
# fewpal <- c("#003f5c", "#444e86", "#955196", "#dd5182", "#ff6e54", "#ffa600")
# fewpal <- c( "#265dab", "#df5c24", "#c7b42e", "#059748", "#cb2027", "#9d722a")

# Define UI ---------------------------------------------------------------

ui <- htmlTemplate(filename = "cville-atlas-template.html", main =
        fluidPage(
          title = "Charlottesville Regional Equity",
          theme = bs_theme(version = 5),
          lang = "en",
                 # tabPanel("DASHBOARD",
                          fluidRow(
                            column(
                              width = 4,
                              h1("Regional Equity Dashboard"),
                              cardComponent(
                                accordianComponent("intro", "Dashboard Instructions",
                                                   "Make selections in the boxes below to show demographic, economic and social data on the maps and correlation plot in tabs below.
                                                    Variables include data related to Health, Housing, People, Youth & Education, Jobs, Wages & Income, and various Indices.",
                                                   "intro-1", "intro-2")
                              ),
                                cardComponentSelectGeo(
                                  selectInput("indicator1",
                                    "First Equity Indicator:",
                                    choices = ind_choices,
                                    selected = ind_choices$People['Estimated Population (All Levels)']) %>% 
                                  helper(type = "inline",
                                      title = "First Equity Indicator",
                                      icon = "question-circle",
                                      content = helpers$indicator,
                                      size = "m"),
                                  textOutput("ind_geo1", inline = TRUE),
                                  accordianComponent("ind1", "Show Selected Indicator Definition", textOutput("ind1_abt", inline = TRUE),"var-def-1", "map-ind-1")
                                ),
                              cardComponentSelectGeo(
                                selectInput("indicator2",
                                  "Second Equity Indicator:",
                                  choices = ind_choices,
                                  selected = ind_choices$Housing['Total Housing Units (All Levels)']) %>% 
                                helper(type = "inline",
                                    title = "Second Equity Indicator",
                                    icon = "question-circle",
                                    content = helpers$indicator2,
                                    size = "m"),
                                textOutput("ind_geo2", inline = TRUE),
                                accordianComponent("ind2", "Show Selected Indicator Definition", textOutput("ind2_abt", inline = TRUE),"var-def-2", "map-ind-2")
                              ),
                              cardComponentSelect(
                              checkboxGroupInput(
                                  inputId = "geo",
                                  label = "Localities:",
                                  choices = counties,
                                  selected = counties,
                                  inline = TRUE) %>%
                                  helper(type = "inline",
                                          title = "Localities",
                                          icon = "question-circle",
                                          content = helpers$counties,
                                          size = "m"),
                              # add select all option - not working well
                              # actionButton(inputId = "selectall_geo", 
                              #             label = "Select/Unselect All"),
                              radioButtons(inputId = "geo_df",
                                  label = "Geographic Level:",
                                  choices = c("County", "Census Tract", "Block Group"),
                                  selected = "Census Tract",
                                  inline = TRUE) %>%
                                  helper(type = "inline",
                                            title = "Geographic Level",
                                            icon = "question-circle",
                                            content = helpers$geo,
                                            size = "m")
                              )
                            ), # end column width=4
                            column(
                              width = 8,
                              tabsetPanel(
                                id = "tabs",
                                # type = 'pills',
                                tabPanel(
                                    "First Indicator Map",
                                    icon = icon('map'),
                                    value = "tab1",
                                    h2(textOutput("ind1_name", inline = TRUE)),
                                    p("Click on areas below to view names and indicator values."),
                                    leafletOutput("map1", height=600),
                                    # br(),
                                    textOutput("source")
                                  ),
                                tabPanel("Second Indicator Map",
                                         icon = icon('map'),
                                    value = "tab2",
                                    h2(textOutput("ind2_name", inline = TRUE)),
                                    p("Click on areas below to view names and indicator values."),
                                    leafletOutput("map2", height=600),
                                    # br(),
                                    textOutput("source2")
                                  ),
                                # tabPanel("Differences",
                                #          icon = icon('chart-simple'),
                                #          h2(textOutput("terctitle", inline = TRUE)),
                                #          p("Each census tract in the selected Charlottesville region is ranked into three groups representing tracts with Low, Middle, or High values on the measure you select in the first indicator. The height of the bar shows the average value of the measure you select on the second indicator within that group of tracts. Hover over each bar to see the average value on the second indicator."),
                                #          br(),
                                #          plotlyOutput("tercile_plot"),
                                #          br(),
                                #          textOutput("source3")
                                # ),
                                tabPanel("Relationship",
                                         icon = icon('chart-line'),
                                    h2(textOutput("comptitle", inline = TRUE)) %>% 
                                      helper(type = "inline",
                                              title = "Relationship of Equity Indicators",
                                              icon = "question-circle",
                                              content = helpers$correlation,
                                              size = "m"),
                                    p("Each circle represents a county, census tract, or block group (depending on the selected geographic level), plotted by the values of the two selected Equity Indicators. The size of each circle is based on the estimated population of the county or census tract. The gray figures on the top and right show how frequently high and low values of the selected variables occur in the region; taller bars mean that range of values is more common.")%>% 
                                        tagAppendAttributes(class = 'small-text'),
                                    br(),
                                    plotlyOutput("scatterplot"),
                                    br(),
                                    textOutput("source_c")
                                  )
                              ) # end tabsetPanel
                            ) # end column width=8
                          ), br(), hr(),# end fluidRow
                          fluidRow(
                            column(
                              width = 12,
                              h2("Download Data"),
                              p("Data in this Atlas is provided as a compressed folder, which includes CSVs for data at the county, census tract, and block group levels and a data dictionary."),
                              downloadButton("downloadBtn", "Download")
                              # br(), hr(), br()
                            )
                          ) # end fluidRow
                 # ) # end tabPanel
)) # end fluidPage / end HTML template

# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  
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

  # county selections (select/deselect all) --- not working well
  # observe({
  #   if (input$selectall_geo == 0){
  #     return(NULL)
  #   } 
  #   else if (input$selectall_geo %% 2 == 0){
  #     updateCheckboxGroupInput(session, inputId = "geo", "Counties",
  #                              choices = counties, selected = counties,
  #                              inline = TRUE)
  #   } else {
  #     updateCheckboxGroupInput(session, "geo", "Counties",
  #                              choices = counties, inline = TRUE)
  #   }
  # })

# Map Functions -------------------------------------------------------
  ## Leaflet base map function ----
  renderLeafletFunction <- function(map) {
    renderLeaflet({
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
        addMapPane('countyBoundaries', zIndex = 410) %>%
        addMapPane('cvilleBoundaries', zIndex = 420) %>%
        addMapPane('parks', zIndex = 440) %>%
        addMapPane('schools', zIndex = 450) %>%
        addMapPane('elemSchoolZone', zIndex = 460) %>%
        addMapPane('magisterialDistricts', zIndex = 470) %>%
        addPolygons(data= counties_geo, color = "#969997",
                    fill = FALSE,
                    weight = 2,
                    group = 'countyBoundaries',
                    options = pathOptions(pane = 'countyBoundaries')) %>%
        addPolygons(data = cville_geo, color = FALSE,
                    fill = FALSE,
                    weight = 2,
                    group = 'cvilleBoundaries',
                    options = pathOptions(pane = 'cvilleBoundaries')) %>%
        addResetMapButton() %>% 
        addCvilleMapButton() %>%
        addCircles(data = st_collection_extract(parks_sf, "POINT"), color = "green",
                   group="Parks",
                   popup = ~ParkName,
                   options = pathOptions(pane = 'parks')) %>%
        addPolygons(data = st_collection_extract(parks_sf, "POLYGON"), color = "green",
                    group="Parks",
                    popup = ~ParkName,
                    options = pathOptions(pane = 'parks')) %>%
        addCircles(data =  filter(schools_sf),
                   group="Schools",
                   popup = ~NAME,
                   options = pathOptions(pane = 'schools')) %>%
        addPolygons(data = filter(sabselem_sf),
                    group="Elem School Zone",
                    color = "blue", fill = FALSE, weight = 2,
                    popup = ~schnam,
                    highlight = highlightOptions(weight = 3,
                                                 color = "blue",
                                                 bringToFront = TRUE),
                    options = pathOptions(pane = 'elemSchoolZone')) %>%
        addPolygons(data = filter(mcd_sf),
                    group="Magisterial Districts",
                    color = "purple", fill = FALSE, weight = 2,
                    popup = ~NAMELSAD,
                    highlight = highlightOptions(weight = 3,
                                                 color = "purple",
                                                 bringToFront = TRUE),
                    options = pathOptions(pane = 'magisterialDistricts')) %>%
        addLayersControl(overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magisterial Districts"),
                         options = layersControlOptions(collapsed = FALSE),
                         position = "bottomright") %>%
        hideGroup("Parks") %>%
        hideGroup("Schools") %>%
        hideGroup("Elem School Zone") %>%
        hideGroup("Magisterial Districts")
    }) 
  }
  
  ## leafletProxy Map Function ---- 
  mapProxyFunction <- function(mapData, mapId, fillColor, ind, popupText){
    
    # popup content
    popupContent <- if (input$geo_df == "County"){
      paste0(attr(ind, "goodname"), ": ",
             popupText, "<br>",
             mapData[["NAME"]])
    } else {
      paste0(attr(ind, "goodname"), ": ",
             popupText, "<br>",
             mapData[["NAME"]], "<br>",
             "Tract Name: ", mapData[["tractnames"]])
    }

    # map proxy
    proxy <- leafletProxy(mapId, data = mapData)

    # observe
    observe({
      proxy %>%
        clearGroup('indicatorSelection') %>% 
        addPolygons(data = mapData, fillColor = fillColor,
                      fillOpacity = 0.5,
                      color = "#969997",
                      weight = 2,
                      smoothFactor = 0.2,
                      popup = popupContent,
                      highlight = highlightOptions(
                        weight = 5,
                        fillOpacity = 0.7,
                        bringToFront = F),
                    group = 'indicatorSelection') %>%
        # mapGroupFunction(mapData) %>%
        clearControls() %>%
        addLegend(pal = colorNumeric(mycolors, domain = ind),
                    values = ind,
                    position = "topright",
                    opacity = 0.25,
                    title = attr(ind, "goodname"))
    })
  }
  
  ## Parks/Schools/District Map Components Function NOT USING THIS ATM 6/7---- 
  # mapGroupFunction <- function(map, mapData){
  #   addPolygons(map, data = mapData, color = "#969997",
  #               fill = FALSE,
  #               weight = 2) %>%
  #     addCircles(data = st_collection_extract(parks_sf, "POINT"), color = "green",
  #                group="Parks",
  #                popup = ~ParkName) %>% 
  #     addPolygons(data = st_collection_extract(parks_sf, "POLYGON"), color = "green",
  #                 group="Parks",
  #                 popup = ~ParkName) %>%
  #     addCircles(data =  filter(schools_sf),
  #                group="Schools",
  #                popup = ~NAME) %>%
  #     addPolygons(data = filter(sabselem_sf), 
  #                 group="Elem School Zone",
  #                 color = "blue", fill = FALSE, weight = 2,
  #                 popup = ~schnam,
  #                 highlight = highlightOptions(weight = 3,
  #                                              color = "blue",
  #                                              bringToFront = TRUE)) %>%
  #     addPolygons(data = filter(mcd_sf), 
  #                 group="Magisterial Districts",
  #                 color = "purple", fill = FALSE, weight = 2,
  #                 popup = ~NAMELSAD,
  #                 highlight = highlightOptions(weight = 3,
  #                                              color = "purple",
  #                                              bringToFront = TRUE)) %>% 
  #     addLayersControl(overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magisterial Districts"),
  #                      options = layersControlOptions(collapsed = FALSE), 
  #                      position = "bottomright") %>% 
  #     hideGroup("Parks") %>% 
  #     hideGroup("Schools") %>% 
  #     hideGroup("Elem School Zone") %>% 
  #     hideGroup("Magisterial Districts") 
  # }
  
  ## Add Map Reset button function ----
  addResetMapButton <- function(leaf) {
    leaf %>%
      addEasyButton(
        easyButton(
          icon = "ion-arrow-expand",
          title = "Reset View", 
          onClick = JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }")
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
  
  ## Add zoom to Cville button function ----
  addCvilleMapButton <- function(leaf) {
    leaf %>%
      addEasyButton(
        easyButton(id = "buttonid",
                   icon = "fa-crosshairs", title = "Zoom to Charlottesville",
                   onClick = JS("function(btn, map) { 
                                var groupLayer = map.layerManager.getLayerGroup('cvilleBoundaries');
                                map.fitBounds(groupLayer.getBounds()); 
                                }")
        ))
  }

# Build Map 1 -------------------------------------------------------

  # render leaflet map1
  output$map1 <- renderLeafletFunction()

  # update map1 based on selected indicator (listen_indicator1 reactive)
  observeEvent(listen_indicator1(), {
    # get selected indicator column from map data
    ind1 <- md() %>% 
      filter(!is.na(.data[[input$indicator1]])) %>% 
      pull(input$indicator1)

    if (all(is.na(md()[[input$indicator1]]))){
      # update map w/ gray polygons if data not available for geo level
      mapProxyFunction(md(), "map1", "#969997", ind1, "<b>Data not available for the current Geographic Level</b>")
    } else {
      # update map1 with selected indicator
      mapProxyFunction(md(), "map1", colorNumeric(mycolors, domain = ind1)(ind1), ind1, ind1)
    }
  })

  # output title and source for map1
  output$maptitle <- renderText({paste0(attr(md()[[input$indicator1]], "goodname"))})
  output$source <- renderText({paste0("Source: ", attr(md()[[input$indicator1]], "source"))})
 
# Build Map 2 -------------------------------------------------------
 
  # render leaflet map2
  output$map2 <- renderLeafletFunction()

  # render leaflet map on hidden (second) tab
  outputOptions(output, "map2", suspendWhenHidden = FALSE)
  
  # update map2 based on selected indicator (listen_indicator2 reactive)
  observeEvent(listen_indicator2(), {
    # get selected indicator column from map data
    ind2 <- md() %>%
      filter(!is.na(.data[[input$indicator2]])) %>%
      pull(input$indicator2)

    if (all(is.na(md()[[input$indicator2]]))){
      # update map w/ gray polygons if data not available for geo level
      mapProxyFunction(md(), "map2", "#969997", ind2, "<b>Data not available for the current Geographic Level</b>")
    } else {
      # update map2 with selected indicator
      mapProxyFunction(md(), "map2", colorNumeric(mycolors, domain = ind2)(ind2), ind2, ind2)

    }
  })
  
  # output title and source for map2
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

  output$scatterplot <- renderPlotly({ # add loess line to this?
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
                        c(300,3000)
                      } else {
                        c(50, 600)
                      },
              marker = list(line = list(color = 'rgba(0, 0, 0, .4)',
                                        width = 1)),
              colors = fewpal,
              alpha = .75,
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

#replaced with above for now, was just easier (6/7 update)
  #  output$scatterplot <- renderPlotly({
  #     d <- st_drop_geometry(md())
  #     # if (!input$indicator2=="None") {
  #     xhist <- plot_ly(data = d, 
  #                      x = ~get(input$indicator1),
  #                      type = "histogram", nbinsx = 20,
  #                      alpha =.75, color = I("grey")) %>%
  #       layout(yaxis = list(showgrid = FALSE,
  #                           showticklabels = FALSE,
  #                           fixedrange = T),
  #              xaxis = list(showticklabels = FALSE,
  #                           fixedrange = T))
      
  #     yhist <- plot_ly(data = d, 
  #                      y = ~get(input$indicator2),
  #                      type = "histogram", nbinsx = 20,
  #                      alpha = .75, color = I("grey")) %>%
  #       layout(xaxis = list(showgrid = FALSE,
  #                           showticklabels = TRUE,
  #                           fixedrange = T),
  #              yaxis = list(showticklabels = FALSE,
  #                           fixedrange = T))
      
  #     xyscatter <- plot_ly(data=d,  
  #                         x = ~get(input$indicator1),
  #                         y=  ~get(input$indicator2),
  #                          type = "scatter",
  #                          mode = "markers",
  #                          fill = ~"", # to remove line.width error
  #                          size = ~as.numeric(d$pop),
  #                          sizes = c(1, 500),
  #                          color = ~d$county.nice,
  #                          colors = fewpal,
  #                          alpha = .75,
  #                          text = paste0("Locality: ", d$county.nice, "<br>",
  #                                        "Census tract: ", d$tract, "<br>",
  #                                        "Tract Name: ", d$tractnames, "<br>",
  #                                        "Population: ", d$pop, "<br>",
  #                                        attr(d[[input$indicator1]], "goodname"), ": ", round(as.numeric(d[[input$indicator1]]), 2), "<br>",
  #                                        attr(d[[input$indicator2]], "goodname"), ": ", round(as.numeric(d[[input$indicator2]]), 2), "<br>"),
  #                          hoverinfo = "text") %>%
  #       layout(xaxis = list(title = attr(d[[input$indicator1]], "goodname"), showticklabels = TRUE, fixedrange = T),
  #              yaxis = list(title = attr(d[[input$indicator2]], "goodname"), showticklabels = TRUE, fixedrange = T),
  #              legend = list(orientation = "h", x = 0.1, y = 1.2))
      
  #     # note: in the legend, we hide trace 1 (the xhist) and trace (3 + length(input$locality)), which is the yhist;
  #     #       the yhist's trace # changes as a user selects different localities to map, but it can be dynamically
  #     #       referenced as... yhist trace number = 1 (xhist) + 1 (plotly_empty) + n_localities + 1 (to reach the yhist)
  #     subplot(xhist, plotly_empty(), xyscatter, yhist,
  #             nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0,
  #             shareX = TRUE, shareY = TRUE) %>%
  #       style(showlegend = FALSE, traces = c(1, sum(3 + length(input$geo)))) %>%
  #       layout(xaxis = list(showgrid = TRUE),
  #              yaxis2 = list(showgrid = TRUE))
  # })
  
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
  
  # Build Tercile plots -------------------------------------------------------
  # removed from app for now - change to median plots 

  # output$terctitle <- renderText({
  #   if (input$indicator2=="None") {
  #     paste("You have not selected a second indicator. To compare, select a second indicator from the control panel.")
  #   } else { 
  #     paste(attr(md()[[input$indicator2]], "goodname"), "averages by ", 
  #           attr(md()[[input$indicator1]], "goodname"), "rank")
  #   }
  # })
  # 
  # output$tercile_plot <- renderPlotly({
  #   td <- md() %>% mutate(x = .data[[input$indicator1]], y = .data[[input$indicator2]])
  #     to_tercile <- bi_class(td, x = x, y = y, style = "quantile", dim = 3)
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
  # })
  # 
  # # output title and source for tercile plots
  # output$differencestitle <- renderText({
  #   if (input$indicator2 != "None") paste0(attr(md()[[input$indicator2]], "goodname")) })
  # output$source3 <- renderText({
  #   if (input$indicator2 != "None") attr(md()[[input$indicator2]], "source")})
  
# Build Data Table -------------------------------------------------------
  
  # ## output data table ----
  # output$tbl <-  renderDT({
  #   datatable(st_drop_geometry(md()),
  #             options = list(scrollX = TRUE))
  # })
  
  # ## data table title ----
  # output$tbltitle <- renderText({
  #   paste("Data by", input$geo_df)
  # })
  
  # ## data download button function ----
  # output$downloadBtn <- downloadHandler(
  #   filename = paste0("data_download.csv"),
  #   content = function(file) {
  #     write.csv(st_drop_geometry(md()), file)
  #   }
  # )

  # ## data download title ----
  # output$dltitle <- renderText({
  #   paste0("Download Table Data (Current Geographic Level: ", input$geo_df, ")")
  # })

# Data Download -------------------------------------------------------
  
  ## data download button function ----
  output$downloadBtn <- downloadHandler(
    filename = "data_download.zip",
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("regional_atlas_county.csv", "regional_atlas_census_tract.csv", "regional_atlas_block_groups.csv", "variable_dictionary.csv")
      write.csv(all_data %>% filter(GEO_LEVEL == "County" &
                                      year == "2021") %>% st_drop_geometry(), 
                file = "regional_atlas_county.csv", sep =",")
      write.csv(all_data %>% filter(GEO_LEVEL == "Census Tract" &
                                      year == "2021") %>% st_drop_geometry(), 
                file = "regional_atlas_census_tract.csv", sep =",")
      write.csv(all_data %>% filter(GEO_LEVEL == "Block Group" &
                                      year == "2021") %>% st_drop_geometry(), 
                file = "regional_atlas_block_groups.csv", sep =",")
      write.csv(data_dict, file = "variable_dictionary.csv", sep = ",")
      print (fs)
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )

}

# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)


