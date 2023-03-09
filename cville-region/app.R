# Published version
# Cville Region Equity Atlas Dashboard
# Updated 3/8/2023
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
      navbarPage(title = "Charlottesville Regional Equity",
                 collapsible = TRUE,
                 fluid = TRUE,
                 theme = bs_theme(version = 5),
                 tabPanel("DASHBOARD",
                          fluidRow(
                            column(
                              width = 12,
                              cardComponent(
                                accordianComponent("intro", "Dashboard Instructions",
                                                   "Make selections in the boxes below to show demographic, economic and social data on the maps and correlation plot in tabs below.
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
                                          # add select all option - not working well
                              # actionButton(inputId = "selectall_geo", 
                              #             label = "Select/Unselect All"),
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
                                    h2(textOutput("comptitle", inline = TRUE)) %>% 
                                      helper(type = "inline",
                                              title = "Distribution",
                                              icon = "question-circle",
                                              content = helpers$correlation,
                                              size = "m"),
                                    p("Each census tract in the Charlottesville region is represented with a dot, plotted by the value of the tract on the measures you select on the left (Variable 1) and the right (Variable 2). The size of each dot is based on the population of the tract so that tracts with more people appear larger and the color is based on the locality of the tract. The gray figures on the top and right show how frequently high and low values of the selected variables occur in the region; taller bars mean that range of values is more common."),
                                    br(),
                                    plotlyOutput("scatterplot"),
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
      counties_geo %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
        addPolygons(color = "#969997",
                    fill = FALSE,
                    weight = 2,
                    group = 'countyBoundaries') %>% 
        addPolygons(data = cville_geo, color = FALSE,
                    fill = FALSE,
                    weight = 2,
                    group = 'cvilleBoundaries') %>%
        addResetMapButton() %>% 
        addCvilleMapButton()
    }) 
  }
  
  ## leafletProxy Map Function ---- 
  mapProxyFunction <- function(mapData, mapId, fillColor, ind, popupText){
    # map proxy
    proxy <- leafletProxy(mapId, data = mapData)

    # observe
    observe({
      proxy %>%
        clearShapes() %>%
        addPolygons(data = counties_geo, color = "#969997",
                    fill = FALSE,
                    weight = 2,
                    group = 'countyBoundaries') %>% 
        addPolygons(data = cville_geo, color = FALSE,
                    fill = FALSE,
                    weight = 2,
                    group = 'cvilleBoundaries') %>% 
        mapGroupFunction(mapData) %>%
        addPolygons(data = mapData, fillColor = fillColor,
                      fillOpacity = 0.5,
                      color = "#969997",
                      weight = 2,
                      smoothFactor = 0.2,
                      popup = paste0(attr(ind, "goodname"), ": ",
                                     popupText, "<br>",
                                     mapData[["NAME"]], "<br>",
                                     "Tract Name: ", mapData[["tractnames"]]),
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
      showModal(
        modalDialog(
          title = "Data not available",
          "Data not available for the current Geographic Level"
        ))
        # update map1 w/ gray polygons if data not available for geo level
        mapProxyFunction(md(), "map1", "#969997", ind1, "<b>Data not available for the current Geographic Level</b>")
    } else {
      # update map1 with selected indicator
      mapProxyFunction(md(), "map1", colorNumeric(mycolors, domain = ind1)(ind1), ind1, ind1)
    }
  })

  # output title and source for map1
  output$maptitle <- renderText({paste0(attr(md()[[input$indicator1]], "goodname"))})
  output$source <- renderText({attr(md()[[input$indicator1]], "source")})

 
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
      showModal(
        modalDialog(
          title = "Data not available",
          "Data not available for the current Geographic Level"
        ))
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

   output$scatterplot <- renderPlotly({
      d <- st_drop_geometry(md())
      # if (!input$indicator2=="None") {
      xhist <- plot_ly(data = d, 
                       x = ~get(input$indicator1),
                       type = "histogram", nbinsx = 20,
                       alpha =.75, color = I("grey")) %>%
        layout(yaxis = list(showgrid = FALSE,
                            showticklabels = FALSE,
                            fixedrange = T),
               xaxis = list(showticklabels = FALSE,
                            fixedrange = T))
      
      yhist <- plot_ly(data = d, 
                       y = ~get(input$indicator2),
                       type = "histogram", nbinsx = 20,
                       alpha = .75, color = I("grey")) %>%
        layout(xaxis = list(showgrid = FALSE,
                            showticklabels = TRUE,
                            fixedrange = T),
               yaxis = list(showticklabels = FALSE,
                            fixedrange = T))
      
      xyscatter <- plot_ly(data=d,  
                          x = ~get(input$indicator1),
                          y=  ~get(input$indicator2),
                           type = "scatter",
                           mode = "markers",
                           fill = ~"", # to remove line.width error
                           size = ~as.numeric(d$pop),
                           sizes = c(1, 500),
                           color = ~d$county.nice,
                           colors = fewpal,
                           alpha = .75,
                           text = paste0("Locality: ", d$county.nice, "<br>",
                                         "Census tract: ", d$tract, "<br>",
                                         "Tract Name: ", d$tractnames, "<br>",
                                         "Population: ", d$pop, "<br>",
                                         attr(d[[input$indicator1]], "goodname"), ": ", round(as.numeric(d[[input$indicator1]]), 2), "<br>",
                                         attr(d[[input$indicator2]], "goodname"), ": ", round(as.numeric(d[[input$indicator2]]), 2), "<br>"),
                           hoverinfo = "text") %>%
        layout(xaxis = list(title = attr(d[[input$indicator1]], "goodname"), showticklabels = TRUE, fixedrange = T),
               yaxis = list(title = attr(d[[input$indicator2]], "goodname"), showticklabels = TRUE, fixedrange = T),
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
    filename = paste0("data_download.csv"),
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
