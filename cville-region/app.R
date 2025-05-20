# Published version
# Cville Region Equity Atlas Dashboard
# Last updated: 5/20/2025
  # fixed download csv -removed year filter
# update: 6/14/2024
  # Removed year argument from md() - only one year should be present in all_data
  # Update all dashboard styling to bslib defaults, to avoid future conflicts with bootstrap updates
  # Moved data prep to combine_data.R
# Last Deployed: 5/20/2025
  # Deployed to both cville-region and cville_equity_atlas

library(shiny)
library(bslib)
library(bsicons)
library(shinyhelper)
library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(DT)
library(biscale) # for tercile plots 
library(stringi) # for tercile plots 


# Load Data ---------------------------------------------------------------

load("www/app_data.Rdata")

# Define UI ---------------------------------------------------------------
ui <-  
  htmlTemplate(filename = "cville-atlas-template.html", main =  # Removed CSS/footer due to issues bootstrap update
    page_sidebar(
          title = div(h1("Charlottesville Regional Equity Dashboard") %>% 
                        tagAppendAttributes(class = 'bslib-page-title navbar-brand text-wrap fw-light'),
                      actionLink("infoModal", label = "", icon = icon("circle-info")) %>%
                        tagAppendAttributes(class = 'bslib-page-title fs-4', style = "position: absolute;top: 0.5rem;right: 1rem;")),
          fillable = FALSE,
          window_title = "Charlottesville Regional Equity Dashboard",
          lang = "en",
          theme = bs_theme(version = 5) %>%
            bs_add_rules(
              list(".navbar { background-color: #f8f9fa !important; }")
            ),
          sidebar = sidebar(
            bg = "white",
            width = "28%",
            selectInput("indicator1",
                        label = "First Equity Indicator:",
                        choices = ind_choices,
                        selected = ind_choices$People['Estimated Population (All Levels)']) %>% 
              helper(type = "inline",
                     title = "First Equity Indicator",
                     icon = "question-circle",
                     content = helpers$indicator,
                     size = "m"),
            textOutput("ind_geo1", inline = TRUE) %>% 
              tagAppendAttributes(class = "small"),
            accordion(
              open = FALSE,
              accordion_panel(
                title = "Show Selected Indicator Definition & Source",
                textOutput("ind1_abt", inline = TRUE), 
                textOutput("ind1_source", inline = TRUE)
              )
            ),
            selectInput("indicator2",
                        label = "Second Equity Indicator:",
                        choices = ind_choices,
                        selected = ind_choices$Housing['Total Housing Units (All Levels)']) %>% 
              helper(type = "inline",
                     title = "Second Equity Indicator",
                     icon = "question-circle",
                     content = helpers$indicator2,
                     size = "m"),
            textOutput("ind_geo2", inline = TRUE) %>% 
              tagAppendAttributes(class = "small"),
            accordion(
              open = FALSE,
              accordion_panel(
                "Show Selected Indicator Definition & Source",
                textOutput("ind2_abt", inline = TRUE), 
                textOutput("ind2_source", inline = TRUE)
              )
            ),
            radioButtons(
              inputId = "geo_df",
              label = "Geographic Level:",
              choices = c("County", "Census Tract", "Block Group"),
              selected = "Census Tract",
              inline = TRUE) %>% 
              helper(type = "inline",
                     title = "Geographic Level",
                     icon = "question-circle",
                     content = helpers$geo,
                     size = "m"),
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
            actionButton(
              inputId = "selectall_geo", 
              label = "Select/Unselect All")
          ), # end sidebar
          navset_pill(
            nav_panel(
              title = "First Indicator Map",
              icon = icon('map'),
              value = "tab1",
              br(), h2(textOutput("ind1_name", inline = TRUE)),
              accordion(
                open = FALSE,
                accordion_panel(
                  "Map Instructions",
                  HTML("<p>The map below shows values for the <strong>First Equity Indicator</strong>. Click on areas below to view names and indicator values.</p>
                  <p>Zoom in to see specific areas more closely. Click the reset button on the map to return the view to the full region. Click the \'Zoom to Charlottesville\' button to center the map on the city of Charlottesville.</p>")
                )
              ),
              br(),
              leafletOutput(outputId = "map1", width = '100%', height = 650)
            ),
            nav_panel(
              title = "Second Indicator Map",
              icon = icon('map'),
              value = "tab2",
              br(), h2(textOutput("ind2_name", inline = TRUE)),
              accordion(
                open = FALSE,
                accordion_panel(
                  "Map Instructions",
                  HTML("<p>The map below shows values for the <strong>Second Equity Indicator</strong>. Click on areas below to view names and indicator values.</p>
                  <p>Zoom in to see specific areas more closely. Click the reset button on the map to return the view to the full region. Click the \'Zoom to Charlottesville\' button to center the map on the city of Charlottesville.</p>")
                )
              ), br(),
              leafletOutput(outputId = "map2", width = '100%', height = 650)
            ),
            nav_panel(
              title = "Differences",
              icon = icon('chart-simple'),
              value = "tab3",
              br(), h2(textOutput("terctitle", inline = TRUE)),
              accordion(
                open = FALSE,
                accordion_panel(
                  "Tercile Plot Instructions",
                  HTML(
                    "<p>This plot divides the selected geographic level (counties, census tracts, or block groups) in the selected localities into three groups, representing the Low, Middle, and High values of the measure selected for the First Equity Indicator.</p>
                    <p>The height of the bar shows the average value of the measure selected for Second Equity Indicator within that group of counties, tracts, or blocks. Hover over each bar to see the average value of the Second Equity Indicator.</p>
                    <p>For Example, when Estimated Population (First Indicator) and Total Housing Units (Second Indicator) are selected with the Geographic Level set to Census Tracts, this tercile plot shows that the tracts with the Lowest populations have an average value of Total Housing Units of 1,365, and the tracts with the Highest populations have an average value of Total Housing Units of 2,257.</p>"
                  )
                )
              ), br(),
              plotlyOutput(outputId = "tercile_plot", width = '100%', height = 500)
            ),
            nav_panel(
              title = "Relationship",
              icon = icon('chart-line'),
              value = "tab4",
              br(), h2(textOutput("comptitle", inline = TRUE)),
              accordion(
                open = FALSE,
                accordion_panel(
                  "Correlation Plot Instructions",
                  HTML(
                    "<p>This plot shows the correlation, or relationship, between the two selected indicators for the localities selected. This helps us see how two indicators relate to one another.</p>
                    <p>Each circle represents a census tract, county, or block group, depending on the selected Geographic Level. The size of each circle is based on the population of that geographic level so that geographies (counties, tracts, or blocks) with more people appear larger and those with less people appear smaller. The color of the circle is based on the locality.</p>
                    <p>The gray figures on the top and right show how frequently high and low values of the selected variables occur in the region; taller bars mean that range of values is more common.</p>
                    <p>To identify possible correlation between two indicators, look at the graph and ask: As the value of the first indicator increases, does the value of the second indicator notably increase or decrease?</p>
                    <p>It can also be useful to identify geographic areas (counties, census tracts, or block groups) that have extreme values on both measures. That is, are there geographic areas in the very bottom left corner of the graph, or in the top right?</p>"      )
                )
              ), br(),
              plotlyOutput(outputId = "scatterplot", width = '100%', height = 600)
            ),
            nav_panel(
              title = "Data",
              icon = icon('table-cells-large'),
              value = "tab5",
              br(), h2(textOutput("tbltitle", inline = TRUE)),
              p("The data table below shows the values for the selected Equity Indicators and the tercile group (Low, Medium, High) for the selected geographic level (counties, census tracts, or block groups) in the selected localities."),
              p("To download the data for all available measures and localities in this app, see the Download Data section below."),
              DTOutput("tbl"), br(),
              h2("Download Data"),
              p("Data in this Atlas is provided as a compressed folder, which includes CSVs for data at the county, census tract, and block group levels and a data dictionary."),
              downloadButton("downloadBtn", "Download")
            ),
            nav_panel(
              title = "Citation",
              icon = icon('at'),
              value = "tab5",
              br(), h2("Citation"),
              p("\"Charlottesville Regional Equity Dashboard\"; Center for Community Partnerships, University of Virginia; Accessed ", Sys.Date(), "; https://equityatlas.virginiaequitycenter.org/dashboards/cville-equity-dashboard/"),
              br(), br(),
              h3("Charlottesville Equity Atlas"),
              p("For more tools and reports focused on regional equity measures:"),
              a(href = "https://virginiaequitycenter.github.io/cville-equity-atlas/", "Visit the full Charlottesville Regional Equity Atlas site.", target="_top")
            ),
            # nav_spacer(),
            # nav_item(actionLink("infoModal", label = "", icon = icon("question-circle")))
          ) # end navset_pill
        ) # end page_sidebar
      ) # end htmlTemplate

# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  
  # for dashboard instructions modal
  observeEvent(input$infoModal, {
    showModal(
      modalDialog(title = "Dashboard Instructions",
                  footer = modalButton("Okay"),
                  size = c("l"),
                  easyClose = TRUE,
                  HTML(helpers$instructions))
    )
  })
  
  # to make helper() info render
  observe_helpers()
  
  # output available indicator geographic levels
  output$ind_geo1 <- renderText({
    paste0("Available Geographic Levels: ", attr(md()[[input$indicator1]], "geo_level"))
  })
  
  output$ind_geo2 <- renderText({
    paste0("Available Geographic Levels: ", attr(md()[[input$indicator2]], "geo_level"))
  })

  # output indicator 1 name, definition, and source
  output$ind1_name <- renderText({
    attr(md()[[input$indicator1]], "goodname")
  })

  output$ind1_abt <- renderText({
    attr(md()[[input$indicator1]], "about")
  })

  output$ind1_source <- renderText({
    attr(md()[[input$indicator1]], "source")
  })

  # output indicator 2 name, definition, and source
  output$ind2_name <- renderText({
    req(input$indicator2)
    attr(md()[[input$indicator2]], "goodname")
  })
  
  output$ind2_abt <- renderText({
    req(input$indicator2)
    attr(md()[[input$indicator2]], "about") 
  })

  output$ind2_source <- renderText({
    req(input$indicator2)
    attr(md()[[input$indicator2]], "source")
  })

  # get map data
  md <- reactive({
    all_data %>% filter(county.nice %in% input$geo &
                          GEO_LEVEL == input$geo_df) 
  })
  
  listen_indicator1 <- reactive({
    list(input$indicator1, input$geo, input$geo_df)
  })

  listen_indicator2 <- reactive({
    list(input$indicator2, input$geo, input$geo_df)
  })

  # county selections (select/deselect all)
  observe({
    if (input$selectall_geo == 0){
      return(NULL)
    } 
    else if (input$selectall_geo %% 2 == 0){
      updateCheckboxGroupInput(session, inputId = "geo", label = "Localities:",
                               choices = counties, selected = counties,
                               inline = TRUE)
    } else {
      updateCheckboxGroupInput(session, "geo", label = "Localities:",
                               choices = counties, inline = TRUE)
    }
  })

# Map Functions -------------------------------------------------------
  ## Leaflet base map function ----
  renderLeafletFunction <- function(map) {
    renderLeaflet({
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
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
      paste0("<strong>", attr(ind, "goodname"), ": </strong>",
             prettyNum(popupText, big.mark=",", preserve.width="none"), "<br>",
             mapData[["NAME"]])
    } else {
      paste0("<strong>", attr(ind, "goodname"), ": </strong>",
             prettyNum(popupText, big.mark=",", preserve.width="none"), "<br>",
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
                      fillOpacity = 0.4,
                      color = "#969997",
                      weight = 2,
                      smoothFactor = 0.2,
                      popup = popupContent,
                      highlight = highlightOptions(
                        weight = 3,
                        fillOpacity = 0.7,
                        bringToFront = FALSE),
                    group = 'indicatorSelection') %>%
        # mapGroupFunction(mapData) %>%
        clearControls() %>%
        addLegend(pal = colorNumeric(mycolors, domain = ind),
                    values = ind,
                    position = "topright",
                    opacity = 0.4,
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

  # output map1 title
  output$maptitle <- renderText({paste0(attr(md()[[input$indicator1]], "goodname"))})

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
 
# Build Map 2 -------------------------------------------------------
   # output title and source for map2
  output$maptitle2 <- renderText({
    if (input$indicator2 != "None") paste0(attr(md()[[input$indicator2]], "goodname")) })
  output$source2 <- renderText({
    if (input$indicator2 != "None") paste0("Source: ", attr(md()[[input$indicator2]], "source"))})

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
    
# Build Scatterplot -------------------------------------------------------
  
  # scatterplot title  
  output$comptitle <- renderText({
    if (all(is.na(md()[[input$indicator1]])) | all(is.na(md()[[input$indicator2]])) | length(input$geo) == 0) {
      paste("Please make sure you have selected indicators available in the selected geographic level and/or at least one locality.")
    } else { 
      paste(attr(md()[[input$indicator1]], "goodname"), " vs. ", 
            attr(md()[[input$indicator2]], "goodname"))
    }
  })

  output$scatterplot <- renderPlotly({ # add loess line to this?
    if (all(is.na(md()[[input$indicator1]])) | all(is.na(md()[[input$indicator2]])) | length(input$geo) == 0){
      plotly_empty()
    } else {

    d <- st_drop_geometry(md())
    xhist <- plot_ly(data = d, x = ~get(input$indicator1),
                      type = "histogram", nbinsx = 20,
                      alpha =.75, color = I("grey")) %>%
      layout(yaxis = list(showgrid = FALSE,
                          showticklabels = FALSE,
                          fixedrange = T),
              xaxis = list(showticklabels = FALSE,
                          fixedrange = T))

    yhist <- plot_ly(data = d, y = ~get(input$indicator2),
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
        layout(xaxis=list(title=attr(d[[input$indicator1]], "goodname"), showticklabels = TRUE, fixedrange = T),
               yaxis=list(title=attr(d[[input$indicator2]], "goodname"), showticklabels = TRUE, fixedrange = T),
               legend = list(orientation = "h", x = 0, y = -0.2))

      # note: in the legend, we hide trace 1 (the xhist) and trace (3 + length(input$geo)), which is the yhist;
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
  
  # scatterplot source caption, if present - not used anymore
  # output$source_c <- renderText({
  #   if (input$indicator2=="None") {
  #     paste("")
  #   } else {
  #     paste0(
  #       "Sources: ",
  #       attr(md()[[input$indicator1]], "goodname"), ": ", 
  #       attr(md()[[input$indicator1]], "source"), " ",
  #       attr(md()[[input$indicator2]], "goodname"), ": ", 
  #       attr(md()[[input$indicator2]], "source")
  #     )
  #   }
  # })
  
  # Build Tercile plots -------------------------------------------------------

  # tercile plot title
  output$terctitle <- renderText({
    if (all(is.na(md()[[input$indicator1]])) | all(is.na(md()[[input$indicator2]])) | length(input$geo) == 0) {
      paste("Please make sure you have selected indicators available in the selected geographic level and/or at least one locality.")
    } else if (((input$indicator1 %in% no_tercile_tract | input$indicator2 %in% no_tercile_tract) && input$geo_df == "Census Tract") | ((input$indicator1 %in% no_tercile_block | input$indicator2 %in% no_tercile_block) && input$geo_df == "Block Group")){
      "One or more selected indicators are not available for this analysis at the selected geographic level. Select a larger geographic area to view."
    } else { 
      paste(attr(md()[[input$indicator1]], "goodname"), "rank by ", 
            attr(md()[[input$indicator2]], "goodname"), "averages")
    }
  })
  
  output$tercile_plot <- renderPlotly({
    if (all(is.na(md()[[input$indicator1]])) | all(is.na(md()[[input$indicator2]])) | length(input$geo) == 0){
      plotly_empty()
    } else if (((input$indicator1 %in% no_tercile_tract | input$indicator2 %in% no_tercile_tract) && input$geo_df == "Census Tract") | ((input$indicator1 %in% no_tercile_block | input$indicator2 %in% no_tercile_block) && input$geo_df == "Block Group")){
      plotly_empty()
    } else {
    td <- md() %>% mutate(x = .data[[input$indicator1]], y = .data[[input$indicator2]])
    to_tercile <- bi_class(td, x = x, y = y, style = "quantile", dim = 3) %>%
        mutate(var1_tercile = stri_extract(bi_class, regex = '^\\d{1}(?=-\\d)'),
                var_1_group = case_when(var1_tercile == 1 ~ 'Low',
                                        var1_tercile == 2 ~ 'Medium',
                                        var1_tercile == 3 ~ 'High'),
                goodname_x = attr(md()[[input$indicator1]], "goodname"),
                goodname_y = attr(md()[[input$indicator2]], "goodname"),
                label_geo = case_when(input$geo_df == "County" ~ 'counties',
                                        input$geo_df == "Census Tract" ~ 'tracts',
                                        input$geo_df == "Block Group" ~ 'blocks')
                ) %>% 
        group_by(var1_tercile) %>% 
        mutate(var_2_mean = mean(y, na.rm = TRUE)) %>%
        # nulls removed before plotting to avoid extra bar being drawn
        filter_at(vars(x, y), all_vars(!is.na(.))) %>% 
        slice(1)

      t <- ggplot(to_tercile, aes(x = var1_tercile, y = var_2_mean,
                                  fill = var1_tercile, label = var_1_group,
                                  text = paste0('Mean of ', goodname_y, ': ', round(var_2_mean, digits = 2)))) +
        geom_bar(stat = 'identity', width = 0.66) +
        scale_fill_manual(values = c('#dfb0d6', '#a5add3', '#569ab9')) +
        scale_x_discrete(labels = paste0(c('Lowest ', 'Middle ', 'Highest '), 'third of ', to_tercile$label_geo)) +
        theme_minimal()
      
      ggplotly(t, tooltip = c('text')) %>%
        layout(showlegend = FALSE, 
              xaxis = list(title = list(text = attr(md()[[input$indicator1]], "goodname"), font = list(size=14)), showticklabels = TRUE, fixedrange = TRUE),
              yaxis = list(title = list(text = attr(md()[[input$indicator2]], "goodname"), font = list(size=14)),showticklabels = TRUE, fixedrange = TRUE)
        )
  }
})
  
# Build Data Table -------------------------------------------------------

  ## data table title ----
  output$tbltitle <- renderText({
        if (all(is.na(md()[[input$indicator1]])) | all(is.na(md()[[input$indicator2]])) | length(input$geo) == 0) {
      paste("Please make sure you have selected indicators available in the selected geographic level and/or at least one locality.")
        } else {
    paste("Data by", input$geo_df, ": ", attr(md()[[input$indicator1]], "goodname"), "(Variable 1) and ", 
          attr(md()[[input$indicator2]], "goodname"), "(Variable 2)")
        }
  })

  ## output data table ----
  ## Only shows currenly selected variables - added for accessibility purposes
  output$tbl <- renderDT({
    if (all(is.na(md()[[input$indicator1]])) | all(is.na(md()[[input$indicator2]])) | length(input$geo) == 0){
      NULL
    } else if (((input$indicator1 %in% no_tercile_tract | input$indicator2 %in% no_tercile_tract) && input$geo_df == "Census Tract") | ((input$indicator1 %in% no_tercile_block | input$indicator2 %in% no_tercile_block) && input$geo_df == "Block Group")){
      tble_data <- md() %>% mutate(x = .data[[input$indicator1]], y = .data[[input$indicator2]])
      tble_data <- st_drop_geometry(tble_data) %>% 
        mutate(x = round(x, digits = 2),
              y = round(y, digits = 2)) %>%
        select(NAME, county.nice, tractnames, x, y, pop)
      tble_data <- tble_data[with(tble_data, order(county.nice, NAME)), ]
      datatable(tble_data,
                colnames=c("Name", "Locality", "Tract Name", attr(md()[[input$indicator1]], "goodname"), attr(md()[[input$indicator2]], "goodname"), "Est. Population"),
                rownames = FALSE,
                options = list(pageLength = 10, list(3, 'asc'), scrollX = TRUE))

    } else {
      tble_data <- md() %>% mutate(x = .data[[input$indicator1]], y = .data[[input$indicator2]])
      tble_data <- st_drop_geometry(tble_data) 
      tble_data <- bi_class(tble_data, x = x, y = y, style = "quantile", dim = 3) %>% 
        mutate(var1_tercile = stri_extract(bi_class, regex = '^\\d{1}(?=-\\d)'),
              var1_tercile_cat = case_when(var1_tercile == 1 ~ 'Low',
                                            var1_tercile == 2 ~ 'Medium',
                                            var1_tercile == 3 ~ 'High'),
              var2_tercile = stri_extract(bi_class, regex = '(?<=\\d-)\\d{1}$'),
              var2_tercile_cat = case_when(var2_tercile == 1 ~ 'Low',
                                            var2_tercile == 2 ~ 'Medium',
                                            var2_tercile == 3 ~ 'High'),
              x = round(x, digits = 2),
              y = round(y, digits = 2)) %>%
        select(NAME, county.nice, tractnames, x, var1_tercile_cat, y, var2_tercile_cat, pop)
      tble_data <- tble_data[with(tble_data, order(county.nice, NAME)), ]
      datatable(tble_data,
                colnames=c("Name", "Locality", "Tract Name", attr(md()[[input$indicator1]], "goodname"), "Tercile Group Var 1", attr(md()[[input$indicator2]], "goodname"), "Tercile Group Var 2", "Est. Population"),
                rownames = FALSE,
                options = list(pageLength = 10, list(3, 'asc'), scrollX = TRUE))
    }
  })

# Data Download -------------------------------------------------------
  
  ## data download button function ----
  output$downloadBtn <- downloadHandler(
    filename = "data_download.zip",
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("regional_atlas_county.csv", "regional_atlas_census_tract.csv", "regional_atlas_block_groups.csv", "variable_dictionary.csv")
      write.csv(all_data %>% filter(GEO_LEVEL == "County") %>% st_drop_geometry(), 
                file = "regional_atlas_county.csv", sep =",")
      write.csv(all_data %>% filter(GEO_LEVEL == "Census Tract") %>% st_drop_geometry(), 
                file = "regional_atlas_census_tract.csv", sep =",")
      write.csv(all_data %>% filter(GEO_LEVEL == "Block Group") %>% st_drop_geometry(), 
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


