# Published version
# Equity Indicators

# Load libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)

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

# arrange hhinc_race for plotting: 
race_inc <- select(county_data, GEOID:year, hhinc_blackE:hhinc_ltnxM)
race_inc <- gather(race_inc, hhinc_blackE:hhinc_ltnxM, key="race", value="hhinc")
race_inc <- separate(race_inc, race, into=c("race", "type"), sep=-1)
race_inc <- separate(race_inc, race, into=c("var", "race"), sep="_")
race_inc <- separate(race_inc, NAME, into=c("NAME", "state"), sep=",")
race_inc <- mutate(race_inc, 
                   race = fct_recode(race, 
                                     "Black"="black", 
                                     "Asian"="asian", 
                                     "Hispanic"="hisp", 
                                     "Multi-racial"="multi",
                                     "White"="white"))


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
            tags$br(),
            
            selectInput(
              inputId = "geo", 
              label = "Counties",
              choices = levels(factor(tract_data_geo@data$county.nice)), 
              selected = levels(factor(tract_data_geo@data$county.nice)), 
              multiple = T),
            tags$p("Select an indicator:"),
            htmlOutput("category"),
            htmlOutput("indicator"),
            htmlOutput("indicator2"),
            htmlOutput("time"),
            radioButtons(inputId = "df_geo",
                         label = "Select a Geographic Level:",
                         choices = c("County", "Census Tract", "Block Group"),
                         selected = "County",
                         inline = TRUE),
            radioButtons(inputId = "map_geo",
                         label = "Select a Base Map:",
                         choices = c("Minimal", "Detailed"),
                         selected = "Minimal",
                         inline = TRUE),
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
        box(strong(textOutput("ind1_name")), 
            textOutput("ind1_abt"), tags$br(),
            strong(textOutput("ind2_name")), 
            textOutput("ind2_abt"),
            width=4),
        tabBox(width=8,
          tabPanel("Distribution",
                   textOutput("histtitle"), 
                   plotlyOutput("hist"),
                   textOutput("source_b")),
          tabPanel("Compare", 
                   textOutput("comptitle"), 
                   plotlyOutput("compare"),
                   textOutput("source_c")), 
          tabPanel("By Race", 
                   textOutput("racetitle"), 
                   plotlyOutput("byrace"))
          )
      
      ), # fluid row
      fluidRow(
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
        tags$p("Dashboard Creators: Charlotte McClintock, Michele Claibourn"),
        tags$p("Contributors: Clay Ford"),
        tags$div(style="display: inline-block;vertical-align:top;", tags$p("We welcome feedback and suggestions at")),
        tags$div(style="display: inline-block;vertical-align:top;", tags$a(href="mailto:commpaslab@virginia.edu", tags$p("commpaslab@virginia.edu")))),
        
        box(tags$a(href = "http://virginiaequitycenter.org/", tags$h2("Equity Center")),
            tags$p("The Equity Center will tangibly redress racial and socioeconomic inequality in 
                   university communities by advancing a transformative approach to the fundamental 
                   research mission, which will, in turn, reform institutional values, pedagogy, 
                   and operations."),
            tags$p("Within the Democratization of Data Initiative, the Equity Center will co-create a 
                   comprehensive Equity Atlas, which will aim to be inclusive of an open-source 
                   platform that makes data and information that the community wants to know about 
                   itself broadly available for use in the pursuit of equity throughout the region."))
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
server <- function(input, output) {

# .....................................................................................
  
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
  
  # generate UI
  # categories of indicators
  output$category <- renderUI({
    selectInput(
      inputId = "category", 
      label = "Category of Indicator:",
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
    df <- filter(data_geo()@data, county.nice %in% input$geo)
    col <- paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])
    df <- na.omit(df[,c(col, "year")])
    df$year <- as.numeric(df$year)
    sort(unique(df$year))
  })
  
  output$time <- renderUI({
    sliderTextInput(inputId = "time", 
                    label = "",
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
    if (!input$indicator2=="Median Household Income"&!input$indicator=="Median Household Income") {
      paste("Breakouts by race are not yet available for the indicator you selected.")
    }
    else {paste("Histogram of Median Household Income by Race by Locality")}
  })
 
  output$racetitle <- renderText({race.title()})
  
  
  # .....................................................................................
  
  # create indicator 1 data frame ----
  d <- reactive({
    req(input$indicator)
    df <- filter(data_geo()@data, county.nice %in% input$geo & 
                   year %in% input$time)
    col <- paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])
    df[,col]
  })
  
  # create indicator 1 palette function
  pal <- reactive({
    req(input$indicator)
    df <- filter(data_geo()@data, county.nice %in% input$geo & 
                   year %in% input$time)
    col <- paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])
    colorNumeric(palette = mycolors,
                 domain = df[,col]) })
  
  # create inidicator 2 data frame 
  d2 <- reactive({
    req(input$indicator2)
    df <- filter(data_geo()@data, county.nice %in% input$geo & 
                   year %in% input$time)
    col <- paste(prettytab()[prettytab()$goodname==input$indicator2, "varname"])
    df[,col]
  })
  
  # create indicator 2 palette function
  pal2 <- reactive({
    req(input$indicator2)
    df <- filter(data_geo()@data, county.nice %in% input$geo & 
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
  
  # create map tile layer
  tile_geo <- reactive({
    tile <- switch(input$map_geo,
                   "Minimal" = "CartoDB.Positron",
                   "Detailed" = "OpenStreetMap.Mapnik")
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
                                data_geo()$NAME.y[data_geo()$county.nice %in% input$geo & data_geo()$year %in% input$time], "<br>"
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
                  color = "blue", fill = FALSE, weight = 3,
                  popup = paste(sabselem()$schnam)) %>% 
      addLayersControl(
            overlayGroups = c("Parks", "Schools", "Elem School Zone"),
            options = layersControlOptions(collapsed = FALSE), 
            position = "bottomright"
          ) %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
      hideGroup("Elem School Zone") %>% 
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
                                 data_geo()$NAME.y[data_geo()$county.nice %in% input$geo & data_geo()$year %in% input$time], "<br>"
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
      addLayersControl(
        overlayGroups = c("Parks", "Schools"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "bottomright"
      ) %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
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
  output$compare <- renderPlotly({ # add regression line to this
    req(input$indicator2)
    if (!input$indicator2=="None") {
      plot_ly(data=data_geo()@data,
              x=data_geo()@data[data_geo()$county.nice %in% input$geo,
                                    paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])],
              y=data_geo()@data[data_geo()$county.nice %in% input$geo,
                                    paste(prettytab()[prettytab()$goodname==input$indicator2, "varname"])],
              color=data_geo()@data[data_geo()$county.nice %in% input$geo,
                                        "county.nice"],
              type = "scatter", mode = "markers", 
              # Set3 has 12 values, which matches the 12 cities/counties
              colors = "Set3", 
              text=paste0(data_geo()@data[data_geo()$county.nice %in% input$geo,"county.nice"], "<br>",
                          input$indicator, ": ", data_geo()@data[data_geo()$county.nice %in% input$geo,
                                                                     paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])], "<br>",
                         input$indicator2, ": ", data_geo()@data[data_geo()$county.nice %in% input$geo,
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
    p1 <- plot_ly(data=data_geo()@data,
            x=data_geo()@data[data_geo()$county.nice %in% input$geo,
                                  paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])],
            type="histogram", marker = list(color = '#2c7fb8')) %>%
      layout(xaxis=list(title=input$indicator))
    p2 <- plot_ly(data=data_geo()@data,
            x=data_geo()@data[data_geo()$county.nice %in% input$geo,
                                  paste(prettytab()[prettytab()$goodname==input$indicator2, "varname"])],
            type="histogram", marker = list(color = '#41b6c4')) %>%
      layout(xaxis=list(title=input$indicator2))
    subplot(p1, p2, titleX = T, titleY = T) %>%
      layout(showlegend = FALSE)
    }
    else {
      plot_ly(data=data_geo()@data,
              x=data_geo()@data[data_geo()$county.nice %in% input$geo,
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
    if (input$indicator=="Median Household Income"|input$indicator2=="Median Household Income") {
    ggplotly(
      ggplot(subset(race_inc, NAME %in% input$geo),
                    aes(x=race, y=hhinc, fill=race)) + 
               stat_summary(fun.y="sum", geom="bar", 
                            aes(text=paste0(NAME,"<br>", race, 
                                            " Median Household Income: ", "$", 
                                            prettyNum(hhinc, big.mark=",",scientific=F)))) + 
               scale_fill_manual(values=brewer.pal(6, "YlGnBu")[2:6]) +
               facet_wrap(~NAME, scales="free_y") +
        scale_y_continuous(labels=dollar_format(prefix="$")) +
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
