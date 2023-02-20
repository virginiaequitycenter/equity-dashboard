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

# Load Data ---------------------------------------------------------------

load("www_2023/app_data_2022.Rdata")


# Define User Interface ---------------------------------------------------
ui <- navbarPage(title = "Charlottesville Regional Equity Atlas",
                 collapsible = TRUE,
                 fluid = TRUE,
                 tabPanel("Atlas",
                          fluidRow(
                              column(width = 4,
                                     "selection content"
                                     ),
                              
                              column(width = 8,
                                     tabsetPanel(
                                       tabPanel("Map 1",
                                                "map here"
                                                ),
                                       tabPanel("Map 2",
                                                "map here"
                                                )
                                       ) # end tabsetPabel
                                     ) # end column width 8
                            ) # end fluidRow
                   
                 ) # end tabPanel
  
) # end navbarPage

# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  
  
}

# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
