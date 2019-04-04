
##################################################################
# Wildfire Risk Decision Support System
# SYST 542 Decision Support Systems Engineering
# George Mason University
# Authors: Yuxin Li, Erica Maciejewski,  Mark Richard, Tyler Reif

##################################################################
# Import Packages
##################################################################

library(sf)
library(dplyr)
library(raster)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(shiny)

#################################################################
# User Interface 
#################################################################

ui <- fluidPage(
        titlePanel("Wildfire Risk DSS"),
        sidebarLayout(
          sidebarPanel(
            # Sidebar Inputs here
          ),
          mainPanel(
            # Main Panel leaflet map
          )
        )
)

#################################################################
# Server
#################################################################
server <- function(input, output) {
  #Server function 
}

shinyApp(ui = ui, server = server)
