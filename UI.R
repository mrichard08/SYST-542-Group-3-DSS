
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

ui <- navbarPage("Wildfire Risk DSS",
                 tabPanel('Risk Map',
                          sidebarLayout(
                            sidebarPanel(
                              # Sidebar Inputs here
                              h3('Map Display'),
                              selectInput(
                                "baseMap",
                                label = "Base Map",
                                choices = c('Satelitte'='satellite')),
                              selectInput(
                                "riskMapLayer",
                                label = "Risk Map Layer",
                                choices = c('Total Risk Index'='totalRiskIndex')),
                              # Highlight checkbox.
                              checkboxInput('firefightingFacilities', 'Firefighting Facilities', value = FALSE),
                              checkboxInput('histFireBoundaries', 'Historic Fire Boundaries', value = FALSE),
                              h3('Effects Risk Index Weight'),
                              # Slider input
                              sliderInput("accessibility", "Accessiblity",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              # Slider input
                              sliderInput("populationDensity", "Population Density",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              # Slider input
                              sliderInput("prox", "Proximity To Critical Assests",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05)
                            ), 
                            mainPanel(
                              # tableOuput goes here
                            ))),
                 
                 tabPanel('Risk Table',
                          sidebarLayout(
                            sidebarPanel(
                              # Sidebar Inputs here
                              h3('Table Display'),
                              selectInput(
                                "sortVariable",
                                label = "Sort by",
                                choices = c('Zip Code'='zip',
                                            'Total Risk Index'='totalRiskIndex',
                                            'Hazard Risk'='hazardRisk',
                                            'Effects Risk'='effectsRisk')),
                              # Numeric Input
                              numericInput('maxRiskLevel', 'Maximum Allowable Risk Level', 
                                           0.9, min = 0, max = 1, step = 0.05),
                              # Highlight checkbox.
                              checkboxInput('highlightCheck', 'Highligh at-risk cells', value = FALSE),
                              h3('Effects Risk Index Weight'),
                              # Slider input
                              sliderInput("accessibility", "Accessiblity",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              # Slider input
                              sliderInput("populationDensity", "Population Density",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              # Slider input
                              sliderInput("prox", "Proximity To Critical Assests",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05)
                            ), 
                            mainPanel(
                              # tableOuput goes here
                            )))
)
#################################################################
# Server
#################################################################
server <- function(input, output) {
  #Server function 
}

shinyApp(ui = ui, server = server)
