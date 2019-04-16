##################################################################
# Wildfire Risk Decision Support System App Script
# SYST 542 Decision Support Systems Engineering
# George Mason University
# Authors: Yuxin Li, Erica Maciejewski,  Mark Richard, Tyler Reif
##################################################################

setwd("~/Grad School/SYST 542/perf_dir")

##################################################################
# Import Packages
##################################################################

library(sf)
library(dplyr)
library(raster)
library(leaflet)
library(mapview)
library(leaflet.extras)
library(tidyverse)
library(shiny)
library(RColorBrewer)
library(DT)
library(shinythemes)

#################################################################
# Read Data 
#################################################################

# import rasters

ar_raster <- raster("access_risk_norm.tif")
fr_raster <- raster("facility_risk_layer_norm.tif")
pop_raster <- raster("pop_risk_calc.tif")
asset_raster <- raster("crit_asset_risk.tif")
burn_prob <- raster("burn_prob_norm.tif")

# Import historic fire polygons
hist_fires <- st_read("la_hist_fires.gpkg")

# Import firefighting facility lat-long and popup fields
la_facilities <- st_read("la_facilities.gpkg")  
fac_lat <- la_facilities$LAT
fac_lng <- la_facilities$LON
fac_address <- paste0(la_facilities$ADDRESS, ", ",la_facilities$CITY, ", CA ", la_facilities$ZIP)
fac_phone <- la_facilities$PHONE_NUM

# Set color palette for rasters and legend
#pal <- colorNumeric("Spectral", values(la_fac), 
                  #  na.color = "transparent")

#################################################################
# User Interface Script
#################################################################

ui <- navbarPage(theme = shinytheme("slate"), 
                 "Wildfire Risk DSS", 
                 tabPanel('Risk Map',
                          sidebarLayout(
                            sidebarPanel(width = 3,
                              # Sidebar Inputs here
                              h3('Map Display'),
                              # Date Range for Fire Boundaries
                              dateRangeInput('dateRange',
                                             label = 'Historic Fire Date range: yyyy-mm-dd',
                                             start = Sys.Date() - 2, end = Sys.Date() + 2
                              ),
                              h3('Effects Risk Index Weight'),
                              # Slider input - Population Risk
                              sliderInput("w1", "Population Risk",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              # Slider input - Firefighting Facility Risk
                              sliderInput("w2", "Firefighting Facility Proximity Risk",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              # Slider input - Accessibility Risk
                              sliderInput("w3", "Acessibility Risk",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              # Slider input - Critical Asset Risk
                              sliderInput("w4", "Critical Asset Risk",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              actionButton("render", "Recreate Risk Map", class = "btn-primary")
                            ), 
                            mainPanel(width = 9,
                              leafletOutput("map",  width= 1050, height=680)
                            ))),
                 
                 tabPanel('Risk Table',
                          sidebarLayout(
                            sidebarPanel(width = 3,
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
                              sliderInput("prox", "Distance from Firefighting Resources",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              actionButton("rerender", "Recreate Table", class = "btn-primary")
                            ), 
                            mainPanel(width = 9,
                              DT::dataTableOutput("mytable2")
                            )))
)
#################################################################
# Server Script
#################################################################
server <- function(input, output) {
  
  # Calculate weight total
  w_tot <- eventReactive(input$ender,{sum(input$w1,input$w2, input$w3, input$w4)}, ignoreNULL = FALSE)
  
  # scale rasters
  ar_raster_scale <- eventReactive(input$render, 
                                   {(input$w1/w_tot())*ar_raster}, 
                                   ignoreNULL = FALSE)
  fr_raster_scale <- eventReactive(input$render, 
                                   {(input$w2/w_tot())*fr_raster}, 
                                   ignoreNULL = FALSE)
  pop_raster_scale <- eventReactive(input$render, 
                                    {(input$w3/w_tot())*pop_raster}, 
                                    ignoreNULL = FALSE)
  asset_raster_scale <- eventReactive(input$render, 
                                      {(input$w4/w_tot())*asset_raster}, 
                                      ignoreNULL = FALSE)
  
  # Calculate combined effects risk raster using average of 4 scaled input rasters
  effects_risk <- eventReactive(input$render,
                                {overlay(ar_raster_scale(), 
                                        fr_raster_scale(), 
                                        pop_raster_scale(), 
                                        asset_raster_scale(), 
                                        fun = function(w,x,y,z){return((w+x+y+z)/4)})
                                  },
                                ignoreNULL = FALSE)
  
  # Calculate Overall Risk
  overall_risk <- eventReactive(input$render,
                                {overlay(effects_risk(), 
                                         burn_prob, 
                                         fun = function(x, y){return((x+y*16)/2)})
                                },
                                ignoreNULL = FALSE)
  
  # Reactive expression to select historic fire polygons for years 
  # the user selects
  #filteredHistData <- reactive({
    #hist[hist$YEAR_ >= input$range[1] & hist$YEAR_ <= input$range[2],]
  #})
  
  # Map output with widgets
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 9)) %>%
      addProviderTiles("Esri", group = "Esri") %>%
      setView(lng = -118.2437,
              lat = 34.3000,
              zoom = 9) %>%
     # leafletOptions(minZoom = 8) %>% 
      addResetMapButton() %>% 
      addSearchOSM() %>% 
      addReverseSearchOSM() %>% 
      addRasterImage(overall_risk(), opacity = 0.5, group ="Overall Risk") %>%
      addRasterImage(effects_risk(), opacity = 0.5, group ="Effects Risk") %>%
      addRasterImage(pop_raster, opacity = 0.5, group ="Population Risk") %>% 
      addRasterImage(fr_raster, opacity = 0.5, group ="Facility Risk") %>%
      addRasterImage(burn_prob, opacity = 0.5, group ="Hazard Risk") %>%
      addRasterImage(ar_raster, opacity = 0.5, group ="Access Risk") %>%
      addRasterImage(asset_raster, opacity = 0.5, group ="Critical Asset Risk") %>%
      addCircleMarkers(fac_lng, fac_lat, 
                 label = paste(fac_address," | ", fac_phone),
                 labelOptions = labelOptions(direction = "bottom",
                 style = list(
                    "color" = "blue",
                    "font-family" = "arial",
                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "12px",
                    "border-color" = "rgba(0,0,0,0.5)")),
                 radius = 3,
                group = "Firefighting Facilities") %>% 
      addMouseCoordinates() %>% 
      addLayersControl(
        overlayGroups = c("Overall Risk",
                          "Effects Risk",
                          "Population Risk", 
                          "Facility Risk", 
                          "Hazard Risk",
                          "Access Risk",
                          "Critical Asset Risk",
                          "Historic Fire Boundaries",
                          "Firefighting Facilities"),
        options = layersControlOptions(collapsed = TRUE)) %>% 
      hideGroup(c("Effects Risk",
                  "Population Risk",
                  "Facility Risk",
                  "Access Risk",
                  "Critical Asset Risk",
                  "Hazard Risk",
                  "Historic Fire Boundaries",
                  "Firefighting Facilities")) %>%
      setMaxBounds(lng1 = -119.5, 
                   lat1 = 33.5, 
                   lng2 = -117.0, 
                   lat2 = 35.0) %>% 
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "miles",
        primaryAreaUnit = "sqmiles",
        activeColor = "#3D535D",
        completedColor = "#7D4479") 
    })
  # Data table output
  output$mytable2 <- DT::renderDataTable({
    datatable(mtcars, options = list(orderClasses = TRUE))
  })
}

shinyApp(ui = ui, server = server)