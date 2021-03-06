##################################################################
# Wildfire Risk DSS Application Script
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
library(shiny)
library(DT)
library(shinythemes)
library(rsconnect)
library(webshot)
library(htmlwidgets)

##################################################################
# Read Data 
##################################################################

# import rasters
ar_raster <- raster("access_risk_norm.tif")
fr_raster <- raster("facility_risk_layer_norm.tif")
pop_raster <- raster("pop_risk_calc.tif")
asset_raster <- raster("crit_asset_risk.tif")
burn_prob <- raster("burn_prob_norm.tif")
burn_prob_scale <- burn_prob*16

# Import firefighting facility lat-long and popup fields
la_facilities <- st_read("la_facilities.gpkg")  
fac_lat <- la_facilities$LAT
fac_lng <- la_facilities$LON
fac_type <- la_facilities$TYPE
fac_address <- paste0(la_facilities$ADDRESS, ", ",la_facilities$CITY, ", CA ", la_facilities$ZIP)
fac_phone <- la_facilities$PHONE_NUM

# Import Critical Asset Layers

subs <- st_read("subs.geojson")
sub_name <- subs$Substation_Name
sub_coord <- as.data.frame(st_coordinates(subs))

airports <- st_read("airports.geojson")
airport_name <- airports$FACILITY
airport_coord <- as.data.frame(st_coordinates(airports))

hospitals <- st_read("hospitals.geojson")
hospital_name <- hospitals$Facility
hospital_coord <- as.data.frame(st_coordinates(hospitals))

# Import Zip code risk dataframe
zip_risk <- read.csv("risk_table.csv", row.names = 1)

# Set color palette for rasters and legend

# Yellow - Red
pal_1 <- colorNumeric(c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929',
                        '#ec7014','#cc4c02','#993404','#662506'),
                      domain = c(0,1.1),
                      na.color = "transparent")

# Green - Yellow - Red
pal_2 <- colorNumeric(c('#006837', '#1a9850', '#66bd63', '#a6d96a', '#d9ef8b', 
                        '#ffffbf', '#fee08b', '#fdae61', '#f46d43', '#d73027', '#a50026'),
                    domain = c(0,1.1),
                    na.color = "transparent")

# Blue - Yellow - Red
pal_3 <- colorNumeric(c('#313695', '#4575b4', '#74add1', '#abd9e9', '#e0f3f8', 
                        '#ffffbf', '#fee090', '#fdae61', '#f46d43', '#d73027', '#a50026'),
                      domain = c(0,1.1),
                      na.color = "transparent")

# Data Table - create 19 breaks and 20 rgb color values ranging from white to red
brks <- quantile(zip_risk, probs = seq(.05, .95, .05), na.rm = TRUE)
clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
{paste0("rgb(255,", ., ",", ., ")")}

##################################################################
# User Interface Script
##################################################################

ui <- navbarPage(theme = shinytheme("slate"), 
                 "Wildfire Risk DSS", 
                 tabPanel('Risk Map',
                          sidebarLayout(
                            sidebarPanel(width = 2,
                              h4('Layer Parameters'),
                              # Sidebar Inputs here
                              selectInput("col", "Color Palette", choices = c("Yellow-Red", 
                                                                              "Green-Yellow-Red", 
                                                                              "Blue-Yellow-Red")),
                              sliderInput("opacity", "Opacity",
                                          min = 0, max = 1,
                                          value = 0.7, step = 0.05),
                              actionButton("renderColor", "Render Layers", class = "btn-primary"),
                              h4('Effects Risk Weights'),
                              # Slider input - Accessiblity Risk
                              sliderInput("w1", "Accessibility Risk",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              # Slider input - Firefighting Facility Risk
                              sliderInput("w2", "Firefighting Facility Proximity Risk",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              # Slider input - Population Risk
                              sliderInput("w3", "Population Risk",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              # Slider input - Critical Asset Risk
                              sliderInput("w4", "Critical Asset Risk",
                                          min = 0, max = 1,
                                          value = 0.5, step = 0.05),
                              actionButton("renderWeight", "Recalculate Effects Risk", class = "btn-primary")
                            ), 
                            mainPanel(width = 10,
                              
                              leafletOutput("map",  width= 1280, height=750),
                                downloadButton('map_down', class = "btn-primary"))
                            )),
                 
                 tabPanel('Risk Table',
                          sidebarLayout(
                            sidebarPanel(width = 2,
                              # Sidebar Inputs here
                              h3('Table Display'),
                              selectInput(
                                "sortVariable",
                                label = "Sort by",
                                choices = c('Zip Code'='zip',
                                            'City'='City',
                                            'FP Boundary'='FP Boundary')),
                              # Numeric Input
                              numericInput('maxRiskLevel', 'Maximum Allowable Risk Level', 
                                           0.9, min = 0, max = 1, step = 0.05),
                              # Cell Shading options
                              radioButtons("highlight", "Cell shading method:",
                                           c("Highlight at-risk cells" = "norm",
                                             "Shade by Risk" = "shade"), selected = "norm"),
                              checkboxGroupInput("show_vars", "Columns to show:",
                                                 names(zip_risk), selected = names(zip_risk))
                            ), 
                            mainPanel(width = 10,
                                      tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff;
                    }
                    .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}
                    .dataTables_length select {
                           color: #ffffff;
                                                      background-color: #0E334A
                    }
                    .dataTables_filter input {
                            color: #0E334A;
                            background-color: #ffffff
                           }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #000000;
                    }

                   "
                                                      
                                                      
                                      )),
                              DT::dataTableOutput("table")
                            )))
)
#################################################################
# Server Script
#################################################################

server <- function(input, output) {
  
  # Calculate weight total
  w_tot <- eventReactive(input$renderWeight,{sum(input$w1, input$w2, input$w3, input$w4)}, 
                         ignoreNULL = FALSE)
  
  # scale effects rasters by weights
  ar_raster_scale <- eventReactive(input$renderWeight, 
                                   {(input$w1/w_tot())*ar_raster}, 
                                   ignoreNULL = FALSE)
  fr_raster_scale <- eventReactive(input$renderWeight, 
                                   {(input$w2/w_tot())*fr_raster}, 
                                   ignoreNULL = FALSE)
  pop_raster_scale <- eventReactive(input$renderWeight, 
                                    {(input$w3/w_tot())*pop_raster}, 
                                    ignoreNULL = FALSE)
  asset_raster_scale <- eventReactive(input$renderWeight, 
                                      {(input$w4/w_tot())*asset_raster}, 
                                      ignoreNULL = FALSE)
  
  # Calculate combined effects risk raster using sum of 4 scaled input rasters
    effects_risk <- eventReactive(input$renderWeight,
                                {overlay(ar_raster_scale(), 
                                        fr_raster_scale(), 
                                        pop_raster_scale(), 
                                        asset_raster_scale(), 
                                        fun = function(w,x,y,z){return((w+x+y+z))})
                                  },
                                ignoreNULL = FALSE)
  
  # Calculate Overall Risk
    overall_risk <- eventReactive(input$renderWeight,
                                {overlay(effects_risk(), 
                                         burn_prob, 
                                         fun = function(x, y){return((x+y*16)/2)})
                                },
                                ignoreNULL = FALSE)
  
  # Establish color palette based on user input
  pal <- eventReactive(input$renderColor, {if (input$col == "Yellow-Red"){return (pal_1)}
                                      else if(input$col == "Green-Yellow-Red"){return (pal_2)}
                                      else if(input$col == "Blue-Yellow-Red"){return (pal_3)}
                                      }, ignoreNULL = FALSE)
  
  op <- eventReactive(input$renderColor, {input$opacity}, ignoreNULL = FALSE)
  
  # Map output with widgets
    output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 9)) %>%
      addProviderTiles('CartoDB.Positron', group = "Gray") %>% 
      addProviderTiles('Esri.WorldImagery', group = "Satellite") %>% 
      addProviderTiles("Esri", group = "Topographic") %>%
      setView(lng = -118.2437,
              lat = 34.3000,
              zoom = 10) %>%
      addResetMapButton() %>% 
      addSearchOSM() %>% 
      addRasterImage(overall_risk(), opacity = op(), colors = pal(), group ="Overall Risk") %>%
      addRasterImage(effects_risk(), opacity = op(), colors = pal(), group ="Effects Risk") %>% 
      addRasterImage(pop_raster, opacity = op(), colors = pal(), group ="Population Risk") %>% 
      addRasterImage(fr_raster, opacity = op(), colors = pal(), group ="Facility Risk") %>%
      addRasterImage(burn_prob_scale, opacity = op(), colors = pal(), group ="Hazard Risk") %>%
      addRasterImage(ar_raster, opacity = op(), colors = pal(), group ="Access Risk") %>%
      addRasterImage(asset_raster, opacity = op(), colors = pal(), group ="Critical Asset Risk") %>%
      addLegend(position = "bottomright", pal = pal(), values = c(0,1),
                title = "Risk",
                opacity = 1.0) %>% 
      addCircleMarkers(fac_lng, fac_lat,
                 label = paste0("Type: ", fac_type, " | ",
                                fac_address, " | ",
                                fac_phone),
                 labelOptions = labelOptions(direction = "top",
                 style = list(
                    "color" = "blue",
                    "font-family" = "arial",
                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "12px",
                    "border-color" = "rgba(0,0,0,0.5)")),
                 radius = 5,
                group = "Firefighting Facilities") %>%
      addCircleMarkers(sub_coord$X, sub_coord$Y, 
                       color = 'red',
                       label = paste0("Substation Name: ", sub_name),
                       labelOptions = labelOptions(direction = "top",
                                                   style = list(
                                                     "color" = "blue",
                                                     "font-family" = "arial",
                                                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                     "font-size" = "12px",
                                                     "border-color" = "rgba(0,0,0,0.5)")),
                       radius = 5,
                       group = "Substations") %>% 
        addCircleMarkers(airport_coord$X, airport_coord$Y, 
                         color = 'purple',
                         label = paste0("Airport Name: ", airport_name),
                         labelOptions = labelOptions(direction = "top",
                                                     style = list(
                                                       "color" = "blue",
                                                       "font-family" = "arial",
                                                       "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                       "font-size" = "12px",
                                                       "border-color" = "rgba(0,0,0,0.5)")),
                         radius = 5,
                         group = "Airports") %>% 
        addCircleMarkers(hospital_coord$X, hospital_coord$Y, 
                         color = 'green',
                         label = paste0("Hospital Name: ", hospital_name),
                         labelOptions = labelOptions(direction = "top",
                                                     style = list(
                                                       "color" = "blue",
                                                       "font-family" = "arial",
                                                       "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                       "font-size" = "12px",
                                                       "border-color" = "rgba(0,0,0,0.5)")),
                         radius = 5,
                         group = "Hospitals") %>% 
      addMouseCoordinates() %>% 
      addDrawToolbar(
          targetGroup = "Added Shapes",
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
        ) %>%
      addStyleEditor(position = "bottomleft", openOnLeafletDraw = FALSE) %>% 
      addLayersControl(baseGroups = c("Gray", "Satellite", "Topographic"),
                       overlayGroups = c("Overall Risk",
                                         "Hazard Risk",
                                         "Effects Risk",
                                         "Population Risk", 
                                         "Facility Risk", 
                                         "Access Risk",
                                         "Critical Asset Risk",
                                         "Firefighting Facilities",
                                         "Added Shapes",
                                         "Substations",
                                         "Airports",
                                         "Hospitals"),
                      options = layersControlOptions(collapsed = TRUE)) %>% 
                      hideGroup(c("Hazard Risk",
                                  "Effects Risk",
                                  "Population Risk",
                                  "Facility Risk",
                                  "Access Risk",
                                  "Critical Asset Risk",
                                  "Hazard Risk",
                                  "Firefighting Facilities",
                                  "Substations",
                                  "Airports",
                                  "Hospitals")) %>%
      setMaxBounds(lng1 = -119.5,
                   lat1 = 33.5,
                   lng2 = -117.0,
                   lat2 = 35.0) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "miles",
        primaryAreaUnit = "sqmiles",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>% 
      addFullscreenControl()
    })
 
    # Update overall risk layer and combined effects layer when weights are adjusted
    observeEvent(input$renderWeight, {
    m <- leafletProxy("map") %>% 
      addRasterImage(overall_risk(), opacity = op(), colors = pal(), group ="Overall Risk") %>%
      addRasterImage(effects_risk(), opacity = op(), colors = pal(), group ="Effects Risk")   
    })
 
    # Update all risk layers when opacity or palette is adjusted
    observeEvent(input$renderColor, {
      n <- leafletProxy("map") %>% 
        addRasterImage(overall_risk(), opacity = op(), colors = pal(), group ="Overall Risk") %>%
        addRasterImage(effects_risk(), opacity = op(), colors = pal(), group ="Effects Risk") %>% 
        addRasterImage(pop_raster, opacity = op(), colors = pal(), group ="Population Risk") %>% 
        addRasterImage(fr_raster, opacity = op(), colors = pal(), group ="Facility Risk") %>%
        addRasterImage(burn_prob_scale, opacity = op(), colors = pal(), group ="Hazard Risk") %>%
        addRasterImage(ar_raster, opacity = op(), colors = pal(), group ="Access Risk") %>%
        addRasterImage(asset_raster, opacity = op(), colors = pal(), group ="Critical Asset Risk")
      })

    # Add marker when user clicks on the map. Return the coordinates and pixel risk scores
    
    # store the click
    # observeEvent(input$map_marker_click,{
    #   data_of_click$clickedMarker <- input$map_marker_click
    #   print(data_of_click)
    # })
   
  # Data table output
  zip_risk_update = zip_risk[sample(nrow(zip_risk), 315), ]
  
  table_input <- reactive({
    color_scheme <- switch(input$highlight,
                          norm =  styleInterval(input$maxRiskLevel, c('clear','red')),
                          shade = styleInterval(brks, clrs))
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(zip_risk_update[, input$show_vars, drop = FALSE], 
                  options = list(orderClasses = TRUE) )%>% 
                  formatStyle(input$show_vars, backgroundColor = table_input())
  })
}

#######################################################################
# Call App function and deploy to the cloud
#######################################################################

shinyApp(ui = ui, server = server)

#deployApp("C:/Users/mrich/OneDrive/Documents/Grad School/SYST 542/perf_dir")
