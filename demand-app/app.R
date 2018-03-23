# Load packages ----
library(shiny)
library(tmap)
library(tmaptools)
library(sf)
library(leaflet)

# Load data ----
map_dataframe <- readRDS("data/map.rds")

# Source helper functions -----
#source("map.R")

# User interface ----
ui <- fluidPage(
  titlePanel("demandViz"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("average spatio-temporal bike share
               demand in hoboken, nj"),
      
      selectInput("day", 
                  label = "day",
                  choices = c("monday", "tuesday", "wednesday",
                              "thursday", "friday", "saturday",
                              "sunday"),
                  selected = "monday"),
      
      selectInput("time", 
                  label = "time",
                  choices = c("00:00", "01:00", "02:00", "03:00",
                              "04:00", "05:00", "06:00", "07:00",
                              "08:00", "09:00", "10:00", "11:00",
                              "12:00", "13:00", "14:00", "15:00",
                              "16:00", "17:00", "18:00", "19:00",
                              "20:00", "21:00", "22:00", "23:00"),
                  selected = "12:00"),
      
      radioButtons("startfinish",
                   label = "",
                   choices = c("ride start",
                               "ride finish"),
                   selected = "ride start")
    ),
    
    mainPanel(leafletOutput("map"))
  )
  
)

# Server logic ----
server <- function(input, output) {
  output$map <- renderLeaflet({
    
    # Data.
    day_dig <- switch(input$day, 
                   "monday" = 0, "tuesday" = 1,
                   "wednesday" = 2, "thursday" = 3,
                   "friday" = 4, "saturday" = 5,
                   "sunday" = 6)
    
    time_dig <- switch(input$time, 
                       "00:00" = 0, "01:00" = 1, "02:00" = 2,
                       "03:00" = 3, "04:00" = 4, "05:00" = 5,
                       "06:00" = 6, "07:00" = 7, "08:00" = 8,
                       "09:00" = 9, "10:00" = 10, "11:00" = 11,
                       "12:00" = 12, "13:00" = 13, "14:00" = 14,
                       "15:00" = 15, "16:00" = 16, "17:00" = 17,
                       "18:00" = 18, "19:00" = 19, "20:00" = 20,
                       "21:00" = 21, "22:00" =22, "23:00" = 23)
    
    sf_dig <- switch(input$startfinish, 
                     "ride start" = "S",
                     "ride finish" = "F")
    
    var <- paste(sf_dig, day_dig, ".", time_dig, sep="")
    
    # Palette.
    palette <- colorNumeric(palette = "Reds",
                            domain=c(0, 5))
    
    # Leaflet.
    leaflet(map_dataframe) %>%
      setView(lat = 40.7455, lng = -74.0313, zoom = 14) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = "#444444",
                  weight = 0.5,
                  smoothFactor = 0.2,
                  fillOpacity = 0.4,
                  fillColor = ~palette(map_dataframe[[var]]))
  })
}
  
# Run app ----
shinyApp(ui, server)
  