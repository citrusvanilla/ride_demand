# Load packages ----
library(shiny)
library(tmap)
library(tmaptools)
library(sf)
library(leaflet)

# Load data ----
map_dataframe <- readRDS("data/map3.rds")

# Get a numeric version for the color pallete.
map_df_num <- cbind(map_dataframe)
st_geometry(map_df_num) <- NULL
map_df_num$id <- NULL

# Source helper functions -----
#source("map.R")

# User interface ----
ui <- fluidPage(
  titlePanel("demandViz"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("average spatio-temporal bike share
               demand in hoboken, NJ.  data from
               jumpbikes.com.  made by justin fung."),
      
      selectInput("day", 
                  label = "day",
                  choices = c("monday", "tuesday", "wednesday",
                              "thursday", "friday", "saturday",
                              "sunday"),
                  selected = "monday"),
      
      sliderInput("hour",
                  label ="hour",
                  post=":00",
                  min = 0, 
                  max = 23,
                  value = 0,
                  step=1,
                  animate=animationOptions(interval=700,
                                           loop=TRUE)),
      
      radioButtons("startfinish",
                   label = "ride start or finish",
                   choices = c("start",
                               "finish"),
                   selected = "start")
    ),
    
    mainPanel(
      leafletOutput("map")
      )
  )
  
)

# Server logic ----
server <- function(input, output) {
  # Palette.
  palette <- colorBin(palette = "Reds",
                      domain=map_df_num,
                      bins = c(0,1,3,6,10,15,21,28,35))
  
  output$map <- renderLeaflet({
    
    # Leaflet.
    leaflet(map_dataframe) %>%
      setView(lat = 40.7455, lng = -74.0313, zoom = 14) %>%
      addProviderTiles("CartoDB.Positron")

  })
  
  observeEvent({input$day
                input$hour
                input$startfinish}, {
    
    # Data.
    day_dig <- switch(input$day, 
                      "monday" = 0, "tuesday" = 1,
                      "wednesday" = 2, "thursday" = 3,
                      "friday" = 4, "saturday" = 5,
                      "sunday" = 6)
    
    sf_dig <- switch(input$startfinish, 
                     "start" = "S",
                     "finish" = "F")
    
    var <- paste(sf_dig, day_dig, ".", input$hour, sep="")
    print(var)
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = map_dataframe,
                  color = "#444444",
                  weight = 0.5,
                  smoothFactor = 0.2,
                  fillOpacity = 0.4,
                  fillColor = ~palette(map_dataframe[[var]]))
  })
}
  
# Run app ----
shinyApp(ui, server)
  