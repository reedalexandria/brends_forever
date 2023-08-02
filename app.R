library(leaflet)
library(tidyverse)
library(googlesheets4)
library(htmltools)
library(htmlwidgets)
library(lubridate)
library(shiny)
library(jsonlite)
library(readr)

# Read data from Google sheet
# data <- read_sheet("https://docs.google.com/spreadsheets/d/1F4758VgFYaW55gD3_b32t8_J4q6yt0__BKKwvJBHJyU/edit#gid=0") 

data <- read_csv("data.csv")
data$birthday <- mdy(paste(data$birthday, "2023"))

# Extract only the filenames from the photo_url column
data$photo_url <- basename(data$photo_url)

# Define UI for Shiny app
ui <- fluidPage(
  # # Page title
  # titlePanel("Brends Forever"),

  # Add the Bren logo to the left corner and center it vertically
  tags$div(
    style = "display: flex; align-items: center;",
    tags$img(src = "Bren_Logo.png", height = "50px"),
    
    # Center the title horizontally
    tags$h1("Brends Forever", style = "text-align: center; flex: 1;")
  ),
  
  
  # Photo and map section
  fluidRow(
    column(width = 12, align = "center",
           div(style = "max-width: 100%; height: 600px; display: flex; align-items: center; justify-content: center;",
               tags$img(src = "MEDS.jpg", style = "max-width: 100%; max-height: 100%;")
           ),
           leafletOutput("brennies_map", height = "500px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data for Santa Barbara
  santa_barbara_data <- data %>%
    filter(city == "Santa Barbara")
  
  # Create a custom icon for the markers
  custom_icon <- makeIcon(
    iconUrl = "Bren_Logo.png",
    iconWidth = 25, 
    iconHeight = 25 
  )
  
  # Create the Leaflet map
  output$brennies_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
      addMarkers(
        data = data,
        lng = ~longitude,
        lat = ~latitude,
        popup = ~ifelse(city == "Santa Barbara",
                        paste(
                          "<b>Name:</b> ", paste(santa_barbara_data$first, santa_barbara_data$last, sep = " ", collapse = "<br>"),
                          "<br><b>Location:</b> ", paste(santa_barbara_data$city, santa_barbara_data$state, sep = ", "),
                          "<br><b>Photos:</b><br>",
                          paste("<img src='", santa_barbara_data$photo_url, "' width='150px'>", collapse = "<br>")
                        ),
                        paste0(
                          "<b>Name:</b> ", first, " ", last, "<br><b>Location:</b> ", city, ", ", state, 
                          "<br><img src='", photo_url, "' width='150px'>"
                        )
        ),
        icon = custom_icon
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)

