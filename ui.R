#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Mapview for Mobile Operators"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("mapType","Select Type of Info to Show in the Map", 
                        choices = c("Base Stations", "TMS Points", "KPI Value")
            ),
            fileInput("csvFile", "Select a *.csv for the Selected option", accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
            ),
            actionButton("updateMap","Show the info in the map"),
            h5("Status"),  
            textOutput("validInfo"), 
        ),
        # Show a plot of the generated distribution
        mainPanel(
          h4("Map"),
          leafletOutput("leafletMap"),
          h4("Information"),  
          textOutput("selectedInputs"),  
        )
    )
  )
)
