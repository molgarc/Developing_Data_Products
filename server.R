#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    checkFields<-function(dataDF, type)
        {
        if(type == "Base Stations"){
            names = c("LATITUDE","LONGITUDE","SITE")
            if (all(names %in% colnames(dataDF))){
                output$validInfo<- renderText({paste("Valid CSV")})
                return(TRUE)
            }
        }
        else if(type == "TMS Points"){
            names = c("LATITUDE","LONGITUDE","ZOOM_LEVEL","XPIXEL_ID_ABSOLUTE","YPIXEL_ID_ABSOLUTE","TILE_QUADKEY","XTILE_STANDARD","YTILE_STANDARD","XPIXEL_ID_TILE_RELATIVE","YPIXEL_ID_TILE_RELATIVE","RESOLUTION_ZOOM_LEVEL","RESOLUTION_LATITUDE_LONGITUDE")
            if (all(names %in% colnames(dataDF))){
                output$validInfo<- renderText({paste("Valid CSV")})
                return(TRUE)
            }
        }
        else if(type == "KPI Value"){
            names = c("LATITUDE","LONGITUDE","VALUE")
            if (all(names %in% colnames(dataDF))){
                output$validInfo<- renderText({paste("Valid CSV")})
                return(TRUE)
            }
        }
        output$validInfo<- renderText({paste("Not Valid CSV")})
        return(FALSE)
    }
        
        
        
    updatedMapType <- eventReactive(input$updateMap, {
        input$mapType}, ignoreNULL = FALSE
    )
    updatedFilename <- eventReactive(input$updateMap, {
        input$csvFile}, ignoreNULL = FALSE
    )
    
    output$leafletMap <- renderLeaflet({
        if (is.null(updatedFilename()))
            { leafletMap <- leaflet() %>% addTiles()}
        else{
            dataDF <- read.csv(updatedFilename()$datapath)
            if (updatedMapType() == "Base Stations"){
                if(checkFields(dataDF,"Base Stations")){
                    BSIcon <- makeIcon(
                        iconUrl = "antenna-3.png",
                        iconWidth = 31*215/230, iconHeight = 31,
                        iconAnchorX = 31*215/230/2, iconAnchorY = 16
                    )
                    return(BSmap<- dataDF %>% leaflet() %>%
                        addTiles()%>%
                        addMarkers(icon = BSIcon, lng = dataDF$LONGITUDE, lat = dataDF$LATITUDE,
                                   popup = paste("SITE:", dataDF$SITE, "<br>",
                                                 "LATITUDE:", dataDF$LATITUDE , "<br>",
                                                 "LONGITUDE:", dataDF$LONGITUDE , "<br>",
                                                 clusterOptions = markerClusterOptions())))
                }
                return(leafletMap <- leaflet() %>% addTiles())
            }
            else if (updatedMapType() == "TMS Points"){
                if(checkFields(dataDF,"TMS Points")){
                    return(MPmap <- dataDF %>% leaflet() %>% 
                     addTiles()%>%
                     addCircleMarkers(lng = dataDF$LONGITUDE, lat = dataDF$LATITUDE,
                                      popup = paste("LATITUDE",dataDF$LATITUDE, "<br>",
                                                    "LONGITUDE:",dataDF$LONGITUDE , "<br>",
                                                    "ZOOM_LEVEL:",dataDF$ZOOM_LEVEL , "<br>",
                                                    "XPIXEL_ID_ABSOLUTE:",dataDF$XPIXEL_ID_ABSOLUTE , "<br>",
                                                    "YPIXEL_ID_ABSOLUTE:",dataDF$YPIXEL_ID_ABSOLUTE , "<br>",
                                                    "TILE_QUADKEY:",dataDF$TILE_QUADKEY , "<br>",
                                                    "XTILE_STANDARD:",dataDF$XTILE_STANDARD , "<br>",
                                                    "YTILE_STANDARD:",dataDF$YTILE_STANDARD , "<br>",
                                                    "XPIXEL_ID_TILE_RELATIVE:",dataDF$XPIXEL_ID_TILE_RELATIVE , "<br>",
                                                    "YPIXEL_ID_TILE_RELATIVE:",dataDF$YPIXEL_ID_TILE_RELATIVE , "<br>",
                                                    "RESOLUTION_ZOOM_LEVEL:",dataDF$RESOLUTION_ZOOM_LEVEL , "<br>",
                                                    "RESOLUTION_LATITUDE_LONGITUDE:",dataDF$RESOLUTION_LATITUDE_LONGITUDE
                                                    )))
                }
                return(leafletMap <- leaflet() %>% addTiles())
            }
            else if (updatedMapType() == "KPI Value"){
                if(checkFields(dataDF,"KPI Value")){
                    pal <- colorNumeric("Spectral",reverse = TRUE, c(max(dataDF$VALUE),min(dataDF$VALUE)), na.color = "transparent")
                    return(RFMap <-  dataDF %>% leaflet() %>% addTiles() %>% addCircles(lng = dataDF$LONGITUDE, lat = dataDF$LATITUDE,color = pal(dataDF$VALUE), 
                                                                                        popup = paste("LATITUDE",dataDF$LATITUDE, "<br>",
                                                                                                      "LONGITUDE:",dataDF$LONGITUDE , "<br>",
                                                                                                      "KPI_VALUE:", dataDF$VALUE
                                                                                                      )
                                                                                        ) %>% addLegend(position = c("topright"), pal=pal,values = dataDF$VALUE,title = "KPI Value"))
                }   
                return(leafletMap <- leaflet() %>% addTiles())
            }
        }
        
   })
    output$selectedInputs<- renderText({
        paste("The selected map type is: ", updatedMapType(), "and the Csv file selected is: ",updatedFilename()$name)
    })
    output$validInfo<- renderText({
        paste("No Info")
    })
    
})
