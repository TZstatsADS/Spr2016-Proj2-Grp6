library(devtools)
#install_github('arilamstein/choroplethrZip@v1.3.0')
library(choroplethrZip)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)


shinyServer(function(input, output, session) {
        
        ## Interactive Map ###########################################
        
        # Create the map
        output$map <- renderLeaflet({
                leaflet() %>%
                        addTiles(
                                urlTemplate = "https://api.mapbox.com/v4/mapbox.dark/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiZnJhcG9sZW9uIiwiYSI6ImNpa3Q0cXB5bTAwMXh2Zm0zczY1YTNkd2IifQ.rjnjTyXhXymaeYG6r2pclQ"
                                #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                                #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                        ) %>%
                        setView(lng = -73.97, lat = 40.75, zoom = 13)
        })
        
        
        # Select crime type, multiple selections are allowed
#        ctype1 <- reactive({
#                if (input$crime == '') {
#                        return(crime)
#                }
#                else {
#                t <- filter(crime, Offense == input$crime)
#                return(t)
#        }})
#        
#        ctype2 <- reactive({
#                if (input$month == '') {
#                        return(ctype1())
#                }
#                else {
#                        t <- filter(ctype1(), Occurrence.Month == input$month)
#                        return(t)
#                }})
#        
#        ctype3 <- reactive({
#                if (input$day == '') {
#                        return(ctype2())
#                }
#                else {
#                        t <- filter(ctype2(), Occurrence.Day == input$day)
#                        return(t)
#                }})
#        
#        ctype4 <- reactive({
#                if (input$hour == '') {
#                        return(ctype3())
#                }
#                else {
#                        t <- filter(ctype3(), Occurrence.Hour == input$hour)
#                        return(t)
#                }})

        cdata <- reactive({
                draw <- crime
                if (input$crime != '') {
                        t <- filter(draw, Offense == input$crime)
                        draw <- t
                }
                
                if (input$month != '') {
                        t <- filter(draw, Occurrence.Month == input$month)
                        draw <- t
                }
                
                if (input$day != '') {
                        t <- filter(draw, Occurrence.Day == input$day)
                        draw <- t
                }
                
                if (input$hour != '') {
                        t <- filter(draw, Occurrence.Hour == input$hour)
                        draw <- t
                }
                
                #if (length(as.matrix(draw)) == 0) {
                #        draw <- data.frame()
                #}
                return(draw)
        })
        
        
        
        
                
        #colorBy <- input$crime
        #colorData <- ctype1()[[colorBy]]
        # Add crime circles to map 
        
        observe({  
                #pal <- "red"
                pal <- colorFactor(palette()[-1], levels(crime$Offense))
                Radius <- 10
                if (length(as.matrix(cdata())) == 0) {
                        leafletProxy("map") %>%
                                clearShapes()
                }
                else {
                        leafletProxy("map", data = cdata()) %>%
                                clearShapes() %>%
                                #showGroup('crime') %>%
                                addCircles(~Long, ~Lat, radius = Radius, stroke = FALSE, fillOpacity = 0.8, fillColor = pal(cdata()[["Offense"]])) %>%
                                addLegend("bottomleft", pal=pal, values=cdata()[["Offense"]], title="crime",
                                          layerId="colorLegend")
                }
        }) 
        
        
})
