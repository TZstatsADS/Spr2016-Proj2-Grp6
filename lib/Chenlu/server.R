library(shiny)
library(leaflet)
library(RColorBrewer)
#library(scales)
#library(lattice)
library(dplyr)

shinyServer(function(input, output, session) {
  
## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
    addTiles(
        urlTemplate = "https://api.mapbox.com/v4/mapbox.streets/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiZnJhcG9sZW9uIiwiYSI6ImNpa3Q0cXB5bTAwMXh2Zm0zczY1YTNkd2IifQ.rjnjTyXhXymaeYG6r2pclQ"
      ) %>%
    setView(lng = -73.97, lat = 40.75, zoom = 13)
  })


  # Select toilet type, multiple selections are allowed
  ttype <- reactive({
    t <- toilet
    if (input$handicap == TRUE){
      t <- filter(t, Handicap == "Yes")
    }
    if (input$yearround == TRUE){
      t <- filter(t, Yearround == "Yes")
    }
    return(t)
  })
  
  # Filter crime data
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
    
    return(draw)
  })
  
  # Create toiletIcon
  restroomIcon <- makeIcon(
    iconUrl = "https://github.com/TZstatsADS/project2-group6/blob/master/doc/restroomIconr.png?raw=true",
    #iconUrl = "https://github.com/TZstatsADS/project2-group6/blob/master/doc/restroomIcong.png?raw=true",
    iconWidth = 30, iconHeight = 30,
    iconAnchorX = 15, iconAnchorY = 15
    #shadowUrl = "http://leafletjs.com/docs/images/leaf-shadow.png",
    #shadowWidth = 50, shadowHeight = 64,
    #shadowAnchorX = 4, shadowAnchorY = 62
  )
  
  # Add toilet and crime circles to map  
  observe({  
    pal1 <- colorFactor(palette()[-1], levels(crime$Offense))
    Radius1 <- 3
    if (input$addcrime == TRUE&length(as.matrix(cdata())) != 0){
        leafletProxy("map") %>%
          clearMarkers() %>%
          addMarkers(data = ttype(), ~Long, ~Lat, icon = restroomIcon, options = markerOptions(opacity = 0.8), popup = ~Name) %>%
          addCircleMarkers(data = cdata(), ~Long, ~Lat, radius = Radius1, stroke = FALSE, fillOpacity = 0.7, fillColor = pal1(cdata()[["Offense"]])) %>%
          addLegend("bottomleft", pal=pal1, values=cdata()[["Offense"]], title="crime",
                    layerId="colorLegend")
    }
    else {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(data = ttype(), ~Long, ~Lat, icon = restroomIcon, options = markerOptions(opacity = 0.8), popup = ~Name)
    }
  }) 
})
