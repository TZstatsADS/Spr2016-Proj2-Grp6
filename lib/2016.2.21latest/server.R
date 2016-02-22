library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)

shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/v4/mapbox.streets/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiZnJhcG9sZW9uIiwiYSI6ImNpa3Q0cXB5bTAwMXh2Zm0zczY1YTNkd2IifQ.rjnjTyXhXymaeYG6r2pclQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
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
    iconUrl = "https://github.com/TZstatsADS/project2-group6/blob/master/doc/toiletIcon/toilet12.png?raw=true",
    iconWidth = 25, iconHeight = 25,
    iconAnchorX = 13, iconAnchorY = 13
  )
  
  # Add toilet and crime circles to map  
  observe({  
    pal1 <- colorFactor(palette()[-1], levels(crime$Offense))
    Radius1 <- 3
    if (input$addcrime == TRUE&length(as.matrix(cdata())) != 0){
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(data = ttype(), ~Long, ~Lat, icon = restroomIcon, options = markerOptions(opacity = 0.9), popup = ~Name) %>%
        addCircleMarkers(data = cdata(), ~Long, ~Lat, radius = Radius1, stroke = FALSE, fillOpacity = 0.7, fillColor = pal1(cdata()[["Offense"]])) %>%
        addLegend("bottomleft", pal=pal1, values=cdata()[["Offense"]], title="crime",
                  layerId="colorLegend")
    }
    else {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(data = ttype(), ~Long, ~Lat, icon = restroomIcon, options = markerOptions(opacity = 0.9), 
                   popup = paste("*Name:", ttype()$Name, "<br>",
                                 "*Address:", ttype()$Address, "<br>"))
    }
  })
  
  ## Dynamic Map ###########################################
  
  # Create the map
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/v4/mapbox.dark/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiZnJhcG9sZW9uIiwiYSI6ImNpa3Q0cXB5bTAwMXh2Zm0zczY1YTNkd2IifQ.rjnjTyXhXymaeYG6r2pclQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -73.97, lat = 40.75, zoom = 13)
  })
  
  
  # Filter crime data
  drawvalue <- reactive({
    if (input$offense == ''){
      t <- filter(crime, minute == input$minute)
      return(t)
    }
    else{
      t <- filter(crime, Offense == input$offense, minute==input$minute)
      return(t)
    }})
  
  observe({
    draw <- drawvalue()
    pal <- colorFactor(palette()[-1], levels(crime$Offense))
    radius <-  50
    if (length(as.matrix(draw)) != 0) {
      leafletProxy("map2", data = draw) %>%
        clearShapes() %>%
        addCircles(~Long, ~Lat, radius=radius,
                   stroke=FALSE, fillOpacity=0.8,fillColor=pal(draw[["Offense"]])) %>%
        addLegend("bottomleft", pal=pal, values=levels(draw[["Offense"]]), layerId="colorLegend")
    }
    else {
      leafletProxy("map2", data = draw) %>%
        clearShapes()
    }
    
    
  })
  
})