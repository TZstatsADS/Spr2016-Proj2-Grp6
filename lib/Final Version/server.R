library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

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
  restroomIcon <- reactive({
   makeIcon(
      iconUrl = paste("https://github.com/TZstatsADS/project2-group6/blob/master/doc/toiletIcon/toilet", as.character(input$icon), ".png?raw=true", sep = ""),
     iconWidth = 25, iconHeight = 25,
     iconAnchorX = 13, iconAnchorY = 13
   ) %>%
      return()
  }) 
  
  a <- as.data.frame(decodeLine("qy}wF`dkbM`EpCdCaIgAs@qHaF{FuDyFuDgC~H^V"))
  
  # Add toilet and crime circles to map  
  observe({  
    pal1 <- colorFactor(palette()[-1], levels(crime$Offense))
    Radius1 <- 3
    if (input$addcrime == TRUE&length(as.matrix(cdata())) != 0){
      leafletProxy("map") %>%
        clearMarkers() %>%
        # addPolylines(lng=c(a$lon,a$lon1),lat=c(a$lat,a$lat1),color="red") %>%
        addMarkers(data = ttype(), ~Long, ~Lat, icon = restroomIcon(), options = markerOptions(opacity = 0.9), popup = ~Name) %>%
        addCircleMarkers(data = cdata(), ~Long, ~Lat, radius = Radius1, stroke = FALSE, fillOpacity = 0.7, fillColor = pal1(cdata()[["Offense"]])) %>%
        addLegend("bottomleft", pal=pal1, values=cdata()[["Offense"]], title="crime",
                  layerId="colorLegend")
    }
    else {
 
      leafletProxy("map") %>%
        clearMarkers() %>%
        # addPolylines(lng=c(a$lon,a$lon1),lat=c(a$lat,a$lat1),color="red") %>%
                addMarkers(data = ttype(), ~Long, ~Lat, icon = restroomIcon(), options = markerOptions(opacity = 0.9), 
                   popup = paste("*Name:", ttype()$Name, "<br>",
                                 "*Address:", ttype()$Address, "<br>")) #%>%
    }
  })
  
  # Show a circle at the given location
  show <- reactive({
    function(eventid, lat, lng) {
    leafletProxy("map") %>% addCircles(lng=lng,lat=lat, radius=input$circleR, fillColor="red",layerId = eventid, group="overlays")
    } %>%
      return()
  })
  
  # When mouseover, show a circle
  observe({
    leafletProxy("map") %>% clearGroup("overlays") # %>%
    # leafletProxy("map") %>%
    # addPolylines(lng=c(a$lon,a$lon1),lat=c(a$lat,a$lat1),color="red")
      # addPolylines(lng=a$lon,lat=a$lat,color="red")
    event <- input$map_marker_mouseover
    if (is.null(event))
      return()
    
#         opts(
#           panel.background = theme_rect(fill = "transparent",colour = NA), # or theme_blank()
#           panel.grid.minor = theme_blank(), 
#           panel.grid.major = theme_blank(),
#           plot.background = theme_rect(fill = "transparent",colour = NA)
#         )
    # })#,bg="transparent")
    isolate({
      show()(event$id, event$lat, event$lng)
    })
  })
  
  observe({
    #print(input$submit[1])
    if(input$submit[1] > 0) {
      # add <- isolate(input$address)
      
      xlatlon<-geoCode(input$address)
      xlat <- as.numeric(xlatlon[1])
      xlon <- as.numeric(xlatlon[2])
      leafletProxy("map") %>% 
        addMarkers(lng=xlon,lat=xlat)
      
      event <- input$map_marker_click
      if (is.null(event))
        return()
      
      #filtered_crime <- filter_crime(event$lat,event$lng,input$circleR)
      #output$totalcrime <- renderText({
        
      #  if (nrow(filtered_crime) == 0) {
      #    return(NULL)
      #  }
      #  paste("Total Number of Crimes:",nrow(filtered_crime))
      #})
      
      #output$circ_plot <- renderPlot({
      #  if (nrow(filtered_crime) == 0) {
      #    return(NULL)
      #  }
        
      #  levels(filtered_crime$Offense)[levels(filtered_crime$Offense) ==  "GRAND LARCENY OF MOTOR VEHICLE"] <- "GTA"
      #  ggplot(filtered_crime,aes(x=factor(1),fill=as.factor(Offense))) + geom_bar() + coord_polar(theta='y') + ylab("") + xlab("") +
      #    theme(panel.background= element_blank(),plot.background= element_blank())
      #})
      
      rlat <- event$lat
      rlon <- event$lng
      poly <- geoRoute(xlat,xlon,rlat,rlon)
      llatlon <- decodeLine(poly)
      # print(llatlon)
      leafletProxy("map") %>% clearShapes() %>%
        addPolylines(lng=llatlon$lon,lat=llatlon$lat,color="red") #%>%
      # addMarkers(lng=xlon,lat=xlat)
    }
  })
  
  observe({
    #print(input$submit[1])
    #if(input$submit[1] > 0) {
      # add <- isolate(input$address)
      
      #xlatlon<-geoCode(input$address)
      #xlat <- as.numeric(xlatlon[1])
      #xlon <- as.numeric(xlatlon[2])
      #leafletProxy("map") %>% 
      #  addMarkers(lng=xlon,lat=xlat)
  
      event <- input$map_marker_click
      if (is.null(event))
        return()
      
      filtered_crime <- filter_crime(event$lat,event$lng,input$circleR)
      output$totalcrime <- renderText({
        
        if (nrow(filtered_crime) == 0) {
          return(NULL)
        }
        paste("Total Number of Crimes:",nrow(filtered_crime))
      })
      
      output$circ_plot <- renderPlot({
        if (nrow(filtered_crime) == 0) {
          return(NULL)
        }
        
        levels(filtered_crime$Offense)[levels(filtered_crime$Offense) ==  "GRAND LARCENY OF MOTOR VEHICLE"] <- "GTA"
        ggplot(filtered_crime,aes(x=factor(1),fill=as.factor(Offense))) + geom_bar() + coord_polar(theta='y') + ylab("") + xlab("") +
          theme(panel.background= element_blank(),plot.background= element_blank())
      })
      
      #rlat <- event$lat
      #rlon <- event$lng
      #poly <- geoRoute(xlat,xlon,rlat,rlon)
      #llatlon <- decodeLine(poly)
      #leafletProxy("map") %>% clearShapes() %>%
      #  addPolylines(lng=llatlon$lon,lat=llatlon$lat,color="red") #%>%
      
    #}
  })
  
  # breaks <- hist(plot=FALSE, )
  

  
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