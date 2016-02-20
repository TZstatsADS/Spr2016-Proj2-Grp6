library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

draw <- cleantable

shinyServer(function(input, output, session) {
  
## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -73.97, lat = 40.75, zoom = 13)
  })

# Add toilet markers to map  
  drawvalue <- reactive({if (input$handicap == ''){return(cleantable)}else{
    t <- filter(cleantable, Handicap == input$handicap | Handicap == input$handicap)
    return(t)
  }})
  
 observe({  
   colorBy <- input$type
#   sizeBy <- input$size
  # draw <- drawvalue()
 
#   pal <- colorBin(heat.colors(7), c(0:7), 7)
   pal <- "red"
   Radius <- 5
  leafletProxy("map", data = cleantable) %>%
    #clearShapes() %>%
    #hideGroup('Cluster') %>%
    addCircleMarkers(~Long, ~Lat, radius = Radius, stroke = FALSE, fillOpacity = 0.8, fillColor = pal) 
  #%>%
    #addLegend("bottomleft", pal=pal, values=pal, title=colorBy,
    #          layerId="colorLegend")
 }) 
})
