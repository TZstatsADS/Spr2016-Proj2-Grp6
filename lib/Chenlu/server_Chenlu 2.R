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
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -73.97, lat = 40.75, zoom = 13)
  })

 # Select toilet type, multiple selections are allowed
  ttype <- reactive({
    t <- cleantable
   if (input$handicap == TRUE){
        t <- filter(t, Handicap == "Yes")
    }
   if (input$yearround == TRUE){
       t <- filter(t, Yearround == "Yes")
    }
    return(t)
  })

  # Add toilet circles to map  
 observe({  
   pal1 <- "red"
   pal2 <- "black"
   Radius1 <- 100
   Radius2 <- 10
   if (input$crime == TRUE){
     leafletProxy("map", data = ttype()) %>%
       clearShapes() %>%
       addCircles(~Long, ~Lat, radius = Radius1, stroke = FALSE, fillOpacity = 0.8, fillColor = pal1) %>%
       addCircles(data = crime, ~Long, ~Lat, radius = Radius2, stroke = FALSE, fillOpacity = 0.8, fillColor = pal2)
   }
   else {
     leafletProxy("map", data = ttype()) %>%
       clearShapes() %>%
       addCircles(~Long, ~Lat, radius = Radius1, stroke = FALSE, fillOpacity = 0.8, fillColor = pal1) 
   }
    }) 
})
