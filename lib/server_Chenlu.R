suppressMessages(library(shiny))
suppressMessages(library(ggplot2))
suppressMessages(library(ggmap))
suppressMessages(library(RJSONIO))
suppressMessages(library(png))
suppressMessages(library(grid))
suppressMessages(library(RCurl))
suppressMessages(library(plyr))
suppressMessages(library(markdown))
suppressMessages(library(rCharts))
suppressMessages(library(doParallel))
suppressMessages(library(xts)) #added this for trends
suppressMessages(library(stringr)) #added this for time, not sure if still needed
suppressMessages(library(gtable)) #added this for trends
library(bfast)
library(forecast)
#load(file = "../data/weather.rda")
load(file = "../data/crimesfull2.rda")

xtsMelt <- function(data) {
  require(reshape2)
  #translate xts to time series to json with date and data
  #for this behavior will be more generic than the original
  #data will not be transformed, so template.rmd will be changed to reflect
  #convert to data frame
  data.df <- data.frame(cbind(format(index(data),"%Y-%m-%d"),coredata(data)))
  colnames(data.df)[1] = "date"
  data.melt <- melt(data.df,id.vars=1,stringsAsFactors=FALSE)
  colnames(data.melt) <- c("date","indexname","value")
  #remove periods from indexnames to prevent javascript confusion
  #these . usually come from spaces in the colnames when melted
  data.melt[,"indexname"] <- apply(matrix(data.melt[,"indexname"]),2,gsub,pattern="[.]",replacement="")
  return(data.melt)
  #return(df2json(na.omit(data.melt)))
}

shinyServer(function(input, output) {
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reactive Functions
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #Subset by date, primary type, and community area
  datetypesubset <- reactive({
    tempdate   <- subset(df, PosixDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & PosixDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")))
    
    if (input$crimetype == "VIOLENCE") {
      temp <- subset(IUCR,ViolentCrime == 1)
      violence <- temp$IUCR
      tempdatetype <- subset(tempdate, IUCR %in% violence)
      tempdatetype$Primary.Type <- "VIOLENCE"
      return (tempdatetype)
    }
    
    if (input$crimetype == "PROPERTYCRIME") {
      temp <- subset(IUCR,PropertyCrime == 1)
      property <- temp$IUCR
      tempdatetype <- subset(tempdate, IUCR %in% property)
      tempdatetype$Primary.Type <- "PROPERTYCRIME"
      return (tempdatetype)
    }
    
    tempdate <- subset(tempdate, Primary.Type == input$crimetype)
    
    if (input$community != "Chicago-All") {
      temp.inter <- grep (input$community,community1$Community.Area)
      tempdate <- subset(tempdate, Community.Area == temp.inter)
    }
    
    #Not working - need to align the Primary description between IUCR and crime data
    #                  temp <- subset(IUCR,'Primary Description' == input$crimetype)
    #                  crime <- temp$IUCR
    #                  print (crime)
    #                  tempdatetype <- subset(tempdate, IUCR %in% crime)
    #                  tempdatetype$Primary.Type <- input$crimetype
    
    
    return (tempdate)
  })  

  #Creates XTS object with totals by user selected period
  crimebytimeXTS <- reactive({
    dfin <- datetypesubset()
    df.xts <- xts(x = dfin[, c("Primary.Type","PosixDate")], order.by = dfin[, "PosixDate"])
    if (input$period == "Daily") {crimebytime <- apply.daily(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
    if (input$period == "Weekly") {crimebytime <- apply.weekly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
    if (input$period == "Monthly") {crimebytime <- apply.monthly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
    if (input$period == "Yearly") {crimebytime <- apply.yearly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
    df.xts <- NULL
    return(crimebytime)
  })

## ~~~~~~ output$datatable <- renderDataTable({
  output$datatable <- renderDataTable({
    
    datetypesubset() 
    
  }, options = list(aLengthMenu = c(10, 25, 50, 100, 1000), iDisplayLength = 10))


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Output 2 - Map
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$maptitle <- renderUI({helpText(HTML("<b>MAP SETTINGS</b>"))})
output$mapcenter <- renderUI({textInput("center", "To Re-Center Map, Enter a Location such as city or zipcode, then click Update", "Chicago")})
output$maptype <- renderUI({selectInput("type", "Choose Google Map Type:", choice = c("roadmap", "satellite", "hybrid","terrain"))})
output$mapres <- renderUI({checkboxInput("res", "High Resolution?", FALSE)})
output$mapbw <- renderUI({checkboxInput("bw", "Black & White?", FALSE)})
output$mapzoom <- renderUI({sliderInput("zoom", "Zoom Level (Recommended - 14):", min = 9, max = 20, step = 1, value = 14)})

output$map <- renderPlot({
  
  # Set Defaults for when Map starts
  
  if (is.null(input$bw)) {temp.color <- "color"}
  else {
    temp.color <- "color"
    if (input$bw) {temp.color <- "bw"}}
  
  if (is.null(input$res)) {temp.scale <- 2}
  else {
    temp.scale <- 1
    if (input$res) {temp.scale <- 2}}
  
  if (is.null(input$zoom)) {temp.zoom <- 14}
  else {temp.zoom <- input$zoom }
  
  ## add crime points
  crimetypedatabase <- datetypesubset() 
  
  #Center Map Code
  if (is.null(input$center))  {
    #Center based on data if Null
    map.center <- head(crimetypedatabase,n=2)
    map.center <- map.center[c("Longitude","Latitude")]
    names(map.center)[1]<-"lon"
    names(map.center)[2]<-"lat"
    map.center <- map.center[-1,]
  }                          
  else   {
    #Center based if people leave default Chicago
    if (match(input$center,"Chicago",nomatch=0)) {
      print("hi")
      print(input$center)
      #Center Map on Area of interest
      map.center <- head(crimetypedatabase,n=2)
      map.center <- map.center[c("Longitude","Latitude")]
      names(map.center)[1]<-"lon"
      names(map.center)[2]<-"lat"
      map.center <- map.center[-1,]
    } 
    
    #Use the value in the box provided
    else 
    {map.center <- geocode(input$center)}
  }

  
  #Get Base Map
  map.base <- get_googlemap(
    as.matrix(map.center),
    maptype = input$type, ## Map type as defined above (roadmap, terrain, satellite, hybrid)
    # markers = map.center,
    zoom = temp.zoom,            ## 14 is just about right for a 1-mile radius
    color = temp.color,   ## "color" or "bw" (black & white)
    scale = temp.scale,  ## Set it to 2 for high resolution output
    messaging = FALSE
  )
  
  ## Convert the base map into a ggplot object
  ## All added Cartesian coordinates to enable more geom options later on
  map.base <- ggmap(map.base, extend = "panel", messaging = FALSE) + coord_cartesian() + coord_fixed(ratio = 1.5)
  
  
  plot(p)

})})