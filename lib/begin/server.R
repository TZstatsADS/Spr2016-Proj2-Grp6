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
suppressMessages(library(parallel))
suppressMessages(library(xts)) #added this for trends
suppressMessages(library(stringr)) #added this for time, not sure if still needed
suppressMessages(library(gtable)) #added this for trends
library(bfast)
library(forecast)
load(file = "./data/weather.rda")
load(file = "./data/crimesfull2.rda")
#load(file = "./data/crimes2014.rda")
load(file = "./data/IUCR.rda")
load(file = "./data/community2.RDA")

#XTS Melt to use HighCharts
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


## Define server logic required to summarize and view the selected dataset
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

#Not Using
#   trafficr <- reactive ({
#                 URL <- 'http://data.cityofchicago.org/resource/n4j6-wkkf.json'
#                 temptraffic <- fromJSON(file=URL, method='C')
#                 temptraffic2 <- lapply(temptraffic, function(x) {
#                   x[sapply(x, is.null)] <- NA
#                   unlist(x)
#                 })
#                 temptraffic <- rbind(temptraffic2, cbind(start_lon))
#                 temptraffic3 <-   do.call("rbind", temptraffic2)
#                 return (temptraffic)
#                   })
  

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output 1 - Data Table
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$datatable <- renderDataTable({
   
    datetypesubset() 
  
    }, options = list(aLengthMenu = c(10, 25, 50, 100, 1000), iDisplayLength = 10))
  

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
      messaging = FALSE,
    )
    
    ## Convert the base map into a ggplot object
    ## All added Cartesian coordinates to enable more geom options later on
    map.base <- ggmap(map.base, extend = "panel", messaging = FALSE) + coord_cartesian() + coord_fixed(ratio = 1.5)
   
 p <- map.base + geom_point(aes(x=Longitude, y=Latitude), colour="red", size = 4, na.rm=TRUE, data=crimetypedatabase)
  
 plot(p)
  })
 #, width = 1800, height = 1800)
  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## WEATHER
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 output$weather <- renderPlot({  

  crimebytime <-crimebytimeXTS()
  crimebytime<-data.frame(index(crimebytime),coredata(crimebytime[,1]))
    colnames(crimebytime)<-c("dates","crime")
  #print(crimebytime)
  
 ##ADD WEATHER
 weatherdata <- subset(weatherdata, PosixDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & PosixDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")))
 weatherxts <- xts(weatherdata$TempFahr,weatherdata$PosixDate)
 weatherxts<-data.frame(index(weatherxts),coredata(weatherxts[,1]))
 colnames(weatherxts)<-c("dates","temperature")
 
 #Use central average to smooth out the data
 if (input$period == "Weekly") {mavwindow=3}
 if (input$period== "Monthly") {mavwindow=9}
 if (input$period == "Yearly") {mavwindow=31}
 if (input$period != "Daily") { mav <- function(x,n=mavwindow){filter(x,rep(1/n,n), sides=2)}
                              weatherxts$temperature <- mav(weatherxts$temperature)}                           
   
#New approach to get two Y lines:
grid.newpage()

# two plots
# from http://rpubs.com/kohske/dual_axis_in_ggplot2

p1 <-ggplot(crimebytime,aes(dates,crime)) + geom_line(colour="red", size=2) + theme_bw()
p2 <-ggplot(weatherxts,aes(dates,temperature)) + geom_line(colour="blue", size=2) + theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)

#print(data3)
  }, width = 1280, height = 1280)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ANALYSIS
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$analysis <- renderChart2({

  crimebytime <-crimebytimeXTS()
  
  #Convert data using xtsMelt for highcharts plot
  ust.melt <- na.omit(xtsMelt(crimebytime))
  ust.melt$date2 <- as.Date(ust.melt$date, format = "%Y-%m-%d")
  ust.melt$Crime <- as.numeric(as.character(ust.melt$value))
  ust.melt$date4  <- as.numeric(as.POSIXct(ust.melt$date2, origin="1970-01-01")) * 1000
  
  #Highchart plot
  h1 <- hPlot(
    Crime ~ date4,  #or x="date", y="value"
    data = ust.melt, 
    color = '#4572A7',
    type = 'spline',
    title = paste("Crimes for ",input$crimetype)
  ) 
  h1$xAxis(type = "datetime")
  
  h1
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Output - Heat Crime Map
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$hmaptitle <- renderUI({helpText(HTML("<b>HEAT MAP SETTINGS</b>"))})
output$hmapcenter <- renderUI({textInput("center", "Enter a Location to Center Map, such as city or zipcode, the click Update", "Chicago")})
output$hmaptype <- renderUI({selectInput("type", "Choose Google Map Type:", choice = c("roadmap", "satellite", "hybrid","terrain"))})
output$hmapres <- renderUI({checkboxInput("res", "High Resolution?", FALSE)})
output$hmapbw <- renderUI({checkboxInput("bw", "Black & White?", FALSE)})
output$hmapzoom <- renderUI({sliderInput("zoom", "Zoom Level (Recommended - 13):", min = 9, max = 20, step = 1, value = 13)})
output$halpharange <-renderUI({sliderInput("halpharanage", "Alpha Range:",
                    min = 0, max = 1, step = 0.1, value = c(0.1, 0.4))})
output$hbins <-renderUI({sliderInput("hbins", "Number of Bins:", 
                      min = 5, max = 50, step = 5, value = 15)})
output$hboundwidth <-renderUI({sliderInput("hboundwidth", "Boundary Lines Width:", 
                      min = 0, max = 1, step = 0.1, value = 0.1)})
output$hboundcolor <-renderUI({selectInput("hboundcolor", "Boundary Lines Colour:", 
                      choice = c("grey95","black", "white", "red", "orange", "yellow", "green", "blue", "purple"))})
output$hlow <-renderUI({selectInput("hlow", "Fill Gradient (Low):", 
                      choice = c("yellow", "red", "orange", "green", "blue", "purple", "white", "black", "grey"))})
output$hhigh <-renderUI({selectInput("hhigh", "Fill Gradient (High):", 
                      choice = c("red", "orange", "yellow", "green", "blue", "purple", "white", "black", "grey"))})



output$heatmap <- renderPlot({
  crimetypedatabase <- datetypesubset() 
  
  # Set Defaults for when Map starts
  #if (is.null(input$center)) {map.center <- geocode("Chicago")}
 # else {map.center = geocode(input$center)}
  
  if (is.null(input$hmapcenter))  {
                  #Center based on data if Null
                  map.center <- head(crimetypedatabase,n=2)
                  map.center <- map.center[c("Longitude","Latitude")]
                  names(map.center)[1]<-"lon"
                  names(map.center)[2]<-"lat"
                  map.center <- map.center[-1,]
                }                          
      else   {
              #Center based if people leave default Chicago
                if (match(input$hmapcenter,"Chicago",nomatch=0)) {
                  print("hi")
                  print(input$center)
                  #Center Map on Area of interest
                  map.center <- head(crimetypedatabase,n=2)
                  map.center <- map.center[c("Longitude","Latitude")]
                  names(map.center)[1]<-"lon"
                  names(map.center)[2]<-"lat"
                  map.center <- map.center[-1,]}
                                                 
                  #Use the value in the box provided
                 else 
                  {map.center <- geocode(input$hmapcenter)}
         }
  
  
  
  if (is.null(input$bw)) {temp.color <- "color"}
  else {
    temp.color <- "color"
    if (input$bw) {temp.color <- "bw"}}
  
  if (is.null(input$res)) {temp.scale <- 2}
  else {
    temp.scale <- 1
    if (input$res) {temp.scale <- 2}}
  
  if (is.null(input$zoom)) {temp.zoom <- 13}
  else {temp.zoom <- input$zoom }
  
  if (is.null(input$halpharange)) {temp.halpharange <- c(0.1, 0.4)}
  else {temp.halpharange<- input$halpharange}
  
  if (is.null(input$hbins)) {temp.hbins <- 15}
  else {temp.hbins<- input$hbins}
  
  if (is.null(input$hboundwidth)) {temp.hboundwidth <- .1}
  else {temp.hboundwidth<- input$hboundwidth}

  if (is.null(input$hboundcolor)) {temp.hboundcolor <- "grey95"}
  else {temp.hboundcolor<- input$hboundcolor}

  if (is.null(input$hlow)) {temp.hlow <- "yellow" }
  else {temp.hlow<- input$hlow}

  if (is.null(input$hhigh)) {temp.hhigh <- "red"}
  else {temp.hhigh<- input$hhigh}
  
  #Get Base Map
  map.base <- get_googlemap(
    as.matrix(map.center),
    maptype = input$type, ## Map type as defined above (roadmap, terrain, satellite, hybrid)
    # markers = map.center,
    zoom = temp.zoom,            ## 14 is just about right for a 1-mile radius
    color = temp.color,   ## "color" or "bw" (black & white)
    scale = temp.scale,  ## Set it to 2 for high resolution output
    messaging = FALSE,
  )
  
  ## Convert the base map into a ggplot object
  ## All added Cartesian coordinates to enable more geom options later on
  map.base <- ggmap(map.base, extend = "panel", messaging = FALSE) + coord_cartesian() + coord_fixed(ratio = 1.5)
  
  ## add heat map
  map.final <- map.base  +    
    
    ## Create a density plot
    ## based on the ggmap's crime data example
    stat_density2d(aes(x = Longitude, 
                       y = Latitude, 
                       fill = ..level.., 
                       alpha = ..level..),
                   size = temp.hboundwidth, 
                   bins = temp.hbins,  ## Change and experiment with no. of bins
                   data = crimetypedatabase, 
                   geom = "polygon", 
                   colour = temp.hboundcolor) +
    
    ## Configure the scale and panel
    scale_fill_gradient(low = temp.hlow, high = temp.hhigh) +
    scale_alpha(range = temp.halpharange) +
    
    ## Title and labels    
    labs(x = "Longitude", y = "Latitude") +
   # ggtitle(paste("Crimes in/around ", map.center))+ 
             #     " from ", temp.period[1],
             #     " to ", temp.period[length(temp.period)], sep="")) +
    
    ## Other theme settings
    theme_bw() +
    theme(
      plot.title = element_text(size = 36, face = 'bold', vjust = 2),
      #title = element_text(face = 'bold'),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      #axis.text.x = element_text(size = 28),
      #axis.text.y = element_text(size = 28),
      #axis.title.x = element_text(size = 32),
      #axis.title.y = element_text(size = 32),
      strip.background = element_rect(fill = 'grey80'),
      strip.text = element_text(size = 26)
    )

  plot(map.final)
  })

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Output - Trends
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## DECOMPOSE
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$decomintro <- renderUI({helpText(HTML("<br><b>The Time Series analysis requires a large number of data points to run.  
                                              If you get an error, try again after enlarging the data.
                                             The first plot shows overall trend.  The second plot (Tt) shows abrupt changes in crime. </b>"))})

output$decomplot <- renderPlot({ 
  # GET XTS data
  crimebytime <-crimebytimeXTS()
  
  #Set frequence for time series
  n <- 1
  if (input$period == "Daily") {n <- 365}
  if (input$period == "Weekly") {n <- 52}
  if (input$period == "Monthly") {n <- 12}
  if (input$period == "Yearly") {n <- 1}
  
  #Convert to time series
  crimebytimeTS <- ts(crimebytime,frequency=n)
  
  #Decompose
  f <- decompose(crimebytimeTS)
  plot(f, sub = "Decomposition that shows overall trend")
})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Trends with bfast
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


output$trends <- renderPlot({ 
        # GET XTS data
        crimebytime <-crimebytimeXTS()
      
        #Set frequence for time series
        n <- 1
        if (input$period == "Daily") {n <- 365}
        if (input$period == "Weekly") {n <- 52}
        if (input$period == "Monthly") {n <- 12}
        if (input$period == "Yearly") {n <- 1}
        
        #Convert to time series
        crimebytimeTS <- ts(crimebytime,frequency=n)
        (dim(crimebytimeTS) <- NULL)
        #Decompose
        d <- bfast(crimebytimeTS,season="dummy",max.iter=1)
        
        plot(d, main = "Break detection highlights abrupt changes")
    })

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Trends with bfast
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##NOT WORKING YET
output$bfastmonitor <- renderText({ 
  # GET XTS data
  crimebytime <-crimebytimeXTS()
  
  #Set frequence for time series
  n <- 1
  if (input$period == "Daily") {n <- 365}
  if (input$period == "Weekly") {n <- 52}
  if (input$period == "Monthly") {n <- 12}
  if (input$period == "Yearly") {n <- 1}
  
  #Convert to time series
  crimebytimeTS <- ts(crimebytime,frequency=n)
  (dim(crimebytimeTS) <- NULL)
  #Decompose
  w <- bfastmonitor(crimebytimeTS, start = c(2014, 1))
  if (is.null(w$breakpoint)) {
    print ("hi")
    return() }
  else {
  paste("You have selected", w$breakpoint)
  }
})


#Server
######################## NEW MAP
############ Not working

output$map2 <- renderMap({
  df3 <- datetypesubset()
  df3 <- rename(df3, c("Latitude"="lat", "Longitude"="lon"))
  
  #Get the center from the first value
  map.center <- head(df3,n=1)
  map.center <- map.center[c("lat","lon")]
  
  
  #Get text for labels
  #collisiontype <- (df3$collisiontypecode)
  #collisioncodes <- c(Pedestrian=1, Pedalcyclist=2, Train=3, Animal=4,Overturned=5, 
  #                    "Fixed Object"=6, "Other Object"=7, "Other non-collision"=8, "Parked Motor vehicle"=9, Turning= 10,
  #                   "Read-end"=11, "Sideswipe-same direction"=12, "Sideswipe-opposite direction"=13, "Head-on"=14, Angle=15)
  #df3$CollisionType <- names(collisioncodes)[match(collisiontype,collisioncodes)]
  
  #Infor for popup tags
  df3$color <- "#050505"
  df3$popup <- paste0("<p>Primary Type:  ", df3$Primary.Type, 
                      "<br>Case Number:  ", df3$Case.Number) 
  
  #Convert to list for JSON
  tmp.data <- apply(df3, 1, as.list)
  
  
  mapc <- Leaflet$new()
  mapc$setView(c(map.center$lat,map.center$lon), zoom = 12)
  #mapc$setView(c(41,-87), zoom = 13)
  mapc$tileLayer(provider = 'Stamen.TonerLite')
  mapc$tileLayer(provider = 'OpenStreetMap.Mapnik')
  mapc$addAssets(
    jshead = "https://github.com/SINTEF-9012/PruneCluster/blob/master/dist/PruneCluster.js"
  )
  mapc$addLayer(pruneCluster)
  mapc$geoJson(toGeoJSON(tmp.data, lat = 'lat', lon = 'lon'),
               onEachFeature = '#! function(feature, layer){
               layer.bindPopup(feature.properties.popup)
} !#',
               pointToLayer =  "#! function(feature, latlng){
               return L.circleMarker(latlng, {
               radius: 8,
               fillColor: feature.properties.color || 'red', 
               color: '#FF0000',
               weight: 1,
               fillOpacity: 0.8
               })
} !#"           
)
# map$marker(
#    c(map.center$lat,map.center$lon),
#    bindPopup = 'Hi. I am a popup'
#  )
mapc$enablePopover(TRUE)
mapc
})



  })
    
