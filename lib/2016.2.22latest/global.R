
library(dplyr)

# Data cleaning to get rid of records with no geo info
# Original data NYPD_Motor_Vehicle_Collisions.csv is 119.5 MB, which is too large to be included in this repo

############### Code ##############

# Import toilet data (pt is short for public toilet)

decodeLine <- function(encoded){
  require(bitops)
  
  vlen <- nchar(encoded)
  vindex <- 0
  varray <- NULL
  vlat <- 0
  vlng <- 0
  
  while(vindex < vlen){
    vb <- NULL
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen){
        vindex <- vindex + 1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      # print(vb)
      if(vb < 32) break
    }
    
    dlat <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlat <- vlat + dlat
    
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex <- vindex+1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlng <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlng <- vlng + dlng
    
    varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
  }
  coords <- data.frame(varray)
  names(coords) <- c("lat", "lon")
  coords
}

filter_crime<-function(lat,lng,radius){
  latlimit<-0.003619/400*radius
  lnglimit<-0.000316/400*radius
  latrange_upper<-lat+latlimit
  latrange_lower<-lat-latlimit
  lngrange_upper<-lng+lnglimit
  lngrange_lower<-lng-lnglimit
  crime_sub<-filter(crime, Lat>latrange_lower,Lat<latrange_upper,
                    Long>lngrange_lower,Long<lngrange_upper)
  return(crime_sub)
}
# lat=40.748730
# lng=-73.988315
# filter_crime(lat,lng)



url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}


geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}


url2 <- function(ori_lat,ori_lng,des_lat,des_lng,return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/directions/"
  u <- paste(root, return.call, "?origin=", ori_lat,"+",ori_lng,"&destination=",des_lat,"+",des_lng,"&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoRoute <- function(ori_lat,ori_lng,des_lat,des_lng) {
  #if(verbose) cat(address,"\n")
  u <- url2(ori_lat,ori_lng,des_lat,des_lng)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    route <- x$route[[1]]$overview_polyline$points
    return(route)
    Sys.sleep(0.5)
  } else {
    return(NA)
  }
}








pt <- read.csv("data/publictoilet.csv")
pt$LAT <- as.character(pt$LAT)
pt$LNG <- as.character(pt$LNG)
options(digits=15)
pt$LAT <- as.numeric(pt$LAT)
pt$LNG <- as.numeric(pt$LNG)
# Import columns and change column names
toilet <- pt
toilet <- toilet %>%
  select(
    Name = NAME,
    Address = ADDRESS,
    Yearround = OPEN_YEAR_ROUND,
    Handicap = HANDICAP_ACCESSIBLE,
    Indicator = indicator,
    Lat1 = LAT,
    Long1 = LNG,
    Lat2 = LAT1,
    Long2 = LNG1)

# Obtain observation numbers
n <- dim(toilet)[1]
toilet$Lat <- numeric(length = n)
toilet$Long <- numeric(length = n)
# locate each toilet's latitude and longitude
for (i in 1:n)
{
  if(toilet$Indicator[i] == 1){
    toilet$Lat[i] <- toilet$Lat1[i]
    toilet$Long[i] <- toilet$Long1[i]
  }
  if(toilet$Indicator[i] == 2){
    toilet$Lat[i] <- toilet$Lat2[i]
    toilet$Long[i] <- toilet$Long2[i]
  }  
}

# Remove observations with NA in Lat or Long
toilet <- toilet[complete.cases(toilet[,c("Lat","Long")]),]

# remove redundant columns
drops <- c("Lat1","Long1","Lat2","Long2")
toilet <- toilet[,!(names(toilet) %in% drops)]

##################################
# Import crime data 
crime <- read.csv("data/NYPD_7_Major_Felony_Incidents.csv", header = T)
crime <- crime[which(crime$Occurrence.Year == 2015), ]
crime <- na.omit(crime)
crime <- crime[ , c("Occurrence.Date", "Day.of.Week", "Occurrence.Month", "Occurrence.Day", "Occurrence.Year", "Occurrence.Hour", "Offense", "Borough", "XCoordinate", "YCoordinate", "Location.1")]

location <- unlist(strsplit(as.character(crime$Location.1), split=c(", ")))
index1 <- seq(1, length(location), 2)
index2 <- seq(2, length(location), 2)
crime$Lat <- location[index1]
crime$Lat <- unlist(strsplit(crime$Lat, "\\("))[index2]
crime$Long <- location[index2]
crime$Long <- unlist(strsplit(crime$Long, "\\)"))
options(digits=15)
crime$Lat <- as.numeric(crime$Lat)
crime$Long <- as.numeric(crime$Long)

# Data for tab 2.
crime$hour <- as.numeric(format(as.POSIXct(crime$Occurrence.Date,format="%m/%d/%Y %I:%M:%S %p"),"%H"))
hour.minute <- cbind(as.numeric(format(as.POSIXct(crime$Occurrence.Date,format="%m/%d/%Y %I:%M:%S %p"),"%H")), as.numeric(format(as.POSIXct(crime$Occurrence.Date,format="%m/%d/%Y %I:%M:%S %p"),"%M")))
hour.minute <- as.data.frame(hour.minute)
crime$minute <- ceiling((hour.minute$V1 * 60 + hour.minute$V2)/20)


time.start <- 0
time.end <- 72