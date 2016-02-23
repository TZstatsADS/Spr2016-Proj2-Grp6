library(RCurl)
library(RJSONIO)
library(plyr)
################# function of loacate me#######################
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

url("250W 100th street,New York,NY,10025")
############### End ############################################

###############function for the route###########################

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
    route <- toJSON(x$routes)
    return(route)
    Sys.sleep(0.5)
  } else {
    return(NA)
  }
}


origin_lat=40.7902926
origin_lng=-73.9544024
destination_lat=40.7937661
destination_lng=-73.9524574
route<-geoRoute(origin_lat,origin_lng,destination_lat,destination_lng)

