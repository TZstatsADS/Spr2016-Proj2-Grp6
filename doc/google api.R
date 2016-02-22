setwd('/Users/ruixiongshi/Documents/')
pubtoilet<-readRDS("pubtoilet.RDS")

library(RCurl)
library(RJSONIO)
library(plyr)

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

for(i in 291:length(pubtoilet$NAME)){
  
  # Every nine records, pause 3 seconds
  if(i %% 9 == 0) Sys.sleep(1.5)
  
  address<-geoCode(pubtoilet$NAME[i])
  address1<-geoCode(pubtoilet$ADDRESS[i])

  pubtoilet$add[i]<-address[4]
  pubtoilet$add1[i]<-address1[4]
  
}

as.double(pubtoilet$LAT[1])

pubtoilet$pro<-abs(as.numeric(pubtoilet$LAT)-as.numeric(pubtoilet$LAT1)) * abs(as.numeric(pubtoilet$LNG)-as.numeric(pubtoilet$LNG1))
address <- geoCode("E 72nd St, New York, NY 10021, United States")
pubtoilet$LAT[1]<-address[1]
pubtoilet$LNG[1]<-address[2]

write.csv(pubtoilet, "publictoilet.csv")
library(data.table)
pubtoilet<-fread("publictoilet.csv")
df <- tbl_df(pubtoilet)
df$NAME<-as.factor(df$NAME)
summary(df$NAME)
write.csv(pubtoilet, "pubtoilet1.csv")