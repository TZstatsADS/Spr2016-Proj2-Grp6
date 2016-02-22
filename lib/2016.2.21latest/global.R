library(dplyr)

# Data cleaning to get rid of records with no geo info
# Original data NYPD_Motor_Vehicle_Collisions.csv is 119.5 MB, which is too large to be included in this repo

############### Code ##############

# Import toilet data (pt is short for public toilet)

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