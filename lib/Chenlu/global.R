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
cleantable <- pt
cleantable <- cleantable %>%
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
n <- dim(cleantable)[1]
cleantable$Lat <- numeric(length = n)
cleantable$Long <- numeric(length = n)
# locate each toilet's latitude and longitude
for (i in 1:n)
{
  if(cleantable$Indicator[i] == 1){
    cleantable$Lat[i] <- cleantable$Lat1[i]
    cleantable$Long[i] <- cleantable$Long1[i]
  }
  if(cleantable$Indicator[i] == 2){
    cleantable$Lat[i] <- cleantable$Lat2[i]
    cleantable$Long[i] <- cleantable$Long2[i]
  }  
}

# Remove observations with NA in Lat or Long
cleantable <- cleantable[complete.cases(cleantable[,9:10]),]

# remove redundant columns
drops <- c("Lat1","Long1","Lat2","Long2")
cleantable <- cleantable[,!(names(cleantable) %in% drops)]

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

