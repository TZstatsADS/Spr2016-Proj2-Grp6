library(dplyr)

# Data cleaning to get rid of records with no geo info
# Original data NYPD_Motor_Vehicle_Collisions.csv is 119.5 MB, which is too large to be included in this repo

############### Code ##############

# Import truncate data (pt is short for public toilet)

pt <- read.csv("data/publictoilet.csv")

# Import columns and change column names
cleantable <- pt
cleantable <- cleantable %>%
  select(
    Yearround = OPEN_YEAR_ROUND,
    Handicap = HANDICAP_ACCESSIBLE,
    indicator = indicator,
    Lat1 = LAT,
    Long1 = LNG,
    Lat2 = LAT1,
    Long2 = LNG1)

# locate each toilet's latitude and longitude
for (i in 1:length(cleantable))
{
  if(cleantable$indicator[i] == 1){
    cleantable$Lat = cleantable$Lat1
    cleantable$Long = cleantable$Long1
  }
  if(cleantable$indicator[i] == 2){
    cleantable$Lat = cleantable$Lat2
    cleantable$Long = cleantable$Long2
  }  
}

# Remove observations with NA in Lat or Long
cleantable <- cleantable[complete.cases(cleantable[,8:9]),]

# remove redundant columns
drops <- c("Lat1","Long1","Lat2","Long2")
cleantable <- cleantable[,!(names(cleantable) %in% drops)]
