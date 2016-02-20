library(dplyr)
library(stringr)
# Data cleaning to get rid of records with no geo info
# Leaflet bindings are a bit slow; for now we'll just sample to compensate (10000 samples)
# Original data NYPD_Motor_Vehicle_Collisions.csv is 119.5 MB, which is too large to be included in this repo

############### Code ##############
# vc <- read.csv("data/NYPD_Motor_Vehicle_Collisions.csv") # 618,358 observations
# vc <- subset(vc,!(is.na(vc['ZIP.CODE']))) # 470,687 observations
# set.seed(100)
# vc <- vc[sample.int(nrow(vc), 10000),]
# write.csv(vc, file = 'data/NYPD_Motor_Vehicle_Collisions (10000 obs).csv')
###################################

# Import truncate data (vc is short for vehicle collision)

a <- read.csv('data/NYPD_7_Major_Felony_Incident_Map.csv')
f <- as.data.frame(t(as.data.frame(strsplit(as.character(a$Location.1),','))))
names(f) <- c("lat","long")
f$lat <- str_replace(f$lat,"\\(",'')
f$long <- str_replace(f$long,"\\)",'')
rownames(f) <- NULL
g <- cbind(a,f)
g$lat <- as.numeric(g$lat)
g$long <- as.numeric(g$long)

coltokeep=c("OBJECTID","Occurrence.Date","Identifier","Offense","Offense.Classification","Sector","Precinct","Borough","Jurisdiction","Location.1","lat","long")
b <- g[,coltokeep]
names(b)[11] <- "LATITUDE"
names(b)[12] <- "LONGITUDE"
vc <- b
vc$cntMUR <- rep.int(0,nrow(vc))
vc[vc$Offense=="MURDER",]$cntMUR <- 1

vc$cntRAP <- rep.int(0,nrow(vc))
vc[vc$Offense=="RAPE",]$cntRAP <- 1

vc$cntROB <- rep.int(0,nrow(vc))
vc[vc$Offense=="ROBBERY",]$cntROB <- 1

vc$cntGTA <- rep.int(0,nrow(vc))
vc[vc$Offense=="GRAND LARCENY OF MOTOR VEHICLE",]$cntGTA <- 1

vc$cntGRD <- rep.int(0,nrow(vc))
vc[vc$Offense=="GRAND LARCENY",]$cntGRD <- 1

vc$cntBUR <- rep.int(0,nrow(vc))
vc[vc$Offense=="BURGLARY",]$cntBUR <- 1

vc$cntASS <- rep.int(0,nrow(vc))
vc[vc$Offense=="FELONY ASSAULT",]$cntASS <- 1

vc$hour <- as.numeric(format(as.POSIXct(vc$Occurrence.Date,format="%m/%d/%Y %I:%M:%S %p"),"%H"))

#count number of accidents at the same location(latitude and longitude)
# cleantable <- vc %>%
#   group_by(LONGITUDE,LATITUDE) %>%
#   mutate(count = n(),totalinjury = sum(NUMBER.OF.PERSONS.INJURED),
#             totaldeath = sum(NUMBER.OF.PERSONS.KILLED))
cleantable <- vc %>%
  group_by(LONGITUDE,LATITUDE) %>%
  mutate(count = n(),totalMUR = sum(cntMUR),totalRAP = sum(cntRAP),
         totalROB = sum(cntROB),totalGTA = sum(cntGTA),
         totalGRD = sum(cntGRD),totalBUR = sum(cntBUR),
         totalASS = sum(cntASS))

cleantable <- cleantable[!duplicated(cleantable$Location.1), ]

cleantable <- cleantable %>%
  select(
    Offense = Offense,
    Borough = Borough,
    Crimes = count,
    Rape = totalRAP,
    Murder = totalMUR,
    Robbery = totalROB,
    GTA = totalGTA,
    Larceny = totalGRD,
    Burglary = totalBUR,
    Assault = totalASS,
    Lat = LATITUDE,
    Long = LONGITUDE)
