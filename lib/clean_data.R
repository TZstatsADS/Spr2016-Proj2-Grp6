a <- readRDS("datawewant.RDS")
a$`Complaint Type` <- as.factor(a$`Complaint Type`)
a$date <- as.POSIXct(a$`Created Date`,format="%m/%d/%Y %I:%M:%S %p")
a <- a[ !is.na(a$Latitude),]
a$Descriptor <- as.factor(a$Descriptor)
a$Agency <- as.factor(a$Agency)
a <- a[ !is.na(a$date),]

# saveRDS(a,"cleandata.RDS")


