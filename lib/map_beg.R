library(stringr)
library(ggplot2)
library(ggmap)

a <- read.csv("../data/DOE_High_School_Directory_2014-2015.csv")
b <- as.data.frame(strsplit(as.character(a$Location.1),"\n",fixed=TRUE))
d <- cbind(a,t(b))
rownames(d) <- NULL

names(d)[61] <- "longlat"
e <- d[c("school_name","longlat")]

f <- as.data.frame(t(as.data.frame(strsplit(as.character(e$longlat),','))))

names(f) <- c("lat","long")
f$lat <- str_replace(f$lat,"\\(",'')
f$long <- str_replace(f$long,"\\)",'')
rownames(f) <- NULL
g <- cbind(e$school_name,f)
g$lat <- as.numeric(g$lat)
g$long <- as.numeric(g$long)

mapgilbert <- get_map(location = c(lon = mean(g$long),lat = mean(g$lat)), zoom = 11, scale = 2)
# maptype = "satellite",

ggmap(mapgilbert) +
  geom_point(data = g, aes(x = long, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

# x <- get_googlemap(center=c(lon=-73.992585, lat=40.706336),zoom=10)
# x <- get_googlemap(center=c(lon=mean(g$lat), lat=mean(g$long)),zoom=11)
# ggmap(x) +
#   geom_point(data = g, aes(x = lat, y = long, fill = "red", alpha = 0.8), size = 5, shape = 21) +
#   guides(fill=FALSE, alpha=FALSE, size=FALSE)
# 40.706336, -73.992585


