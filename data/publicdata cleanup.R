setwd('/Users/ruixiongshi/Documents/')
library(data.table)
library(dplyr)
coltokeep1 = c("NAME", "ADDRESS", "HOURS", "HANDICAP ACCESSIBLE?")
coltokeep2 = c("Name", "Location", "Open Year-Round", "Handicap Accessible")
pubtoilet1<-fread("publictoilet.csv", select = coltokeep1)
pubtoilet2<-fread("publictoilet2.csv", select = coltokeep2)
to1<-tbl_df(pubtoilet1)
to2<-tbl_df(pubtoilet2)
to2<-rename(to2, OPEN_YEAR_ROUND = `Open Year-Round`, NAME = Name, ADDRESS = Location, HANDICAP_ACCESSIBLE= `Handicap Accessible`)
to1<-rename(to1, HANDICAP_ACCESSIBLE = `HANDICAP ACCESSIBLE?`, OPEN_YEAR_ROUND = HOURS)

to1$OPEN_YEAR_ROUND[to1$OPEN_YEAR_ROUND != " "]<- "Yes"
to1$HANDICAP_ACCESSIBLE[to1$HANDICAP_ACCESSIBLE == "yes"]<- "Yes"
to1$HANDICAP_ACCESSIBLE[to1$HANDICAP_ACCESSIBLE == "no"]<- "No"
to1$HANDICAP_ACCESSIBLE[to1$HANDICAP_ACCESSIBLE == ""]<- "Not Available"
to2$HANDICAP_ACCESSIBLE[to2$HANDICAP_ACCESSIBLE == ""]<- "Not Available"

pubtoilet<-rbind(to1,to2)

rm(to1,to2)
saveRDS(pubtoilet, "pubtoilet.RDS")