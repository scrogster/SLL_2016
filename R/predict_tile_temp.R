
#Basic idea here is to work out whether tile temps can be predicted using RandomForests
#and whether presence under tiles at know occ sites can be predicted as well.

library(randomForest)
library(dplyr)
library(lubridate)
library(gbm)

load("prepped_data.Rdata")

rfdat<-DelmaFiltered %>% 
	filter(GridCMA %in% unique(GridCMA[DelmaLizards>0 | DelmaOther>0])) %>%
	select(SoilTemp, Date, Time, AirTemp, HumidA, HumidS, Sun, Cloud, DelmaLizards) %>%
	mutate(yearday = yday(Date), h=hour(Time), DelmaPres=DelmaLizards>0)
rfdat<-data.frame(na.omit(rfdat))
rfdat$SoilTemp[rfdat$SoilTemp>100]<-mean(rfdat$SoilTemp, na.rm=TRUE)
rfmod<-randomForest(DelmaPres~AirTemp+yearday+h, data=rfdat, trees=1000)

rfmod
importance(rfmod)
varImpPlot(rfmod)
plot(rfmod)
partialPlot(rfmod, rfdat, AirTemp)
partialPlot(rfmod, rfdat, yearday)
partialPlot(rfmod, rfdat, h)

gbmmod<-gbm(DelmaPres~SoilTemp+AirTemp+yearday+h+Sun+HumidA, data=rfdat, n.trees=1000, interaction.depth = 3)

summary(gbmmod)
plot(gbmmod, c(3, 6))
