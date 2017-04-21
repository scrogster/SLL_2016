require(jagsUI)
require(lubridate)
require(dplyr)
load("prepped_data_plusGIS.Rdata")

#variables to get for model
tapply(DelmaFiltered$AirTemp, month(DelmaFiltered$Date), "mean", na.rm=TRUE)
tapply(DelmaFiltered$AirTemp, month(DelmaFiltered$Date), "length")

#rough imputation of missing temps
TempS = DelmaFiltered$SoilTemp
TempS[is.na(TempS)]<-mean(TempS, na.rm=TRUE)
TempS[TempS>60]<-mean(TempS, na.rm=TRUE)

SurvHour = ifelse(hour(DelmaFiltered$Time)<7, hour(DelmaFiltered$Time)+12, hour(DelmaFiltered$Time))
SurvMinute = minute(DelmaFiltered$Time)
SurvHour[is.na(SurvHour)]<-mean(SurvHour, na.rm=TRUE)
SurvMinute[is.na(SurvMinute)]<-30
timeofday<-(SurvHour + SurvMinute/60)-12

#getting the grassland measures matched up with the survey data. 
grassjoin<-DelmaFiltered %>%
	ungroup() %>%
	dplyr::select(GridCMA, Date) %>%
	left_join(data.frame(vicgrid_spdf), by="GridCMA") %>%
	dplyr::select(GridCMA, Date, grass1, grass2, clay, LandUse, GrazeScore, FireHistory, EastingVG, northingVG) %>%
	mutate(grasstot = grass1+grass2) %>%
	group_by(GridCMA) %>%
	summarise(grass1=first(grass1), grass2=first(grass2), grasstot=first(grasstot), 
						LandUse=first(LandUse), clay=first(clay), Grazing=first(GrazeScore), FireHistory=first(FireHistory), Easting=first(EastingVG), Northing=first(northingVG)) %>%
	mutate(Conservation=grepl("Conservation", LandUse), 
				 Roadside=grepl("Road", LandUse))
#check that sites are in same order as sites in detection data:
all.equal(levels(factor(DelmaFiltered$GridCMA)) , grassjoin$GridCMA)

#BRING IN THE SUMMARY BURN DATA FROM ABC, FIREHISTORY and Garry's spreadsheets
burn_summary<-readr::read_csv("DataFromGarry/Season_Burn_Summary.csv") %>%
	rename(GridCMA=gridCMA)

#merge the burn summary fire data into grassjoin
grassjoin<-grassjoin %>%
	left_join(burn_summary, by="GridCMA") %>%
	mutate(Autumn=ifelse(is.na(Autumn), 0, Autumn)) %>%  #replace NAs with zero.
	mutate(Spring=ifelse(is.na(Spring), 0, Spring)) %>%
	mutate(Summer=ifelse(is.na(Summer), 0, Summer)) %>%
	mutate(Winter=ifelse(is.na(Winter), 0, Winter)) %>%
	mutate(TotFires=ifelse(is.na(TotFires), 0, TotFires))


#make the site variables:
grassland<-grassjoin$grasstot
grassland[which(is.na(grassland))]<-mean(grassland, na.rm=TRUE)
clay<-grassjoin$clay
clay[which(is.na(clay))]<-mean(clay, na.rm=TRUE)
grazing<-grassjoin$Grazing
conservation<-grassjoin$Conservation
roadside<-grassjoin$Roadside

firecode<-grassjoin$TotFires #initially, just use total number of fires as fire measure.



#determine cluster codes for sites. This is done on the basis of geographic proximity. 
#Sites within 2km of each other are same cluster.
#many clusters are only a single site, quite a few pairs, and few with more.
spatpoints<-grassjoin[,c("Easting", "Northing")]
dmat<-dist(as(spatpoints, "matrix"))
clus<-hclust(dmat)  #heirarchical clustering by geographic distance
clusters<-cutree(clus, h=1500)  #cut the clustering at 1500m threshold
num_clust<-max(clusters)


jags_dat<-list(
	tot.sites= max(as.numeric(factor(DelmaFiltered$GridCMA))),
	max.time= max(DelmaFiltered$yearnum),
	tot.surveys = nrow(DelmaFiltered),
	detect.liz =as.numeric(DelmaFiltered$DelmaLizards>0),
	detect.skin=as.numeric(DelmaFiltered$DelmaOther>0),
	site= as.numeric(factor(DelmaFiltered$GridCMA)),
	t = DelmaFiltered$yearnum,
	year.frac = DelmaFiltered$yeardayfrac,
	TempS = TempS,
	time.of.day=timeofday,
	grassland=grassland,
	clay=clay,
	grazing=grazing,
	grazinglo=1.0*(grazing==0),
	grazingmid=1.0*(grazing==1),
	grazinghi=1.0*(grazing>=2),
	conservation=conservation*1.0, #convert Boolean to numeric
	roadside=roadside*1.0, #convert Boolean to numeric
	firecode=firecode,
	firegt0=(firecode>0)*1.0,
	firegt1=(firecode>1)*1.0,
	firegt2=(firecode>2)*1.0,
	firegt3=(firecode>3)*1.0,
	firegt4=(firecode>4)*1.0,
	#below are group ids for clusters, and maximum number of clusters for indexing purposes.
	cluster_id=clusters,
	num_clust=num_clust
)

save.image("formatted_for_JAGS.Rdata")
