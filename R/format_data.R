require(lubridate)
require(dplyr)
load("prepped_data_plusGIS.Rdata")

#rough imputation of the very few missing and anomolous temperatures
TempS = DelmaFiltered$SoilTemp
TempS[is.na(TempS)]<-mean(TempS, na.rm=TRUE)
TempS[TempS>60]<-mean(TempS, na.rm=TRUE)

TempA = DelmaFiltered$AirTemp
TempA[is.na(TempA)]<-mean(TempA, na.rm=TRUE)
TempA[TempA>60]<-mean(TempA, na.rm=TRUE)

SurvHour = ifelse(hour(DelmaFiltered$Time)<7, hour(DelmaFiltered$Time)+12, hour(DelmaFiltered$Time))
SurvMinute = minute(DelmaFiltered$Time)
SurvHour[is.na(SurvHour)]<-mean(SurvHour, na.rm=TRUE)
SurvMinute[is.na(SurvMinute)]<-30
timeofday<-(SurvHour + SurvMinute/60)-12

#getting the grassland measures matched up with the survey data. 
DelmaFiltered<-DelmaFiltered %>%
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


#BRING IN THE SUMMARY BURN DATA FROM ABC, FIREHISTORY and Garry's spreadsheets
burn_summary<-readr::read_csv("DataFromGarry/Season_Burn_Summary.csv") %>%
	rename(GridCMA=gridCMA)

#merge the burn summary fire data into grassjoin
DelmaFiltered<-DelmaFiltered %>%
	left_join(burn_summary, by="GridCMA") %>%
	mutate(Autumn=ifelse(is.na(Autumn), 0, Autumn)) %>%  #replace NAs with zero.
	mutate(Spring=ifelse(is.na(Spring), 0, Spring)) %>%
	mutate(Summer=ifelse(is.na(Summer), 0, Summer)) %>%
	mutate(Winter=ifelse(is.na(Winter), 0, Winter)) %>%
	mutate(TotFires=ifelse(is.na(TotFires), 0, TotFires))


#make the site variables for grassland cover and clay (impute a couple of missing values):
grassland<-DelmaFiltered$grasstot
grassland[which(is.na(grassland))]<-mean(grassland, na.rm=TRUE)
clay<-DelmaFiltered$clay
clay[which(is.na(clay))]<-mean(clay, na.rm=TRUE)

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
	TempA = TempA,
	Tdiff = TempS-TempA,
	time.of.day=timeofday,
	weeks.first.surv=DelmaFiltered$WeeksSinceFirstSurvey,   #this is the number of weeks since the first time surveyed, use to test "bedding in".
	flipslast12months=DelmaFiltered$flipslast12months,   #this is the number of times the tiles were flipped in last 12 months.
	grassland=grassland,
	clay=clay,
	graze=DelmaFiltered$grazing
	firecode=DelmaFiltered$TotFires
)

save(jags_dat, DelmaFiltered, file="formatted_for_JAGS.Rdata")
