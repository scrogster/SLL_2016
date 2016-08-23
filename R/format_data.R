
require(jagsUI)
require(lubridate)
require(dplyr)
load("prepped_data_plusGIS.Rdata")

#variables to get for model

tapply(DelmaFiltered$AirTemp, month(DelmaFiltered$Date), "mean")

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
xx<-DelmaFiltered %>%
	ungroup() %>%
	dplyr::select(GridCMA, Date) %>%
	left_join(data.frame(vicgrid_spdf), by="GridCMA") %>%
	dplyr::select(GridCMA, Date, grass1, grass2, LandUse, FireHistory) %>%
	mutate(grasstot = grass1+grass2) %>%
	group_by(GridCMA) %>%
	summarise(grass1=first(grass1), grass2=first(grass2), grasstot=first(grasstot), 
						LandUse=first(LandUse), FireHistory=first(FireHistory)) %>%
	mutate(Conservation=grepl("Conservation", LandUse), Grazing=grepl("Grazing", LandUse))
#check that sites are in same order as sites in detection data:
all.equal(levels(factor(DelmaFiltered$GridCMA)) , xx$GridCMA)

#make the site variables:
grassland<-xx$grasstot
grassland[which(is.na(grassland))]<-mean(grassland, na.rm=TRUE)
grazing<-xx$Grazing
#let's get the site variables...
#we need to get the matching values for grassland area, (cons/grazing/roadside), fire. Need to match up 
#with variable "i" (site index) in occ model.

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
  grazing=grazing*1.0 #convert Boolean to numeric
)

modfile <- 'R/prototype_occmod.txt'
params <- c('B', 'C', 'D', 'BETA')

z.init<-
tapply(DelmaFiltered$DelmaLizards+DelmaFiltered$DelmaOther, 
			 list(DelmaFiltered$GridCMA, DelmaFiltered$yearnum), 
			 function(x){min(sum(x), 1)  }  ) 
z.init[z.init==0]<-NA #just assign one to years with known occurences. otherwise NA for init.

inits <- function(){  
	list(B=rnorm(4,0,1),
			 C=rnorm(4,0,1),
			 D=rnorm(4,0,1), 
			 BETA=rnorm(14,0,0.2),
	    Z=z.init)
}


out <- jags(data = jags_dat,
						parameters.to.save = params,
						inits=inits,
						model.file = modfile,
						parallel=FALSE, 
						n.chains = 3,
						n.adapt = 100,
						n.iter = 2000,
						n.burnin = 500,
						n.thin = 5)

out

plot(out)

#trial plotting of seasonal response curves 
BETA<-colMeans(out$sims.list$BETA)

x<-seq(0, 1, by=0.01)
aa<-BETA[1]+BETA[2]*(cos(2*pi*x))+BETA[3]*(sin(2*pi*x)) + 
	BETA[4]*(cos(4*pi*x))+BETA[5]*(sin(4*pi*x))

bb<-BETA[10]+BETA[11]*(cos(2*pi*x))+BETA[12]*(sin(2*pi*x))+ 
	BETA[13]*(cos(4*pi*x))+BETA[14]*(sin(4*pi*x))

pdf("Figures/detection_curve.pdf", width=8, height=8)
plot(plogis(aa)~x, col="red", type="l", ylim=c(0, 1), lwd=2, ylab="Pr(detect)",
		 xlab="Fraction of Calendar Year", las=1)
lines(y=plogis(bb), x=x, col="blue", lwd=2)
lines(y = 1- (1-plogis(aa))*(1-plogis(bb)), x=x, col="green", lwd=2)
title(main=c("Red=lizards, Blue=sloughs, Green=combined"))
dev.off()

