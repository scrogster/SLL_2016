#load required packages
library(readxl)
library(dplyr)
library(sp)
library(maptools)
library(rgdal)
library(stringr)
library(tidyr)

#read in Garry's point data from excel
#contrary to what the spreadsheets say, all data is in the first (GHCMA) leaf.
site_points<-read_excel("SpatialData/Non_GIS Delma grid variables30JUNE2009forMS.xls",1)


site_points<-site_points %>%
	select(Site, LandUse=`Land use`, LandUseCode=`Aggr. Land Use`, 
				 FireHistory=`Fire History`, Area=`Area (ha)`, 
				 Easting=`Easting GDA94`, Northing=`Northing GDA94`) %>%
	separate(Site, c("Grid", "CMA"), sep=" ") %>%
	mutate(GridCMA = paste0(Grid, tolower(CMA))) %>%
	mutate(utmzone = ifelse(Easting<35000, 55, 54)) #kludge to work out which utm zone the eastings and northings are


plot(Northing~Easting, data=site_points)

load("prepped_data.Rdata")

#experimental merge between site point data and survey data
test<-left_join(ungroup(DelmaFiltered), site_points, "GridCMA") %>%
	select(GridCMA, Easting, Northing)

#these are the sites that don't have grid refs...
unique(test$GridCMA[which(is.na(test$Easting))])
unique(test$GridCMA[which(is.na(test$Northing))])

#a bit more info about these sites....

#"10.3.2ccma"   10.3.2 Bannockburn #single survey only in 2010 EXCLUDE
#"10.3.3ccma"   10.3.3 Bannockburn  #single survey only in 2010 EXCLUDE
DelmaFiltered %>% filter(GridCMA=="10.3.2ccma") %>% select(Date, DelmaLizards, DelmaOther)
DelmaFiltered %>% filter(GridCMA=="10.3.3ccma") %>% select(Date, DelmaLizards, DelmaOther)

#"17.3.1ghcma"  17.3.1     col    #Single survey only in 2005 EXCLUDE
#"17.6.1ghcma"  17.6.1     col    #Single survey only in 2005 EXCLUDE
DelmaFiltered %>% filter(GridCMA=="17.3.1ghcma") %>% select(Date, DelmaLizards, DelmaOther)
DelmaFiltered %>% filter(GridCMA=="17.6.1ghcma") %>% select(Date, DelmaLizards, DelmaOther)

#"10.2.2wcma"  10.2.2 Longerenong #Surveyed in 2010, 2011, 2012.
#"19.3.1wcma"   19.3.1 Lallat Plain #Surveyed in 2010, 2011, 2012.
#"20.8.1wcma"   20.8.1  Minyip  #Surveyed in 2010, 2011
DelmaFiltered %>% filter(GridCMA=="10.2.2wcma") %>% select(Date, DelmaLizards, DelmaOther)
DelmaFiltered %>% filter(GridCMA=="19.3.1wcma") %>% select(Date, DelmaLizards, DelmaOther)
DelmaFiltered %>% filter(GridCMA=="20.8.1wcma") %>% select(Date, DelmaLizards, DelmaOther)
