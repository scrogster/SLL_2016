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
	mutate(utmzone = ifelse(Easting<350000, 55, 54)) #kludge to work out which utm zone the eastings and northings are

#we'll need to separately convert the zone 55 and zone 54 points
s55<-site_points$utmzone==55 & !is.na(site_points$Easting) #logical flag for nonNA, zone 55 sites
s54<-site_points$utmzone==54 & !is.na(site_points$Easting) #logical flag for nonNA, zone 54 sites.
  #make sps
sites_points_55<-SpatialPoints(coords=site_points[s55,c("Easting", "Northing")], 
							proj4string = CRS("+init=epsg:28355")) #28355
sites_points_54<-SpatialPoints(coords=site_points[s54,c("Easting", "Northing")], 
							proj4string = CRS("+init=epsg:28354")) #28354

#Conver to vicgrid. Because of the split, these will be out of order. Fix once we've got what we need.
sites_points_55_VG<-spTransform(sites_points_55, CRS("+init=epsg:3111"))
sites_points_54_VG<-spTransform(sites_points_54, CRS("+init=epsg:3111"))
 plot(sites_points_54_VG, pch=16)
 plot(sites_points_55_VG, pch=16, add=TRUE)

vicgrid_coords<-data.frame("EastingVG"=rep(NA, nrow(site_points)), "northingVG"=rep(NA, nrow(site_points)) )
vicgrid_coords[s55,]<-coordinates(sites_points_55_VG)
vicgrid_coords[s54,]<-coordinates(sites_points_54_VG)

vicgrid_points<-SpatialPoints(coords=vicgrid_coords[!is.na(vicgrid_coords$EastingVG),], CRS("+init=epsg:3111"))
vicgrid_spdf<-SpatialPointsDataFrame(vicgrid_points, data.frame(site_points[!is.na(vicgrid_coords$EastingVG),]))

plot(vicgrid_points, pch=16) 

writeOGR(vicgrid_spdf, "SpatialData", "GridPoints", driver = "ESRI Shapefile", overwrite_layer = TRUE)

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
