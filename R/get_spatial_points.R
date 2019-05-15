#load required packages
library(readxl)
library(dplyr)
library(sp)
library(maptools)
library(rgdal)
library(stringr)
library(tidyr)
library(raster)

load("prepped_data.Rdata")
#read in Garry's point data from excel
#contrary to what the spreadsheets say, all data is in the first (GHCMA) leaf.
site_points<-read_excel("SpatialData/DelmaGridVariables_15Feb2017.xlsx",1)

site_points<-site_points %>%
	dplyr::select(Site, LandUse=`Land use`, LandUseCode=`Aggr. Land Use`, 
				 FireHistory=`Fire History`, GrazeScore=`GrazeScore`, Area=`Area (ha)`, 
				 Easting=`Easting GDA94`, Northing=`Northing GDA94`) %>%
	separate(Site, c("Grid", "CMA"), sep=" ") %>%
	mutate(GridCMA = paste0(Grid, tolower(CMA))) %>%
	mutate(utmzone = ifelse(Easting<350000, 55, 54)) #kludge to work out which utm zone the eastings and northings are

#we'll need to separately convert the zone 55 and zone 54 points
s55<-site_points$utmzone==55 & !is.na(site_points$Easting) #logical flag for nonNA, zone 55 sites
s54<-site_points$utmzone==54 & !is.na(site_points$Easting) #logical flag for nonNA, zone 54 sites.
  #make sps
sites_points_55<-SpatialPoints(coords=data.frame(site_points[s55,c("Easting", "Northing")]), 
							proj4string = CRS("+init=epsg:28355")) #28355
sites_points_54<-SpatialPoints(coords=data.frame(site_points[s54,c("Easting", "Northing")]), 
							proj4string = CRS("+init=epsg:28354")) #28354

#Convert to vicgrid. Because of the split, these will be out of order. Fix once we've got what we need.
sites_points_55_VG<-spTransform(sites_points_55, CRS("+init=epsg:3111"))
sites_points_54_VG<-spTransform(sites_points_54, CRS("+init=epsg:3111"))

vicgrid_coords<-data.frame("EastingVG"=rep(NA, nrow(site_points)), "northingVG"=rep(NA, nrow(site_points)) )
vicgrid_coords[s55,]<-coordinates(sites_points_55_VG)
vicgrid_coords[s54,]<-coordinates(sites_points_54_VG)

vicgrid_points<-SpatialPoints(coords=vicgrid_coords[!is.na(vicgrid_coords$EastingVG),], CRS("+init=epsg:3111"))
vicgrid_spdf<-SpatialPointsDataFrame(vicgrid_points, data.frame(site_points[!is.na(vicgrid_coords$EastingVG),]))

#merge between site point data and survey data
test<-left_join(ungroup(DelmaFiltered), site_points, "GridCMA") %>%
	dplyr::select(GridCMA, Easting, Northing)

#extract grassland coverage data
grassland<-raster("Rasters/NatGrassland_clip_simp.tif")

#in the raster 2 =very likely grassland, 1=likely grassland
grass1<-raster::extract(grassland, vicgrid_spdf, buffer=1000, fun=function(x,...){sum(x==1)/length(x)}, na.rm=TRUE)
grass2<-raster::extract(grassland, vicgrid_spdf, buffer=1000, fun=function(x,...){sum(x==2)/length(x)}, na.rm=TRUE)

vicgrid_spdf$grass1<-grass1 #these encode for likely and highly likely grassland respectively
vicgrid_spdf$grass2<-grass2

#extract soil clay content data Source:http://www.clw.csiro.au/aclep/soilandlandscapegrid/GetData.html
clay<-raster("Rasters/clay_clipped_compressed.tif")
claypoints<-raster::extract(clay, vicgrid_spdf)
vicgrid_spdf$clay<-claypoints

writeOGR(vicgrid_spdf, "SpatialData", "GridPoints", driver = "ESRI Shapefile", overwrite_layer = TRUE)

save(vicgrid_spdf, DelmaFiltered, file="prepped_data_plusGIS.Rdata")
