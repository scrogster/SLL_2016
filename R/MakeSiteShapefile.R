require(rgdal)
load("fitted_model.Rdata")




#get the following from the formatted data:
#Easting, Northing, EverDetect, Grassland, Clay, Roadside, Firescore, GrazingScore

coords<-grassjoin[,c( "Easting", "Northing")]

covariates<-data.frame(
	          GridCMA=grassjoin$GridCMA, 
ever_detect=as.numeric(1.0*(rowSums(tapply(DelmaFiltered$DelmaLizards+DelmaFiltered$DelmaOther, 
														list(DelmaFiltered$GridCMA, DelmaFiltered$yearnum), 
														function(x){min(sum(x), 1)  }  ) , na.rm=T)>0)),
                  grassland=grassland,
                  clay=clay,
                  grazing=grazing,
                  roadside=roadside,
                  fire=firecode,
                  grazing=grazing)

#make up a SpatialPointsDataFrame from this.
sites_spdf<-SpatialPointsDataFrame(coords=as.matrix(coords),
																	 data=covariates, proj4string = CRS("+init=epsg:3111"))


writeOGR(obj=sites_spdf, dsn="SiteShapefile", layer="tilesites", driver="ESRI Shapefile", overwrite=TRUE)


