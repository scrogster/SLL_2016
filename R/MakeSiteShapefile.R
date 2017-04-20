require(rgdal)
load("formatted_for_JAGS.Rdata")

spdump<-grassjoin[,c( "Easting", "Northing", "GridCMA")]


sites_spdf<-SpatialPointsDataFrame(coords=as.matrix(spdump[,c("Easting", "Northing")]), 
																	 data=data.frame(GridCMA=spdump$GridCMA),
																	 proj4string = CRS("+init=epsg:3111"))


writeOGR(obj=sites_spdf, dsn="SiteShapefile", layer="tilesites", driver="ESRI Shapefile")


