require(rgdal)
load("fitted_model.Rdata")




#get the following from the formatted data:
#Easting, Northing, EverDetect, Grassland, Clay, Roadside, Firescore, GrazingScore

coords<-grassjoin[,c( "Easting", "Northing")]

#this code block makes a data.frame of detection/non-detection/not surveyed(NA) for each year/site.
detect_mat<-data.frame(tapply(DelmaFiltered$DelmaLizards+DelmaFiltered$DelmaOther, 
															 list(DelmaFiltered$GridCMA, DelmaFiltered$yearnum), 
															function(x) {as.numeric(sum(x)>0)}
															))
names(detect_mat)<-paste0("y", 2005:2013)
detect_mat[is.na(detect_mat)]<--99

covariates<-data.frame(
	          GridCMA=grassjoin$GridCMA, 
ever_detect=as.numeric(1.0*(rowSums(tapply(DelmaFiltered$DelmaLizards+DelmaFiltered$DelmaOther, 
														list(DelmaFiltered$GridCMA, DelmaFiltered$yearnum), 
														function(x){min(sum(x), 1)  }  ) , na.rm=T)>0)),
                  detect_mat,
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

#Code below makes a map of sites surveyed each year, with color coded detects/not detects.
year_plot<-function(year=2006){
	yy<-paste0("y", year)
	ind<-which(names(detect_mat)==yy)
	detects<-detect_mat[,ind]==1
	nondetects<-detect_mat[,ind]==0
	
	Ndetects<-apply(detect_mat, 2, function(x) {sum(x==1)})[ind]
	Nnondetects<-apply(detect_mat, 2, function(x) {sum(x==0)})[ind]
plot(sites_spdf, pch="")
plot(sites_spdf[nondetects, ], pch=1, add=TRUE, cex=0.7, col="black")
plot(sites_spdf[detects, ], pch=16, add=TRUE, cex=0.7, col="red")
box()
text(x=2390000, y=2575000, labels=paste(year))
#title(sub=paste(Ndetects+Nnondetects, "sites, detected at", Ndetects))
}

pdf("Figures/Year_map.pdf", height=10, width=10)
layout(matrix(1:9, nrow=3))
par(mar=c(1, 1, 1, 1) + 0.1)
year_plot(2005)
year_plot(2006)
year_plot(2007)
year_plot(2008)
year_plot(2009)
year_plot(2010)
year_plot(2011)
year_plot(2012)
year_plot(2013)
dev.off()
