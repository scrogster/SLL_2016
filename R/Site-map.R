library(sp)
library(rgdal)
#library(gdata)
library(raster)
library(rgeos)
library(dplyr)

load('fitted_model.Rdata')

#read in map of australia
aus_map<-readOGR("SpatialData", "AUS_adm1")
aus_map_simple<-gSimplify(aus_map, 0.002)

#use grassland raster to get projection data
grass_rast<-raster("Rasters/NatGrassland_clip_simp.tif")
proj<-crs(grass_rast)

#split off and simplify victoria
VIC<-aus_map[aus_map$NAME_1=="Victoria",]
VIC_simple<-gSimplify(VIC, 0.002)

#split off and simplify Australian states
ausmain<-aus_map[aus_map$NAME_1 %in% 
								 	c("Victoria", "Tasmania", "South Australia", "Western Australia", "Northern Territory", "Queensland", "New South Wales"),]
ausmain<-gSimplify(ausmain, 0.05)

#transform vecs to same crs as raster
VIC_simple<-spTransform(VIC_simple, CRS(paste(proj)))
aus_map_simple<-spTransform(aus_map_simple, CRS(paste(proj)))

#which sites was Delma detected at?
ever.detect<-as.numeric(rowSums(z.init, na.rm=TRUE)>0)

#filter off sites without coordinates
sample_points<-data.frame(grassjoin, ever.detect) %>%
	                filter(!is.na(Easting))

#make a spatial points dataframe with the 
lizpoints<-SpatialPointsDataFrame(cbind(sample_points$Easting, sample_points$Northing), data=sample_points)
proj4string(lizpoints)=CRS("+init=epsg:3111")

lizpoints$colvec[lizpoints$ever.detect==1]<-'red'
lizpoints$colvec[lizpoints$ever.detect==0]<-'blue'
lizpoints$pchvec[lizpoints$ever.detect==1]<-16
lizpoints$pchvec[lizpoints$ever.detect==0]<-1
lizpoints$cexvec[lizpoints$ever.detect==1]<-0.5
lizpoints$cexvec[lizpoints$ever.detect==0]<-0.4

#make WGS84 versions so that map has lat/longs
lizpoints_wgs84<-spTransform(lizpoints, CRS("+init=EPSG:4326")	)
VIC_simple_wgs84<-spTransform(VIC_simple, CRS("+init=EPSG:4326")	)
aus_map_simple_wgs84<-spTransform(aus_map_simple, CRS("+init=EPSG:4326")	)

#make a map with an inset
pdf("Figures/Site-map.pdf", width=7, height=5)
par(mar=c(3, 3.3, 0.25, 0.25))
grid_col=gray(0.5)
plot(VIC_simple_wgs84, col=gray(0.8))
plot(lizpoints_wgs84, col=lizpoints$colvec, add=TRUE, pch=lizpoints$pchvec, cex=lizpoints$cexvec)
longdegs=seq(140, 150, by=2)
a = sapply(longdegs,function(x) bquote(.(x)*degree ~ E))
axis(1, at=longdegs, lab=do.call(expression, a),las=1)
latdegs=seq(-34, -39, by=-1)
b = sapply(latdegs,function(x) bquote(.(-x)*degree ~ S))
axis(2, at=latdegs, lab=do.call(expression, b),las=1)
box()
legend(x=143.8, y=-33.9, legend=
			 	c(expression(paste(italic("D. impar"), " detected" )  ), 
			 		expression(paste(italic("D. impar"), " not detected" )  )),
			   pch=c(16, 1), col=c('red', 'blue'), pt.cex=c(0.5, 0.4))
par(fig = c(.75, 0.99, .7, 0.985), mar=c(0,0,0,0), new=TRUE)
plot(ausmain)
plot(VIC, add=TRUE, col=gray(0.8))
box()
dev.off()





