library(sp)
library(rgdal)
#library(gdata)
library(raster)
library(rgeos)
library(dplyr)

load("fitted_model.Rdata")

length(vicgrid_spdf)

points_dd<-spTransform(vicgrid_spdf, CRS("+init=epsg:4326")) 


out<-data.frame(coordinates(points_dd), points_dd$GridCMA)
names(out)<-c("Long", "Lat", "GridCMA")

write.csv(out, "SpatialData/latlongs_for_mike.csv", row.names = FALSE)