#quick map of the study area
library(rgdal)
library(maptools)
library(ggmap)

survey_points<-readOGR("SpatialData", "GridPoints")

survey_points_2<-spTransform(survey_points, CRS("+init=epsg:4326") )

map <- get_map("Ararat, Victoria, Australia", zoom=7, maptype = 'satellite')   # get Google map
ggmap(map) +
	geom_point(data=as.data.frame(survey_points_2), aes(coords.x1,coords.x2), 
						 color="red", size=2, shape=21)
ggsave("Figures/map_figure.pdf", width=8, height=6)
