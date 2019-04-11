library(raster)
library(sp)
library(maptools)

data(wrld_simpl)
antarctica <- wrld_simpl[wrld_simpl$NAME == "Antarctica", ] 

penguin<-readOGR("~/Desktop/DesktopClutter25Oct/Service/AntarcticaMaps/Pygoscelis_antarcticus/Pygoscelis_antarcticus.shp") #Read in shape file
ice_thick<-raster("~/Desktop/DesktopClutter25Oct/Service/AntarcticaMaps/Antarctic_data/Ice_thickness/ice_thick") #Read in raster data

### Perform transformations
wrld_simpl_transform<-spTransform(wrld_simpl,"+proj=aeqd +lat_0=-90 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
antarctica_transform<-spTransform(antarctica,"+proj=aeqd +lat_0=-90 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
penguin_transform<-spTransform(penguin,"+proj=aeqd +lat_0=-90 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
ice_transform<-projectRaster(ice_thick,crs="+proj=aeqd +lat_0=-90 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

plot(penguin_transform,col="NA",border=NA)
plot(wrld_simpl_transform,ylim=c(-90,-2),add=T)
plot(penguin_transform,col="blue",add=T)

png("~/Desktop/AntarcticaFig.png")
plot(penguin_transform,col="NA",border=NA)
plot(ice_transform,add=T)
plot(wrld_simpl_transform,ylim=c(-90,-2),add=T)
plot(penguin_transform,col="#00FF0070",add=T)
dev.off()

world