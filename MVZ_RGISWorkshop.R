### Install and load required packages ###
install.packages(c("sp","sf","dismo","raster","maptools","rgeos","rvertnet"))
library("sp")
library("sf")
library("dismo")
library("raster")
library("maptools")
library("rgeos")
library("rvertnet")

### Download GADM vector data for political boundaries via raster package ###
usa_county_gadm<-getData(name="GADM",country="USA",level=2)
str(usa_county_gadm)
usa_county_gadm@data

### Get only polygons that correspond to California ###
CA_county<-usa_county_gadm[usa_county_gadm@data$NAME_1=="California",]
plot(CA_county)

### You can change lots of plotting parameters like fill color, line thickness, line color, etc.. ###
plot(CA_county,col=rainbow(nrow(CA_county@data)))
plot(CA_county,col=rainbow(nrow(CA_county@data)),lwd=2)
plot(CA_county,col="blue",border="gold",lwd=2)

### You can layer plots on top of each other using add = T ###
plot(CA_county)
plot(CA_county[which(CA_county@data$NAME_2=="Alameda"),],col="red",add=T)
plot(CA_county[which(CA_county@data$NAME_2=="Contra Costa"),],col="blue",add=T)
plot(CA_county[which(CA_county@data$NAME_2=="Contra Costa"),],border="gold",add=T)

### Search Vertnet by terms ###
search_results<-searchbyterm(genus="cinclus",specificepithet="mexicanus",inst="MVZ",limit=1000)
str(search_results) #This is the output of RVertNet

search_results<-as.data.frame(search_results$data)
search_results<-search_results[!is.na(search_results$decimallatitude),] #Remove records with not lat lon

search_results_trim<-search_results[sample(1:nrow(search_results),50),] #Randomly sample 100 points for plotting

### Plot points on top of map (2 different vector types in same fig) ###
plot(CA_county)
points(search_results_trim$decimallongitude, search_results_trim$decimallatitude,pch=21,col="skyblue",cex=3)

### Download bioclim data and digital elevation data via raster package ###
bioclim_world<-getData("worldclim",var="tmin",res=10)

CA_bioclim<-raster:::crop(bioclim_world,CA_county)
CA_bioclim<-raster:::mask(CA_bioclim,CA_county)

plot(CA_bioclim[[1]])
plot(CA_county,add=T,lwd=1.5)

### Read in shape file for cinclus mexicanus ###
cinclus<-readOGR("~/MVZ_GISinRWorkshop/shapefiles/Cinclus_mexicanus/Cinclus_mexicanus.shp")
cinclus@data

cinclus_simple<-gSimplify(cinclus,0.05) #Simplify polygon to speed up plotting process. Omit for pub fig

### Create Figure for Cinclus Mexicanus ###
data(wrld_simpl)
plot(wrld_simpl)
plot(cinclus_simple,add=T,col="purple")

plot(wrld_simpl,xlim=c(c(extent(cinclus_simple)@xmin,extent(cinclus_simple)@xmax)),ylim=c(c(extent(cinclus_simple)@ymin,extent(cinclus_simple)@ymax)))
plot(cinclus_simple,col="#FF05BA80",border=NA,add=T)
points(search_results_trim$decimallongitude, search_results_trim$decimallatitude,pch=21,bg="#0A4A8180",cex=1)

### Add elevational data ###
usa_alt<-getData("alt",country="USA",mask=T)
CA_alt<-raster:::crop(usa_alt[[1]],CA_county)
CA_alt<-raster:::mask(CA_alt,CA_county)

### Mask shapefile to only include California ###
plot(CA_alt,breaks=seq(CA_alt@data@min,CA_alt@data@max,length.out=100),col=rev(gray.colors(100)),legend=F)

cinclus_CA<-raster:::crop(cinclus_simple,CA_county)
plot(cinclus_CA,add=T,col="#FF00FF80",border=NA)
points(search_results_trim$decimallongitude, search_results_trim$decimallatitude,pch=21,bg="#0A4A8180",cex=1)