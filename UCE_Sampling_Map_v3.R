### Make sampling map for molecular and morphological data in concordance with author guidelines for J Biogeography ###
install.packages(c("raster","rgdal","png","gplots","rgeos","maps"))
require(raster)
require(rgdal)
require(png)
require(gplots)
require(rgeos)
require(maps)

### Read in PNGs. These are plates of the birds in this study that will be called later in the script ###
pngs<-list.files(path="~/MVZ_GISinRWorkshop/mason_fig/pngs",pattern="Sporophila",full.names=T)
png.list<-list()

for(i in 1:length(pngs)){
	png.list[[i]]<-readPNG(pngs[i])
}

### Read in csv file with voucher data and lat/long
mast<-read.csv("~/MVZ_GISinRWorkshop/mason_fig/Sporophila_torqueola_MASTER_V4.csv",row.names=15,stringsAsFactors=F)

### Read in shapefiles
stmoreletti_shp<-readOGR("~/MVZ_GISinRWorkshop/mason_fig/ShapeFiles/stmoreletti.shp")
sttorqueola_shp<-readOGR("~/MVZ_GISinRWorkshop/mason_fig/ShapeFiles/sttorqueola.shp")
stsharpei_shp<-readOGR("~/MVZ_GISinRWorkshop/mason_fig/ShapeFiles/stsharpei.shp")

### Create vector of ISO codes to loop through and get GADM data for ###
countries<-c("USA","BLZ","COL","CRI","CUB","CYM","GTM","HND","PAN","MEX","NIC","SLV")
gadm_list_0<-list()
gadm_list_1<-list()
ele_list<-list()

### This for loop will go through each ISO code in the vector above (called countries) and will download GADM boundaries and elevational data #
for(i in 1:length(countries)){
	gadm_list_0[[i]]<-raster:::getData("GADM",country=countries[i],level=0)
	gadm_list_1[[i]]<-raster:::getData("GADM",country=countries[i],level=1)
	ele_list[[i]]<-raster:::getData("alt",country=countries[i])
}

ele_list[[1]]<-ele_list[[1]][[1]] #Only take out continental USA for this step
elevation<-do.call(merge, ele_list) #This combines the list of rasters for elevation into a single raster
gadm_0<-do.call(rbind, gadm_list_0) #This combines the list of polygons for GADM level 0 into a single raster
gadm_1<-do.call(rbind, gadm_list_1) #This combines the list of polygons for GADM level 1 into a single raster

gadm_0<-gSimplify(gadm_0,tol=0.01) #This simplifies the polygon by reducing the number of vertices for faster plotting. Omit for final pubs.
gadm_1<-gSimplify(gadm_1,tol=0.01)

png(width=3.11024,height=3,file="~/MVZ_GISinRWorkshop/mason_fig/spotor_samplemap.png",units="in",res=600) #Create a PNG file to read out to.
#quartz(width=3.11024,height=3) #This will generate a plotting window in R that does not get saved to a file
par(mar=c(0,0,0,0))

plot(gadm_0,col=NA,border=NA,mar=c(0,0,0,0),lwd=0.7,xlim=c(-110,-79),ylim=c(10,28))
par(xpd=NA)
plot(elevation,col=paste(grey(100:0/100),"90",sep=""),legend=F,axes=F,add=T)
par(xpd=T)
plot(gadm_1,border="#b3b3b3",mar=c(0,0,0,0),lwd=0.3,add=T)
plot(gadm_0,border='black',mar=c(0,0,0,0),lwd=0.5,add=T)

plot(stmoreletti_shp,col=paste(col2hex("blue3"),"50",sep=""),border="transparent",add=T) #Plot each shape file with add = T to build the figure
plot(sttorqueola_shp,col=paste(col2hex("red3"),"50",sep=""),border="transparent",add=T)
plot(stsharpei_shp,col=paste(col2hex("yellow3"),"50",sep=""),border="transparent",add=T)
points(mast$Long,mast$Lat,pch=21,bg="#00000020",col="#000000",lwd=0.5,cex=1) #Add in our sampling points

rect(xleft=-110,ybottom=13.75,xright=-106,ytop=17.75,border="red3",lwd=2) #Add in plates and borders for each taxon
text(label="S. t. torqueola (19)",y=13.75, x=-105.5, adj=c(0,0),cex=0.85,font=3) #Add in name and sample size of each taxon
rasterImage(png.list[[3]],xleft=-110,ybottom=13.75,xright=-106,ytop=17.75)

rect(xleft=-110,ybottom=9.5,xright=-106,ytop=13.5,border="yellow3",lwd=2)
text(label="S. t. sharpei (3)",y=9.5, x=-105.5, adj=c(0,0),cex=0.85,font=3)
rasterImage(png.list[[2]],xleft=-110,ybottom=9.5,xright=-106,ytop=13.5)

rect(xleft=-110,ybottom=5.25,xright=-106,ytop=9.25,border="blue3",lwd=2)
text(label="S. t. moreletti (46)",y=5.25, x=-105.5, adj=c(0,0),cex=0.85,font=3)
rasterImage(png.list[[1]],xleft=-110,ybottom=5.25,xright=-106,ytop=9.25)

map.scale(x=-90.5,y=7,relwidth=0.175,cex=0.5,ratio=F) #Add scale for map

text(label="N",cex=0.6,x=-91.5,y=7.25)
arrows(x0=-91.5,y0=5.5,x1=-91.5,y1=6.75,angle=30,lwd=1.5,length=0.05) #Add North Arrow

### Key for no. of points per dot ###
points(x=-81.25,y=15.33,pch=21,bg="#00000020",col="#000000",lwd=0.5)
replicate(5,points(x=-81.25,y=13.33,pch=21,bg="#00000020",col="#000000",lwd=0.5))
replicate(10,points(x=-81.25,y=11.33,pch=21,bg="#00000020",col="#000000",lwd=0.5))

text(x=-82,y=15.33,label="1",adj=c(1,0.5),cex=0.5) #Annotate legend with text
text(x=-82,y=13.33,label="5",adj=c(1,0.5),cex=0.5)
text(x=-82,y=11.33,label="≥10",adj=c(1,0.5),cex=0.5)

text(x=-81.75,y=16.5,label= bquote(underline("Samples per point")),cex=0.4)

dev.off()