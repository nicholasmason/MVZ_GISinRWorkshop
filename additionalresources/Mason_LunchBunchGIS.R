### Lunch Bunch 12/3/14 ###
### Maps and GIS in R ###
install.packages(c("dismo","maps","maptools","mapdata"))
require(dismo)
require(maps)
require(maptools)
require(mapdata)

?map

### Map is a funciton that can access multiple databases that contain shape files of geographic entities at different scales, including countries, US States, and US Counties.

map("world")#World map
map("world",regions="USA",exact=T)
map("world",regions="Brazil",exact=T)

map("world",regions="Canada")
map("worldHires",regions="Canada")

### You can manipulate lots of things in R that you can normally manipulate in plots
map("worldHires",regions="Canada",border="red",bg="forestgreen",fill=T,col="yellow")
map("worldHires",regions="Canada",border="red",bg="forestgreen",fill=T,col="yellow",lwd=10)

### You can plot multiple regions by creating a vector
map("state",regions="Minnesota")
map("state",regions=c("Minnesota","Iowa"))
map("state",regions=c("Minnesota","Iowa","Florida"))

### Add map scale ###
scale.position<-locator(1)#locator() is a useful function for finding coordinates associated with a plot
par(xpd=NA)#This is a plotting setting that allows us to display plots outside of the plotting margins
map.scale(x=scale.position$x,y=scale.position$y,cex=0.5,ratio=F)

### NY State County Maps ###
nycounty<-map("county",regions="New York",col="darkgray")#map NY counties
map("state",regions="New York",add=T)#add NY State outline in black

map("state",col="gray90",bg="darkgray",ylim=c(25,50),xlim=c(-99,-65),mar=c(0,0,0,0),fill=T)
box()
map("state",col="gray90",fill=T,bg="skyblue")
map("world","USA",col="black",add=T)

county.col<-rep("white",length(nycounty$names))
county.col[grep("tompkins",nycounty$names)]<-"red"

map("county",regions="New York",border="darkgray",fill=T,col=county.col)
map("state",regions="New York",add=T)#

### Add our coordinate ###
our.loc<-c(42.447282, -76.478599)
points(our.loc[2],our.loc[1],pch="*",cex=2)

### You can also do fairly basic tasks in R like download files, unzip files, and read in various data
### Lets look at range maps for two eastern pines: white pine and red pine
temp <- tempfile() #Create temporary file
pinus_strobus<-download.file("http://esp.cr.usgs.gov/data/little/pinustrb.zip",temp,mode="wb")
pinus_strobus <-unzip(temp)
pinus_strobus<-readShapePoly(pinus_strobus)
unlink(temp) #Remove temporary file

temp <- tempfile() #Create temporary file
pinus_resinosa<-download.file("http://esp.cr.usgs.gov/data/little/pinuresi.zip",temp,mode="wb")
pinus_resinosa <-unzip(temp)
pinus_resinosa <-readShapePoly(pinus_resinosa)
unlink(temp) #Remove temporary file

map("worldHires",regions=c("USA","Canada"),exact=T)
plot(pinus_strobus,add=T,col="#FF000080")
plot(pinus_resinosa,add=T,col="#0000FF80")

### We can also download occurence data for a given species using the dismo package 
install.packages("dismo")
require(dismo)

pstrob_occ<-gbif("Pinus","resinosa")

map("worldHires",regions=c("USA","Canada"),exact=T)
plot(pinus_strobus,add=T,col="#FF000080")
points(pstrob_occ$lon, pstrob_occ$lat,pch=21,bg="#0000FF80",cex=1)