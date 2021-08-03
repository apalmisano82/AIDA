# Install library if not installed -------------------------------------####
if (!('rcarbon' %in% installed.packages())){
  install.packages('rcarbon')
}

# Install library if not installed -------------------------------------####
if (!('doParallel' %in% installed.packages())){
  install.packages('doParallel')
}

# Install library if not installed -------------------------------------####
if (!('rgdal' %in% installed.packages())){
  install.packages('rgdal')
}

# Install library if not installed -------------------------------------####
if (!('spatstat' %in% installed.packages())){
  install.packages('spatstat')
}

# Install library if not installed -------------------------------------####
if (!('raster' %in% installed.packages())){
  install.packages('raster')
}

if (!('maptools' %in% installed.packages())){
  install.packages('maptools')
}

if (!('maps' %in% installed.packages())){
  install.packages('maps')
}

# Install library if not installed -------------------------------------####
if (!('cartography' %in% installed.packages())){
  install.packages('cartography')
}

# Install library if not installed -------------------------------------####
if (!('sf' %in% installed.packages())){
  install.packages('sf')
}

## Add libraries (you may need to install these first)
library(rcarbon)
library(doParallel)
library(rgdal)
library(raster)
library(maptools)
library(maps)
library(gtools)
library(sf)

## Load data
mydates <- read.csv("csv/dates.csv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE)

# Load study area polygon (from GIS-friendly, ESRI-format shapefiles  to "SpatialPolygonsDataFrame" objects in R)
ita<-readOGR("shp/italy_regions.shp", layer="italy_regions")
north<-ita[ita$ID==1,]
central<-ita[ita$ID==2,]
south<-ita[ita$ID==3,]
sicily<-ita[ita$ID==4,]
sardinia<-ita[ita$ID==5,]

# Turn the whole date dataset into a spatial object than can be plotted on a map
# First, make sure the coordinates are treated as numbers
mydates$Longitude <- as.numeric(mydates$Longitude) 
mydates$Latitude <- as.numeric(mydates$Latitude)
# Then throw out any without coordinates
datesp <- mydates[!is.na(mydates$Longitude) & !is.na(mydates$Latitude),]
# Then designate the coordinate columns
coordinates(datesp) <- ~Longitude+Latitude
# Then stipulate that the coordinate system of this data is the same as the polygon data we have (i.e. unprojected latlon)
proj4string(datesp) <- proj4string(north)<-proj4string(central)<-proj4string(south)<-proj4string(sardinia)<-proj4string(sicily)

#Spatial query: selection of points (datesp) located within region (=1)
query_north<-over(datesp,north)
query_central<-over(datesp,central)
query_south<-over(datesp,south)
query_sicily<-over(datesp,sicily)
query_sardinia<-over(datesp,sardinia)

#convert vector into dataframe
north<-as.data.frame(datesp)
central<-as.data.frame(datesp)
south<-as.data.frame(datesp)  
sicily<-as.data.frame(datesp)
sardinia<-as.data.frame(datesp)

#add column for storing spatial query's results
north["query"]<-NA
central["query"]<-NA
south["query"]<-NA
sicily["query"]<-NA
sardinia["query"]<-NA
#update the column "query"
north$query<-query_north
central$query<-query_central
south$query<-query_south
sicily$query<-query_sicily
sardinia$query<-query_sardinia
#subsetting the dates located in the study area
mydates_north<-subset(north, north$query==1)
mydates_central<-subset(central, central$query==2)
mydates_south<-subset(south, south$query==3)
mydates_sicily<-subset(sicily, sicily$query==4)
mydates_sardinia<-subset(sardinia, sardinia$query==5)
#add column for defining region
mydates_north["Region"]<-1
mydates_central["Region"]<-2
mydates_south["Region"]<-3
mydates_sicily["Region"]<-4
mydates_sardinia["Region"]<-5

#aggregate all Radiocarbon dates
mydates<-rbind(mydates_north,mydates_central,mydates_south,mydates_sardinia, mydates_sicily)
#select radiocarbon dates from short-lived samples 
shortlived<-mydates[grep(paste(c("antler","bone","seed","seeds", "tissue","fruit", "bome", "tooth","horn", "twig","grain", "collagen", "hair", "leather"),collapse="|"), mydates$Material),] # subselect shortlived samples

# Turn the whole date dataset into a spatial object than can be plotted on a map
# First, make sure the coordinates are treated as numbers
mydates$Longitude <- as.numeric(mydates$Longitude) 
mydates$Latitude <- as.numeric(mydates$Latitude)
mydates_north$Longitude <- as.numeric(mydates_north$Longitude) 
mydates_north$Latitude <- as.numeric(mydates_north$Latitude)
mydates_central$Longitude <- as.numeric(mydates_central$Longitude) 
mydates_central$Latitude <- as.numeric(mydates_central$Latitude)
mydates_south$Longitude <- as.numeric(mydates_south$Longitude) 
mydates_south$Latitude <- as.numeric(mydates_south$Latitude)
mydates_sicily$Longitude <- as.numeric(mydates_sicily$Longitude) 
mydates_sicily$Latitude <- as.numeric(mydates_sicily$Latitude)
mydates_sardinia$Longitude <- as.numeric(mydates_sardinia$Longitude) 
mydates_sardinia$Latitude <- as.numeric(mydates_sardinia$Latitude)
# Then throw out any without coordinates
datesp<-mydates[!is.na(mydates$Longitude) & !is.na(mydates$Latitude),]
datesp_north <- mydates_north[!is.na(mydates_north$Longitude) & !is.na(mydates_north$Latitude),]
datesp_central <- mydates_central[!is.na(mydates_central$Longitude) & !is.na(mydates_central$Latitude),]
datesp_south <- mydates_south[!is.na(mydates_south$Longitude) & !is.na(mydates_south$Latitude),]
datesp_sicily <- mydates_sicily[!is.na(mydates_sicily$Longitude) & !is.na(mydates_sicily$Latitude),]
datesp_sardinia <- mydates_sardinia[!is.na(mydates_sardinia$Longitude) & !is.na(mydates_sardinia$Latitude),]
# Then designate the coordinate columns
coordinates(datesp) <- ~Longitude+Latitude
lonlat <- CRS("+init=epsg:4326") # LatLon WGS84
proj4string(datesp) <- lonlat
countries<-readOGR("shp/eur_states_lo.shp", layer="eur_states_lo")
datesp <- spTransform(datesp, proj4string(countries)) # UTM WGS84, zone 37 North

coordinates(datesp_north) <- ~Longitude+Latitude
coordinates(datesp_central) <- ~Longitude+Latitude
coordinates(datesp_south) <- ~Longitude+Latitude
coordinates(datesp_sicily) <- ~Longitude+Latitude
coordinates(datesp_sardinia) <- ~Longitude+Latitude
# Then stipulate that the coordinate system of this data is the same as the polygon data we have (i.e. unprojected latlon)
proj4string(datesp_north) <- proj4string(datesp_central)<-proj4string(datesp_south)<-proj4string(datesp_sardinia)<-proj4string(datesp_sicily)

#Plot spatial distribution radiocarbon dates and palaeoclimate proxies
pdf(file="pdf/Fig1.pdf", width=8, height=7)
par(mar=c(0, 0, 0, 0)) #c(bottom, left, top, right)
plot(countries,col="grey75", border="white", lwd=0.2, xlim = c(11, 15),ylim = c(36,47))
dem<-raster("raster/dem_ita.tif")
lakes<-readOGR("shp/lakes.shp", layer="lakes")
image(dem, col= colorRampPalette(c("grey60", "grey36", "grey18", "grey12", "grey0"))(20), add=T, legend=F)
plot(lakes, col="white", border="NA", add=T)
points(datesp[datesp$Region==1,], col="red", pch=19, cex=0.25)
points(datesp[datesp$Region==2,], col="cyan", pch=19, cex=0.25)
points(datesp[datesp$Region==3,], col="green2", pch=19, cex=0.25)
points(datesp[datesp$Region==4,], col="brown", pch=19, cex=0.25)
points(datesp[datesp$Region==5,], col="blue", pch=19, cex=0.25)
palaeoclimate<-readOGR("shp/palaeoclimate_records.shp", layer="palaeoclimate_records") #load shapefile of palaeoclimate records
plot(palaeoclimate,col="black", bg="yellow", pch=24, cex=1, add=T)
text(x=11.76,y=45.78,"1",cex=0.75) #plot the labels of palaoclimate records
text(x=10.92,y=45.44,"2",cex=0.75)
text(x=9.95,y=44.20,"3",cex=0.75)
text(x=10.20,y=43.80,"4",cex=0.75)
text(x=19.32,y=41.95,"5",cex=0.75)
text(x=17.92,y=41.28,"6",cex=0.75)
text(x=17.80,y=39.60,"7",cex=0.75)
text(x=20.25,y=39.90,"8",cex=0.75)
text(x=13.16,y=38.45,"9",cex=0.75)
text(x=12.63,y=37.40,"10",cex=0.75)
text(x=14,y=37.60,"11",cex=0.75)
legend(5.50,41.0, legend=c("North","Central","South","Sicily", "Sardinia", "Palaeoclimate"), pch=c(19,19,19,19,19,17), col=c("red","cyan","green2", "brown","blue","yellow"), bty="n", cex=0.8)
xpos <- 19
ypos <- 37.5
scalesize <- 0.5
lines(c(xpos,xpos),c(ypos-scalesize,ypos+scalesize),col="black")
polygon(c(xpos,xpos-(scalesize/12),xpos,xpos+(scalesize/12),xpos),c((ypos+scalesize/1.5),(ypos+scalesize/1.5),(ypos+scalesize),(ypos+scalesize/1.5),(ypos+scalesize/1.5)), col="black")
text(xpos, ypos, "N", cex=0.7, col="black", font=2)
map.scale(x=16, y=37, ratio=FALSE, relwidth=0.10,cex=0.5)
dev.off()

############################ Map the spatial Kernel intensity of radiocarbon dates #############################################

#load required libraries
library(spatstat)
library(cartography)

#Load the shape file to use as window of analysis
italy<-readOGR("shp/italy.shp", layer="italy")
GDP<-readOGR("shp/italy_GDP_region.shp", layer="italy_GDP_region")
#Assign a coordinate reference system
italy<-spTransform(italy,CRS=CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
GDP<-spTransform(GDP,CRS=CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
datesp_utm<-spTransform(datesp,CRS=CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Convert the window of analysis into a an owin class object
window <- as(italy,"owin")

# Generate a spatial Kernel density map of unbinned radiocarbon dates
sbw <- 50000 # Gaussian bandwdith (1 sd in metres)
cellres <- 1000 # raster cell size (in metres)
mysetppp <- ppp(x=coordinates(datesp_utm)[,1],y=coordinates(datesp_utm)[,2],window=as.owin(window))
# you will get the following message "Warning message:data contain duplicated points". Skip it.  
alldens <- density(mysetppp, sigma=sbw, eps=cellres, edge=FALSE)
alldenstc <- plot(alldens, main="", box=FALSE, do.plot=FALSE)

##Generate a spatial Kernel density map for binned radiocarbon dates
mydates_utm<-as.data.frame(datesp_utm)#create a dataframe with UTM (zone 32) coordinates
#calibrate the radiocarbon dates
x <- calibrate(x=mydates_utm$CRA, errors=mydates_utm$Error, normalised=FALSE,verbose=FALSE)
bins1 <- binPrep(sites=mydates_utm$SiteID, ages=mydates_utm$CRA, h=50)
stkde1 <- stkde(x=x, coords=mydates_utm[,c("Longitude", "Latitude")], win=window, sbw=50000, cellres=1000, focalyears=seq(5200, 5000, -200), tbw=50, bins=bins1, backsight=200, outdir=tempdir(), amount=1, verbose=FALSE)
#skip the warning message as it refers to the radiocarbon dates located on small islands. They are irrelevant to plot the Kernel density by using a 50 km bandwidth.

#Plot the maps
pdf(file="pdf/Fig2.pdf", width=12, height=6)
par(mfrow=c(1,3))
par(mar=c(1, 0, 1, 0)) #c(bottom, left, top, right)
plot(italy,col="white",border="black",xlim = c(548000,1095000),ylim = c(3880000, 5220000))
plot(alldens, main="", box=FALSE, add=TRUE)
points(datesp_utm, pch=19, cex=0.2, col="limegreen")
#draw the density legend
plot(alldenstc,vertical=TRUE, las=2, main="", xlim=c(388000 ,428000), ylim=c(3950000 ,4150000), add=TRUE, cex.axis=0.8, axis=FALSE)
text(x=418000, y=4200000, labels="high", cex=1.5)
text(x=418000, y=3910000, labels="low", cex=1.5)
text(588000, y=4050000, expression(paste(sigma," = 50 km")), cex=1.5, col="black", font=1)
text(x=368000, y=5200000, labels="a", cex=2, font=2)
#draw the north arrow
xpos <- 888000
ypos <- 4020000
lines(c(xpos,xpos),c(4080000,4000000),col="black")
polygon(c(xpos,xpos-(10000),xpos,xpos+(10000),xpos),c((ypos+70000),(ypos+70000),(ypos+90000),(ypos+70000),(ypos+70000)), col="black")
text(xpos, ypos+25000, "N", cex=0.9, col="black", font=2)
#draw the scale bar
polygon(c(780000,780000,980000,980000),c(3900000,3920000,3920000,3900000), col="NA", border="black")
text(880000, y=3950000, "200 km", cex=1, col="black", font=1)
box()
par(mar=c(1, 0, 1, 0)) #c(bottom, left, top, right)
plot(italy,col="white",border="black",xlim = c(548000,1095000),ylim = c(3880000, 5220000))
text(x=368000, y=5200000, labels="b", cex=2, font=2)
plot(stkde1, 5200, type="nonfocal", main="",add=TRUE)
points(datesp_utm, pch=19, cex=0.2, col="limegreen")
#draw the north arrow
xpos <- 888000
ypos <- 4020000
lines(c(xpos,xpos),c(4080000,4000000),col="black")
polygon(c(xpos,xpos-(10000),xpos,xpos+(10000),xpos),c((ypos+70000),(ypos+70000),(ypos+90000),(ypos+70000),(ypos+70000)), col="black")
text(xpos, ypos+25000, "N", cex=0.9, col="black", font=2)
#draw the scale bar
polygon(c(780000,780000,980000,980000),c(3900000,3920000,3920000,3900000), col="NA", border="black")
text(880000, y=3950000, "200 km", cex=1, col="black", font=1)
plot(alldenstc,vertical=TRUE, las=2, main="", xlim=c(388000 ,428000), ylim=c(3950000 ,4150000), add=TRUE, cex.axis=0.8, axis=FALSE)
text(x=418000, y=4200000, labels="high", cex=1.5)
text(x=418000, y=3910000, labels="low", cex=1.5)
text(588000, y=4050000, expression(paste(sigma," = 50 km")), cex=1.5, col="black", font=1)
box()
par(mar=c(1, 0, 1, 0)) #c(bottom, left, top, right)
plot(italy,col="white",border="black",xlim = c(548000,1095000),ylim = c(3880000, 5220000))
#plot(GDP, col=GDP$gdp_capita, add=TRUE)
plot(GDP , col=my_colors ,  bg = "#A6CAE0", add=TRUE)
text(x=368000, y=5200000, labels="c", cex=2, font=2)
#draw the north arrow
xpos <- 888000
ypos <- 4020000
lines(c(xpos,xpos),c(4080000,4000000),col="black")
polygon(c(xpos,xpos-(10000),xpos,xpos+(10000),xpos),c((ypos+70000),(ypos+70000),(ypos+90000),(ypos+70000),(ypos+70000)), col="black")
text(xpos, ypos+25000, "N", cex=0.9, col="black", font=2)
#draw the scale bar
polygon(c(780000,780000,980000,980000),c(3900000,3920000,3920000,3900000), col="NA", border="black")
text(880000, y=3950000, "200 km", cex=1, col="black", font=1)
box()
choroLayer(spdf = GDP, df = GDP@data, var = "gdp_total",col = carto.pal(pal1 = "red.pal", n1 = 5), method="quantile", nclass=5, legend.title.txt = "Total GDP", add=TRUE)
dev.off()


#Calculate the Spearman correlation coefficient between the regional number of radiocarbon dates and the Total GDP
cor.test(GDP@data$n_dates,GDP@data$gdp_total, method="spearman")
#the two samples show a quite strong correlation. R2=0.66, p-value <0.01


############################# SPD radiocarbon dates ###############################################
## General SPD parameters
nsim <- 1000 # number of actual simulations 
ncores <- 6 # multi-core processing (set higher if available)
runm <- 50 #smoothing of SPDs
binh <- 50 #bin clustering
realstartBP <- 10000
realendBP <-2500
bracket <- 1000
workingstartBP <- realstartBP+bracket
workingendBP <- realendBP-bracket
if (workingendBP<0){ workingendBP <- 0 }


# N.B.The generation of some SPDs and null models could be time consuming. So, you can load the RData file storing all the outputs generated via the R script here provided.
#load("rda/radiocarbon.RData")

## Calibrate dates 
alldates <- calibrate(x=mydates$CRA, errors=mydates$Error, calCurves='intcal20', method="standard", normalised=FALSE, ncores=ncores, calMatrix=TRUE)
alldates_norm <- calibrate(x=mydates$CRA, errors=mydates$Error, calCurves='intcal20', method="standard", normalised=TRUE, ncores=ncores, calMatrix=TRUE)
northdates <- calibrate(x=mydates_north$CRA, errors=mydates_north$Error, calCurves='intcal20', method="standard", normalised=FALSE, ncores=ncores, calMatrix=TRUE)
centraldates <- calibrate(x=mydates_central$CRA, errors=mydates_central$Error, calCurves='intcal20', method="standard", normalised=FALSE, ncores=ncores, calMatrix=TRUE)
southdates <- calibrate(x=mydates_south$CRA, errors=mydates_south$Error, calCurves='intcal20', method="standard", normalised=FALSE, ncores=ncores, calMatrix=TRUE)
sicilydates <- calibrate(x=mydates_sicily$CRA, errors=mydates_sicily$Error, calCurves='intcal20', method="standard", normalised=FALSE, ncores=ncores, calMatrix=TRUE)
sardiniadates <- calibrate(x=mydates_sardinia$CRA, errors=mydates_sardinia$Error, calCurves='intcal20', method="standard", normalised=FALSE, ncores=ncores, calMatrix=TRUE)
shortlivedates <- calibrate(x=shortlived$CRA, errors=shortlived$Error, calCurves='intcal20', method="standard", normalised=FALSE, ncores=ncores, calMatrix=TRUE)

#sensitivity analysis to assess how different cut-off values for bin clustering can modify the shape of the SPD
binsense(x=alldates,y=mydates$SiteID,h=seq(50,200,50),timeRange=c(10000,2800)) 
# A visual assessment shows that the overall shape of SPDs are similar with cut-off values ranging from 50 to 200.  


## SUMMED PROBABILITY DISTRIBUTIONS (SPDs)
#The whole Italy
bins <- binPrep(sites=mydates$SiteID, ages=mydates$CRA, h=binh)
allspdn<- spd(x=alldates, bins=bins, timeRange=c(workingstartBP,workingendBP), datenormalised=TRUE, runm=runm)
allspd<- spd(x=alldates, bins=bins, timeRange=c(workingstartBP,workingendBP), datenormalised=FALSE, runm=runm)

#Northern Italy
bins_north <- binPrep(sites=mydates_north$SiteID, ages=mydates_north$CRA, h=binh)
allspd_north <- spd(x=northdates, bins=bins_north, timeRange=c(workingstartBP,workingendBP), datenormalised=FALSE, runm=runm)

#Central Italy
bins_central <- binPrep(sites=mydates_central$SiteID, ages=mydates_central$CRA, h=binh)
allspd_central <- spd(x=centraldates, bins=bins_central, timeRange=c(workingstartBP,workingendBP), datenormalised=FALSE, runm=runm)

#Southern Italy
bins_south <- binPrep(sites=mydates_south$SiteID, ages=mydates_south$CRA, h=binh)
allspd_south <- spd(x=southdates, bins=bins_south, timeRange=c(workingstartBP,workingendBP), datenormalised=FALSE, runm=runm)

#Sicily
bins_sicily <- binPrep(sites=mydates_sicily$SiteID, ages=mydates_sicily$CRA, h=binh)
allspd_sicily <- spd(x=sicilydates, bins=bins_sicily, timeRange=c(workingstartBP,workingendBP), datenormalised=FALSE, runm=runm)

#Sardinia
bins_sardinia <- binPrep(sites=mydates_sardinia$SiteID, ages=mydates_sardinia$CRA, h=binh)
allspd_sardinia <- spd(x=sardiniadates, bins=bins_sardinia, timeRange=c(workingstartBP,workingendBP), datenormalised=FALSE, runm=runm)

#short-lived samples
bins_shortlived <- binPrep(sites=shortlived$SiteID, ages=shortlived$CRA, h=binh)
allspd_shortlived <- spd(x=shortlivedates, bins=bins_shortlived, timeRange=c(workingstartBP,workingendBP), datenormalised=FALSE, runm=runm)

#Calculate the Pearson correlation coefficient between the normalised and unnromalised SPDs of calibrated radiocarbon dates
cor.test(allspdn$grid$PrDens, allspd$grid$PrDens, method = "pearson")
#the two curves are highly correlated. r=0.93, p-value <0.01

#Calculate the Pearson correlation coefficient between the SPD (unnormalised) obtained calibrating all radiocarbon samples and the SPD from short-lived radiocarbon dates
cor.test(allspd$grid$PrDens, allspd_shortlived$grid$PrDens, method = "pearson")
#the two curves are highly correlated. r=0.92, p-value <0.01

#Compute Composite Kernel Density Estimates (CKDE)
randomdates_norm<-sampleDates(alldates_norm, bins = bins, nsim = 1000, boot = TRUE)#sample random calendar dates
ckde_norm = ckde(randomdates_norm,timeRange=c(workingstartBP,workingendBP),bw=200, normalised = TRUE)
# CKDE weighted to emulate an SPD with non-normalised dates
randomdates<-sampleDates(alldates, bins = bins, nsim = 1000, boot = TRUE)#sample random calendar dates
ckde = ckde(randomdates,timeRange=c(workingstartBP,workingendBP),bw=200, normalised = FALSE)

randomdates_north<-sampleDates(northdates, bins = bins_north, nsim = 1000, boot = TRUE)#sample random calendar dates
ckde_north = ckde(randomdates_north,timeRange=c(workingstartBP,workingendBP),bw=200, normalised = FALSE)

randomdates_central<-sampleDates(centraldates, bins = bins_central, nsim = 1000, boot = TRUE)#sample random calendar dates
ckde_central = ckde(randomdates_central,timeRange=c(workingstartBP,workingendBP),bw=200, normalised = FALSE)

randomdates_south<-sampleDates(southdates, bins = bins_south, nsim = 1000, boot = TRUE)#sample random calendar dates
ckde_south = ckde(randomdates_south,timeRange=c(workingstartBP,workingendBP),bw=200, normalised = FALSE)

randomdates_sicily<-sampleDates(sicilydates, bins = bins_sicily, nsim = 1000, boot = TRUE)#sample random calendar dates
ckde_sicily = ckde(randomdates_sicily,timeRange=c(workingstartBP,workingendBP),bw=200, normalised = FALSE)

randomdates_sardinia<-sampleDates(sardiniadates, bins = bins_sardinia, nsim = 1000, boot = TRUE)#sample random calendar dates
ckde_sardinia = ckde(randomdates_sardinia,timeRange=c(workingstartBP,workingendBP),bw=200, normalised = FALSE)

#calculate the median of bins
bins.med<-binMed(x=alldates, bins=bins)

##### Run null models to compare against the observed SPD of calibrated radiocarbon dates
## Exponential model
set.seed(123)
expnull <- modelTest(alldates, errors=mydates$Error, bins=bins, nsim=nsim, runm=runm, timeRange=c(10000,2000), model="exponential", method="uncalsample", ncores=ncores, datenormalised=FALSE)

## Fit a logistic model and create a predicted SPD
# Generate a smoothed SPD
spd.smoothed = spd(alldates,timeRange=c(11000,1500),bins=bins,runm=runm)
# Start values should be adjusted depending on the observed SPD
logFit <- nls(PrDens~SSlogis(calBP, Asym, xmid, scale),data=spd.smoothed$grid,control=nls.control(maxiter=200),start=list(Asym=0.2,xmid=4500,scale=-100))
# Generate a data frame containing the fitted values
logFitDens=data.frame(calBP=spd.smoothed$grid$calBP,PrDens=SSlogis(input=spd.smoothed$grid$calBP,Asym=coefficients(logFit)[1],xmid=coefficients(logFit)[2],scal=coefficients(logFit)[3]))
# Use the modelTest function (returning the raw simulation output - see below)
logisticmod <- modelTest(alldates, errors=mydates$Error, bins=bins,nsim=nsim,timeRange=c(11000,1500), model="custom",predgrid=logFitDens, runm=runm, ncores=ncores, raw=TRUE)

#source the modified code for plotting the composite KDE
source("function/plot.compositeKDE.R") 

## Plot SPDs 
pdf(file="pdf/Fig3.pdf", width=6, height=8)
layout(matrix(c(1,2,3,4), 4, 1, byrow=TRUE), widths=6, heights=c(1.8,1.8,1.8,2.6))
par(mar=c(0, 1, 1, 1)) #c(bottom, left, top, right)
ymax <- max(allspdn$grid$PrDens)
plot(allspdn, xlim=c(10000,2800), xaxt='n', yaxt='n')
abline(v=seq(10000,3000,-500), lty="dotted", col="white")
text(x=9800, ymax*0.98, labels="Italy", font=2, cex=1.2, adj=c(0,0.7))
text(x=9800, ymax*0.88, labels="a. All Dates (normalised)", font=2, cex=0.9, adj=c(0,0.7))
text(x=9800, ymax*0.80, labels=paste("n=",nrow(mydates),", sites=",length(unique(mydates$SiteID)),", bins=",length(unique(bins)),sep=""), font=1, cex=0.9, adj=c(0,0.7))
legend(x=9800,ymax*0.76, legend=c("SPD"), lty=c("solid","solid"), lwd=c(3,0.5,1), col=c("grey90","grey50"), bty="n", cex=0.9)
legend(x=9800,ymax*0.76, legend=c("SPD"), lty=c("solid","solid"), lwd=c(3,0.5,1), col=c("grey90","grey50"), bty="n", cex=0.9)
legend(x=9800,ymax*0.66, legend=c("cKDE"), lty=c("solid","solid"), lwd=c(3,0.5,1), col=c("lightslateblue","lightslateblue"), bty="n", cex=0.9)
barCodes(bins.med)
par(new=TRUE)
plot(ckde_norm, xlim=c(10000,2800),type='envelope', line.col="black", fill.col=(rgb(0,0,255,alpha=60, maxColorValue=255)), xaxt='n', yaxt='n')
box()
par(mar=c(0, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(allspd$grid$PrDens)
plot(allspd, xlim=c(10000,2800), xaxt='n', yaxt='n')
plot(allspd_shortlived, xlim=c(10000,3000), type="simple",col="darkgreen", xaxt='n', yaxt='n', add=T)
abline(v=seq(10000,3000,-500), lty="dotted", col="white")
text(x=9800, ymax*0.88, labels="b. All Dates (unnormalised)", font=2, cex=0.9, adj=c(0,0.7))
text(x=9800, ymax*0.80, labels=paste("n=",nrow(mydates),", sites=",length(unique(mydates$SiteID)),", bins=",length(unique(bins)),sep=""), font=1, cex=0.9, adj=c(0,0.7))
text(x=9800, ymax*0.72, labels=paste("shortlived","  ", "n=",nrow(shortlived),", sites=",length(unique(shortlived$SiteID)),", bins=",length(unique(bins_shortlived)),sep=""), font=1, cex=0.9, adj=c(0,0.7), col="darkgreen")
legend(x=9800,ymax*0.70, legend=c("SPD"), lty=c("solid","solid"), lwd=c(3,0.5,1), col=c("grey90","grey50"), bty="n", cex=0.9)
legend(x=9800,ymax*0.60, legend=c("cKDE"), lty=c("solid","solid"), lwd=c(3,0.5,1), col=c("lightslateblue","lightslateblue"), bty="n", cex=0.9)
par(new=TRUE)
plot(ckde, xlim=c(10000,2800),type='envelope', line.col="black", fill.col=(rgb(0,0,255,alpha=60, maxColorValue=255)), xaxt='n', yaxt='n')
box()
par(mar=c(0, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(logisticmod$result$PrDens)*1.1
plot(logisticmod, ylim=c(0,ymax), xlim=c(10000,2800), drawaxes=FALSE)
lines(logisticmod$fit$calBP,logisticmod$fit$PrDens, col="black", lty="dashed", lwd=0.5)
abline(v=seq(10000,3000,-500), lty="dotted", col="white")
legend(x=9800, ymax*0.94, legend=c("SPD (dates not normalised)","Logistic Model", "95% MC envelope","positive deviation","negative deviation"),col=c(1, "black","lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)), lty=c(1,2,1,1,1), lwd=c(0.5,0.5,5,5,5), cex=0.6, bg="white", title="")
text(x=9700, ymax*0.92, labels="c. Logistic Fit", font=2, cex=0.8, adj=c(0,0.7))
text(x=9400, ymax*0.60, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(logisticmod$pval,4))))
box()
text(x=8850, y=ymax*0.0001, labels="Mesolithic",lty=c("solid"), col=c("black"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(8000, ymax*0.15,8000,ymax*0.02, lty="solid", col="black", lwd=(1.2))
text(x=6450, ymax*0.11, labels="Neolithic",lty=c("solid"), col=c("black"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5450, ymax*0.15,5450,ymax*0.02, lty="solid", col="black", lwd=(1.2))
text(x=7500, ymax*0.01, labels="Early",lty=c("solid"), col=c("black"), font=2, cex=1, adj=c(0.5,-0.5))
segments(7150, ymax*0.12,7150,ymax*0.02, lty="dotted", col="black", lwd=(1.2))
text(x=6800, ymax*0.01, labels="Middle",lty=c("solid"), col=c("black"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6450, ymax*0.12,6450,ymax*0.02, lty="dotted", col="black", lwd=(1.2))
text(x=5950, ymax*0.01, labels="Late",lty=c("solid"), col=c("black"), font=2, cex=1, adj=c(0.5,-0.5))
text(x=4850, ymax*0.01, labels="Copper Age",lty=c("solid"), col=c("black"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(4250, ymax*0.15,4250,ymax*0.02, lty="solid", col="black", lwd=(1.2))
text(x=3580, ymax*0.12, labels="Bronze Age",lty=c("solid"), col=c("black"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(2950, ymax*0.15,2950,ymax*0.02, lty="solid", col="black", lwd=(1.2))
segments(3650, ymax*0.12,3650,ymax*0.02, lty="dotted", col="black", lwd=(1.2))
segments(3275, ymax*0.12,3275,ymax*0.02, lty="dotted", col="black", lwd=(1.2))
segments(3100, ymax*0.12,3100,ymax*0.02, lty="dotted", col="black", lwd=(1.2))
text(x=3950, ymax*0.03, labels="EBA",lty=c("solid"), col=c("black"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3480, ymax*0.03, labels="MBA",lty=c("solid"), col=c("black"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3130, ymax*0.06, labels="LBA",lty=c("solid"), col=c("black"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2960, ymax*0.06, labels="FBA",lty=c("solid"), col=c("black"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2830, ymax*0.011, labels="Iron Age",lty=c("solid"), col=c("black"), font=2, srt=90, cex=0.9, adj=c(-0.1,-0.2))
par(mar=c(6, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(expnull$result$PrDens)*1.1
plot(expnull, ylim=c(0,ymax), xlim=c(10000,2800), drawaxes=FALSE)
lines(expnull$fit$calBP,expnull$fit$PrDens, col="black", lty="dashed", lwd=0.5)
abline(v=seq(10000,3000,-500), lty="dotted", col="white")
legend(x=9800, ymax*0.91, legend=c("SPD (dates not normalised)","Exponential Model", "95% MC envelope","positive deviation","negative deviation"),col=c(1, "black","lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)), lty=c(1,2,1,1,1), lwd=c(0.5,0.5,5,5,5), cex=0.6, bg="white", title="")
text(x=9700, ymax*0.88, labels="d. Exponential Fit", font=2, cex=0.8, adj=c(0,0.7))
text(x=9400, ymax*0.58, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(expnull$pval,4))))
box()
xticks <- seq(10000,3000,-500)
axis(side=1, at=xticks, labels=xticks, las=2, cex.axis=0.8)# add BP axis
mtext("cal BP",1, 1, at=6450, adj=-0.1, font=1, cex=0.5)
axis(side=1, at=xticks-50, labels=xticks-2000, las=2, cex.axis=0.8,pos=-0.17)# add BC/AD axis
mtext("BC/AD",1, 4, at=6400, adj=-0.1, font=1, cex=0.5)
dev.off()


## Permutation Test for Regional Departure from Global Pattern (CPU-intensive, set nsim to 5 to test)
set.seed(123)
perm1 <- permTest(x=alldates, bins=bins, marks=datesp$Region, timeRange=c(workingstartBP,workingendBP), runm=runm, nsim=nsim)

#Plot regional bootstrapped Kernel Density Estimation (KDE)
pdf(file="pdf/Fig4.pdf", width=6, height=7)
layout(matrix(c(1,2,3,4,5), 5, 1, byrow=TRUE), widths=5, heights=c(1.5,1.5,1.5,1.5,2.5))
par(mar=c(0, 1, 0, 1)) 
par(yaxs="i")
par(xaxs="i")
xticks <- seq(10000,3000,-500)
ymax <- max(allspd_north$grid$PrDens)
plot(allspd_north, xlim=c(10000,2800), xaxt='n', yaxt='n')
abline(v=seq(10000,3000,-500), lty="dotted", col="black")
legend(x=9800, y=ymax*0.95,legend=c("SPD","cKDE"),col=c("lightgrey","red"),lty=c(1,1),lwd=c(5,5),cex=0.6, bg="white", title=expression(bold("a. Northern Italy")))
text(x=9800, y=ymax*0.60, labels=paste("n=",nrow(datesp[datesp$Region==1,]),", sites=",length(unique(datesp$SiteName[datesp$Region==1])),", bins=",length(unique(bins[datesp$Region==1])),sep=""), font=1, cex=0.8, adj=c(0,0.7))
text(x=8850, y=ymax*0.0001, labels="Mesolithic",lty=c("solid"), col=c("red"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(7750, ymax*0.15,7750,ymax*0.02, lty="solid", col="red", lwd=(1.2))
text(x=6450, ymax*0.11, labels="Neolithic",lty=c("solid"), col=c("red"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5450, ymax*0.15,5450,ymax*0.02, lty="solid", col="red", lwd=(1.2))
text(x=7300, ymax*0.01, labels="Early",lty=c("solid"), col=c("red"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6950, ymax*0.15,6950,ymax*0.02, lty="dotted", col="red", lwd=(1))
text(x=6600, ymax*0.01, labels="Middle",lty=c("solid"), col=c("red"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6250, ymax*0.12,6250,ymax*0.02, lty="dotted", col="red", lwd=(1))
text(x=5850, ymax*0.01, labels="Late",lty=c("solid"), col=c("red"), font=2, cex=1, adj=c(0.5,-0.5))
text(x=4820, ymax*0.01, labels="Copper Age",lty=c("solid"), col=c("red"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="red", lwd=(1.2))
text(x=3500, ymax*0.13, labels="Bronze Age",lty=c("solid"), col=c("red"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(2850, ymax*0.15,2850,ymax*0.02, lty="solid", col="red", lwd=(1.2))
segments(3600, ymax*0.12,3600,ymax*0.02, lty="dotted", col="red", lwd=(1))
segments(3275, ymax*0.12,3275,ymax*0.02, lty="dotted", col="red", lwd=(1))
segments(3150, ymax*0.12,3150,ymax*0.02, lty="dotted", col="red", lwd=(1))
text(x=3850, ymax*0.01, labels="EBA",lty=c("solid"), col=c("red"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3430, ymax*0.01, labels="MBA",lty=c("solid"), col=c("red"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3140, ymax*0.07, labels="LBA",lty=c("solid"), col=c("red"), font=2, srt=90, cex=0.5, adj=c(0.5,-0.5))
text(x=3000, ymax*0.01, labels="FBA",lty=c("solid"), col=c("red"), font=2, cex=0.8, adj=c(0.5,-0.5))
par(new=TRUE)
plot(ckde_north, xlim=c(10000,2800),type='envelope', line.col="red", fill.col=(rgb(255,51,51,alpha=60, maxColorValue=255)), xaxt='n', yaxt='n')
par(mar=c(0, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(allspd_central$grid$PrDens)
plot(allspd_central, xlim=c(10000,2800), xaxt='n', yaxt='n')
abline(v=seq(10000,3000,-500), lty="dotted", col="black")
legend(x=9800, y=ymax*0.95,legend=c("SPD","cKDE"),col=c("lightgrey","cyan3"),lty=c(1,1),lwd=c(5,5),cex=0.6, bg="white", title=expression(bold("b. Central Italy")))
text(x=9800, y=ymax*0.60, labels=paste("n=",nrow(datesp[datesp$Region==2,]),", sites=",length(unique(datesp$SiteName[datesp$Region==2])),", bins=",length(unique(bins[datesp$Region==2])),sep=""), font=1, cex=0.8, adj=c(0,0.7))
text(x=8850, y=ymax*0.05, labels="Mesolithic",lty=c("solid"), col=c("darkcyan"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(7950, ymax*0.15,7950,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
text(x=6450, ymax*0.06, labels="Neolithic",lty=c("solid"), col=c("darkcyan"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5450, ymax*0.15,5450,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
text(x=7500, ymax*-0.01, labels="Early",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
segments(7150, ymax*0.12,7150,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
text(x=6800, ymax*-0.01, labels="Middle",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6450, ymax*0.12,6450,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
text(x=5950, ymax*-0.01, labels="Late",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
text(x=4850, ymax*0.01, labels="Copper Age",lty=c("solid"), col=c("darkcyan"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(4250, ymax*0.15,4250,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
text(x=3580, ymax*0.11, labels="Bronze Age",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
segments(2950, ymax*0.15,2950,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
segments(3650, ymax*0.12,3650,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
segments(3275, ymax*0.12,3275,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
segments(3100, ymax*0.12,3100,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
text(x=3950, ymax*-0.01, labels="EBA",lty=c("solid"), col=c("darkcyan"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3480, ymax*-0.01, labels="MBA",lty=c("solid"), col=c("darkcyan"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3130, ymax*0.06, labels="LBA",lty=c("solid"), col=c("darkcyan"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2960, ymax*0.06, labels="FBA",lty=c("solid"), col=c("darkcyan"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2865, ymax*0.011, labels="IA",lty=c("solid"), col=c("darkcyan"), font=2, cex=0.8, adj=c(0.5,-0.5))
par(new=TRUE)
plot(ckde_central, xlim=c(10000,2800),type='envelope', line.col="darkcyan", fill.col=(rgb(0,204,204,alpha=60, maxColorValue=255)), xaxt='n', yaxt='n')
par(mar=c(0, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(allspd_south$grid$PrDens)
plot(allspd_south, xlim=c(10000,2800), xaxt='n', yaxt='n')
abline(v=seq(10000,3000,-500), lty="dotted", col="black")
legend(x=9800, y=ymax*0.95,legend=c("SPD","cKDE"),col=c("lightgrey","darkgreen"),lty=c(1,1),lwd=c(5,5),cex=0.6, bg="white", title=expression(bold("c. Southern Italy")))
text(x=9800, y=ymax*0.60, labels=paste("n=",nrow(datesp[datesp$Region==3,]),", sites=",length(unique(datesp$SiteName[datesp$Region==3])),", bins=",length(unique(bins[datesp$Region==3])),sep=""), font=1, cex=0.8, adj=c(0,0.7))
text(x=8950, y=ymax*0.07, labels="Mesolithic",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(8150, ymax*0.15,8150,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
text(x=6650, ymax*0.07, labels="Neolithic",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5650, ymax*0.15,5650,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
text(x=7750, ymax*-0.01, labels="Early",lty=c("solid"), col=c("darkgreen"), font=2, cex=1, adj=c(0.5,-0.5))
segments(7450, ymax*0.12,7450,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
text(x=6800, ymax*-0.01, labels="Middle",lty=c("solid"), col=c("darkgreen"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6150, ymax*0.12,6150,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1))
text(x=5900, ymax*-0.01, labels="Late",lty=c("solid"), col=c("darkgreen"), font=2, cex=1, adj=c(0.5,-0.5))
text(x=4900, ymax*0.01, labels="Copper Age",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
text(x=3550, ymax*0.13, labels="Bronze Age",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(2950, ymax*0.15,2950,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
segments(3600, ymax*0.12,3600,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1))
segments(3250, ymax*0.12,3250,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1))
segments(3100, ymax*0.12,3100,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1))
text(x=3850, ymax*-0.01, labels="EBA",lty=c("solid"), col=c("darkgreen"), font=2, cex=0.7, adj=c(0.5,-0.5))
text(x=3420, ymax*-0.01, labels="MBA",lty=c("solid"), col=c("darkgreen"), font=2, cex=0.7, adj=c(0.5,-0.5))
text(x=3100, ymax*0.07, labels="LBA",lty=c("solid"), col=c("darkgreen"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2950, ymax*0.07, labels="FBA",lty=c("solid"), col=c("darkgreen"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2870, ymax*0.011, labels="IA",lty=c("solid"), col=c("darkgreen"), font=2, cex=0.7, adj=c(0.5,-0.5))
par(new=TRUE)
plot(ckde_south, xlim=c(10000,2800),type='envelope', line.col="darkgreen", fill.col=(rgb(0,102,0,alpha=60, maxColorValue=255)), xaxt='n', yaxt='n')
par(mar=c(0, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(allspd_sicily$grid$PrDens)
plot(allspd_sicily, xlim=c(10000,2800), xaxt='n', yaxt='n')
abline(v=seq(10000,3000,-500), lty="dotted", col="black")
legend(x=9800, y=ymax*0.95,legend=c("SPD","cKDE"),col=c("lightgrey","brown"),lty=c(1,1),lwd=c(5,5),cex=0.6, bg="white", title=expression(bold("d. Sicily")))
text(x=9800, y=ymax*0.60, labels=paste("n=",nrow(datesp[datesp$Region==4,]),", sites=",length(unique(datesp$SiteName[datesp$Region==4])),", bins=",length(unique(bins[datesp$Region==4])),sep=""), font=1, cex=0.8, adj=c(0,0.7))
text(x=9000, y=ymax*0.14, labels="Mesolithic",lty=c("solid"), col=c("brown"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(8050, ymax*0.15,8050,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
text(x=6600, ymax*0.23, labels="Neolithic",lty=c("solid"), col=c("brown"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5450, ymax*0.15,5450,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
text(x=7800, ymax*0.01, labels="Early",lty=c("solid"), col=c("brown"), font=2, cex=0.9, adj=c(0.5,-0.5))
segments(7450, ymax*0.12,7450,ymax*0.02, lty="dotted", col="brown", lwd=(1))
text(x=6750, ymax*0.01, labels="Middle",lty=c("solid"), col=c("brown"), font=2, cex=0.9, adj=c(0.5,-0.5))
segments(6450, ymax*0.12,6450,ymax*0.02, lty="dotted", col="brown", lwd=(1))
text(x=5950, ymax*0.01, labels="Late",lty=c("solid"), col=c("brown"), font=2, cex=0.9, adj=c(0.5,-0.5))
text(x=4800, ymax*-0.01, labels="Copper Age",lty=c("solid"), col=c("brown"), font=2, cex=1, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
text(x=3500, ymax*0.16, labels="Bronze Age",lty=c("solid"), col=c("brown"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(2850, ymax*0.15,2850,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
segments(3450, ymax*0.12,3450,ymax*0.02, lty="dotted", col="brown", lwd=(1))
segments(3200, ymax*0.12,3200,ymax*0.02, lty="dotted", col="brown", lwd=(1))
segments(3000, ymax*0.12,3000,ymax*0.02, lty="dotted", col="brown", lwd=(1))
text(x=3770, ymax*0.01, labels="EBA",lty=c("solid"), col=c("brown"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3330, ymax*0.01, labels="MBA",lty=c("solid"), col=c("brown"), font=2, cex=0.6, adj=c(0.5,-0.5))
text(x=3010, ymax*0.07, labels="LBA",lty=c("solid"), col=c("brown"), font=2, srt=90, cex=0.7, adj=c(0.5,-0.5))
text(x=2840, ymax*0.07, labels="FBA",lty=c("solid"), col=c("brown"), font=2, srt=90, cex=0.7, adj=c(0.5,-0.5))
par(new=TRUE)
plot(ckde_sicily, xlim=c(10000,2800),type='envelope', line.col="brown", fill.col=(rgb(102,51,0,alpha=60, maxColorValue=255)), xaxt='n', yaxt='n')
par(mar=c(6, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(allspd_sardinia$grid$PrDens)
plot(allspd_sardinia, xlim=c(10000,2800), xaxt='n', yaxt='n')
abline(v=seq(10000,3000,-500), lty="dotted", col="black")
legend(x=9800, y=ymax*0.95,legend=c("SPD","cKDE"),col=c("lightgrey","blue"),lty=c(1,1),lwd=c(5,5),cex=0.6, bg="white", title=expression(bold("e. Sardinia")))
text(x=9800, y=ymax*0.60, labels=paste("n=",nrow(datesp[datesp$Region==5,]),", sites=",length(unique(datesp$SiteName[datesp$Region==5])),", bins=",length(unique(bins[datesp$Region==5])),sep=""), font=1, cex=0.8, adj=c(0,0.7))
text(x=9000, y=ymax*0.10, labels="Mesolithic",lty=c("solid"), col=c("blue"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(7850, ymax*0.15,7850,ymax*0.02, lty="solid", col="blue", lwd=(1.2))
text(x=6500, ymax*0.12, labels="Neolithic",lty=c("solid"), col=c("blue"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5250, ymax*0.15,5250,ymax*0.02, lty="solid", col="blue", lwd=(1.2))
text(x=7200, ymax*-0.01, labels="Early",lty=c("solid"), col=c("blue"), font=2, cex=0.9, adj=c(0.5,-0.5))
segments(6700, ymax*0.12,6700,ymax*0.02, lty="dotted", col="blue", lwd=(1))
text(x=6300, ymax*-0.01, labels="Middle",lty=c("solid"), col=c("blue"), font=2, cex=0.9, adj=c(0.5,-0.5))
segments(5950, ymax*0.12,5950,ymax*0.02, lty="dotted", col="blue", lwd=(1))
text(x=5600, ymax*-0.01, labels="Late",lty=c("solid"), col=c("blue"), font=2, cex=0.9, adj=c(0.5,-0.5))
text(x=4700, ymax*-0.01, labels="Copper Age",lty=c("solid"), col=c("blue"), font=2, cex=1, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="blue", lwd=(1.2))
text(x=3500, ymax*0.08, labels="Bronze Age",lty=c("solid"), col=c("blue"), font=2, cex=1, adj=c(0.5,-0.5))
segments(2850, ymax*0.15,2850,ymax*0.02, lty="solid", col="blue", lwd=(1.2))
segments(3650, ymax*0.10,3650,ymax*0.02, lty="dotted", col="blue", lwd=(1))
segments(3300, ymax*0.10,3300,ymax*0.02, lty="dotted", col="blue", lwd=(1))
segments(3150, ymax*0.10,3150,ymax*0.02, lty="dotted", col="blue", lwd=(1))
text(x=3870, ymax*-0.01, labels="EBA",lty=c("solid"), col=c("blue"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3480, ymax*-0.01, labels="MBA",lty=c("solid"), col=c("blue"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3160, ymax*0.05, labels="LBA",lty=c("solid"), col=c("blue"), font=2, srt=90, cex=0.5, adj=c(0.5,-0.5))
text(x=3000, ymax*-0.01, labels="FBA",lty=c("solid"), col=c("blue"), font=2, cex=0.8, adj=c(0.5,-0.5))
par(new=TRUE)
plot(ckde_sardinia, xlim=c(10000,2800),type='envelope', line.col="blue", fill.col=(rgb(0,0,255,alpha=60, maxColorValue=255)), xaxt='n', yaxt='n')
axis(side=1, at=xticks, labels=xticks, las=2, cex.axis=0.75)#add BP axis
mtext("cal BP",1, 1, at=6425, adj=-0.1, font=1, cex=0.5)
axis(side=1, at=xticks-50, labels=xticks-2000, las=2, cex.axis=0.8,pos=-0.00018)# add BC/AD axis
mtext("BC/AD",1, 4, at=6375, adj=-0.1, font=1, cex=0.5)
par(yaxs="r")
par(xaxs="r")
dev.off()


## Plot regional SPDs of calibrated radiocarbon dates
pdf(file="pdf/Fig5.pdf", width=6, height=7)
layout(matrix(c(1,2,3,4,5), 5, 1, byrow=TRUE), widths=5, heights=c(1.5,1.5,1.5,1.5,2.5))
par(mar=c(0, 1, 0, 1)) 
par(yaxs="i")
par(xaxs="i")
xticks <- seq(10000,3000,-500)
ymax <- max(perm1$envelope[["1"]][,2], perm1$observed[["1"]]$PrDens)*1.1
plot(perm1, focalm="1", xlim=c(10000,2800),col.obs="red", lwd.obs=1, drawaxes=FALSE)
abline(v=seq(10000,3000,-500), lty="dotted", col="white")
legend(x=9800, y=ymax*0.85,legend=c("SPD","95% MC envelope","positive deviation","negative deviation"),col=c(1,"lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)),lty=c(1,1,1,1),lwd=c(0.5,5,5,5),cex=0.6, bg="white", title=expression(bold("a. Northern Italy")))
text(x=9800, y=ymax*0.45, labels=paste("n=",nrow(datesp[datesp$Region==1,]),", sites=",length(unique(datesp$SiteName[datesp$Region==1])),", bins=",length(unique(bins[datesp$Region==1])),sep=""), font=1, cex=0.6, adj=c(0,0.7))
text(x=9800, y=ymax*0.38, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(min(perm1$pValueList["1"],1-perm1$pValueList["1"]),3))))
text(x=8850, y=ymax*0.0001, labels="Mesolithic",lty=c("solid"), col=c("red"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(7750, ymax*0.15,7750,ymax*0.02, lty="solid", col="red", lwd=(1.2))
text(x=6450, ymax*0.11, labels="Neolithic",lty=c("solid"), col=c("red"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5450, ymax*0.15,5450,ymax*0.02, lty="solid", col="red", lwd=(1.2))
text(x=7300, ymax*0.01, labels="Early",lty=c("solid"), col=c("red"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6950, ymax*0.15,6950,ymax*0.02, lty="dotted", col="red", lwd=(1))
text(x=6600, ymax*0.01, labels="Middle",lty=c("solid"), col=c("red"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6250, ymax*0.12,6250,ymax*0.02, lty="dotted", col="red", lwd=(1))
text(x=5850, ymax*0.01, labels="Late",lty=c("solid"), col=c("red"), font=2, cex=1, adj=c(0.5,-0.5))
text(x=4820, ymax*0.01, labels="Copper Age",lty=c("solid"), col=c("red"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="red", lwd=(1.2))
text(x=3500, ymax*0.13, labels="Bronze Age",lty=c("solid"), col=c("red"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(2850, ymax*0.15,2850,ymax*0.02, lty="solid", col="red", lwd=(1.2))
segments(3600, ymax*0.12,3600,ymax*0.02, lty="dotted", col="red", lwd=(1))
segments(3275, ymax*0.12,3275,ymax*0.02, lty="dotted", col="red", lwd=(1))
segments(3150, ymax*0.12,3150,ymax*0.02, lty="dotted", col="red", lwd=(1))
text(x=3850, ymax*0.01, labels="EBA",lty=c("solid"), col=c("red"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3430, ymax*0.01, labels="MBA",lty=c("solid"), col=c("red"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3140, ymax*0.07, labels="LBA",lty=c("solid"), col=c("red"), font=2, srt=90, cex=0.5, adj=c(0.5,-0.5))
text(x=3000, ymax*0.01, labels="FBA",lty=c("solid"), col=c("red"), font=2, cex=0.8, adj=c(0.5,-0.5))
#source the code for writing borders/buffers around the labels
source("function/shadowtext.R") 
shadowtext(x=8800, ymax*0.25, labels="Hunther-gatherers",lty=c("solid"), col=c("red"),bg="white",r=0.01, font=1, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7600, ymax*0.45, labels="First farming",lty=c("solid"), col=c("red"),bg="white", font=1,r=0.01, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7350, ymax*0.45, labels="Impressed Ware",lty=c("solid"), col=c("red"),bg="white", font=1,r=0.01, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7200, ymax*0.50, labels="Fiorano-Vho cultures",lty=c("solid"), col=c("red"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=6750, ymax*0.50, labels="Caves, ditched settlements,\npalisades",lty=c("solid"),r=0.01, col=c("red"),bg="white", font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=6300, ymax*0.60, labels="Square Mouthed\npottery nculture",lty=c("solid"), col=c("red"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=5600, ymax*0.50, labels="Chassey and Lagozza\ncultures",lty=c("solid"), col=c("red"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=5100, ymax*0.50, labels="Earliest metal tools",lty=c("solid"), col=c("red"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=4500, ymax*0.50, labels="Draft animals\nand plough ",lty=c("solid"), col=c("red"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=4000, ymax*0.55, labels="Burial mounds, rich graves",lty=c("solid"), col=c("red"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3600, ymax*0.55, labels="Lake-dwelling-type\nsettlements ",lty=c("solid"), col=c("red"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3450, ymax*0.55, labels="Hilltop settlements",lty=c("solid"), col=c("red"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3170, ymax*0.55, labels="Terramare culture",lty=c("solid"), col=c("red"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
box()
par(mar=c(0, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(perm1$envelope[["2"]][,2], perm1$observed[["2"]]$PrDens)*1.1
plot(perm1, focalm="2", xlim=c(10000,2800),col.obs="cyan", lwd.obs=1, drawaxes=FALSE)
abline(v=seq(10000,3000,-500), lty="dotted", col="white")
legend(x=9800, y=ymax*0.85,legend=c("SPD","95% MC envelope","positive deviation","negative deviation"),col=c(1,"lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)),lty=c(1,1,1,1),lwd=c(0.5,5,5,5),cex=0.6, bg="white", title=expression(bold("b. Central Italy")))
text(x=9800, y=ymax*0.45, labels=paste("n=",nrow(datesp[datesp$Region==2,]),", sites=",length(unique(datesp$SiteName[datesp$Region==2])),", bins=",length(unique(bins[datesp$Region==2])),sep=""), font=1, cex=0.6, adj=c(0,0.7))
text(x=9800, y=ymax*0.38, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(min(perm1$pValueList["2"],1-perm1$pValueList["2"]),3))))
text(x=8850, y=ymax*0.05, labels="Mesolithic",lty=c("solid"), col=c("darkcyan"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(7950, ymax*0.15,7950,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
text(x=6450, ymax*0.06, labels="Neolithic",lty=c("solid"), col=c("darkcyan"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5450, ymax*0.15,5450,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
text(x=7500, ymax*-0.01, labels="Early",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
segments(7150, ymax*0.12,7150,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
text(x=6800, ymax*-0.01, labels="Middle",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6450, ymax*0.12,6450,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
text(x=5950, ymax*-0.01, labels="Late",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
text(x=4850, ymax*0.01, labels="Copper Age",lty=c("solid"), col=c("darkcyan"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(4250, ymax*0.15,4250,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
text(x=3580, ymax*0.11, labels="Bronze Age",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
segments(2950, ymax*0.15,2950,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
segments(3650, ymax*0.12,3650,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
segments(3275, ymax*0.12,3275,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
segments(3100, ymax*0.12,3100,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
text(x=3950, ymax*-0.01, labels="EBA",lty=c("solid"), col=c("darkcyan"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3480, ymax*-0.01, labels="MBA",lty=c("solid"), col=c("darkcyan"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3130, ymax*0.06, labels="LBA",lty=c("solid"), col=c("darkcyan"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2960, ymax*0.06, labels="FBA",lty=c("solid"), col=c("darkcyan"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2865, ymax*0.011, labels="IA",lty=c("solid"), col=c("darkcyan"), font=2, cex=0.8, adj=c(0.5,-0.5))
shadowtext(x=8800, ymax*0.25, labels="Hunther-gatherers",lty=c("solid"), col=c("darkcyan"),bg="white",r=0.01, font=1, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7800, ymax*0.45, labels="First farming",lty=c("solid"), col=c("darkcyan"),bg="white", font=1,r=0.01, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7550, ymax*0.45, labels="Impressed-Cardial cultures",lty=c("solid"), col=c("darkcyan"),bg="white", font=1,r=0.01, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7100, ymax*0.50, labels="Caves, wattle and daub buildings,\ncircular heaps ",lty=c("solid"),r=0.01, col=c("darkcyan"),bg="white", font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=6700, ymax*0.55, labels="Brown Ripoli-Lagozza\ncultures ",lty=c("solid"), col=c("darkcyan"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=5700, ymax*0.50, labels="Diana Ware culture",lty=c("solid"), col=c("darkcyan"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=5300, ymax*0.50, labels="Earliest metal tools",lty=c("solid"), col=c("darkcyan"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=4600, ymax*0.50, labels="Intensification of herding\n(sheep and goat)",lty=c("solid"), col=c("darkcyan"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=4000, ymax*0.50, labels="Collective burials, rich graves",lty=c("solid"), col=c("darkcyan"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3300, ymax*0.55, labels="Hilltop settlements",lty=c("solid"), col=c("darkcyan"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=2870, ymax*0.55, labels="Proto-urban centres",lty=c("solid"), col=c("darkcyan"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
box()
par(mar=c(0, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(perm1$envelope[["3"]][,2], perm1$observed[["3"]]$PrDens)*1.1
plot(perm1, focalm="3", xlim=c(10000,2800),col.obs="green", lwd.obs=1, drawaxes=FALSE)
abline(v=seq(10000,3000,-500), lty="dotted", col="white")
legend(x=9800, y=ymax*0.85,legend=c("SPD","95% MC envelope","positive deviation","negative deviation"),col=c(1,"lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)),lty=c(1,1,1,1),lwd=c(0.5,5,5,5),cex=0.6, bg="white", title=expression(bold("c. Southern Italy")))
text(x=9800, y=ymax*0.45, labels=paste("n=",nrow(datesp[datesp$Region==3,]),", sites=",length(unique(datesp$SiteName[datesp$Region==3])),", bins=",length(unique(bins[datesp$Region==3])),sep=""), font=1, cex=0.6, adj=c(0,0.7))
text(x=9800, y=ymax*0.38, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(min(perm1$pValueList["3"],1-perm1$pValueList["3"]),3))))
text(x=8950, y=ymax*0.07, labels="Mesolithic",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(8150, ymax*0.15,8150,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
text(x=6650, ymax*0.07, labels="Neolithic",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5650, ymax*0.15,5650,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
text(x=7750, ymax*-0.01, labels="Early",lty=c("solid"), col=c("darkgreen"), font=2, cex=1, adj=c(0.5,-0.5))
segments(7450, ymax*0.12,7450,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1))
text(x=6800, ymax*-0.01, labels="Middle",lty=c("solid"), col=c("darkgreen"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6150, ymax*0.12,6150,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1))
text(x=5900, ymax*-0.01, labels="Late",lty=c("solid"), col=c("darkgreen"), font=2, cex=1, adj=c(0.5,-0.5))
text(x=4900, ymax*0.01, labels="Copper Age",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
text(x=3550, ymax*0.13, labels="Bronze Age",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(2950, ymax*0.15,2950,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
segments(3600, ymax*0.12,3600,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1))
segments(3250, ymax*0.12,3250,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1))
segments(3100, ymax*0.12,3100,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1))
text(x=3850, ymax*-0.01, labels="EBA",lty=c("solid"), col=c("darkgreen"), font=2, cex=0.7, adj=c(0.5,-0.5))
text(x=3420, ymax*-0.01, labels="MBA",lty=c("solid"), col=c("darkgreen"), font=2, cex=0.7, adj=c(0.5,-0.5))
text(x=3100, ymax*0.07, labels="LBA",lty=c("solid"), col=c("darkgreen"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2950, ymax*0.07, labels="FBA",lty=c("solid"), col=c("darkgreen"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2870, ymax*0.011, labels="IA",lty=c("solid"), col=c("darkgreen"), font=2, cex=0.7, adj=c(0.5,-0.5))
shadowtext(x=8800, ymax*0.25, labels="Hunther-gatherers",lty=c("solid"), col=c("darkgreen"),bg="white",r=0.01, font=1, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7900, ymax*0.45, labels="First farming",lty=c("solid"), col=c("darkgreen"),bg="white", font=1,r=0.01, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7650, ymax*0.45, labels="Caves, ditched settlements",lty=c("solid"), col=c("darkgreen"),bg="white", font=1,r=0.01, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7200, ymax*0.50, labels="Impressed-Stentinello-Matera\ncultures ",lty=c("solid"),r=0.01, col=c("darkgreen"),bg="white", font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=6700, ymax*0.55, labels="Stentinello-Serra d'Alto\ncultures",lty=c("solid"), col=c("darkgreen"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=6300, ymax*0.55, labels="Mixed farming-herding\neconomy",lty=c("solid"), col=c("darkgreen"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=5800, ymax*0.50, labels="Diana Ware culture",lty=c("solid"), col=c("darkgreen"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=5300, ymax*0.50, labels="Earliest metal tools",lty=c("solid"), col=c("darkgreen"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=4600, ymax*0.50, labels="Intensification of herding\n(sheep and goat)",lty=c("solid"), col=c("darkgreen"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=4200, ymax*0.50, labels="Laterza-Malpasso cultures",lty=c("solid"), col=c("darkgreen"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3900, ymax*0.60, labels="collective burials,\n rich graves",lty=c("solid"), col=c("darkgreen"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3500, ymax*0.55, labels="Hilltop settlements",lty=c("solid"), col=c("darkgreen"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3000, ymax*0.53, labels="Large nucleated settlements",lty=c("solid"), col=c("darkgreen"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
box()
par(mar=c(0, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(perm1$envelope[["4"]][,2], perm1$observed[["4"]]$PrDens)*1.1
plot(perm1, focalm="4", xlim=c(10000,2800),col.obs="brown", lwd.obs=1, drawaxes=FALSE)
abline(v=seq(10000,3000,-500), lty="dotted", col="white")
legend(x=9800, y=ymax*0.85,legend=c("SPD","95% MC envelope","positive deviation","negative deviation"),col=c(1,"lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)),lty=c(1,1,1,1),lwd=c(0.5,5,5,5),cex=0.6, bg="white", title=expression(bold("d. Sicily")))
text(x=9450, y=ymax*0.45, labels=paste("n=",nrow(datesp[datesp$Region==4,]),", sites=",length(unique(datesp$SiteName[datesp$Region==4])),", bins=",length(unique(bins[datesp$Region==4])),sep=""), font=1, cex=0.6, adj=c(0,0.7))
text(x=9450, y=ymax*0.38, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(min(perm1$pValueList["4"],1-perm1$pValueList["4"]),3))))
text(x=9000, y=ymax*0.18, labels="Mesolithic",lty=c("solid"), col=c("brown"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(8050, ymax*0.15,8050,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
text(x=6600, ymax*0.23, labels="Neolithic",lty=c("solid"), col=c("brown"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5450, ymax*0.15,5450,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
text(x=7800, ymax*0.01, labels="Early",lty=c("solid"), col=c("brown"), font=2, cex=0.9, adj=c(0.5,-0.5))
segments(7450, ymax*0.12,7450,ymax*0.02, lty="dotted", col="brown", lwd=(1))
text(x=6750, ymax*0.01, labels="Middle",lty=c("solid"), col=c("brown"), font=2, cex=0.9, adj=c(0.5,-0.5))
segments(6450, ymax*0.12,6450,ymax*0.02, lty="dotted", col="brown", lwd=(1))
text(x=5950, ymax*0.01, labels="Late",lty=c("solid"), col=c("brown"), font=2, cex=0.9, adj=c(0.5,-0.5))
text(x=4800, ymax*-0.01, labels="Copper Age",lty=c("solid"), col=c("brown"), font=2, cex=1, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
text(x=3500, ymax*0.16, labels="Bronze Age",lty=c("solid"), col=c("brown"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(2850, ymax*0.15,2850,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
segments(3450, ymax*0.12,3450,ymax*0.02, lty="dotted", col="brown", lwd=(1))
segments(3200, ymax*0.12,3200,ymax*0.02, lty="dotted", col="brown", lwd=(1))
segments(3000, ymax*0.12,3000,ymax*0.02, lty="dotted", col="brown", lwd=(1))
text(x=3770, ymax*0.01, labels="EBA",lty=c("solid"), col=c("brown"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3330, ymax*0.01, labels="MBA",lty=c("solid"), col=c("brown"), font=2, cex=0.6, adj=c(0.5,-0.5))
text(x=3010, ymax*0.07, labels="LBA",lty=c("solid"), col=c("brown"), font=2, srt=90, cex=0.7, adj=c(0.5,-0.5))
text(x=2840, ymax*0.07, labels="FBA",lty=c("solid"), col=c("brown"), font=2, srt=90, cex=0.7, adj=c(0.5,-0.5))
shadowtext(x=8650, ymax*0.30, labels="Hunther-gatherers",lty=c("solid"), col=c("brown"),bg="white",r=0.01, font=1, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7900, ymax*0.45, labels="First farming",lty=c("solid"), col=c("brown"),bg="white", font=1,r=0.01, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7650, ymax*0.55, labels="Caves, ditched settlements",lty=c("solid"), col=c("brown"),bg="white", font=1,r=0.01, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7200, ymax*0.53, labels="Uzzo Pre-Stentinello cultures",lty=c("solid"),r=0.01, col=c("brown"),bg="white", font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=6700, ymax*0.62, labels="Impressed-Stentinello\ncultures",lty=c("solid"), col=c("brown"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=6300, ymax*0.62, labels="Mixed farming-herding\neconomy",lty=c("solid"), col=c("brown"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=5800, ymax*0.60, labels="Diana Ware culture",lty=c("solid"), col=c("brown"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=5300, ymax*0.50, labels="Earliest metal tools",lty=c("solid"), col=c("brown"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=4600, ymax*0.50, labels="Intensification of herding\n(sheep and goat)",lty=c("solid"), col=c("brown"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=4200, ymax*0.50, labels="Castelluccio ware culture",lty=c("solid"), col=c("brown"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3750, ymax*0.55, labels="fortified settlements,\ncircular enclosures",lty=c("solid"), col=c("brown"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3300, ymax*0.57, labels="Tholos-type tombs,\ncollective burials,rich graves",lty=c("solid"), col=c("brown"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3000, ymax*0.55, labels="Large nucleated\n settlements",lty=c("solid"), col=c("brown"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
box()
par(mar=c(6, 1, 0, 1)) #c(bottom, left, top, right)
ymax <- max(perm1$envelope[["5"]][,2], perm1$observed[["5"]]$PrDens)*1.1
plot(perm1, focalm="5", xlim=c(10000,2800),col.obs="blue", lwd.obs=1, drawaxes=FALSE)
abline(v=seq(10000,3000,-500), lty="dotted", col="white")
legend(x=9800, y=ymax*0.85,legend=c("SPD","95% MC envelope","positive deviation","negative deviation"),col=c(1,"lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)),lty=c(1,1,1,1),lwd=c(0.5,5,5,5),cex=0.6, bg="white", title=expression(bold("e. Sardinia")))
text(x=9800, y=ymax*0.45, labels=paste("n=",nrow(datesp[datesp$Region==5,]),", sites=",length(unique(datesp$SiteName[datesp$Region==5])),", bins=",length(unique(bins[datesp$Region==5])),sep=""), font=1, cex=0.6, adj=c(0,0.7))
text(x=9800, y=ymax*0.38, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(min(perm1$pValueList["5"],1-perm1$pValueList["5"]),3))))
text(x=9000, y=ymax*0.10, labels="Mesolithic",lty=c("solid"), col=c("blue"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(7850, ymax*0.15,7850,ymax*0.02, lty="solid", col="blue", lwd=(1.2))
text(x=6500, ymax*0.12, labels="Neolithic",lty=c("solid"), col=c("blue"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5250, ymax*0.15,5250,ymax*0.02, lty="solid", col="blue", lwd=(1.2))
text(x=7200, ymax*-0.01, labels="Early",lty=c("solid"), col=c("blue"), font=2, cex=0.9, adj=c(0.5,-0.5))
segments(6700, ymax*0.12,6700,ymax*0.02, lty="dotted", col="blue", lwd=(1))
text(x=6300, ymax*-0.01, labels="Middle",lty=c("solid"), col=c("blue"), font=2, cex=0.9, adj=c(0.5,-0.5))
segments(5950, ymax*0.12,5950,ymax*0.02, lty="dotted", col="blue", lwd=(1))
text(x=5600, ymax*-0.01, labels="Late",lty=c("solid"), col=c("blue"), font=2, cex=0.9, adj=c(0.5,-0.5))
text(x=4700, ymax*-0.01, labels="Copper Age",lty=c("solid"), col=c("blue"), font=2, cex=1, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="blue", lwd=(1.2))
text(x=3500, ymax*0.08, labels="Bronze Age",lty=c("solid"), col=c("blue"), font=2, cex=1, adj=c(0.5,-0.5))
segments(2850, ymax*0.15,2850,ymax*0.02, lty="solid", col="blue", lwd=(1.2))
segments(3650, ymax*0.10,3650,ymax*0.02, lty="dotted", col="blue", lwd=(1))
segments(3300, ymax*0.10,3300,ymax*0.02, lty="dotted", col="blue", lwd=(1))
segments(3150, ymax*0.10,3150,ymax*0.02, lty="dotted", col="blue", lwd=(1))
text(x=3870, ymax*-0.01, labels="EBA",lty=c("solid"), col=c("blue"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3480, ymax*-0.01, labels="MBA",lty=c("solid"), col=c("blue"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3160, ymax*0.05, labels="LBA",lty=c("solid"), col=c("blue"), font=2, srt=90, cex=0.5, adj=c(0.5,-0.5))
text(x=3000, ymax*-0.01, labels="FBA",lty=c("solid"), col=c("blue"), font=2, cex=0.8, adj=c(0.5,-0.5))
shadowtext(x=8800, ymax*0.25, labels="Hunther-gatherers",lty=c("solid"), col=c("blue"),bg="white",r=0.01, font=1, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7700, ymax*0.45, labels="First farming",lty=c("solid"), col=c("blue"),bg="white", font=1,r=0.01, srt=90, cex=0.6, adj=c(0.5,-0.5))
#shadowtext(x=7650, ymax*0.55, labels="Impressed-Cardial-Filiestru\ncultures" ,lty=c("solid"), col=c("blue"),bg="white", font=1,r=0.01, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=7200, ymax*0.60, labels="Caves, open settlements",lty=c("solid"),r=0.01, col=c("blue"),bg="white", font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=6700, ymax*0.57, labels="Bonnu-Ighinu pottery culture",lty=c("solid"), col=c("blue"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=6300, ymax*0.52, labels="Mixed farming-herding\neconomy",lty=c("solid"), col=c("blue"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=5800, ymax*0.60, labels="Chasey-Lagozza cultures",lty=c("solid"), col=c("blue"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=5100, ymax*0.60, labels="Earliest metal tools",lty=c("solid"), col=c("blue"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=4450, ymax*0.53, labels="Sub Ozieri-Filigosa-Abealzu\ncultures",lty=c("solid"), col=c("blue"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=4050, ymax*0.55, labels="fortified  hilltop settlements",lty=c("solid"), col=c("blue"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3400, ymax*0.60, labels="Nuragic culture",lty=c("solid"), col=c("blue"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3650, ymax*0.55, labels="collective monumental burials,\nrich graves",lty=c("solid"), col=c("blue"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
shadowtext(x=3000, ymax*0.52, labels="multi-towered nuraghi",lty=c("solid"), col=c("blue"),bg="white",r=0.01, font=1, srt=90, cex=0.6, adj=c(0.5,-0.5))
box()
axis(side=1, at=xticks, labels=xticks, las=2, cex.axis=0.75)#add BP axis
mtext("cal BP",1, 1, at=6425, adj=-0.1, font=1, cex=0.5)
axis(side=1, at=xticks-50, labels=xticks-2000, las=2, cex.axis=0.8,pos=-0.040)# add BC/AD axis
mtext("BC/AD",1, 4, at=6375, adj=-0.1, font=1, cex=0.5)
par(yaxs="r")
par(xaxs="r")
dev.off()

#save data
#save(allspd, allspdn,allspd_central,allspd_north,allspd_south,allspd_sicily,allspd_sardinia, bins,datesp,logisticmod,expnull,perm1,logisticmod_central,logisticmod_north, logisticmod_sicily,logisticmod_south, file="rda/radiocarbon.RData")



############## model SPD for each region and compare versus a logistic null model ######################

## Fit a logistic model and create a predicted SPD for north Italy
# Generate a smoothed SPD
spd.smoothed = spd(northdates,timeRange=c(11000,1500),bins=bins_north,runm=runm)
# Start values should be adjusted depending on the observed SPD
logFit <- nls(PrDens~SSlogis(calBP, Asym, xmid, scale),data=spd.smoothed$grid,control=nls.control(maxiter=200),start=list(Asym=0.2,xmid=4500,scale=-100))
# Generate a data frame containing the fitted values
logFitDens=data.frame(calBP=spd.smoothed$grid$calBP,PrDens=SSlogis(input=spd.smoothed$grid$calBP,Asym=coefficients(logFit)[1],xmid=coefficients(logFit)[2],scal=coefficients(logFit)[3]))
# Use the modelTest function (returning the raw simulation output - see below)
logisticmod_north <- modelTest(northdates, errors=mydates_north$Error, bins=bins_north, nsim=nsim, timeRange=c(11000,1500), model="custom", predgrid=logFitDens, runm=runm, ncores=ncores, raw=TRUE)

## Fit a logistic model and create a predicted SPD for central Italy
spd.smoothed = spd(centraldates,timeRange=c(11000,1500),bins=bins_central,runm=runm)
# Start values should be adjusted depending on the observed SPD
logFit <- nls(PrDens~SSlogis(calBP, Asym, xmid, scale),data=spd.smoothed$grid,control=nls.control(maxiter=200),start=list(Asym=0.2,xmid=4500,scale=-100))
# Generate a data frame containing the fitted values
logFitDens=data.frame(calBP=spd.smoothed$grid$calBP,PrDens=SSlogis(input=spd.smoothed$grid$calBP,Asym=coefficients(logFit)[1],xmid=coefficients(logFit)[2],scal=coefficients(logFit)[3]))
# Use the modelTest function (returning the raw simulation output - see below)
logisticmod_central <- modelTest(centraldates, errors=mydates_central$Error, bins=bins_central, nsim=nsim, timeRange=c(11000,1500), model="custom", predgrid=logFitDens, runm=runm, ncores=ncores, raw=TRUE)

## Fit a logistic model and create a predicted SPD for south Italy
spd.smoothed = spd(southdates,timeRange=c(11000,1500),bins=bins_south,runm=runm)
# Start values should be adjusted depending on the observed SPD
logFit <- nls(PrDens~SSlogis(calBP, Asym, xmid, scale),data=spd.smoothed$grid,control=nls.control(maxiter=200),start=list(Asym=0.2,xmid=4500,scale=-100))
# Generate a data frame containing the fitted values
logFitDens=data.frame(calBP=spd.smoothed$grid$calBP,PrDens=SSlogis(input=spd.smoothed$grid$calBP,Asym=coefficients(logFit)[1],xmid=coefficients(logFit)[2],scal=coefficients(logFit)[3]))
# Use the modelTest function (returning the raw simulation output - see below)
logisticmod_south <- modelTest(southdates, errors=mydates_south$Error, bins=bins_south, nsim=nsim, timeRange=c(11000,1500), model="custom", predgrid=logFitDens, runm=runm, ncores=ncores, raw=TRUE)

## Fit a logistic model and create a predicted SPD for Sicily
spd.smoothed = spd(sicilydates,timeRange=c(11000,1500),bins=bins_sicily,runm=runm)
# Start values should be adjusted depending on the observed SPD
logFit <- nls(PrDens~SSlogis(calBP, Asym, xmid, scale),data=spd.smoothed$grid,control=nls.control(maxiter=200),start=list(Asym=0.2,xmid=4500,scale=-100))
# Generate a data frame containing the fitted values
logFitDens=data.frame(calBP=spd.smoothed$grid$calBP,PrDens=SSlogis(input=spd.smoothed$grid$calBP,Asym=coefficients(logFit)[1],xmid=coefficients(logFit)[2],scal=coefficients(logFit)[3]))
# Use the modelTest function (returning the raw simulation output - see below)
logisticmod_sicily <- modelTest(sicilydates, errors=mydates_sicily$Error, bins=bins_sicily, nsim=nsim, timeRange=c(11000,1500), model="custom", predgrid=logFitDens, runm=runm, ncores=ncores, raw=TRUE)


############# plot SPD versus palaeoclimate proxies in northern Italy (Fig. 7) ##########
#Load palaeoclimate records and Pearson's pairwise correlations
palaeoclimate<-read.csv("csv/palaeoclimate.csv", header=TRUE, sep=",")
#Load the Pearson’s correlations (r) values ranging from +1 to -1 by using a 500-year-time moving window.
load("rda/correlations.RData")

#define layout
pdf(file="pdf/Fig7.pdf", width=6, height=4.5)
layout(matrix(c(1,2,3), 3, 1, byrow=TRUE), widths=6, heights=c(1.2,1.2,2.3))
par(mar=c(0, 3, 1, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP.3,palaeoclimate$Ernesto.Cave, type="l", axes=FALSE, xlim = c(10000,2800), ylim=c(250,0), lwd=1, col="black", xaxt="n", ylab="",xaxs = "i", yaxs = "i")
axis(2, at=seq(0,250,50), labels = seq(0,250, 50), lwd=1, line=0.1, cex.axis=0.6, mgp=c(0,0.5,0), col="black")
mtext(2,text=expression(paste("Thickness","  ",mu,"m")),line=1.4, cex = 0.6,col="black")
rect(7610,250,7576,0, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(6972,250,6233,0, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(6219,250,6201,0, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(4550,250,4529,0, col=rgb(00.7,0,0,0.2),border=NA)#red rectangle
rect(4238,250,4165,0, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(3986,250,3210,0, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
segments(7700,2,7200,2, col="orange",lwd=2) #500-year moving window
segments(7700,0,7700,250, col="orange",lwd=2) #500-year moving window
segments(7200,0,7200,250, col="orange",lwd=2) #500-year moving window
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
arrows(9500,160,9500,110, col="black", length=0.1, lwd=2)
arrows(9500,160,9500,210, col="black", length=0.1, lwd=2)
text(x=9800, y=50, labels="Ernesto Cave (1)", font=2, cex=1.2, adj=c(0,0))
text(x=9650, y=195, labels="winter\nrainfall", font=1, srt=90, col="black", cex=1, adj=c(0,0))
text(x=9400, y=140, labels=expression(paste("NAO+\nwarm T (",degree,"C)")), font=1, col="black", cex=1, adj=c(0,0))
text(x=9400, y=220, labels=expression(paste("NAO-\ncold T (",degree,"C)")), font=1, col="black", cex=1, adj=c(0,0))
par(new=T)# treat the graph window as a new window, so a new graph can be plotted without erasing the old one
plot(runcorr_north$brks_end,runcorr_north$spd_north, axes=FALSE, xlim = c(10000,2800), ylim=c(-1,1),  pch=16, col="red", lwd=2, cex=0.5, xaxt="n", ylab="",xaxs = "i", yaxs = "i") # plot the 500-year running correlation values with the SPD
axis(4, at=seq(-1,1,0.5), labels = seq(-1,1,0.5), lwd=1, line=0.1, cex.axis=0.6, mgp=c(0,0.5,0), col="black")
mtext(4,text="correlation",line=1.4, cex = 0.6,col="black")
par(mar=c(0, 3, 0, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP.2,palaeoclimate$Frassino, type="l", axes=FALSE, xlim = c(10000,2800), ylim=c(-1,-5.5), lwd=1, col="red", xaxt="n",ylab="",xaxs = "i", yaxs = "i")
axis(4, at=seq(-5,-1,1), labels = seq(-5,-1,1), lwd=1, line=0.2, cex.axis=0.6, mgp=c(0,0.4,0), col="red", col.axis="red")
mtext(4,text=expression(paste(sigma^18,"O")),line=1.5, cex = 0.6, col="red")
rect(7610,-1,7576,-5.5, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(6972,-1,6233,-5.5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(6219,-1,6201,-5.5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(4550,-1,4529,-5.5, col=rgb(00.7,0,0,0.2),border=NA)#red rectangle
rect(4238,-1,4165,-5.5, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(3986,-1,3210,-5.5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
segments(7700,-1,7700,-5.5, col="orange",lwd=2) #500-year moving window
segments(7200,-1,7200,-5.5, col="orange",lwd=2) #500-year moving window
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
text(x=9800, y=-4.2, labels="Lake Frassino (2)", font=2, col="red", cex=1.2, adj=c(0,0))
par(mar=c(6, 3, 0, 3)) #c(bottom, left, top, right)
ymax <- max(logisticmod_north$result$PrDens)*1.1
plot(logisticmod_north, ylim=c(0,ymax), xlim=c(10000,2800), drawaxes=FALSE, col="red")
abline(v=seq(10000,3000,-500), lty="dotted", col="gray50",lwd=1)
legend(x=9800, y=ymax*0.95,legend=c("SPD","95% MC envelope","positive deviation","negative deviation"),col=c(1,"lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)),lty=c(1,1,1,1),lwd=c(0.5,5,5,5),cex=0.7, bg="white", title="")
text(x=9600, y=0.315, labels="Northern Italy", font=2, cex=0.9, adj=c(0,0))
text(x=9800, y=ymax*0.52, labels=paste("n=",nrow(datesp[datesp$Region==1,]),", sites=",length(unique(datesp$SiteName[datesp$Region==1])),", bins=",length(unique(bins[datesp$Region==1])),sep=""), font=1, cex=0.6, adj=c(0,0.7))
text(x=9800, ymax*0.45, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(logisticmod_north$pval,4))))
axis(2, at=seq(0,0.35,0.1), labels = seq(0,0.35, 0.1), lwd=1, line=0.1, cex.axis=0.6, mgp=c(0,0.5,0), col="black")
mtext(2,text="SPD",line=1.4, cex = 0.6,col="black")
segments(7700,ymax*-0.01,7200,ymax*-0.01, col="orange",lwd=2) #500-year moving window
segments(7700,ymax,7700,ymax*0.01, col="orange",lwd=2) #500-year moving window
segments(7200,ymax,7200,ymax*0.01, col="orange",lwd=2) #500-year moving window
text(x=8850, y=ymax*0.0001, labels="Mesolithic",lty=c("solid"), col=c("red"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(7750, ymax*0.15,7750,ymax*0.02, lty="solid", col="red", lwd=(1.2))
text(x=6450, ymax*0.11, labels="Neolithic",lty=c("solid"), col=c("red"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5450, ymax*0.15,5450,ymax*0.02, lty="solid", col="red", lwd=(1.2))
text(x=7300, ymax*0.01, labels="Early",lty=c("solid"), col=c("red"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6950, ymax*0.15,6950,ymax*0.02, lty="dotted", col="red", lwd=(1.2))
text(x=6600, ymax*0.01, labels="Middle",lty=c("solid"), col=c("red"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6250, ymax*0.12,6250,ymax*0.02, lty="dotted", col="red", lwd=(1.2))
text(x=5850, ymax*0.01, labels="Late",lty=c("solid"), col=c("red"), font=2, cex=1, adj=c(0.5,-0.5))
text(x=4820, ymax*0.01, labels="Copper Age",lty=c("solid"), col=c("red"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="red", lwd=(1.2))
text(x=3500, ymax*0.13, labels="Bronze Age",lty=c("solid"), col=c("red"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(2850, ymax*0.15,2850,ymax*0.02, lty="solid", col="red", lwd=(1.2))
segments(3600, ymax*0.12,3600,ymax*0.02, lty="dotted", col="red", lwd=(1.2))
segments(3275, ymax*0.12,3275,ymax*0.02, lty="dotted", col="red", lwd=(1.2))
segments(3150, ymax*0.12,3150,ymax*0.02, lty="dotted", col="red", lwd=(1.2))
text(x=3850, ymax*0.01, labels="EBA",lty=c("solid"), col=c("red"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3430, ymax*0.01, labels="MBA",lty=c("solid"), col=c("red"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3140, ymax*0.07, labels="LBA",lty=c("solid"), col=c("red"), font=2, srt=90, cex=0.5, adj=c(0.5,-0.5))
text(x=3000, ymax*0.01, labels="FBA",lty=c("solid"), col=c("red"), font=2, cex=0.8, adj=c(0.5,-0.5))
xticks <- seq(10000,2500,-500)
axis(side=1, at=xticks, labels=xticks, las=2, cex.axis=0.80) # add BP axis
mtext("cal BP",1, 1, at=6450, adj=-0.1, font=1, cex=0.5)
axis(side=1, at=xticks-50, labels=xticks-2000, las=2, cex.axis=0.8,pos=-0.1)# add BC/AD axis
mtext("BC/AD",1, 4, at=6400, adj=-0.1, font=1, cex=0.5)
dev.off()


############# plot SPD versus palaeoclimate proxies central Italy (Fig. 8) ##########
#define layout
pdf(file="pdf/Fig8.pdf", width=6, height=4.5)
layout(matrix(c(1,2,3), 3, 1, byrow=TRUE), widths=6, heights=c(1.2,1.2,2.3))
par(mar=c(0, 3, 1, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP,palaeoclimate$Renella.Cave, type="l", axes=FALSE, xlim = c(10000,2800), ylim=c(-3.4,-5), lwd=1, col="blue", xaxt="n", ylab="",xaxs = "i", yaxs = "i")
axis(2, at=seq(-5,-3.5,0.5), labels = seq(-5,-3.5, 0.5), lwd=1, line=0.1, cex.axis=0.6, mgp=c(0,0.5,0), col="blue", col.axis="blue")
mtext(2,text=expression(paste(sigma^18,"O")),line=1.4, cex = 0.6,col="blue")
rect(8641,-3.4,8444,-5, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(8042,-3.4,7828,-5, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(7572,-3.4,7553,-5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(7526,-3.4,7321,-5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(7173,-3.4,6997,-5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(6409,-3.4,6269,-5, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5328,-3.4,5299,-5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3715,-3.4,3480,-5, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(3166,-3.4,2867,-5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
text(x=9800, y=-4.7, labels="Renella Cave (3)", font=2, col="blue", cex=1.2, adj=c(0,0))
par(new=T)# treat the graph window as a new window, so a new graph can be plotted without erasing the old one
plot(runcorr_renella$brks_end,runcorr_renella$renella_corr, axes=FALSE, xlim = c(10000,2800), ylim=c(-1,1),  pch=16, col="red", lwd=2, cex=0.5, xaxt="n", ylab="",xaxs = "i", yaxs = "i") # plot the 500-year running correlation values with the SPD
axis(4, at=seq(-1,1,0.5), labels = seq(-1,1,0.5), lwd=0.8, line=1.6, cex.axis=0.6, mgp=c(0,0.3,0), col="black")
mtext(4,text="correlation",line=0.1, cex = 0.6,col="black")
par(mar=c(0, 3, 0, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP.1,palaeoclimate$Corchia.Cave, type="l", axes=FALSE, xlim = c(10000,2800), ylim=c(-4,-5.5), lwd=1, col="darkgreen", xaxt="n",ylab="",xaxs = "i", yaxs = "i")
axis(4, at=seq(-5.5,-4,0.5), labels = seq(-5.5,-4, 0.5), lwd=1, line=0.2, cex.axis=0.6, mgp=c(0,0.4,0), col="darkgreen", col.axis="darkgreen")
rect(8641,-4,8444,-5.5, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(8042,-4,7828,-5.5, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(7572,-4,7553,-5.5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(7526,-4,7321,-5.5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(7173,-4,6997,-5.5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(6409,-4,6269,-5.5, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5328,-4,5299,-5.5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3715,-4,3480,-5.5, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(3166,-4,2867,-5.5, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
text(x=9800, y=-5.05, labels="Corchia Cave (4)", font=2, col="darkgreen", cex=1.2, adj=c(0,0))
par(new=T)# treat the graph window as a new window, so a new graph can be plotted without erasing the old one
plot(runcorr_corchia$brks_end,runcorr_corchia$corchia_corr, axes=FALSE, xlim = c(10000,2800), ylim=c(-1,1),  pch=16, col="red", lwd=2, cex=0.5, xaxt="n", ylab="",xaxs = "i", yaxs = "i") # plot the 500-year running correlation values with the SPD
axis(2, at=seq(-1,1,0.5), labels = seq(-1,1,0.5), lwd=0.8, line=1.4, cex.axis=0.6, mgp=c(0,0.5,0), col="black")
mtext(2,text="correlation",line=0.1, cex = 0.6,col="black")
par(mar=c(6, 3, 0, 3)) #c(bottom, left, top, right)
ymax <- max(logisticmod_central$result$PrDens)*1.1
plot(logisticmod_central, ylim=c(0,ymax), xlim=c(10000,2800), drawaxes=FALSE, col="darkcyan")
abline(v=seq(10000,3000,-500), lty="dotted", col="gray50",lwd=1)
legend(x=9800, y=ymax*0.95,legend=c("SPD","95% MC envelope","positive deviation","negative deviation"),col=c(1,"lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)),lty=c(1,1,1,1),lwd=c(0.5,5,5,5),cex=0.7, bg="white", title="")
text(x=9600, y=0.22, labels="Central Italy", font=2, cex=0.9, adj=c(0,0))
text(x=9800, y=ymax*0.52, labels=paste("n=",nrow(datesp[datesp$Region==2,]),", sites=",length(unique(datesp$SiteName[datesp$Region==2])),", bins=",length(unique(bins[datesp$Region==2])),sep=""), font=1, cex=0.6, adj=c(0,0.7))
text(x=9800, ymax*0.45, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(logisticmod_central$pval,4))))
axis(2, at=seq(0,0.20,0.05), labels = seq(0,0.20, 0.05), lwd=1, line=0.1, cex.axis=0.6, mgp=c(0,0.5,0), col="black")
mtext(2,text="SPD",line=1.4, cex = 0.6,col="black")
text(x=8850, y=ymax*0.05, labels="Mesolithic",lty=c("solid"), col=c("darkcyan"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(7950, ymax*0.15,7950,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
text(x=6450, ymax*0.06, labels="Neolithic",lty=c("solid"), col=c("darkcyan"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5450, ymax*0.15,5450,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
text(x=7500, ymax*-0.01, labels="Early",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
segments(7150, ymax*0.12,7150,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1.2))
text(x=6800, ymax*-0.01, labels="Middle",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6450, ymax*0.12,6450,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1.2))
text(x=5950, ymax*-0.01, labels="Late",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
text(x=4850, ymax*0.01, labels="Copper Age",lty=c("solid"), col=c("darkcyan"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(4250, ymax*0.15,4250,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
text(x=3580, ymax*0.11, labels="Bronze Age",lty=c("solid"), col=c("darkcyan"), font=2, cex=1, adj=c(0.5,-0.5))
segments(2950, ymax*0.15,2950,ymax*0.02, lty="solid", col="darkcyan", lwd=(1.2))
segments(3650, ymax*0.12,3650,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1.2))
segments(3275, ymax*0.12,3275,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1.2))
segments(3100, ymax*0.12,3100,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1.2))
text(x=3950, ymax*-0.01, labels="EBA",lty=c("solid"), col=c("darkcyan"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3480, ymax*-0.01, labels="MBA",lty=c("solid"), col=c("darkcyan"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3130, ymax*0.06, labels="LBA",lty=c("solid"), col=c("darkcyan"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2960, ymax*0.06, labels="FBA",lty=c("solid"), col=c("darkcyan"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2865, ymax*0.011, labels="IA",lty=c("solid"), col=c("darkcyan"), font=2, cex=0.8, adj=c(0.5,-0.5))
xticks <- seq(10000,2500,-500)
axis(side=1, at=xticks, labels=xticks, las=2, cex.axis=0.80) # add BP axis
mtext("cal BP",1, 1, at=6450, adj=-0.1, font=1, cex=0.5)
axis(side=1, at=xticks-50, labels=xticks-2000, las=2, cex.axis=0.8,pos=-0.07)# add BC/AD axis
mtext("BC/AD",1, 4, at=6400, adj=-0.1, font=1, cex=0.5)
dev.off()


############# plot SPD versus palaeoclimate proxies southern Italy (Fig. 9) ##########
#define layout
pdf(file="pdf/Fig9.pdf", width=6, height=7.1)
layout(matrix(c(1,2,3,4,5), 5, 1, byrow=TRUE), widths=6, heights=c(1.2,1.2,1.2,1.2,2.3))
par(mar=c(0, 3, 1, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP.9,palaeoclimate$Shkodra.Lake, type="l", axes=FALSE, xlim = c(10000,2800), ylim=c(-6.5,-8.7), lwd=1, col="darkorange", xaxt="n",ylab="",xaxs = "i", yaxs = "i")
axis(2, at=seq(-8.5,-6.5,0.5), labels = seq(-8.5,-6.5, 0.5), lwd=1, line=0.1, cex.axis=0.6, mgp=c(0,0.5,0), col="darkorange", col.axis="darkorange")
mtext(2,text=expression(paste(sigma^18,"O")),line=1.4, cex = 0.6,col="darkorange")
rect(7825,-6.5,7158,-8.7, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(6289,-6.5,6005,-8.7, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5862,-6.5,5857,-8.7, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5844,-6.5,5831,-8.7, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5758,-6.5,5588,-8.7, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5039,-6.5,5032,-8.7, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(4293,-6.5,4152,-8.7, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(3858,-6.5,3324,-8.7, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3300,-6.5,3255,-8.7, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(2834,-6.5,2769,-8.7, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
text(x=9800, y=-8.2, labels="Lake Shkodra (5)", font=2, col="darkorange", cex=1.2, adj=c(0,0))
par(new=T)# treat the graph window as a new window, so a new graph can be plotted without erasing the old one
plot(runcorr_shkodra$brks_end,runcorr_shkodra$shkodra_corr, axes=FALSE, xlim = c(10000,2800), ylim=c(-1,1),  pch=16, col="red", lwd=2, cex=0.5, xaxt="n", ylab="",xaxs = "i", yaxs = "i") # plot the 500-year running correlation values with the SPD
axis(4, at=seq(-1,1,0.5), labels = seq(-1,1,0.5), lwd=0.8, line=1.6, cex.axis=0.6, mgp=c(0,0.3,0), col="black")
mtext(4,text="correlation",line=0.1, cex = 0.6,col="black")
par(mar=c(0, 3, 0, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP.7,palaeoclimate$S.Adriatic.Sea.MD90.917, type="l",axes=FALSE, xlim = c(10000,2800), ylim=c(-1.8,-0.6), lwd=1, col="aquamarine4", xaxt="n",ylab="",xaxs = "i", yaxs = "i")
axis(4, at=seq(-1.8,-0.6,0.2), labels = seq(-1.8,-0.6, 0.2), lwd=1, line=0.2, cex.axis=0.6, mgp=c(0,0.5,0), col="aquamarine4", col.axis="aquamarine4")
mtext(4,text=expression(paste(sigma^13,"C")),line=1.5, cex = 0.6, col="aquamarine4")
rect(7825,-1.8,7158,-0.6, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(6289,-1.8,6005,-0.6, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5862,-1.8,5857,-0.6, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5844,-1.8,5831,-0.6, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5758,-1.8,5588,-0.6, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5039,-1.8,5032,-0.6, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(4293,-1.8,4152,-0.6, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(3858,-1.8,3324,-0.6, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3300,-1.8,3255,-0.6, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(2834,-1.8,2769,-0.6, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
text(x=9800, y=-0.8, labels="South Adriatic Sea (6)", font=2, col="aquamarine4", cex=1.2, adj=c(0,0))
par(mar=c(0, 3, 0, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP.8,palaeoclimate$Gulf.of.Taranto, type="l",axes=FALSE, xlim = c(10000,2800), ylim=c(50,20), lwd=1, col="brown", xaxt="n",ylab="",xaxs = "i", yaxs = "i")
axis(2, at=seq(20,50,5), labels = seq(20,50, 5), lwd=1, line=0.2, cex.axis=0.6, mgp=c(0,0.5,0), col="brown", col.axis="brown")
mtext(2,text="Ca/Ti",line=1.5, cex = 0.6, col="brown")
rect(7825,50,7158,20, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(6289,50,6005,20, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5862,50,5857,20, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5844,50,5831,20, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5758,50,5588,20, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5039,50,5032,20, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(4293,50,4152,20, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(3858,50,3324,20, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3300,50,3255,20, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(2834,50,2769,20, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
text(x=9800, y=28, labels="Gulf of Taranto (7)", font=2, col="brown", cex=1.2, adj=c(0,0))
arrows(3500,40,3500,30, col="brown", length=0.1, lwd=2)
arrows(3500,40,3500,45, col="brown", length=0.1, lwd=2)
text(x=3600, y=40, labels="winter\nrainfall", font=1, srt=90, col="brown", cex=0.8, adj=c(0,0))
text(x=3400, y=35, labels=expression(paste("NAO+\nwarm T (",degree,"C)")), font=1, col="brown", cex=0.6, adj=c(0,0))
text(x=3400, y=45, labels=expression(paste("NAO-\ncold T (",degree,"C)")), font=1, col="brown", cex=0.6, adj=c(0,0))
par(new=T)# treat the graph window as a new window, so a new graph can be plotted without erasing the old one
plot(runcorr_taranto$brks_end,runcorr_taranto$taranto_corr, axes=FALSE, xlim = c(10000,2800), ylim=c(-1,1),  pch=16, col="red", lwd=2, cex=0.5, xaxt="n", ylab="",xaxs = "i", yaxs = "i") # plot the 500-year running correlation values with the SPD
axis(4, at=seq(-1,1,0.5), labels = seq(-1,1,0.5), lwd=0.8, line=1.6, cex.axis=0.6, mgp=c(0,0.3,0), col="black")
mtext(4,text="correlation",line=0.1, cex = 0.6,col="black")
par(mar=c(0, 3, 0, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP.10,palaeoclimate$Lake.Butrint, type="l", axes=FALSE, xlim = c(10000,2800), ylim=c(2.5,1), lwd=1, col="orchid3", xaxt="n",ylab="",xaxs = "i", yaxs = "i")
axis(4, at=seq(1,2.5,0.5), labels = seq(1,2.5,0.5), lwd=1, line=0.2, cex.axis=0.6, mgp=c(0,0.5,0), col="orchid3", col.axis="orchid3")
mtext(4,text="Sr/Ca",line=1.5, cex = 0.6, col="orchid3")
rect(7825,2.5,7158,1, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(6289,2.5,6005,1, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5862,2.5,5857,1, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5844,2.5,5831,1, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5758,2.5,5588,1, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5039,2.5,5032,1, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(4293,2.5,4152,1, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(3858,2.5,3324,1, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3300,2.5,3255,1, col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(2834,2.5,2769,1, col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
text(x=9800, y=1.5, labels="Lake Butrint (8)", font=2, col="orchid3", cex=1.2, adj=c(0,0))
arrows(5000,1.7,5000,1.4, col="orchid3", length=0.1, lwd=2)
arrows(5000,1.7,5000,2, col="orchid3", length=0.1, lwd=2)
text(x=5100, y=1.9, labels="salinity", font=1, srt=90, col="orchid3", cex=0.8, adj=c(0,0))
text(x=4900, y=1.5, labels="lower", font=1, col="orchid3", cex=0.6, adj=c(0,0))
text(x=4900, y=2, labels="higher", font=1, col="orchid3", cex=0.6, adj=c(0,0))
par(new=T)# treat the graph window as a new window, so a new graph can be plotted without erasing the old one
plot(runcorr_butrint$brks_end,runcorr_butrint$Butrint_corr, axes=FALSE, xlim = c(10000,2800), ylim=c(-1,1),  pch=16, col="red", lwd=2, cex=0.5, xaxt="n", ylab="",xaxs = "i", yaxs = "i") # plot the 500-year running correlation values with the SPD
axis(2, at=seq(-1,1,0.5), labels = seq(-1,1,0.5), lwd=0.8, line=1.4, cex.axis=0.6, mgp=c(0,0.5,0), col="black")
mtext(2,text="correlation",line=0.1, cex = 0.6,col="black")
par(mar=c(6, 3, 0, 3)) #c(bottom, left, top, right)
ymax <- max(logisticmod_south$result$PrDens)*1.1
plot(logisticmod_south, ylim=c(0,ymax), xlim=c(10000,2800), drawaxes=FALSE, col="darkgreen")
abline(v=seq(10000,3000,-500), lty="dotted", col="gray50",lwd=1)
legend(x=9800, y=ymax*0.95,legend=c("SPD","95% MC envelope","positive deviation","negative deviation"),col=c(1,"lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)),lty=c(1,1,1,1),lwd=c(0.5,5,5,5),cex=0.7, bg="white", title="")
text(x=9600, ymax*0.87, labels="Southern Italy", font=2, cex=0.9, adj=c(0,0))
text(x=9800, y=ymax*0.52, labels=paste("n=",nrow(datesp[datesp$Region==3,]),", sites=",length(unique(datesp$SiteName[datesp$Region==3])),", bins=",length(unique(bins[datesp$Region==3])),sep=""), font=1, cex=0.6, adj=c(0,0.7))
text(x=9800, ymax*0.45, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(logisticmod_south$pval,4))))
axis(2, at=seq(0,0.20,0.05), labels = seq(0,0.20, 0.05), lwd=1, line=0.1, cex.axis=0.6, mgp=c(0,0.5,0), col="black")
mtext(2,text="SPD",line=1.4, cex = 0.6,col="black")
text(x=8950, y=ymax*0.07, labels="Mesolithic",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(8150, ymax*0.15,8150,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
text(x=6650, ymax*0.07, labels="Neolithic",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5650, ymax*0.15,5650,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
text(x=7750, ymax*-0.01, labels="Early",lty=c("solid"), col=c("darkgreen"), font=2, cex=1, adj=c(0.5,-0.5))
segments(7450, ymax*0.12,7450,ymax*0.02, lty="dotted", col="darkcyan", lwd=(1.2))
text(x=6800, ymax*-0.01, labels="Middle",lty=c("solid"), col=c("darkgreen"), font=2, cex=1, adj=c(0.5,-0.5))
segments(6150, ymax*0.12,6150,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1.2))
text(x=5900, ymax*-0.01, labels="Late",lty=c("solid"), col=c("darkgreen"), font=2, cex=1, adj=c(0.5,-0.5))
text(x=4900, ymax*0.01, labels="Copper Age",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
text(x=3550, ymax*0.13, labels="Bronze Age",lty=c("solid"), col=c("darkgreen"), font=2, cex=1.1, adj=c(0.5,-0.5))
segments(2950, ymax*0.15,2950,ymax*0.02, lty="solid", col="darkgreen", lwd=(1.2))
segments(3600, ymax*0.12,3600,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1.2))
segments(3250, ymax*0.12,3250,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1.2))
segments(3100, ymax*0.12,3100,ymax*0.02, lty="dotted", col="darkgreen", lwd=(1.2))
text(x=3850, ymax*-0.01, labels="EBA",lty=c("solid"), col=c("darkgreen"), font=2, cex=0.7, adj=c(0.5,-0.5))
text(x=3420, ymax*-0.01, labels="MBA",lty=c("solid"), col=c("darkgreen"), font=2, cex=0.7, adj=c(0.5,-0.5))
text(x=3100, ymax*0.07, labels="LBA",lty=c("solid"), col=c("darkgreen"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2950, ymax*0.07, labels="FBA",lty=c("solid"), col=c("darkgreen"), font=2, srt=90, cex=0.6, adj=c(0.5,-0.5))
text(x=2870, ymax*0.011, labels="IA",lty=c("solid"), col=c("darkgreen"), font=2, cex=0.7, adj=c(0.5,-0.5))
xticks <- seq(10000,2500,-500)
axis(side=1, at=xticks, labels=xticks, las=2, cex.axis=0.80) # add BP axis
mtext("cal BP",1, 1, at=6450, adj=-0.1, font=1, cex=0.5)
axis(side=1, at=xticks-50, labels=xticks-2000, las=2, cex.axis=0.8,pos=-0.055)# add BC/AD axis
mtext("BC/AD",1, 4, at=6400, adj=-0.1, font=1, cex=0.5)
dev.off()

############# plot SPD versus palaeoclimate proxies Sicily (Fig. 10) ##########
#define layout
pdf(file="pdf/Fig10.pdf", width=6, height=6)
layout(matrix(c(1,2,3,4), 4, 1, byrow=TRUE), widths=6, heights=c(1.2,1.2,1.2,2.4))
par(mar=c(0, 3, 1, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP.5,palaeoclimate$Carburangeli.Cave, type="l", axes=FALSE, xlim = c(10000,2800), ylim=c(-12,0), lwd=1, col="yellow4", xaxt="n",ylab="",xaxs = "i", yaxs = "i")
axis(2, at=seq(-12,0,4), labels = seq(-12,0,4), lwd=1, line=0.1, cex.axis=0.6, mgp=c(0,0.5,0), col="yellow4", col.axis="yellow4")
mtext(2,text=expression(paste(sigma^13,"C")),line=1.4, cex = 0.6, col="yellow4")
rect(8426,-12,8345,0,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(8174,-12,8021,0,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(7691,-12,7666,0,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(6392,-12,6356,0,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(6334,-12,6281,0,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5463,-12,5436,0,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5403,-12,5401,0,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5397,-12,5394,0,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5392,-12,5314,0,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(4850,-12,4813,0,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(4163,-12,3815,0,col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3580,-12,3388,0,col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3229,-12,2970,0,col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
text(x=9800, y=-10, labels="Carburangeli Cave (9)", font=2, col="yellow4", cex=1.2, adj=c(0,0))
par(new=T)# treat the graph window as a new window, so a new graph can be plotted without erasing the old one
plot(runcorr_carburangeli$brks_end,runcorr_carburangeli$carburangeli_corr, axes=FALSE, xlim = c(10000,2800), ylim=c(-1,1),  pch=16, col="red", lwd=2, cex=0.5, xaxt="n", ylab="",xaxs = "i", yaxs = "i") # plot the 500-year running correlation values with the SPD
axis(4, at=seq(-1,1,0.5), labels = seq(-1,1,0.5), lwd=0.8, line=1.6, cex.axis=0.6, mgp=c(0,0.3,0), col="black")
mtext(4,text="correlation",line=0.1, cex = 0.6,col="black")
par(mar=c(0, 3, 0, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP.4,palaeoclimate$Lake.Preola, type="l", axes=FALSE, xlim = c(10000,2800), ylim=c(1.5,-1), lwd=1, col="gray30", xaxt="n",ylab="",xaxs = "i", yaxs = "i")
axis(4, at=seq(1.5,-1,-0.5), labels = seq(1.5,-1,-0.5), lwd=1, line=0.2, cex.axis=0.6, mgp=c(0,0.4,0), col="gray30", col.axis="gray30")
mtext(4,text="CA scores",line=1.4, cex = 0.6, col="gray30")
rect(8426,1.5,8345,-1,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(8174,1.5,8021,-1,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(7691,1.5,7666,-1,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(6392,1.5,6356,-1,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(6334,1.5,6281,-1,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5463,1.5,5436,-1,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5403,1.5,5401,-1,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5397,1.5,5394,-1,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5392,1.5,5314,-1,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(4850,1.5,4813,-1,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(4163,1.5,3815,-1,col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3580,1.5,3388,-1,col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3229,1.5,2970,-1,col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
text(x=9800, y=1.2, labels="Lake Preola (10)", font=2, col="gray30", cex=1.2, adj=c(0,0))
arrows(3150,0,3150,-0.6, col="gray30", length=0.1, lwd=2)
arrows(3150,0,3150,0.4, col="gray30", length=0.1, lwd=2)
text(x=3250, y=0.2, labels="lake level", font=1, srt=90, col="gray30", cex=0.8, adj=c(0,0))
text(x=3050, y=-0.4, labels="higher", font=1, col="gray30", cex=0.6, adj=c(0,0))
text(x=3050, y=0.4, labels="lower", font=1, col="gray30", cex=0.6, adj=c(0,0))
par(mar=c(0, 3, 0, 3)) #c(bottom, left, top, right)
plot(palaeoclimate$cal.age.BP.6,palaeoclimate$Lago.Di.Pergusa, xlab="",type="l", axes=FALSE, xlim = c(10000,2800), ylim=c(2,-2.4), lwd=1, col="turquoise3", xaxt="n",ylab="",xaxs = "i", yaxs = "i")
axis(2, at=seq(2,-2,-0.5), labels = seq(2,-2,-0.5), lwd=1, line=0.2, cex.axis=0.6, mgp=c(0,0.5,0),col="turquoise3", col.axis="turquoise3")
mtext(2,text=expression(paste(sigma^18,"O")),line=1.4, cex = 0.6, col="turquoise3")
rect(8426,2,8345,-2.4,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(8174,2,8021,-2.4,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(7691,2,7666,-2.4,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(6392,2,6356,-2.4,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(6334,2,6281,-2.4,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5463,2,5436,-2.4,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5403,2,5401,-2.4,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5397,2,5394,-2.4,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(5392,2,5314,-2.4,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(4850,2,4813,-2.4,col=rgb(0,0,0.7,0.2),border=NA)#blue rectangle
rect(4163,2,3815,-2.4,col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3580,2,3388,-2.4,col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
rect(3229,2,2970,-2.4,col=rgb(0.7,0,0,0.2),border=NA)#red rectangle
abline(v=seq(10000,2500,-500), lty="dotted", col="gray50", lwd=1)
text(x=9800, y=0.8, labels="Lake Pergusa (11)", font=2, col="turquoise3", cex=1.2, adj=c(0,0))
par(mar=c(6, 3, 0, 3)) #c(bottom, left, top, right)
ymax <- max(logisticmod_sicily$result$PrDens)*1.1
plot(logisticmod_sicily, ylim=c(0,ymax), xlim=c(10000,2800), drawaxes=FALSE, col="brown")
abline(v=seq(10000,3000,-500), lty="dotted", col="gray50",lwd=1)
legend(x=9800, y=ymax*0.95,legend=c("SPD","95% MC envelope","positive deviation","negative deviation"),col=c(1,"lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)),lty=c(1,1,1,1),lwd=c(0.5,5,5,5),cex=0.7, bg="white", title="")
text(x=9600, ymax*0.89, labels="Sicily", font=2, cex=0.9, adj=c(0,0))
text(x=9800, y=ymax*0.52, labels=paste("n=",nrow(datesp[datesp$Region==4,]),", sites=",length(unique(datesp$SiteName[datesp$Region==4])),", bins=",length(unique(bins[datesp$Region==4])),sep=""), font=1, cex=0.6, adj=c(0,0.7))
text(x=9800, ymax*0.45, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(logisticmod_sicily$pval,4))))
axis(2, at=seq(0,0.08,0.02), labels = seq(0,0.08, 0.02), lwd=1, line=0.1, cex.axis=0.6, mgp=c(0,0.5,0), col="black")
mtext(2,text="SPD",line=1.4, cex = 0.6,col="black")
text(x=9000, y=ymax*0.14, labels="Mesolithic",lty=c("solid"), col=c("brown"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(8050, ymax*0.15,8050,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
text(x=6600, ymax*0.23, labels="Neolithic",lty=c("solid"), col=c("brown"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(5450, ymax*0.15,5450,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
text(x=7800, ymax*0.01, labels="Early",lty=c("solid"), col=c("brown"), font=2, cex=0.9, adj=c(0.5,-0.5))
segments(7450, ymax*0.12,7450,ymax*0.02, lty="dotted", col="brown", lwd=(1.2))
text(x=6750, ymax*0.01, labels="Middle",lty=c("solid"), col=c("brown"), font=2, cex=0.9, adj=c(0.5,-0.5))
segments(6450, ymax*0.12,6450,ymax*0.02, lty="dotted", col="brown", lwd=(1.2))
text(x=5950, ymax*0.01, labels="Late",lty=c("solid"), col=c("brown"), font=2, cex=0.9, adj=c(0.5,-0.5))
text(x=4800, ymax*-0.01, labels="Copper Age",lty=c("solid"), col=c("brown"), font=2, cex=1, adj=c(0.5,-0.5))
segments(4150, ymax*0.15,4150,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
text(x=3500, ymax*0.16, labels="Bronze Age",lty=c("solid"), col=c("brown"), font=2, cex=1.2, adj=c(0.5,-0.5))
segments(2850, ymax*0.15,2850,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
segments(3450, ymax*0.12,3450,ymax*0.02, lty="dotted", col="brown", lwd=(1.2))
segments(3200, ymax*0.12,3200,ymax*0.02, lty="dotted", col="brown", lwd=(1.2))
segments(3000, ymax*0.12,3000,ymax*0.02, lty="dotted", col="brown", lwd=(1.2))
text(x=3770, ymax*0.01, labels="EBA",lty=c("solid"), col=c("brown"), font=2, cex=0.8, adj=c(0.5,-0.5))
text(x=3330, ymax*0.01, labels="MBA",lty=c("solid"), col=c("brown"), font=2, cex=0.6, adj=c(0.5,-0.5))
text(x=3010, ymax*0.07, labels="LBA",lty=c("solid"), col=c("brown"), font=2, srt=90, cex=0.7, adj=c(0.5,-0.5))
text(x=2840, ymax*0.07, labels="FBA",lty=c("solid"), col=c("brown"), font=2, srt=90, cex=0.7, adj=c(0.5,-0.5))
xticks <- seq(10000,2500,-500)
axis(side=1, at=xticks, labels=xticks, las=2, cex.axis=0.80) # add BP axis
mtext("cal BP",1, 1, at=6450, adj=-0.1, font=1, cex=0.5)
axis(side=1, at=xticks-50, labels=xticks-2000, las=2, cex.axis=0.8,pos=-0.025)# add BC/AD axis
mtext("BC/AD",1, 4, at=6400, adj=-0.1, font=1, cex=0.5)
dev.off()










