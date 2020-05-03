##############################################################################
# Contribution in Sat Data (SST, KPAR) to Mareike Project in Lyttelton harbour.
# Goal : To plot locations and Sat product. Check availability and distance to sites.

# FT - 24/04/2020

## Part 1 : Download excel worksheet and show location on nz map using Lealet. Download KPAR and SST.
## Part 2 : Availability of the pixels
## Part 3 : Calculate and plot CV for KPAR and SST. CV=sd/mean
## Part 4 : Scenario 1 - Extract values (KPAR and SST) at site pixel
## Part 5 : Scenario 2 - Extract values (KPAR and SST) at closest pixel offshore above threshold of certain pixel availability
##############################################################################

rm(list=ls())
library(readxl)
library(raster)
library(ncdf4)
library(ggplot2)
library(maps)
library(maptools)
library(RColorBrewer)
library(sp) 
library(rgdal)
library(RgoogleMaps)
library(leaflet)
library(measurements)
library(tibbletime)
library(tibble)
library(lubridate)
library(tidyverse)
library(reshape2)
library(plotly)
library(leafem)
library(dplyr)
library(velox)
library(stringr)

### Part 1 : Download excel worksheet, show location on nz map
#            Download KPAR stack and mean
#            Download SST stack and mean 
#####
## Read site sheet
site <- read.csv(file=paste0(getwd(),'/Coords_sites.csv'))#,skip=2,col_names=c('Site','Lat','Lon','Depth'))
##

## Plot sites on NZ map from bathy file
# Use Leaflet
m <- leaflet(data=site) %>% setView(lng = 173, lat = -43.55, zoom = 10) %>%
  addTiles()  %>%# Print the map
  addMarkers(site, lat = ~lat,lng = ~lon, popup = ~name)
m
##

## Bathy of the zone NIWA grid
## Download Bathy, plot it with sites and transform as contour
bathy <- raster('Bathy_Lyttelton.tif')
bathy[bathy>1] <- NA
bathycontour <- rasterToContour(bathy)
bathycontour <- spTransform(bathycontour,crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))
shoreline <- rasterToPoints(bathy,fun=function(x){x<1&x>-1},spatial=T)
##

## KPAR stack and mean
kpar_mean <- raster('A200207_201903_KPAR_Mean_Lyttelton_QAA.tif')
kpar <- stack('A200207_201903_KPAR_MO_Lyttelton_QAA.tif')
##

## Plot KPAR mean and sites on leaflet map
pal <- colorNumeric(rev(rainbow(10)), values(kpar_mean),
                    na.color = "transparent")

m <- leaflet(site) %>% setView(lng = 173, lat = -43.55, zoom = 10) %>%
  addTiles()  %>%# Print the map
  addScaleBar(position = "bottomright",options = scaleBarOptions(imperial=F)) %>% 
  addMouseCoordinates() %>%
  addMarkers(site, lat = ~lat,lng = ~lon, popup = ~name) %>%
  addRasterImage(kpar_mean,layerId = "Kd (/m)", col=pal, opacity = 0.8,project=T) %>%
  addLegend(pal = pal, values = values(kpar_mean),title = "Kd (m-1)") %>%
  addImageQuery(x=kpar_mean,layerId = "Kd (/m)",type='click',project=T,position='bottomleft') %>%
  addPolylines(data=bathycontour,color = "black",opacity=0.2,popup = bathycontour$level)
m
##

## SST stack and mean 
sst <- stack('A200207_201903_SST_MO_Lyttelton.tif')
sst_mean <- raster('A200207_201903_SST_Mean_Lyttelton.tif')
##

## Plot SST mean and sites on leaflet map
pal <- colorNumeric(rev(rainbow(100)), values(sst_mean),
                    na.color = "transparent")

m <- leaflet(site) %>% setView(lng = 173, lat = -43.55, zoom = 10) %>%
  addTiles()  %>%# Print the map
  addScaleBar(position = "bottomright",options = scaleBarOptions(imperial=F)) %>% 
  addMouseCoordinates() %>%
  addMarkers(site, lat = ~lat,lng = ~lon, popup = ~name) %>%
  addRasterImage(sst_mean,layerId = "SST (degC)", col=pal, opacity = 0.8,project=T) %>%
  addLegend(pal = pal, values = values(sst_mean),title = "SST (degC)") %>%
  addImageQuery(x=sst_mean,layerId =  "SST (degC)",type='click',project=T,position='bottomleft') %>%
  addPolylines(data=bathycontour,color = "black",opacity=0.2,popup = bathycontour$level)
m

# Observation:
# - Higher values of mean SST in harbour and lower values in bays. 
# - Looks weird, let's see the pixel availability

##
#####

### Part 2 : Availability of the pixels
#####

## KPAR
#system.time(kpar_NA_sum <- sum(!is.na(kpar))) #Takes 35s
#writeRaster(kpar_NA_sum,'A200207_201903_KPAR_PixAvailability_Lyttelton.tif')

kpar_NA_sum <- raster('A200207_201903_KPAR_PixAvailability_Lyttelton.tif')

kpar_NA_sum_proj <- projectRaster(kpar_NA_sum,crs=crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"),method='ngb')

pal <- colorNumeric(rev(heat.colors(100)), values(kpar_NA_sum_proj),
                    na.color = "transparent")

m <- leaflet(site) %>% setView(lng = 173, lat = -43.55, zoom = 10) %>%
  addTiles()  %>%# Print the map
  addScaleBar(position = "bottomright",options = scaleBarOptions(imperial=F)) %>% 
  addMouseCoordinates() %>%
  addMarkers(site, lat = ~lat,lng = ~lon, popup = ~name) %>%
  addRasterImage(kpar_NA_sum_proj,layerId = "Number of pixels available", col=pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(kpar_NA_sum_proj),title = "Number of pixels available") %>%
  addImageQuery(x=kpar_NA_sum_proj,layerId =  "Number of pixels available",type='click',position='bottomleft') %>%
  addPolylines(data=bathycontour,color = "black",opacity=0.2,popup = bathycontour$level)
m

##

## SST
#system.time(sst_NA_sum <- sum(!is.na(sst))) #Takes 45s
#writeRaster(sst_NA_sum,'A200207_201903_SST_PixAvailability_Lyttelton.tif',overwrite=TRUE)

sst_NA_sum <- raster('A200207_201903_SST_PixAvailability_Lyttelton.tif')

pal <- colorNumeric(rev(heat.colors(100)), values(sst_NA_sum),
                    na.color = "transparent")

m <- leaflet(site) %>% setView(lng = 173, lat = -43.55, zoom = 10) %>%
  addTiles()  %>%# Print the map
  addScaleBar(position = "bottomright",options = scaleBarOptions(imperial=F)) %>% 
  addMouseCoordinates() %>%
  addMarkers(site, lat = ~lat,lng = ~lon, popup = ~name) %>%
  addRasterImage(sst_NA_sum,layerId = "Number of pixels available", col=pal, opacity = 0.8,project=T) %>%
  addLegend(pal = pal, values = values(sst_NA_sum),title = "Number of pixels available") %>%
  addImageQuery(x=sst_NA_sum,layerId =  "Number of pixels available",type='click',position='bottomleft',project=T) %>%
  addPolylines(data=bathycontour,color = "black",opacity=0.2,popup = bathycontour$level)
m
##
# Observations:
# - "Holes" in pixels at sea very likely due to reprojection

# Conclusion and further Work 

# Because the sat data is available from 07/2002 to 03/2019, to define a time frame to refine the dataset. Use the surveys dates, maybe to focus on ecological data during the baseline period (19-20/01/17 BL1 to 5-8/12/17 BL3) and do another analysis with the data after dredging phase (DP1 only date to fell into sat data dataset range).
# I could then regenerate monthly means within the new time frame and start exploring different scenarios for assigning a value to sites.

#Different scenarios:
  
# 1 : Extract values (KPAR and SST) at site pixel -> Will be not realistic because of missing values due to shore and effect of land.
# 2 : Extract values at closest pixel offshore above threshold of certain pixel availability.
# 3 : Extract values from cluster of pixels at certain distance of site and above threshold of pixel availability.
#####

### Part 3 : Calculate and plot CV for KPAR and SST. CV=sd/mean
#####

## KPAR
#kpar_sd <- calc(kpar,sd,na.rm=T)
#writeRaster(kpar_sd,'A200207_201903_KPAR_Sd_Lyttelton_QAA',format='GTiff')
kpar_sd <- raster('A200207_201903_KPAR_Sd_Lyttelton_QAA.tif')
kpar_cv <- kpar_sd/kpar_mean

pal <- colorNumeric(rev(rainbow(100)), values(kpar_cv),
                    na.color = "transparent")

m <- leaflet(site) %>% setView(lng = 173, lat = -43.55, zoom = 10) %>%
  addTiles()  %>%# Print the map
  addScaleBar(position = "bottomright",options = scaleBarOptions(imperial=F)) %>% 
  addMouseCoordinates() %>%
  addMarkers(site, lat = ~lat,lng = ~lon, popup = ~name) %>%
  addRasterImage(kpar_cv,layerId = "CV KPAR", col=pal, opacity = 0.8,project=T) %>%
  addLegend(pal = pal, values = values(kpar_cv),title = "CV KPAR") %>%
  addImageQuery(x=kpar_cv,layerId =  "CV KPAR",type='click',position='bottomleft',project=T) %>%
  addPolylines(data=bathycontour,color = "black",opacity=0.2,popup = bathycontour$level)
m
##

## SST
#sst_sd <- calc(sst,sd,na.rm=T)
#writeRaster(sst_sd,'A200207_201903_SST_Sd_Lyttelton_QAA',format='GTiff')
sst_sd <- raster('A200207_201903_SST_Sd_Lyttelton_QAA.tif')
sst_cv <- sst_sd/sst_mean

pal <- colorNumeric(rev(rainbow(100)), values(sst_cv),
                    na.color = "transparent")

m <- leaflet(site) %>% setView(lng = 173, lat = -43.55, zoom = 10) %>%
  addTiles()  %>%# Print the map
  addScaleBar(position = "bottomright",options = scaleBarOptions(imperial=F)) %>% 
  addMouseCoordinates() %>%
  addMarkers(site, lat = ~lat,lng = ~lon, popup = ~name) %>%
  addRasterImage(sst_cv,layerId = "CV SST", col=pal, opacity = 0.8,project=T) %>%
  addLegend(pal = pal, values = values(sst_cv),title = "CV SST") %>%
  addImageQuery(x=sst_cv,layerId =  "CV SST",type='click',position='bottomleft',project=T) %>%
  addPolylines(data=bathycontour,color = "black",opacity=0.2,popup = bathycontour$level)
m
##

#####

### Part 4 : Scenario 1 - Extract values (KPAR and SST) at site pixel
#####

## Work on Lat/Lon of site coordinates, reptroject to utm
# Reprojection of sites lon/lat into tmerc (EBED crs)
LatLong_site <- data.frame(Y=site$lat,X=site$lon)
names(LatLong_site) <- c("Y","X")
coordinates(LatLong_site) <- ~ X + Y # longitude first
proj4string(LatLong_site) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0")
utm_sites <- spTransform(LatLong_site, crs( "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
##

## KPAR
kpar_velox <- velox('A200207_201903_KPAR_MO_Lyttelton_QAA.tif')
kpar_sites <- kpar_velox$extract_points(utm_sites)
kpar_sites_df <- data.frame(t(kpar_sites))

colnames(kpar_sites_df) <- site$name
rownames(kpar_sites_df) <- NULL
dateseq <- seq.Date(as.Date("2002/7/1"), by = "month", length.out = dim(kpar_sites)[2])
kpar_sites_df$Date <- dateseq

kpar_sites_tb <- as_tibble(kpar_sites_df)
kpar_sites_tb <- kpar_sites_tb %>% pivot_longer(-Date,names_to="Site",values_to='Kpar') %>% group_by(Date)

q <- ggplot(kpar_sites_tb,aes(Date,Kpar,color=Site)) + geom_point() + geom_line() + labs(y="Kpar (/m)", x = "Date")
ggplotly(q)
##

## KPAR mean, sd, median, min and max values over period
summary_kpar <- kpar_sites_tb %>% group_by(Site) %>% summarise(Mean=mean(Kpar,na.rm=T),Sd=sd(Kpar,na.rm=T),Median=median(Kpar,na.rm=T),Min=min(Kpar,na.rm=T),Max=max(Kpar,na.rm=T))
##

## SST
sst_velox <- velox('A200207_201903_SST_MO_Lyttelton.tif')
sst_sites <- sst_velox$extract_points(utm_sites)
sst_sites_df <- data.frame(t(sst_sites))

colnames(sst_sites_df) <- site$name
rownames(sst_sites_df) <- NULL
dateseq <- seq.Date(as.Date("2002/7/1"), by = "month", length.out = dim(sst_sites)[2])
sst_sites_df$Date <- dateseq

sst_sites_tb <- as_tibble(sst_sites_df)
sst_sites_tb <- sst_sites_tb %>% pivot_longer(-Date,names_to="Site",values_to='SST') %>% group_by(Date)

p <- ggplot(sst_sites_tb,aes(Date,SST,color=Site)) + geom_point() + geom_line() + labs(y="SST (degC)", x = "Date")
ggplotly(p)
##

## SST mean values over period
summary_sst <- sst_sites_tb %>% group_by(Site) %>% summarise(Mean=mean(SST,na.rm=T),Sd=sd(SST,na.rm=T),Median=median(SST,na.rm=T),Min=min(SST,na.rm=T),Max=max(SST,na.rm=T))
##

#####


### Part 5 : Scenario 2 - Extract values (KPAR and SST) at closest pixel offshore above threshold of certain pixel availability
#####

## Calculate raster ov pixel above threshold (==1, 0 otherwise)
threshold <- 202*0.75 #75% available

#KPAR
r_above_kpar <- kpar_NA_sum
r_above_kpar[which(values(kpar_NA_sum)>=threshold)] <- 1
r_above_kpar[which(values(kpar_NA_sum)<threshold)] <- 0

#SST
r_above_sst <- sst_NA_sum
r_above_sst[which(values(sst_NA_sum)>=threshold)] <- 1
r_above_sst[which(values(sst_NA_sum)<threshold)] <- 0
##

## KPAR

# Get adjacent pixel of 1 site, calculate min distance to pixel containing 1
sites_tmerc <- data.frame(utm_sites)[1,]
sites_kpar_mean <- raster::extract(r_above_kpar,sites_tmerc,cellnumbers=T) #Get cell number of pixel where site
ngb_pixels <- raster::adjacent(r_above_kpar,cells=sites_kpar_mean[,1],pairs=F,directions=16,include=T)

ngb_pix_supTh_index <- ngb_pixels[which(r_above_kpar[ngb_pixels]==1)] #Return index of pixel in cluster of 16 pixels around that have value>threshold
ngb_pix_supTh_sp <- xyFromCell(r_above_kpar,cell=ngb_pix_supTh_index,spatial=F) #Sp instead of index, for distanceFromPointsFunction
dist <- pointDistance(rbind(sites_tmerc,sites_tmerc),ngb_pix_supTh_sp,lonlat = F) 

pix_index_minDist <- ngb_pix_supTh_index[which(dist==min(dist))]

pix_index_minDist_sp <- xyFromCell(r_above_kpar,cell=pix_index_minDist,spatial=T)#Convert to sp feature, to use velox::


kpar_sites_minDist <- kpar_velox$extract_points(pix_index_minDist_sp)#Classic extraction process
kpar_sites_minDist_df <- data.frame(t(kpar_sites_minDist))

colnames(kpar_sites_minDist_df) <- site$name[1]
rownames(kpar_sites_minDist_df) <- NULL
dateseq <- seq.Date(as.Date("2002/7/1"), by = "month", length.out = dim(kpar_sites_minDist_df)[1])
kpar_sites_minDist_df$Date <- dateseq

kpar_sites_minDist_tb <- as_tibble(kpar_sites_minDist_df)
#kpar_sites_minDist_tb <- kpar_sites_minDist_tb %>% pivot_longer(-Date,names_to="Site",values_to='Kpar') %>% group_by(Date)

q <- ggplot(kpar_sites_minDist_tb,aes(Date,BP01)) + geom_point() + geom_line() + labs(y="Kpar (/m)", x = "Date")
#q <- ggplot(kpar_sites_minDist_tb,aes(Date,Kpar,color=Site)) + geom_point() + geom_line() + labs(y="Kpar (/m)", x = "Date")
ggplotly(q)

## KPAR mean, sd, median, min and max values over period
summary_kpar_s2 <- kpar_sites_minDist_tb %>% summarise(Mean=mean(BP01,na.rm=T),Sd=sd(BP01,na.rm=T),Median=median(BP01,na.rm=T),Min=min(BP01,na.rm=T),Max=max(BP01,na.rm=T))
##

# TO DO to all sites, if too hard, use Other method and distanceFromPoints()

pix_index_minDist_save <- c()
sites_kpar_mean_save <- c()
for (i in 1:dim(site)[1]) {
  sites_tmerc <- data.frame(utm_sites)[i,]
  sites_kpar_mean <- raster::extract(r_above_kpar,sites_tmerc,cellnumbers=T) #Get cell number of pixel where site
  sites_kpar_mean_save <- c(sites_kpar_mean_save,sites_kpar_mean[1])
  
  ngb_pixels <- raster::adjacent(r_above_kpar,cells=sites_kpar_mean[,1],pairs=F,directions=16,include=T)
  
  ngb_pix_supTh_index <- ngb_pixels[which(r_above_kpar[ngb_pixels]==1)] #Return index of pixel in cluster of 16 pixels around that have value>threshold
  ngb_pix_supTh_sp <- xyFromCell(r_above_kpar,cell=ngb_pix_supTh_index,spatial=F) #Sp instead of index, for distanceFromPointsFunction
  dist <- pointDistance(sites_tmerc,ngb_pix_supTh_sp,lonlat = F) 
  
  pix_index_minDist <- ngb_pix_supTh_index[which(dist==min(dist))]
  pix_index_minDist_save <- c(pix_index_minDist_save,pix_index_minDist)
}
pix_index_minDist_sp <- xyFromCell(r_above_kpar,cell=pix_index_minDist_save,spatial=T)#Convert to sp feature, to use velox::
# ISSUE FOR PIXELS THAT DOES NOT HAVE PIXEL>THRESHOLD IN THEIR NEIGBOURHOOD 

kpar_sites_minDist <- kpar_velox$extract_points(pix_index_minDist_sp)#Classic extraction process
kpar_sites_minDist_df <- data.frame(t(kpar_sites_minDist))

colnames(kpar_sites_minDist_df) <- site$name[1]
rownames(kpar_sites_minDist_df) <- NULL
dateseq <- seq.Date(as.Date("2002/7/1"), by = "month", length.out = dim(kpar_sites_minDist_df)[1])
kpar_sites_minDist_df$Date <- dateseq

kpar_sites_minDist_tb <- as_tibble(kpar_sites_minDist_df)
#kpar_sites_minDist_tb <- kpar_sites_minDist_tb %>% pivot_longer(-Date,names_to="Site",values_to='Kpar') %>% group_by(Date)

q <- ggplot(kpar_sites_minDist_tb,aes(Date,BP01)) + geom_point() + geom_line() + labs(y="Kpar (/m)", x = "Date")
#q <- ggplot(kpar_sites_minDist_tb,aes(Date,Kpar,color=Site)) + geom_point() + geom_line() + labs(y="Kpar (/m)", x = "Date")
ggplotly(q)

## KPAR mean, sd, median, min and max values over period
summary_kpar_s2 <- kpar_sites_minDist_tb %>% summarise(Mean=mean(BP01,na.rm=T),Sd=sd(BP01,na.rm=T),Median=median(BP01,na.rm=T),Min=min(BP01,na.rm=T),Max=max(BP01,na.rm=T))
# ISSUE 

##

## SST

##

#####

