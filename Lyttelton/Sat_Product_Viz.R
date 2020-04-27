##############################################################################
# Contribution in Sat Data (SST, KPAR) to Mareike Project in Lyttelton harbour.
# Goal : To plot locations and Sat product. Check availability and distance to sites.

# FT - 24/04/2020

## Part 1 : Download excel worksheet and show location on nz map using Lealet. Download KPAR and SST.
## Part 2 : Availability of the pixels
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
bathy[bathy>0] <- NA
bathycontour <- rasterToContour(bathy)
bathycontour <- spTransform(bathycontour,crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))
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

#####





