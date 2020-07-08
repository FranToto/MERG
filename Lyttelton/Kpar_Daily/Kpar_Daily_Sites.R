##############################################################################
# Loads KPAR Daily values (in monthly tif files), Calculate Pixel availability,
# Extract time series from pixels/sites

# FT - 08/07/2020
# Next of KPAR_extractDailyValuesZones.R and uses Sat_Product_Viz.R coordinates of pixel/sites

## PART 1 : Load KPAR and Time_var files, calculate pixel availability
## PART 2 : Extract time series at sites/pixel
##############################################################################

rm(list=ls())
library(raster)
library(tidyverse)
library(lubridate)
library(plotly)
library(velox)
library(leaflet)
library(leafem)

## PART 1 : Load KPAR and Time_var files, calculate pixel availability
#####

## KPAR daily values stack (01/01/2015 to 31/03/2016)
files <- list.files(getwd(),pattern="QAA.tif",full.names=TRUE)

kpar_stack <- stack(files)

#system.time(kpar_mean <- mean(kpar_stack,na.rm=T)) #Takes 168s
#writeRaster(kpar_mean,'KPAR_Daily_20150101_20160331_Mean_LH.tif')
kpar_mean <- raster('KPAR_Daily_20150101_20160331_Mean_LH.tif')
##

## Pixel availability
#system.time(kpar_NA_sum <- sum(!is.na(kpar_stack))) #Takes 165s
#writeRaster(kpar_NA_sum,'KPAR_Daily_20150101_201603_PixAvailability_LH.tif')
kpar_pixAv <- raster('KPAR_Daily_20150101_201603_PixAvailability_LH.tif')
##

## Time_var
files_time <- list.files(getwd(),pattern=".csv",full.names=TRUE)

#Bind all files (could save it to avoid doing it again)
for (i in 1:length(files_time)){
  if(i==1){
    time_var <- read.csv(files_time[i])
  }else{
    time_mat <- read.csv(files_time[i])
    time_var <- cbind(time_var,time_mat)
  }
}

##

## Sites
site <- read.csv(file=paste0(getwd(),'/Coords_sites.csv'))
# Work on Lat/Lon of site coordinates, reptroject to utm
# Reprojection of sites lon/lat into tmerc (EBED crs)
LatLong_site <- data.frame(Y=site$lat,X=site$lon)
names(LatLong_site) <- c("Y","X")
coordinates(LatLong_site) <- ~ X + Y # longitude first
proj4string(LatLong_site) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0")
utm_sites <- spTransform(LatLong_site, crs( "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
##

## Kpar velox
kpar_velox <- velox(kpar_stack) #LONG
##

#####


## PART 2 : Extract time series at sites/pixel
# From Sat_Product_Viz.R:
# Scenario 3 - Extract Median value from a cluster of pixels (8+1) around the sites location (cf KPAR_Pix_Investigation.R data in Hauraki Gulf)
#####

## Extracting time series 21 sites
sites_tmerc <- data.frame(utm_sites)
sites_kpar_mean <- raster::extract(kpar_mean,sites_tmerc,cellnumbers=T)
ngb_pixels <- raster::adjacent(kpar_mean,cells=sites_kpar_mean[,1],directions=16,include=T) #direction 8 (9 pix) or 16 (17 pix)


#ngb_pixels_id <- sort(ngb_pixels[,2]) #sort by increasing numbers of cell id, get rid of cul#1
# Previous method not assigning right id to pixels, to check in KPAR_Pix_Investigation.R
ngb_pixels_id <- ngb_pixels #useless but make sense cf script of origin and previous line

ngd_sites_sp <- xyFromCell(kpar_mean,cell=ngb_pixels_id[,2],spatial=T)#Convert to sp feature, to use velox::

# Map of selected pixels, to check position
ngd_sites_sp_latlon <- spTransform(ngd_sites_sp,crs(LatLong_site))
ngd_sites_sp_latlon_df <- data.frame(ngd_sites_sp_latlon)
ngd_sites_sp_latlon_df <- cbind(ngd_sites_sp_latlon_df,ngb_pixels)#seq(1,dim(ngd_sites_sp_latlon_df)[1])

m <- leaflet(data=ngd_sites_sp_latlon_df) %>% setView(lng = 173, lat = -43.55, zoom = 10) %>%
  addTiles()  %>%# Print the map
  addMarkers(ngd_sites_sp_latlon_df, lng = ~x, lat = ~y, label = ~from)
m

# Extract
kpar_ngb_sites <- kpar_velox$extract_points(ngd_sites_sp)
##

## Tidying and Plot
kpar_ngb_sites_df <- data.frame(t(kpar_ngb_sites))
rownames(kpar_ngb_sites_df) <- NULL

colnames(kpar_ngb_sites_df) <- paste0(ngb_pixels_id[,1],'_',ngb_pixels_id[,2])

dateseq <- date_decimal(as.numeric(time_var[5,])) #Use 5th row as decimal date contains hour/day/month/year 
kpar_ngb_sites_df$Date <- dateseq

kpar_ngb_sites_tb <- as_tibble(kpar_ngb_sites_df)

# Groupping clusters of pixels
kpar_ngb_sites_tb <- kpar_ngb_sites_tb %>% pivot_longer(-Date,names_to=c("SiteFrom","CellidTo"),names_pattern = "(.*)_(.*)",values_to = "KPAR") %>% group_by(SiteFrom,Date)

# Test by filtering by 1 site
kpar_ngb_sites_tb_1site <- kpar_ngb_sites_tb %>% filter(SiteFrom==4402)
g <- ggplot(kpar_ngb_sites_tb_1site,aes(Date,KPAR,color=SiteFrom,shape=CellidTo,group=interaction(SiteFrom,CellidTo))) + geom_point() + geom_line() + labs(y="KPAR") #+ scale_shape_manual(values=rep(seq(0,8,1),3))
ggplotly(g)


# Blend 17 pixels by taking Median of all TS (21 sites)
kpar_ngb_median_sites_tb <- kpar_ngb_sites_tb %>% summarise(Median=median(KPAR,na.rm=T))

# Change names of sites, from pixel ID to actual name
# 4402 == BP01, 4407 == BP02, 4507 == BP03, 4804 == BP05, 4807 == BP06, 5009 == BP08,
# 5404 == BP10, 4107 == BP13, 3912 == BP14, 4202 == LH01, 4198 == LH02, 4682 == LH07,
# 4687 == LH10, 5093 == PB02, 5384 == PB03, 5386 == PB10, 5193 == PB11, 4693 == PL02,
# 4887 == PL03, 4889 == PL14, 4599 == PL16

kpar_ngb_median_sites_tb$SiteFrom <- kpar_ngb_median_sites_tb$SiteFrom %>% str_replace('4402', 'BP01') %>%
  str_replace('4407', 'BP02') %>% str_replace('4507', 'BP03') %>% str_replace('4804', 'BP05') %>% str_replace('4807', 'BP06') %>%
  str_replace('5009', 'BP08') %>% str_replace('5404', 'BP10') %>% str_replace('4107', 'BP13') %>% str_replace('3912', 'BP14') %>%
  str_replace('4202', 'LH01') %>% str_replace('4198', 'LH02') %>% str_replace('4682', 'LH07') %>% str_replace('4687', 'LH10') %>%
  str_replace('5093', 'PB02') %>% str_replace('5384', 'PB03') %>% str_replace('5386', 'PB10') %>% str_replace('5193', 'PB11') %>%
  str_replace('4693', 'PL02') %>% str_replace('4887', 'PL03') %>% str_replace('4889', 'PL14') %>% str_replace('4599', 'PL16')

q <-  ggplot(kpar_ngb_median_sites_tb,aes(x=Date,y=Median,color=SiteFrom)) + geom_point() + geom_line()
ggplotly(q)


# Export Time series of Median Kpar for 2 sizes of clusters
# write_csv(kpar_ngb_median_sites_tb,path=paste0(getwd(),'/KPAR_Daily_Sites_S3_Median_16pix.csv'))
#write_csv(kpar_ngb_median_sites_tb,path=paste0(getwd(),'/KPAR_Sites_S3_Median_8pix.csv'))

#####

## PART 3 : Data analysis on Kpar daily values at sites (s3, kpar_ngb_median_sites_tb)
#####

## Calculate Daily Mean
# kpar_ngb_median_sites_tb <- read_csv('KPAR_Daily_Sites_S3_Median_16pix.csv')
kpar_daily_s3_tidy <- kpar_ngb_median_sites_tb %>% mutate(Day = as.Date(Date)) %>% group_by(SiteFrom,Day) %>% summarise(Kpar_day=mean(Median,na.rm=T))

q <-  ggplot(kpar_daily_s3_tidy,aes(x=Day,y=Kpar_day,color=SiteFrom)) + geom_point() + geom_line()
ggplotly(q)
##

## Calculate Monthly Mean
#kpar_monthly_s3_tidy <- kpar_ngb_median_sites_tb %>% mutate(Month = paste0(month(as.Date(Date),label=T),year(as.Date(Date)))) %>% group_by(SiteFrom,Month) %>% summarise(Kpar_month=mean(Median,na.rm=T))
kpar_monthly_s3_tidy <- kpar_ngb_median_sites_tb %>% mutate(Month = format(as.Date(Date), "%Y-%m")) %>% group_by(SiteFrom,Month) %>% summarise(Kpar_month=mean(Median,na.rm=T))

q <-  ggplot(kpar_monthly_s3_tidy,aes(x=Month,y=Kpar_month,color=SiteFrom)) + geom_point() + geom_line()
ggplotly(q)
##

## Calculate Weekly Mean
#kpar_weekly_s3_tidy <- kpar_ngb_median_sites_tb %>% mutate(Weeks = week(Date))%>% group_by(SiteFrom,Weeks) %>% summarise(Kpar_weeks=mean(Median,na.rm=T))
kpar_weekly_s3_tidy <- kpar_ngb_median_sites_tb %>% mutate(Weeks = format(as.Date(Date), "%Y-%W")) %>% group_by(SiteFrom,Weeks) %>% summarise(Kpar_weeks=mean(Median,na.rm=T))

q <-  ggplot(kpar_weekly_s3_tidy,aes(x=Weeks,y=Kpar_weeks,color=SiteFrom)) + geom_point() + geom_line()
ggplotly(q)
##



#####



