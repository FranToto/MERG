#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readxl)
library(rsconnect)
library(raster)
library(leaflet)
library(ggplot2)
library(readxl)
library(measurements)
library(knitr)
library(leafem)

## Files Loading
#ebed <- stack('EBED_Lyttelton_v2017.tif')
#ebed_mean <- raster('EBED_mean_Lyttelton_v2017.tif')

sst <- stack('A200207_201903_SST_MO_Lyttelton.tif')
sst_mean <- raster('A200207_201903_SST_Mean_Lyttelton.tif')

kd <- stack('A200207_201903_KPAR_MO_Lyttelton_QAA.tif')
kd_mean <- raster('A200207_201903_KPAR_Mean_Lyttelton_QAA.tif')

bathy <- raster('Bathy_Lyttelton.tif')

site <- read.csv(file=paste0(getwd(),'/Coords_sites.csv'))
##

## Color palette
palKd <- colorNumeric(rev(rainbow(10)), values(kd_mean),na.color = "transparent")
#palEbed <- colorNumeric(rev(rainbow(100)), values(log10(ebed_mean)),na.color = "transparent")
palSST <- colorNumeric(rainbow(100), values(sst_mean),na.color = "transparent")

##

## Creating Vector of data matching slider

# Kd
start_dateKd <- as.Date("2002-07-01")
end_dateKd <- as.Date("2019-03-01")
daKd <- seq(start_dateKd,end_dateKd,by='month')

# SST
start_dateSST <- as.Date("2002-07-01")
end_dateSST <- as.Date("2019-03-01")
daSST <- seq(start_dateSST,end_dateSST,by='month')

# # Ebed
# start_date <- as.Date("2002-07-01")
# end_date <- as.Date("2017-02-01")
# da <- seq(start_date,end_date,by='month')

monthStart <- function(x) {#Function to reset slider input date to start of the month to fit with number of layers
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}
##

## Quick work on bathy
bathy[bathy>0] <- NA
bathycontour <- rasterToContour(bathy)
bathycontour <- spTransform(bathycontour,crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))
##

## Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Kd and Ebed in Lyttelton Harbour"),
   
   # Sidebar 
   sidebarLayout(
      sidebarPanel(
        #sliderInput(inputId ="sliderKd", "Month", min = 1,max = 201,value=1),
        sliderInput(inputId ="sliderKd", "Time", min = as.Date(start_dateKd),max =as.Date(end_dateKd),value=as.Date("2006-07-01"),timeFormat="%b %Y"),
        #sliderInput(inputId ="sliderEbed", "Month", min = 1,max = 176,value=1)
        sliderInput(inputId ="sliderSST", "Time", min = as.Date(start_dateSST),max =as.Date(end_dateSST),value=as.Date("2006-07-01"),timeFormat="%b %Y")
        
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h3("Kd Monthly Mean"),
         leafletOutput("kdplot"),
         h3("SST Monthly Mean"),
         leafletOutput("SSTplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## Kd
  
  nlayerKd <- reactive({ #Reactive variable, link vector of date (for slider) to index of layer
    which(daKd==monthStart(input$sliderKd))#Need to reset to first of the month
  })
  
  # kdmonth_reac <- reactive({
  #   subset(kd,input$sliderKd)
  # })  
  
  kdmonth_reac <- reactive({
    subset(kd,nlayerKd())
  })
  
  
  output$kdplot <- renderLeaflet({
    leaflet(data=site) %>% setView(lng = 173.5, lat = -43.5, zoom = 8) %>%
      addTiles()  %>%
      addScaleBar(position = "bottomright",options = scaleBarOptions(imperial=F)) %>% 
      addMouseCoordinates() %>%
      addMarkers(site, lat = ~lat,lng = ~lon, popup = ~name) %>%
      addRasterImage(kdmonth_reac(), col=palKd,layerId = "Kdvalues", opacity = 0.8,project=T) %>%
      addImageQuery(kdmonth_reac(),layerId = "Kdvalues",type='click',project=T,position='bottomleft') %>%
      addLegend(pal = palKd, values = values(kdmonth_reac()),title = "Kd (m-1)") %>%
      addPolylines(data=bathycontour,color = "black",opacity=0.2,popup = bathycontour$level) 
 }) 
  
  # observe({
  #  leafletProxy("kdplot") %>%
  #  clearImages() %>%
  #  addRasterImage(kdmonth_reac(), col=palKd,layerId = "Kdvalues", opacity = 0.8,project=T) %>%
  #  addLegend(pal = palKd, values = values(kdmonth_reac()),title = "Kd (m-1)") %>%
  #  addImageQuery(kdmonth_reac(),layerId = "Kdvalues",type='click',project=T,position='bottomleft')
  # })
  
  ## SST
  nlayer <- reactive({ #Reactive variable, link vector of date (for slider) to index of layer
    which(daSST==monthStart(input$sliderSST))#Need to reset to first of the month
  })
  
   # ebedmonth_reac <- reactive({
   #   subset(ebed,input$sliderEbed)
   # }) 
   
   SSTmonth_reac <- reactive({
     subset(sst,nlayer())
   })
   
   output$SSTplot <- renderLeaflet({
     
     leaflet(data=site) %>% setView(lng = 173.5, lat = -43.5, zoom = 8) %>%
       addTiles()  %>%
       addScaleBar(position = "bottomright",options = scaleBarOptions(imperial=F)) %>% 
       addMouseCoordinates() %>%
       addMarkers(site, lat = ~lat,lng = ~lon, popup = ~name) %>%
       addRasterImage(SSTmonth_reac(), col= palSST,layerId = "SSTvalues", opacity = 0.8,project=T) %>%
       addLegend(pal = palSST, values = values(SSTmonth_reac()),title = "SST (DegC)") %>%
       addPolylines(data=bathycontour,color = "black",opacity=0.2,popup = bathycontour$level) 
   }) 
   
   # output$ebedplot <- renderLeaflet({
   #   
   #   leaflet(data=site) %>% setView(lng = 173.5, lat = -43.5, zoom = 8) %>%
   #     addTiles()  %>%
   #     addScaleBar(position = "bottomright",options = scaleBarOptions(imperial=F)) %>% 
   #     addMouseCoordinates() %>%
   #     addMarkers(site, lat = ~lat,lng = ~lon, popup = ~name) %>%
   #     addRasterImage(log10(ebedmonth_reac()), col= palEbed,layerId = "Ebedvalues", opacity = 0.8,project=T) %>%
   #     #addImageQuery(ebedmonth_reac(),layerId = "Ebedvalues",type='click',project=T,position='bottomleft') %>%
   #     addLegend(pal = palEbed, values = values(log10(ebedmonth_reac())),title = "Ebed log(E/m2/d)") %>%
   #     addPolylines(data=bathycontour,color = "black",opacity=0.2,popup = bathycontour$level) 
   #   }) 
   
  # observe({
  #  #leafletProxy("ebedplot",data=ebedmonth_reac()) %>%
  #  leafletProxy("ebedplot") %>%
  #  clearImages() %>%
  #  #addRasterImage(log10(ebedmonth_reac()),col=rainbow(100), opacity = 0.8,project=T)
  #  addRasterImage(ebedmonth_reac(),col=rainbow(100),layerId = "values", opacity = 0.8,project=T) %>%
  #  addImageQuery(ebedmonth_reac(),layerId = "values",type='click',project=T,position='bottomleft')
  # })
  # --> addImageQuery doesnt work in the leafletProxy at the moment, cf google.
   
   # observe({
   #   print(input$slider)
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)

