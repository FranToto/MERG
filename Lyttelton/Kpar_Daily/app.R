##############################################################################
# Satellite estimates of Daily Kd in Lyttelton harbour.
# Goal : Display time series of different sites between 2 months (inputs) and calculate Mean and Max 

# Suite of Lyttelton_Collab/app. 
# Values will be used by Mareike.

# FT - 08/07/2020

##############################################################################

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)

## Loading and tidying Data
#kpar_sites_s3_16 <- read.csv(file=paste0(getwd(),'/KPAR_Daily_Sites_S3_Median_16pix.csv')) #S3 Median
kpar_ngb_median_sites_tb <- read_csv('KPAR_Daily_Sites_S3_Median_16pix.csv')


#Function to reset slider input date to start of the month to fit with kpar_sites$Date
monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
}

## Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("Satellite Estimated Kd"),
    
    # Sidebar 
    # sidebarLayout(
    #      sidebarPanel(
    #          dateRangeInput("dateRangeinput", "Date Range", min=as.Date("2015-10-01"), max=as.Date("2016-01-01"), start=as.Date("2015-10-01"), end=as.Date("2016-01-01"),startview = "year",format = "yyyy-mm-dd", sep="") 
    #      ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Daily Values",plotlyOutput("tsPlot_S3_16_day")),
                tabPanel("Weekly averaged",plotlyOutput("tsPlot_S3_16_week")),
                tabPanel("Monthly averaged",plotlyOutput("tsPlot_S3_16_month")),
                tabPanel("Pixel Available Per Month",plotlyOutput("tsPlot_S3_16_PixAv"))
            )
        )
    )
#)

server <- function(input, output) {
    
    
    ## Calculate Daily Mean
    kpar_daily_s3_tidy <- kpar_ngb_median_sites_tb %>% mutate(Day = as.Date(Date)) %>% group_by(SiteFrom,Day) %>% summarise(Kpar_day=mean(Median,na.rm=T))
    
    #q <-  ggplot(kpar_daily_s3_tidy,aes(x=Day,y=Kpar_day,color=SiteFrom)) + geom_point() + geom_line()
    #ggplotly(q)
    ##
    
    ## Calculate Monthly Mean
    #kpar_monthly_s3_tidy <- kpar_ngb_median_sites_tb %>% mutate(Month = paste0(month(as.Date(Date),label=T),year(as.Date(Date)))) %>% group_by(SiteFrom,Month) %>% summarise(Kpar_month=mean(Median,na.rm=T))
    kpar_monthly_s3_tidy <- kpar_ngb_median_sites_tb %>% mutate(Month = format(as.Date(Date), "%Y-%m")) %>% group_by(SiteFrom,Month) %>% summarise(Kpar_month=mean(Median,na.rm=T)) %>% ungroup()
    
    #q <-  ggplot(kpar_monthly_s3_tidy,aes(x=Month,y=Kpar_month,color=SiteFrom)) + geom_point() + geom_line()
    #ggplotly(q)
    ##
    
    ## Calculate Weekly Mean
    #kpar_weekly_s3_tidy <- kpar_ngb_median_sites_tb %>% mutate(Weeks = week(Date))%>% group_by(SiteFrom,Weeks) %>% summarise(Kpar_weeks=mean(Median,na.rm=T))
    kpar_weekly_s3_tidy <- kpar_ngb_median_sites_tb %>% mutate(Weeks = format(as.Date(Date), "%Y-%W")) %>% group_by(SiteFrom,Weeks) %>% summarise(Kpar_weeks=mean(Median,na.rm=T)) %>% ungroup()
    
    #q <-  ggplot(kpar_weekly_s3_tidy,aes(x=Weeks,y=Kpar_weeks,color=SiteFrom)) + geom_point() + geom_line()
    #ggplotly(q)
    ##
    
    ## Calculate Number of values per months
    kpar_s3_pixAv <- kpar_ngb_median_sites_tb %>% mutate(Month = format(as.Date(Date), "%Y-%m")) %>% group_by(SiteFrom,Month) %>% summarise(non_na_count = sum(!is.na(Median))) %>% ungroup()
    
    
    output$tsPlot_S3_16_day <- renderPlotly({
        q <-  ggplot(kpar_daily_s3_tidy,aes(x=Day,y=Kpar_day,color=SiteFrom)) + geom_point() + geom_line()
        ggplotly(q)
    })
    
    output$tsPlot_S3_16_week <- renderPlotly({
        q <-  ggplot(kpar_weekly_s3_tidy,aes(x=Weeks,y=Kpar_weeks,group=SiteFrom,color=SiteFrom)) + geom_point() + geom_line()
        ggplotly(q)
    })
    
    output$tsPlot_S3_16_month <- renderPlotly({
        q <-  ggplot(kpar_monthly_s3_tidy,aes(x=Month,y=Kpar_month,group=SiteFrom,color=SiteFrom)) + geom_point() + geom_line()
        ggplotly(q)
    })
    
    output$tsPlot_S3_16_PixAv <- renderPlotly({
        q <-  ggplot(kpar_s3_pixAv,aes(x=Month,y=non_na_count,group=SiteFrom,color=SiteFrom)) + geom_point() + geom_line()
        ggplotly(q)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)