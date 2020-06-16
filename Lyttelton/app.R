##############################################################################
# Satellite estimates of Kd and SST in Lyttelton harbour.
# Goal : Display time series of different sites between 2 months (inputs) and calculate Mean and Max 

# Following Sat_Product_Viz.Rmd - Scenario 2 choice of pixels. 
# Values will be used by Mareike.

# FT - 09/06/2020

##############################################################################


library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)

## Loading and tidying Data
kpar_sites <- read.csv(file=paste0(getwd(),'/KparSites_S2.csv'))
kpar_sites_s3_8 <- read.csv(file=paste0(getwd(),'/Kpar_Sites_S3_Median_8pix.csv')) #S3 Median
kpar_sites_s3_16 <- read.csv(file=paste0(getwd(),'/Kpar_Sites_S3_Median_16pix.csv')) #S3 Median

kpar_sites_tb <- as_tibble(kpar_sites) %>% mutate(Date=as.Date(Date)) #%>% filter(grepl('LH', Site) | grepl('BP', Site))
kpar_sites_s3_8_tb <- as_tibble(kpar_sites_s3_8) %>% mutate(Date=as.Date(Date))#, SiteFrom = as.character(SiteFrom)) #S3 Median
kpar_sites_s3_16_tb <- as_tibble(kpar_sites_s3_16) %>% mutate(Date=as.Date(Date))#, SiteFrom = as.character(SiteFrom)) #S3 Median


sst_sites <- read.csv(file=paste0(getwd(),'/SST_Sites_S2.csv'))
sst_sites_s3_8 <- read.csv(file=paste0(getwd(),'/SST_Sites_S3_Median_8pix.csv'))
sst_sites_s3_16 <- read.csv(file=paste0(getwd(),'/SST_Sites_S3_Median_16pix.csv'))

sst_sites_tb <- as_tibble(sst_sites) %>% mutate(Date=as.Date(Date)) #%>% filter(grepl('LH', Site) | grepl('BP', Site))
sst_sites_s3_8_tb <- as_tibble(sst_sites_s3_8) %>% mutate(Date=as.Date(Date))#, SiteFrom = as.character(SiteFrom))# %>% filter(grepl('LH', Site) | grepl('BP', Site))
sst_sites_s3_16_tb <- as_tibble(sst_sites_s3_16) %>% mutate(Date=as.Date(Date))#, SiteFrom = as.character(SiteFrom))# %>% filter(grepl('LH', Site) | grepl('BP', Site))

par_sites <- read.csv(file=paste0(getwd(),'/PAR_4km_Sites.csv'))
par_sites_tb <- as_tibble(par_sites) %>% mutate(Date=as.Date(Date)) 



#Function to reset slider input date to start of the month to fit with kpar_sites$Date
monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
}

## Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("Satellite Estimated Kd and SST"),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("dateRangeinput", "Date Range", min=min(kpar_sites_tb$Date), max=max(kpar_sites_tb$Date), start=as.Date("2015-10-01"), end=as.Date("2016-01-01"),startview = "year",format = "yyyy-mm-dd", sep="") 
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Kd_S2",plotlyOutput("tsPlot"),tableOutput("summary")),
                tabPanel("Kd_S3_8pix",plotlyOutput("tsPlot_S3_8"),tableOutput("summary_S3_8")),
                tabPanel("Kd_S3_16pix",plotlyOutput("tsPlot_S3_16"),tableOutput("summary_S3_16")),
                tabPanel("SST_S2",plotlyOutput("tsPlot_sst"),tableOutput("summary_sst")),
                tabPanel("SST_S3_8pix",plotlyOutput("tsPlot_sst_S3_8"),tableOutput("summary_sst_S3_8")),
                tabPanel("SST_S3_16pix",plotlyOutput("tsPlot_sst_S3_16"),tableOutput("summary_sst_S3_16")),
                tabPanel("PAR_MO_4km",plotlyOutput("tsPlot_PAR"),tableOutput("summary_PAR"))
            )
        )
    )
)


server <- function(input, output) {
    
    kpar_ts_reac <- reactive({
        kpar_sites_tb %>% filter(Date>=monthStart(input$dateRangeinput[1]) & Date<=monthStart(input$dateRangeinput[2]))
        })
    
    output$tsPlot <- renderPlotly({
        q <- ggplot(kpar_ts_reac(),aes(Date,Kpar,color=Site)) + geom_point() + geom_line() + labs(y="Kpar (/m)", x = "Date") #S2 closest most available pixel
        ggplotly(q)
    })
    
    output$summary <- renderTable({
        summary_kpar <- kpar_ts_reac() %>% group_by(Site) %>% summarise(Mean=mean(Kpar,na.rm=T),Sd=sd(Kpar,na.rm=T),Median=median(Kpar,na.rm=T),Min=min(Kpar,na.rm=T),Max=max(Kpar,na.rm=T),Npix=sum(!is.na(Kpar)))
        summary_kpar
    })
    
    
    kpar_ts_s3_8_reac <- reactive({
        kpar_sites_s3_8_tb %>% filter(Date>=monthStart(input$dateRangeinput[1]) & Date<=monthStart(input$dateRangeinput[2]))
    })
    
    output$tsPlot_S3_8 <- renderPlotly({
        q <- ggplot(kpar_ts_s3_8_reac(),aes(Date,Median,color=SiteFrom)) + geom_point() + geom_line() + labs(y="Kpar (/m)", x = "Date") #S3 Median
        ggplotly(q)
    })
    
    output$summary_S3_8 <- renderTable({
        summary_kpar <- kpar_ts_s3_8_reac() %>% group_by(SiteFrom) %>% summarise(Mean=mean(Median,na.rm=T),Sd=sd(Median,na.rm=T),M_edian=median(Median,na.rm=T),Min=min(Median,na.rm=T),Max=max(Median,na.rm=T),Npix=sum(!is.na(Median)))
        summary_kpar
    })
    
    
    kpar_ts_s3_16_reac <- reactive({
        kpar_sites_s3_16_tb %>% filter(Date>=monthStart(input$dateRangeinput[1]) & Date<=monthStart(input$dateRangeinput[2]))
    })
    
    output$tsPlot_S3_16 <- renderPlotly({
        q <- ggplot(kpar_ts_s3_16_reac(),aes(Date,Median,color=SiteFrom)) + geom_point() + geom_line() + labs(y="Kpar (/m)", x = "Date") #S3 Median
        ggplotly(q)
    })

    output$summary_S3_16 <- renderTable({
        summary_kpar <- kpar_ts_s3_16_reac() %>% group_by(SiteFrom) %>% summarise(Mean=mean(Median,na.rm=T),Sd=sd(Median,na.rm=T),M_edian=median(Median,na.rm=T),Min=min(Median,na.rm=T),Max=max(Median,na.rm=T),Npix=sum(!is.na(Median)))
        summary_kpar
    })
    
### SST ###
    sst_ts_reac <- reactive({
        sst_sites_tb %>% filter(Date>=monthStart(input$dateRangeinput[1]) & Date<=monthStart(input$dateRangeinput[2]))
    })
    
    output$tsPlot_sst <- renderPlotly({
        q <- ggplot(sst_ts_reac(),aes(Date,SST,color=Site)) + geom_point() + geom_line() + labs(y="SST (degC)", x = "Date")
        ggplotly(q)
    })
    
    output$summary_sst <- renderTable({
        summarysst <- sst_ts_reac() %>% group_by(Site) %>% summarise(Mean=mean(SST,na.rm=T),Sd=sd(SST,na.rm=T),Median=median(SST,na.rm=T),Min=min(SST,na.rm=T),Max=max(SST,na.rm=T),Npix=sum(!is.na(SST)))
        summarysst
    })
    
    sst_ts_S3_8_reac <- reactive({
        sst_sites_s3_8_tb %>% filter(Date>=monthStart(input$dateRangeinput[1]) & Date<=monthStart(input$dateRangeinput[2]))
    })
    
    output$tsPlot_sst_S3_8 <- renderPlotly({
        q <- ggplot(sst_ts_S3_8_reac(),aes(Date,Median,color=SiteFrom)) + geom_point() + geom_line() + labs(y="SST (degC)", x = "Date")
        ggplotly(q)
    })
    
    output$summary_sst_S3_8 <- renderTable({
        summarysst <- sst_ts_S3_8_reac() %>% group_by(SiteFrom) %>% summarise(Mean=mean(Median,na.rm=T),Sd=sd(Median,na.rm=T),M_edian=median(Median,na.rm=T),Min=min(Median,na.rm=T),Max=max(Median,na.rm=T),Npix=sum(!is.na(Median)))
        summarysst
    })
    
    sst_ts_S3_16_reac <- reactive({
        sst_sites_s3_16_tb %>% filter(Date>=monthStart(input$dateRangeinput[1]) & Date<=monthStart(input$dateRangeinput[2]))
    })
    
    output$tsPlot_sst_S3_16 <- renderPlotly({
        q <- ggplot(sst_ts_S3_16_reac(),aes(Date,Median,color=SiteFrom)) + geom_point() + geom_line() + labs(y="SST (degC)", x = "Date")
        ggplotly(q)
    })
    
    output$summary_sst_S3_16 <- renderTable({
        summarysst <- sst_ts_S3_16_reac() %>% group_by(SiteFrom) %>% summarise(Mean=mean(Median,na.rm=T),Sd=sd(Median,na.rm=T),M_edian=median(Median,na.rm=T),Min=min(Median,na.rm=T),Max=max(Median,na.rm=T),Npix=sum(!is.na(Median)))
        summarysst
    })
    
### PAR ###
    par_ts_reac <- reactive({
        par_sites_tb %>% filter(Date>=monthStart(input$dateRangeinput[1]) & Date<=monthStart(input$dateRangeinput[2]))
    })
    
    output$tsPlot_PAR <- renderPlotly({
        q <- ggplot(par_ts_reac(),aes(Date,PAR,color=Site)) + geom_point() + geom_line() + labs(y="PAR (Einstein m-2 d-1)", x = "Date") 
        ggplotly(q)
    })
    
    output$summary_PAR <- renderTable({
        summary_par <- par_ts_reac() %>% group_by(Site) %>% summarise(Mean=mean(PAR,na.rm=T),Sd=sd(PAR,na.rm=T),Median=median(PAR,na.rm=T),Min=min(PAR,na.rm=T),Max=max(PAR,na.rm=T),Npix=sum(!is.na(PAR)))
        summary_par
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)