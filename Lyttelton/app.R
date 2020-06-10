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
kpar_sites_tb <- as_tibble(kpar_sites) %>% mutate(Date=as.Date(Date)) %>% filter(grepl('LH', Site) | grepl('BP', Site))

sst_sites <- read.csv(file=paste0(getwd(),'/SST_Sites_S2.csv'))
sst_sites_tb <- as_tibble(sst_sites) %>% mutate(Date=as.Date(Date)) %>% filter(grepl('LH', Site) | grepl('BP', Site))

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
                tabPanel("Kd",plotlyOutput("tsPlot"),tableOutput("summary")),
                tabPanel("SST",plotlyOutput("tsPlot_sst"),tableOutput("summary_sst"))
            )
        )
    )
)


server <- function(input, output) {
    
    kpar_ts_reac <- reactive({
        kpar_sites_tb %>% filter(Date>=monthStart(input$dateRangeinput[1]) & Date<=monthStart(input$dateRangeinput[2]))
        })
    
    output$tsPlot <- renderPlotly({
        q <- ggplot(kpar_ts_reac(),aes(Date,Kpar,color=Site)) + geom_point() + geom_line() + labs(y="Kpar (/m)", x = "Date")
        ggplotly(q)
    })
    
    output$summary <- renderTable({
        summary_kpar <- kpar_ts_reac() %>% group_by(Site) %>% summarise(Mean=mean(Kpar,na.rm=T),Sd=sd(Kpar,na.rm=T),Median=median(Kpar,na.rm=T),Min=min(Kpar,na.rm=T),Max=max(Kpar,na.rm=T),Npix=sum(!is.na(Kpar)))
        summary_kpar
    })

    
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)