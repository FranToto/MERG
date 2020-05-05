#################################
# Ch2- Site species compositions stats
# PERMANOVA Trail
# Mareike Babuder
# May 2020
#################################


#----------------------------------------
#Stats
AVdate<-read.csv("AV_Cover_date.csv")

#subset to 2016
AVdate16<-subset(AVdate,AVdate$ID=="BL0")

x <- unlist(AVdate16$AVCover)
h <- hist(x, breaks=10, col="red", xlab="AVCover",  main="Histogram with Normal Curve")
x <- log(x[x!=0])
h <- hist(x, breaks=10, col="red", xlab="log10(AVCover)",  main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)


# PERMANOVA (https://rpubs.com/collnell/manova)


#AVdate16$logAV<-log(AVdate16$AVCover+1)
#write.csv(AVdate16,"log_AVdate16.csv")


AVdate16<-read.csv("log_AVdate16.csv")

obs<-AVdate16[2:6]
Site<-AVdate16[1]

#runs MDS
ord <- metaMDS(obs)

#basic plot with text labels
plot(ord, type = "t")

## Fit environmental variables
ef <- envfit(ord, Site)
ef
# Fit is 1?! Something must be wrong here


#adds arrows onto plot with environmental variables   
plot(ef, p.max = 0.05)
#not working yet

#shannon index calculated for each 50 hectare plot in BCI data
diversity(obs, index = "shannon")

plots<- diversity(obs, index = "shannon") #makes an object 
summary(plots) #gives summary statistics for the plots
median(plots) #gives the median
mean(plots) #gives the mean

#Species-area and species-individual curves:

spa <- specaccum(obs)
plot(spa) #plots the species accumulation curve and the confidence intervals for sites.
plot(spa, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue") #males a prettier plot


#not working yet
spi<-specaccum(obs, method="rarefaction")
plot(spi)
plot(spa, add=TRUE, col=4) #color number 4 is blue


                 

set.seed(36) #reproducible results

species.div<-adonis(AVdate16 ~Site, data=AVdate16, permutations = 999, method="bray")
species.div
