#####################################
### CH2- PCA and CCA for species composition
## Mareike Babuder
# May 2020
#####################################


setwd("C:/Users/Mareike Babuder/Desktop/Phd/Chapter 2- Species distribution/SDM")

obs<-read.csv("NMDS_trail_obs.csv")
env<-read.csv("NMDS_trail_env.csv")

#--------------------------------------
# PCA- Principal component analysis
#--------------------------------------
library(devtools)
library(ggfortify)


df<-obs[2:6]
pca_res <- prcomp(df, scale. = TRUE)


autoplot(pca_res,data=obs,colour="region",
         loadings=T, loadings.label=T, loadings.label.size=3)

library(cluster)
autoplot(clara(obs[-1],3),frame = TRUE,frame.type = 'norm')

#--------------------------------------
# CCA- Canonical Correspondence Analysis
#--------------------------------------

library(labdsv)
df2<-env[2:10]
vare.cca <- cca(df ~ AVSST+AVKD+AVFetch+MaxSST+MaxKD+MaxSWH+SST_22+SWH_2+KD_1, data=df2)
vare.cca
plot(vare.cca)
summary(vare.cca)





#--------------------------
#example
data(varespec)
data(varechem)
vare.cca <- cca(varespec ~ Baresoil+Humdepth+pH+N+P+K+Ca+Mg+S+Al+Fe, data=varechem)
vare.cca
plot(vare.cca)
summary(vare.cca)
