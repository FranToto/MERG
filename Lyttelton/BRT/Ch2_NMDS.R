#################################
# Ch2- Site species compositions stats
# NMDS 
# Mareike Babuder
# May 2020
#################################

library(vegan)
library(ggplot2)

#dataset survey BL0 + env variables
all<- read.csv("NMDS_trail_obs.csv", header= TRUE)


#bray curtis dissimilarity between Sites

# Responses (abundance)
community.data<-as.matrix(all[,2:6])

# Factors (site)
rownames(community.data)<-all[,1]# 1=Site, 3=Species

#distance matrix
alpha.matrix<-vegdist(community.data,method="bray")

#alpha.nmds<-metaMDS(alpha.matrix, distance="bray", k=3, maxit=500,trymax=500,wascores=T)
alpha.mds<-metaMDS(alpha.matrix)
#goodness(nmds)
#stressplot(nmds)
plot(alpha.mds)

# hierachical cluster analysis using hclust, d= distance matrix--> difference of alpha across species
alphaW_hc<-hclust(alpha.matrix,method="average")
plot(alphaW_hc)# matrix

#MDS plot 
#winter_alpha<-winter[2:6]

#transform into df
MDS_alpha <- as.data.frame(alpha.mds$points)

#add column for sites and orders using original dataset

MDS_alpha$region <- all$region


#plot


ggplot(MDS_alpha, aes(MDS1, MDS2, color = region)) +
  geom_point() +
  #scale_color_manual(values=)+
  geom_text(aes(x = MDS1, y = MDS2, label = region), size=4,vjust=0,hjust=0) +
  stat_ellipse()+
  theme_classic()

#interesingly the LH sites do not stand out but two sites outside the bays
