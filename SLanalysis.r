


## setwd("C:/Users/danny/Documents/git/Research_Methods")

## ---- Loadpackages --------

require(geomorph) #for GM analysis
require(SlicerMorphR) #for importing Slicer data
require(plyr) #for wireframe specs
require(dplyr) #for data processing/cleaning
require(tidyr) #for data processing/cleaning
require(skimr) #for nice visualization of data 
require(knitr) #for qmd building
require(gapminder) #for plot aesthetics


## ---- loaddata --------

data_location1 <- "Data/Processed_data/output.rds"
d1 <- readRDS(data_location1)

data_location2 <- "Data/Processed_data/Coords.rds"
Coords <- readRDS(data_location2) 

data_location3 <- "Data/Processed_data/gpa.rds"
d1array.gpa <- readRDS(data_location3)

data_location4 <- "Data/Processed_data/SMlog.rds"
SMlog <- readRDS(data_location4)

data_location5 <- "Data/Processed_data/PD.rds"
PD <- readRDS(data_location5)



###Work in progress#### 


## ---- PCA -------

par(mar=c(1,1,1,1))

d1.pca<-gm.prcomp(d1array.gpa$coords)
d1.pca
plot(d1.pca)


 SlicerMorph.MS <- read.table(file = paste(SMlog$output.path,
                                            SMlog$MeanShape,
                                            sep="/"),
                               sep=",", header = TRUE, row.names=1)
							   
							   
plot(d1.pca, main = "PCA by Sex",
col=d1$Sex,
pch=16
)
legend("bottomright", pch = 20, col=unique(d1$Sex), legend = unique(d1$Sex))
							   
plot(d1.pca, main = "PCA",
col=d1$Ancestry,
pch=16)
legend("bottomright", pch = 20, col=unique(d1$Ancestry), legend = unique(d1$Ancestry))

## ---- fourplot -------
par(mfrow= c(2,2))
plot(d1.pca, main = "PCA",
col=d1$Ancestry,
pch=16)
legend("topright", pch = 20, col=unique(d1$Ancestry), legend = unique(d1$Ancestry))

plot(d1.pca, main = "PCA",
	axis1=1, axis2=3,
	col=d1$Ancestry,
	pch=16
)
legend("topright", pch = 20, col=unique(d1$Ancestry), legend = unique(d1$Ancestry))

plot(d1.pca, main = "PCA",
	axis1=2, axis2=3,
	col=d1$Ancestry,
	pch=16
)
legend("topright", pch = 20, col=unique(d1$Ancestry), legend = unique(d1$Ancestry))

plot(d1.pca, main = "PCA",
	axis1=3, axis2=4,
	col=d1$Ancestry,
	pch=16
)
legend("topright", pch = 20, col=unique(d1$Ancestry), legend = unique(d1$Ancestry))

## ---- gdf -------

gdf <- geomorph.data.frame(PD,
Ancestry = d1$Ancestry,
Sex = d1$Sex,
Csize = d1$centeroid)
attributes(gdf)

lm.fit <- procD.lm(Coords~Ancestry*Sex, data=gdf)
summary(lm.fit)
plot(lm.fit)


anova(procD.lm(Coords~Csize + Ancestry*Sex, data=gdf))


