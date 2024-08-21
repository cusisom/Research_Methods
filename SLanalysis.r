


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

SM.log.file = "Data/Analysis_8-15/analysis.log"
SM.log <- parser(SM.log.file)
head(SM.log)

SM.output <- read.csv(file=paste(SM.log$output.path,
SM.log$OutputData,
sep = "/"))

SlicerMorph.PCs <- read.table(file = paste(SM.log$output.path,
SM.log$pcScores,
sep="/"),
sep=",", header = TRUE, row.names=1)

PD <- SM.output[,2]
if (!SM.log$skipped) {
no.LM <- SM.log$no.LM
} else {
no.LM <- SM.log$no.LM - length(SM.log$skipped.LM)
}

dem<-read.csv(paste("Data/dem.csv", sep=""))
dem


## ---- merge -------

dat<- merge(SM.output, dem, by= "ID")
dat$ID <- as.factor(dat$ID)
dat$Sex <- as.factor(dat$Sex)
dat$Age <- as.factor(dat$Age)
dat$Ancestry <- as.factor(dat$Ancestry)

d1 <- dat %>% relocate(where(is.factor), .before = proc_dist)
skim(d1[1:6], )

## ---- BuildingArray -------

Coords <- arrayspecs(SM.output[,-c(1:3)],
p=no.LM,
k=3)

dimnames(Coords) <- list(1:no.LM,
c("x","y","z"),
SM.log$ID)

## ---- Array -------

d1array.gpa <- gpagen(Coords, print.progress=FALSE)

summary(d1array.gpa)

Mshape.Coords<-mshape(Coords)
head(Mshape.Coords)

## ---- GPA -------

d1.pca<-gm.prcomp(d1array.gpa$coords)
plot(d1.pca)


 SlicerMorph.MS <- read.table(file = paste(SM.log$output.path,
                                            SM.log$MeanShape,
                                            sep="/"),
                               sep=",", header = TRUE, row.names=1)
							   
							   
plot(d1.pca, main = "PCA by Sex",
col=dat$Sex,
pch=16
)
legend("bottomright", pch = 20, col=unique(dat$Sex), legend = unique(dat$Sex))
							   
plot(d1.pca, main = "PCA",
col=dat$Ancestry,
pch=16)
legend("bottomright", pch = 20, col=unique(dat$Ancestry), legend = unique(dat$Ancestry))

## ---- fourplot -------
par(mfrow= c(2,2))
plot(d1.pca, main = "PCA",
col=dat$Ancestry,
pch=16)
legend("topright", pch = 20, col=unique(dat$Ancestry), legend = unique(dat$Ancestry))

plot(d1.pca, main = "PCA",
	axis1=1, axis2=3,
	col=dat$Ancestry,
	pch=16
)
legend("topright", pch = 20, col=unique(dat$Ancestry), legend = unique(dat$Ancestry))

plot(d1.pca, main = "PCA",
	axis1=2, axis2=3,
	col=dat$Ancestry,
	pch=16
)
legend("topright", pch = 20, col=unique(dat$Ancestry), legend = unique(dat$Ancestry))

plot(d1.pca, main = "PCA",
	axis1=3, axis2=4,
	col=dat$Ancestry,
	pch=16
)
legend("topright", pch = 20, col=unique(dat$Ancestry), legend = unique(dat$Ancestry))

## ---- gdf -------

gdf <- geomorph.data.frame(PD,
Ancestry = dat$Ancestry,
Sex = dat$Sex,
Csize = dat$centeroid)
attributes(gdf)

lm.fit <- procD.lm(Coords~Ancestry*Sex, data=gdf)
summary(lm.fit)
plot(lm.fit)


anova(procD.lm(Coords~Csize + Ancestry*Sex, data=gdf))


plot(PD,
col=dat$Ancestry,
pch=16)
