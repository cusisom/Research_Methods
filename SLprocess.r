
####Data Processing#####
#######8/20/2024########

## setwd("C:/Users/danny/Documents/git/Research_Methods")

## ---- Loadpackages --------

require(geomorph) #for GM analysis
require(SlicerMorphR) #for importing Slicer data
require(plyr) #for wireframe specs
require(dplyr) #for data processing/cleaning
require(tidyr) #for data processing/cleaning
require(skimr) #for nice visualization of data 
require(knitr) #for qmd building

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



