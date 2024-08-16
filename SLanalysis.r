


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

## ---- GPA -------

d1array.gpa <- gpagen(Coords, print.progress=FALSE)

summary(d1array.gpa)

Mshape.Coords<-mshape(Coords)
head(Mshape.Coords)

d1.pca<-gm.prcomp(d1array.gpa$coords)
plot(d1.pca)


 SlicerMorph.MS <- read.table(file = paste(SM.log$output.path,
                                            SM.log$MeanShape,
                                            sep="/"),
                               sep=",", header = TRUE, row.names=1)
							   
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

anova(procD.lm(Coords~Csize + Ancestry*Sex, data=gdf))

plot(d1.pca, main = "PCA",
col=dat$Sex,
pch=16
)

## ---- wireframe -------

plot.coords <- function(A, W, points.col="black", points.cex=1, lines.col="black", lines.wd=2, bg.col=NULL, 
                        main=NULL, main.line=2, main.cex=2, legend=NULL, legend.pos="topright", legend.title="", 
                        legend.col=NULL, legend.cex=1.2, legend.lwd=2, legend.bty="n", params=NULL, add=FALSE) {
  if (!is.null(params)) {par3d(params)}
  points.col <- rep(points.col, length.out=nrow(A))
  points.cex <- rep(points.cex, length.out=nrow(A))
  lines.col <- rep(lines.col, length.out=nrow(W))
  lines.wd <- rep(lines.wd, length.out=nrow(W))
  if (!is.null(bg.col)) rgl.bg(sphere=TRUE, color=bg.col, lit=FALSE, back="fill")
  plot3d(A, type="s", col=points.col, xlab="", ylab="", zlab="", size=points.cex, aspect=FALSE, box=FALSE, axes=FALSE, add=add)
    if (!is.null(main) | !is.null(legend)) {
      if (!is.null(legend) & is.null(legend.col)) stop("must supply legend colors")
      bgplot3d({plot.new()
    if (!is.null(main)) title(main=main, line=main.line, cex.main=main.cex)
    if (!is.null(legend)) legend(legend.pos, title=legend.title, legend=legend, col=legend.col, lwd=legend.lwd, cex=legend.cex, bty=legend.bty)})}
  for (i in 1:nrow(W)) {
    segments3d(rbind(A[W[i,1],], A[W[i,2],]), lwd=lines.wd[i], col=lines.col[i])
  }
}

## ---- Plotpoints -------

d1Links <- "Data/d1Links.csv"
d1Links <- read.csv(d1Links)
save_data_location <- "Data/d1Links.rds"
saveRDS(d1Links, file = save_data_location)
d1rds <- "Data/d1Links.rds"
d1RD <- readRDS(d1rds)



 plot(Mshape.Coords, d1RD[ ,2:3])


## ---- EndChunk --------

plot.coords(Mshape.Coords, d1Links[ ,2:3], points.col="brown2", points.cex=1.5)
plot.coords(d1array.gpa$consensus, d1Links[,2:3], points.col="coral", points.cex=2, lines.col="aquamarine4", lines.wd=3)
 