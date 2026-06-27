
####Data Processing#####
#######06/2026########

## setwd("C:/Users/danny/Documents/git/Research_Methods")

## ---- Loadpackages --------

require(geomorph) #for GM analysis
require(SlicerMorphR) #for importing Slicer data
require(plyr) #for wireframe specs
require(dplyr) #for data processing/cleaning
require(tidyr) #for data processing/cleaning
require(skimr) #for nice visualization of data 
require(knitr) #for qmd building
require(magrittr) 

## ---- loaddata1 --------

SM.json.file = "Data/Analysis_6-9-26/analysis.json"
SMlog <- parser2(SM.json.file)
# Once you import the .json file be sure to pipe into a simple identifing name. I'm using SMlog.
# Preview the file with the following command
head(SMlog)

# Be sure to save the log file in an appropriate folder for easy access. I created a couple of nested folders to pipe my data into
save_data_location<-"Data/Processed_data/Analysis1/SMlog.rds"
saveRDS(SMlog, file=save_data_location)

## ---- loaddata2 --------
SM_output <- read.csv(file=paste(SMlog$output.path,
SMlog$OutputData,
sep = "/"))

save_data_location<-"Data/Processed_data/Analysis1/SM_output.rds"
saveRDS(SM_output, file=save_data_location)

SlicerMorph_PCs <- read.table(file = paste(SMlog$output.path,
SMlog$pcScores,
sep="/"),
sep=",", header = TRUE, row.names=1)

save_data_location<-"Data/Processed_data/Analysis1/SlicerMorph_PCs.rds"
saveRDS(SlicerMorph_PCs, file=save_data_location)

PD <- SM_output[,2]
if (!SMlog$skipped) {
no.LM <- SMlog$no.LM
} else {
no.LM <- SMlog$no.LM - length(SMlog$skipped.LM)
}

save_data_location<-"Data/Processed_data/Analysis1/PD.rds"
saveRDS(PD, file=save_data_location)


## ---- merge1 -------

# I'm changing the pipe notation in this code. Where |> exists, it was once %>%

dem<-read.csv(paste("Data/dem2.csv", sep=""))
dem

## ---- merge2 --------

dat <- merge(SM_output, dem, by.x = "Sample_name", by.y = "X.Sample_name.")
dat$Sample_name <- as.factor(dat$Sample_name)
dat$Sex <- as.factor(dat$Sex)
dat$Age <- as.factor(dat$Age)
dat$Ancestry <- as.factor(dat$Ancestry)
plot(dat$Ancestry)
plot(dat$Sex)

## ---- merge3 --------
d1 <- dat |> relocate(where(is.factor), .before = proc_dist)
skim(d1[1:6], )

save_data_location<-"Data/Processed_data/Analysis1/output.rds"
saveRDS(d1, file=save_data_location)


## ---- BuildingArray -------

Coords <- arrayspecs(SM_output[,-c(1:3)],
p=no.LM,
k=3)

# Be sure to appropriatly lable the x, y, and z axes

dimnames(Coords) <- list(1:no.LM,
c("x","y","z"),
SMlog$Sample_name)

save_data_location<-"Data/Processed_data/Analysis1/Coords.rds"
saveRDS(Coords, file=save_data_location)

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

## ---- Array -------

d1array.gpa <- gpagen(Coords, print.progress=FALSE)

save_data_location<-"Data/Processed_data/Analysis1/gpa.rds"
saveRDS(d1array.gpa, file=save_data_location)

summary(d1array.gpa)

Mshape.Coords<-mshape(Coords)


## ---- Plotpoints -------

d1Links <- "Data/d1Links.csv"
d1Links <- read.csv(d1Links)
save_data_location <- "Data/d1Links.rds"
saveRDS(d1Links, file = save_data_location)
d1rds <- "Data/d1Links.rds"
d1RD <- readRDS(d1rds)
d1RD2 <- d1RD[,2:3]




 plot(Mshape.Coords, d1RD2)


## ---- EndChunk --------

plot.coords(Mshape.Coords, d1Links[ ,2:3], points.col="blue", points.cex=1.5)
plot.coords(d1array.gpa$consensus, d1Links[,2:3], points.col="blue", points.cex=1, lines.col="black", lines.wd=3)
 

