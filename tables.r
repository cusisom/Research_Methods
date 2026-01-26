# Code for importing tables
# I'm hoping that this will make for easy access
#Working directory setwd("C:/Users/danny/Documents/git/Research_Methods/Analysis")

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

data_path <- "C:/Users/danny/Documents/git/Research_Methods/docs/"

## ---- Table1 --------
dictionary1 <- read.csv(print("C:/Users/danny/Documents/git/Research_Methods/docs/Book1.csv"))
knitr::kable(dictionary1)





