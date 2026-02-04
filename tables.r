# Code for importing tables
# I'm hoping that this will make for easy access
#Working directory setwd("C:/Users/danny/Documents/git/Research_Methods")

## ---- Loadpackages --------

require(geomorph) #for GM analysis
require(SlicerMorphR) #for importing Slicer data
require(plyr) #for wireframe specs
require(dplyr) #for data processing/cleaning
require(tidyr) #for data processing/cleaning
require(skimr) #for nice visualization of data 
require(knitr) #for qmd building
require(gapminder) #for plot aesthetics


## ---- Table1 --------

dictionary1 <- read.csv(print("C:/Users/danny/Documents/git/Research_Methods/Book1.csv", check.names = FALSE, StringAsFactors = FALSE))

colnames(dictionary1) <- c("Species", "Time Period", "Specimens/Assemblages", "Number", "Continent")

knitr::kable(dictionary1)


## ---- Table2 --------

dictionary2 <- read.csv(print("C:/Users/danny/Documents/git/Research_Methods/landmarks.csv"))

knitr::kable(dictionary2)


## ---- Table3 --------

dictionary3 <- read.csv(print("C:/Users/danny/Documents/git/Research_Methods/clp.csv"))

knitr::kable(dictionary3)

