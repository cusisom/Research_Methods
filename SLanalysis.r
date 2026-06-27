


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

data_location1 <- "Data/Processed_data/Analysis1/output.rds"
d1 <- readRDS(data_location1)

data_location2 <- "Data/Processed_data/Analysis1/Coords.rds"
Coords <- readRDS(data_location2) 

data_location3 <- "Data/Processed_data/Analysis1/gpa.rds"
d1array.gpa <- readRDS(data_location3)

data_location4 <- "Data/Processed_data/Analysis1/SMlog.rds"
SMlog <- readRDS(data_location4)

data_location5 <- "Data/Processed_data/Analysis1/PD.rds"
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
							   
plot(d1.pca, main = "PCA by Ancestry",
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


plot(d1.pca, main = "PCA",
	axis1=2, axis2=3,
	col=d1$Ancestry,
	pch=16
)


plot(d1.pca, main = "PCA",
	axis1=3, axis2=4,
	col=d1$Ancestry,
	pch=16
)


## ---- gdf -------

gdf <- geomorph.data.frame(PD,
Ancestry = d1$Ancestry,
Sex = d1$Sex,
Csize = d1$centroid)
attributes(gdf)

lm.fit <- procD.lm(Coords~Ancestry*Sex, data=gdf)
summary(lm.fit)



anova(procD.lm(Coords~Csize + Ancestry*Sex, data=gdf))

## ---- Procrustes ANOVA Model Analysis -------

# 1. Build the data frame (Explicitly mapping Coords = PD)
gdf <- geomorph.data.frame(
  Coords = PD,
  Ancestry = d1$Ancestry,
  Sex = d1$Sex,
  Csize = d1$centroid
)

# 2. Fit the primary interaction model 
lm.fit <- procD.lm(Coords ~ Ancestry * Sex, data = gdf)

# 3. Fit the Allometry + Interaction model
lm.allometry <- procD.lm(Coords ~ Csize + Ancestry * Sex, data = gdf)


# ==========================================
# POLISH TABLE 1: Baseline Interaction Model
# ==========================================
anova_fit_raw <- anova(lm.fit)
anova_fit_df  <- as.data.frame(anova_fit_raw$table)
anova_fit_df[is.na(anova_fit_df)] <- "" # Strip NA spaces

colnames(anova_fit_df) <- c("Df", "SS", "MS", "Rsq", "F", "Z (Effect Size)", "P-Value")

knitr::kable(
  anova_fit_df,
  caption = "Procrustes ANOVA: Shape Variation Accounted for by Ancestry and Sex Interaction",
  digits = c(0, 4, 4, 4, 3, 3, 4), 
  align = "rcccccc"
)


# ==========================================
# POLISH TABLE 2: Size (Allometry) Added Model
# ==========================================
anova_allo_raw <- anova(lm.allometry)
anova_allo_df  <- as.data.frame(anova_allo_raw$table)
anova_allo_df[is.na(anova_allo_df)] <- "" # Strip NA spaces

colnames(anova_allo_df) <- c("Df", "SS", "MS", "Rsq", "F", "Z (Effect Size)", "P-Value")

knitr::kable(
  anova_allo_df,
  caption = "Procrustes ANOVA: Shape Variation Accounted for by Size (Allometry), Ancestry, and Sex",
  digits = c(0, 4, 4, 4, 3, 3, 4), 
  align = "rcccccc"
)



## ---- CV -------
library(cvequality)
# 1. Run the Feltz & Miller Asymptotic Test
cv_test_sex_asymp <- cvequality::asymptotic_test(x = gdf$Csize, y = gdf$Sex)
print(cv_test_sex_asymp)

# 2. Run the Modified Signed-Likelihood Ratio Test (MSLR)
cv_test_sex_mslr <- cvequality::mslr_test(x = gdf$Csize, y = gdf$Sex)
print(cv_test_sex_mslr)

## What to look for: Both tests output a p-value. If either p-value falls under 0.05, the CV for centroid size is significantly different between the two groups.


## ---- Morphol.disparity ------

# 1. Build the data frame using PD directly for coords
gdf <- geomorph.data.frame(coords = PD,
                           Ancestry = d1$Ancestry,
                           Sex = d1$Sex,
                           Csize = d1$centroid)

# 2. FIX: Capture and hide the calculation progress text completely
invisible(
  capture.output(
    shape_disp_ancestry <- morphol.disparity(coords ~ Ancestry, 
                                             groups = ~ Ancestry, 
                                             data = gdf, 
                                             iter = 999)
  )
)

# 3. CREATE TABLE 1: Procrustes Variances per Ancestry Group
disp_variance_df <- data.frame(
  Ancestry = names(shape_disp_ancestry$Procrustes.var),
  `Procrustes Variance` = as.numeric(shape_disp_ancestry$Procrustes.var)
)

knitr::kable(
  disp_variance_df, 
  caption = "Morphological Disparity (Procrustes Variance) by Ancestry Group",
  digits = 6,
  align = "lc"
)

# 4. CREATE TABLE 2: Pairwise P-Values Matrix (Cleaned Academic Triangle)
disp_p_matrix <- shape_disp_ancestry$PV.dist.Pval

# Safe check to ensure the matrix cleared out properly before rendering
if(!is.null(disp_p_matrix)) {
  disp_p_matrix[upper.tri(disp_p_matrix, diag = TRUE)] <- ""
  disp_p_table <- as.data.frame(disp_p_matrix)
  
  knitr::kable(
    disp_p_table, 
    caption = "Pairwise P-Values for Differences in Morphological Disparity Across Ancestries (Alpha = 0.05)",
    align = "c"
  )
}



# ---- Morphological Disparity by Ancestry -------

# 1. FIX: Capture and hide the calculation progress text completely
invisible(
  capture.output(
    ancestry_variance <- morphol.disparity(
      Coords ~ Ancestry, 
      groups = ~ Ancestry, 
      data = gdf,
      iter = 999
    )
  )
)

# 2. Extract the variance vector
ancestry_vars <- ancestry_variance$Procrustes.var

# 3. Generate the barplot for Ancestry
par(mar = c(8, 4, 3, 1)) 
barplot(
  ancestry_vars, 
  col = "tomato",          
  las = 2,                 
  main = "Morphological Disparity by Ancestry",
  ylab = "Procrustes Variance"
)



# ---- Morphological Disparity Analysis & Plot -------

# 1. FIX: Capture and hide the calculation progress text completely
invisible(
  capture.output(
    morph_variance <- morphol.disparity(
      Coords ~ Ancestry * Sex, 
      groups = ~ interaction(Ancestry, Sex), 
      data = gdf,
      iter = 999
    )
  )
)

# 2. View the complete statistics table in your console (or hide this in Quarto if needed)
summary(morph_variance)

# 3. Extract the variance vector
group_variances <- morph_variance$Procrustes.var

# 4. Generate your barplot directly without assigning manual names
par(mar = c(10, 4, 3, 1)) 
barplot(
  group_variances, 
  col = "skyblue", 
  las = 2,                 
  main = "Morphological Disparity by Group",
  ylab = "Procrustes Variance"
)


# ---- Format Disparity Tables for Quarto -------

# 1. Capture the console summary text to hide the progress statement
invisible(capture.output(summary_table <- summary(ancestry_variance)))

# 2. CREATE TABLE 1: Procrustes Variances per Ancestry Group
variance_df <- data.frame(
  Ancestry = names(ancestry_variance$Procrustes.var),
  `Procrustes Variance` = as.numeric(ancestry_variance$Procrustes.var)
)

knitr::kable(
  variance_df, 
  caption = "Morphological Disparity (Procrustes Variance) by Ancestry Group",
  digits = 6,
  align = "lc"
)

# 3. CRITICAL FIX: Pull the pairwise P-values matrix straight from the source object
p_matrix <- ancestry_variance$PV.dist.Pval

# 4. Clean up the matrix layout for markdown rendering
# This drops the redundant duplicate upper triangle safely
p_matrix[upper.tri(p_matrix, diag = TRUE)] <- ""
p_table_clean <- as.data.frame(p_matrix)

# 5. CREATE TABLE 2: Pairwise P-Values Matrix
knitr::kable(
  p_table_clean, 
  caption = "Pairwise P-Values for Differences in Morphological Disparity (Alpha = 0.05)",
  align = "c"
)
