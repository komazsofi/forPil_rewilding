library(raster)
library(parallel) 
library(doParallel)
library(foreach)

library(tidyverse)
library(plyr)

library(MatchIt)

source("D:/Zsofia/github_komazsofi/forPil_rewilding/Derek_SplitRaster.R") # need to be changed to the absolute path where the file is located

workingdirectory="D:/Zsofia/forPil_run/"
setwd(workingdirectory)

filelist=list.files(pattern = "*.tif")

### read in predictors

predictors_crop=stack(filelist)

# make small area of interest for testing

#predictors=stack(filelist)
#e <- extent(585697, 610859, 6216443, 6239524)
#predictors_crop=crop(predictors,e)

### Split raster and convert to data frame

setwd(paste0(workingdirectory,"/","processing_full_test5","/"))

old <- Sys.time()

SplitRas(Raster = predictors_crop, ppside = 5, nclus = 1)
Files <- list.files(pattern = "SplitRas", full.names = T)

new <- Sys.time() - old 
print(new) 

old2 <- Sys.time()

DF_complete_cases <- SplitsToDataFrame(Splits = Files, ncores = 20)

new2 <- Sys.time() - old2 
print(new2) 

### Clean up the data frame for matching 

colnames(DF_complete_cases)<-c("UTM_X","UTM_Y",filelist)

names(DF_complete_cases)<-c("UTM_X","UTM_Y","Nature_types","Size","AMP","TWI","MDI","DTM","NDVI_before","NDVI_max",
                            "Habitat_nature","MPD","Rewilding_id","Treatment","RZC","Slope")

# scientific clean up

# rewilding or not
DF_complete_cases$Treatment[DF_complete_cases$Treatment<10]<-0
DF_complete_cases$Treatment[DF_complete_cases$Treatment==10]<-1

# add nat types with name
DF_complete_cases$Nature_types<-as.factor(DF_complete_cases$Nature_types)
DF_complete_cases$Nature_types<-revalue(DF_complete_cases$Nature_types,
                                                     c("1"="Freshwater meadow", "2"="Heathland", "3"="Bog", "4"="Grassland","5"="s","6"="Lake","7"="Coastal meadow"))

# what type of rewilding # need to put the UTF-8
DF_complete_cases$Rewilding_id<-as.factor(DF_complete_cases$Rewilding_id)

DF_complete_cases$Rewilding_id<-revalue(DF_complete_cases$Rewilding_id,
                                        c("0"="Article 3",
                                          "1"="Saksfjed-Hyllekrog", "2"="Tofte", "3"="Mellemområdet",
                                          "4"="Molslaboratoriet","5"="Klise Nor","6"="Dovns Klint",
                                          "7"="Almindingen", "8"="Bøtøskoven", "9"="Brandsø",
                                          "10"="Skavenhus", "11"="Ulsvhale Nord", "12"="Ulvshale Syd",
                                          "13"="Klelund", "14"="Geding Kasted Mose", "15"="Merritskoven",
                                          "16"="Flådet", "17"="Stengade", "18"="Tærø",
                                          "19"="Næstved Øvelsesterræn", "20"="Nørrestrand Horsens"))


write.csv2(DF_complete_cases,"forMatching_cleaned.csv")

# check dimensions
dim(DF_complete_cases)
str(DF_complete_cases)  
levels(as.factor(DF_complete_cases$Treatment))
nrow(DF_complete_cases[DF_complete_cases$Treatment==1,])
nrow(DF_complete_cases[DF_complete_cases$Treatment!=1,])

### Matching - modelling

# with NDVI

old3 <- Sys.time()

rewilding_match_PSM_output <- matchit(Treatment~Nature_types + AMP + TWI + MDI + DTM + NDVI_before + Habitat_nature + RZC + Slope,
                                      data =DF_complete_cases, replace=FALSE, caliper = 0.25, method = "nearest", exact= "Nature_types", distance = "logit",verbose=TRUE)

new3 <- Sys.time() - old3 
print(new3) 

plot(summary(rewilding_match_PSM_output))
m.data <- match.data(rewilding_match_PSM_output)

saveRDS(rewilding_match_PSM_output,"rewilding_match_PSM_output.rds") 

# without NDVI
rewilding_match_PSM_output_noNDVI <- matchit(Treatment~Nature_types + AMP + TWI + MDI + DTM + Habitat_nature + RZC + Slope,
                                      data =DF_complete_cases, replace=FALSE, caliper = 0.25, method = "nearest", exact= "Nature_types", distance = "logit",verbose=TRUE)


plot(summary(rewilding_match_PSM_output_noNDVI))
m.data <- match.data(rewilding_match_PSM_output_noNDVI)

saveRDS(rewilding_match_PSM_output_noNDVI,"rewilding_match_PSM_output_noNDVI.rds") 










