library(raster)
require(parallel) 
require(doParallel)
require(foreach)

source("C:/_Koma/GitHub/komazsofi/forPil_rewilding/Derek_SplitRaster.R")

workingdirectory="O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/Project_1/"
setwd(workingdirectory)

filelist=list.files(pattern = "*.tif")

# read in predictors

predictors=stack(filelist)

# make small area of interest for testing

e <- extent(509566, 551074, 6322118, 6348959)
predictors_crop=crop(predictors,e)

# Split raster and convert to data frame

setwd(paste0(workingdirectory,"/","processing_formatchit","/"))

SplitRas(Raster = predictors_crop, ppside = 4, nclus = 2)
Files <- list.files(pattern = "SplitRas", full.names = T)

DF <- SplitsToDataFrame(Splits = Files, ncores = 2)

# Clean up the data frame for matching

colnames(DF)<-c("UTM_X","UTM_Y",filelist)
write.csv2(DF,"forMatching.csv")

DF_complete_cases=DF[complete.cases(DF),]




