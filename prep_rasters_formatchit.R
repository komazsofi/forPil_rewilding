library(raster)
require(parallel) 
require(doParallel)
require(foreach)

library(tidyverse)
library(plyr)

library(MatchIt)

source("C:/_Koma/GitHub/komazsofi/forPil_rewilding/Derek_SplitRaster.R")

workingdirectory="O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/Project_1/"
setwd(workingdirectory)

filelist=list.files(pattern = "*.tif")

### read in predictors

predictors=stack(filelist)

# make small area of interest for testing

e <- extent(585697, 610859, 6216443, 6239524)
predictors_crop=crop(predictors,e)

### Split raster and convert to data frame

setwd(paste0(workingdirectory,"/","processing_formatchit","/"))

SplitRas(Raster = predictors_crop, ppside = 4, nclus = 2)
Files <- list.files(pattern = "SplitRas", full.names = T)

DF <- SplitsToDataFrame(Splits = Files, ncores = 2)

### Clean up the data frame for matching

colnames(DF)<-c("UTM_X","UTM_Y",filelist)
write.csv2(DF,"forMatching.csv")

DF_complete_cases=DF[complete.cases(DF),]

# scientific clean up

# rewilding or not
DF_complete_cases$Rewilding_PA_combined_10m_utm32.tif[DF_complete_cases$Rewilding_PA_combined_10m_utm32.tif<10]<-0
DF_complete_cases$Rewilding_PA_combined_10m_utm32.tif[DF_complete_cases$Rewilding_PA_combined_10m_utm32.tif==10]<-1

# add nat types with name
DF_complete_cases$Article3_NATYPE_raster_10m_utm32.tif<-as.factor(DF_complete_cases$Article3_NATYPE_raster_10m_utm32.tif)
DF_complete_cases$Article3_NATYPE_raster_10m_utm32.tif<-revalue(DF_complete_cases$Article3_NATYPE_raster_10m_utm32.tif,
                                                     c("1"="Freshwater meadow", "2"="Heathland", "3"="Bog", "4"="Grassland","5"="s","6"="Lake","7"="Coastal meadow"))

# what type of rewilding
DF_complete_cases$Rewilding_ID_inclNA_10m_utm32.tif<-as.factor(DF_complete_cases$Rewilding_ID_inclNA_10m_utm32.tif)

DF_complete_cases$Rewilding_ID_inclNA_10m_utm32.tif<-revalue(DF_complete_cases$Rewilding_ID_inclNA_10m_utm32.tif,
                                                      c("0"="Article 3",
                                                        "1"="Saksfjed-Hyllekrog", "2"="Tofte", "3"="Mellemomr?det",
                                                        "4"="Molslaboratoriet","5"="Klise Nor","6"="Dovns Klint",
                                                        "7"="Almindingen", "8"="B?t?skoven", "9"="Brands?",
                                                        "10"="Skavenhus", "11"="Ulsvhale Nord", "12"="Ulvshale Syd",
                                                        "13"="Klelund", "14"="Geding Kasted Mose", "15"="Merritskoven",
                                                        "16"="Fl?det", "17"="Stengade", "18"="T?r?",
                                                        "19"="N?stved ?velsesterr?n", "20"="N?rrestrand Horsens"))


write.csv2(DF_complete_cases,"forMatching_cleaned.csv")

# check dimensions
dim(DF_complete_cases)
str(DF_complete_cases)  
levels(as.factor(DF_complete_cases$Rewilding_PA_combined_10m_utm32.tif))
nrow(DF_complete_cases[DF_complete_cases$Rewilding_PA_combined_10m_utm32.tif==1,])
nrow(DF_complete_cases[DF_complete_cases$Rewilding_PA_combined_10m_utm32.tif!=1,])

### Modelling









