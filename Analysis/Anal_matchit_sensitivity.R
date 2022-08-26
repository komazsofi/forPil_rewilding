library(raster)
library(rgdal)
library(parallel) 
library(doParallel)
library(foreach)

library(tidyverse)
library(plyr)

library(MatchIt)

source("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/__code/forPil_rewilding/Derek_SplitRaster.R") # need to be changed to the absolute path where the file is located

shpname="mols_5km"

workingdirectory="O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/_InputLayers/"
setwd(workingdirectory)

filelist=list.files(pattern = "*.tif")

study_area=readOGR(dsn=paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/Sensitivity_analysis_ofmatchit_260822/Rewilding site cases/",shpname,".shp"))

### read in predictors for specific study area

predictors=stack(filelist)
predictors_crop=crop(predictors,study_area)

### Split raster and convert to data frame

setwd(paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/Sensitivity_analysis_ofmatchit_260822/",shpname,"/"))

old <- Sys.time()

SplitRas(Raster = predictors_crop, ppside = 5, nclus = 1)
Files <- list.files(pattern = "SplitRas", full.names = T)

new <- Sys.time() - old 
print(new) 

old2 <- Sys.time()

DF_complete_cases <- SplitsToDataFrame(Splits = Files, ncores = 3)

new2 <- Sys.time() - old2 
print(new2) 

### Clean up the data frame for matching 

colnames(DF_complete_cases)<-c("UTM_X","UTM_Y",filelist)

names(DF_complete_cases)<-c("UTM_X","UTM_Y","Size","AMP","TWI","MDI","DTM","NDVI_before","NDVI_max",
                            "Nature_types","MPD","Rewilding_id","Treatment","RZC","Slope")

names(predictors_crop)<-c("Size","AMP","TWI","MDI","DTM","NDVI_before","NDVI_max",
                            "Nature_types","MPD","Rewilding_id","Treatment","RZC","Slope")

jpeg(paste(shpname,"predictors.jpg")) 
plot(predictors_crop)
dev.off() 

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


write.csv2(DF_complete_cases,paste0("forMatching_cleaned",shpname,".csv"))

# check dimensions

sink(paste0("Metadata",shpname,".txt"))

dim(DF_complete_cases)
str(DF_complete_cases)  
levels(as.factor(DF_complete_cases$Treatment))
nrow(DF_complete_cases[DF_complete_cases$Treatment==1,])
nrow(DF_complete_cases[DF_complete_cases$Treatment!=1,])

summary(DF_complete_cases)

sink()

### Matching - modelling

# with NDVI

old3 <- Sys.time()

rewilding_match_PSM_output <- matchit(Treatment~AMP + TWI + MDI + DTM + NDVI_before + MPD + RZC + Slope,
                                      data =DF_complete_cases, replace=FALSE, caliper = 0.25, method = "nearest", exact= "Nature_types", distance = "logit",verbose=TRUE)

new3 <- Sys.time() - old3 
print(new3) 

plot(summary(rewilding_match_PSM_output))
m.data <- match.data(rewilding_match_PSM_output)

saveRDS(rewilding_match_PSM_output,paste0("rewilding_match_PSM_output_",shpname,".rds")) 

# without NDVI
rewilding_match_PSM_output_noNDVI<- matchit(Treatment~AMP + TWI + MDI + DTM +  MPD + RZC + Slope,
                                            data =DF_complete_cases, replace=FALSE, caliper = 0.25, method = "nearest", exact= "Nature_types", distance = "logit",verbose=TRUE)


plot(summary(rewilding_match_PSM_output_noNDVI))
m.data <- match.data(rewilding_match_PSM_output_noNDVI)

saveRDS(rewilding_match_PSM_output_noNDVI,paste0("rewilding_match_PSM_output_noNDVI_",shpname,".rds"))