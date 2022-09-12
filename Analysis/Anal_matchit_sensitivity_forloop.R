library(raster)
library(rgdal)
library(parallel) 
library(doParallel)
library(foreach)

library(tidyverse)
library(plyr)

library(MatchIt)

#source("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/__code/forPil_rewilding/Derek_SplitRaster.R") # need to be changed to the absolute path where the file is located

shpname=c("mols_10km","mols_50km","Dovn_5km","Dovn_10km","Dovn_50km","klelund_5km","klelund_10km","klelund_50km")
#shpname=c("mols_5km")

for (i in 1:length(shpname)) {
  print(shpname[i])
  
  workingdirectory="O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/_InputLayers/"
  setwd(workingdirectory)
  
  filelist=list.files(pattern = "*.tif")
  
  study_area=readOGR(dsn=paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/Sensitivity_analysis_ofmatchit_260822/Rewilding site cases/",shpname[i],".shp"))
  
  ### read in predictors for specific study area
  
  predictors=stack(filelist)
  predictors_crop=crop(predictors,study_area)
  
  ### Split raster and convert to data frame
  
  setwd(paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/Sensitivity_analysis_ofmatchit_260822/",shpname[i],"/"))
  
  old <- Sys.time()
  
  Temp <- as.data.frame(predictors_crop, row.names = NULL, col.names = NULL, xy =TRUE)
  DF_complete_cases <- Temp[complete.cases(Temp),]
  
  new <- Sys.time() - old 
  print(new) 
  
  ### Clean up the data frame for matching 
  
  colnames(DF_complete_cases)<-c("UTM_X","UTM_Y",filelist)
  
  names(DF_complete_cases)<-c("UTM_X","UTM_Y","Nature_types","Size","canopy_height","AMP","TWI","MDI","DTM","NDVI_before","NDVI_max",
                              "MPD","Rewilding_id","Treatment","RZC","Slope","Shallow_summer_water")
  
  names(predictors_crop)<-c("Nature_types","Size","canopy_height","AMP","TWI","MDI","DTM","NDVI_before","NDVI_max",
                            "MPD","Rewilding_id","Treatment","RZC","Slope","Shallow_summer_water")
  
  jpeg(paste(shpname[i],"predictors.jpg")) 
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
                                            "1"="Saksfjed-Hyllekrog", "2"="Tofte", "3"="MellemomrÃ¥det",
                                            "4"="Molslaboratoriet","5"="Klise Nor","6"="Dovns Klint",
                                            "7"="Almindingen", "8"="BÃ¸tÃ¸skoven", "9"="BrandsÃ¸",
                                            "10"="Skavenhus", "11"="Ulsvhale Nord", "12"="Ulvshale Syd",
                                            "13"="Klelund", "14"="Geding Kasted Mose", "15"="Merritskoven",
                                            "16"="FlÃ¥det", "17"="Stengade", "18"="TÃ¦rÃ¸",
                                            "19"="NÃ¦stved ÃvelsesterrÃ¦n", "20"="NÃ¸rrestrand Horsens"))
  
  
  write.csv2(DF_complete_cases,paste0("forMatching_cleaned",shpname[i],".csv"))
  
  # check dimensions
  
  sink(paste0("Metadata",shpname[i],".txt"))
  
  print(dim(DF_complete_cases))
  print(str(DF_complete_cases))  
  print(levels(as.factor(DF_complete_cases$Treatment)))
  print(nrow(DF_complete_cases[DF_complete_cases$Treatment==1,]))
  print(nrow(DF_complete_cases[DF_complete_cases$Treatment!=1,]))
  
  print(summary(DF_complete_cases))
  
  sink()
  
  ### Matching - modelling
  
  # with NDVI
  
  old3 <- Sys.time()
  
  rewilding_match_PSM_output_wNDVI <- matchit(Treatment~AMP + TWI + MDI + DTM + NDVI_before + MPD + RZC + Slope,
                                              data =DF_complete_cases, replace=FALSE, caliper = 0.25, method = "nearest", exact= "Nature_types", distance = "logit",verbose=TRUE)
  
  new3 <- Sys.time() - old3 
  print(new3) 
  
  saveRDS(rewilding_match_PSM_output_wNDVI,paste0("rewilding_match_PSM_output_wNDVI",shpname[i],".rds")) 
  
  # without NDVI
  rewilding_match_PSM_output_noNDVI<- matchit(Treatment~AMP + TWI + MDI + DTM +  MPD + RZC + Slope,
                                              data =DF_complete_cases, replace=FALSE, caliper = 0.25, method = "nearest", exact= "Nature_types", distance = "logit",verbose=TRUE)
  
  
  saveRDS(rewilding_match_PSM_output_noNDVI,paste0("rewilding_match_PSM_output_noNDVI",shpname[i],".rds"))
  
  # with NDVI with DSM
  
  old3 <- Sys.time()
  
  rewilding_match_PSM_output_wNDVI_wDSM <- matchit(Treatment~AMP + TWI + MDI + DTM + NDVI_before + MPD + RZC + Slope+canopy_height,
                                                   data =DF_complete_cases, replace=FALSE, caliper = 0.25, method = "nearest", exact= "Nature_types", distance = "logit",verbose=TRUE)
  
  new3 <- Sys.time() - old3 
  print(new3) 
  
  saveRDS(rewilding_match_PSM_output_wNDVI_wDSM,paste0("rewilding_match_PSM_output_wNDVI_wDSM",shpname[i],".rds")) 
  
  # without NDVI with DSM
  rewilding_match_PSM_output_noNDVI_wDSM<- matchit(Treatment~AMP + TWI + MDI + DTM +  MPD + RZC + Slope+canopy_height,
                                                   data =DF_complete_cases, replace=FALSE, caliper = 0.25, method = "nearest", exact= "Nature_types", distance = "logit",verbose=TRUE)
  
  saveRDS(rewilding_match_PSM_output_noNDVI_wDSM,paste0("rewilding_match_PSM_output_noNDVI_wDSM",shpname[i],".rds"))
  
  
}