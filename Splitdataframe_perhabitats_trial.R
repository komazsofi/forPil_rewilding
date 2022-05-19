library(MatchIt)

library(parallel) 
library(doParallel)
library(foreach)

setwd("D:/Zsofia/forPil_run/processing_full_test5/")

DF_complete_cases=read.csv2("forMatching_cleaned.csv")
saveRDS(DF_complete_cases,"DF_complete_cases.rds")

habitats=unique(DF_complete_cases$Nature_types)

for (i in habitats) {
  print(i)
  
  DF_complete_cases_selhab=DF_complete_cases[DF_complete_cases$Nature_types==i,]
  
  saveRDS(DF_complete_cases_selhab,paste0("D:/Zsofia/forPil_run/processing_machit_parallel/","DF_complete_",i,".rds"))
  print(dim(DF_complete_cases_selhab))
  
}

setwd("D:/Zsofia/forPil_run/processing_machit_parallel/")

list.rds=list.files(pattern = "*.rds")

cl <- parallel::makeCluster(15)
doParallel::registerDoParallel(cl)

foreach(i=1:length(list.rds), .packages=c("MatchIt")) %dopar% {
  
  data=readRDS(list.rds[i])
  
  rewilding_match_PSM_output <- matchit(Treatment~ AMP + TWI + MDI + DTM + NDVI_before +  RZC + Slope,
                                        data=data, replace=FALSE, caliper = 0.25, method = "nearest", distance = "logit", verbose=TRUE)
  
  saveRDS(rewilding_match_PSM_output,paste0("Match_PSM_output_",list.rds[i])) 
  
}

parallel::stopCluster(cl)
