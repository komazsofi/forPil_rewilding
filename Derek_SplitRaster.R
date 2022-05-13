SplitRas <- function(Raster,ppside, nclus = 1){
  TempRast <- paste0(getwd(), "/Temp")
  h        <- ceiling(ncol(Raster)/ppside)
  v        <- ceiling(nrow(Raster)/ppside)
  agg      <- aggregate(Raster,fact=c(h,v))
  agg[]    <- 1:ncell(agg)
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  if(nclus == 1){
    for(i in 1:ncell(agg)){
      dir.create(TempRast)
      rasterOptions(tmpdir=TempRast)
      e1          <- extent(agg_poly[agg_poly$polis==i,])
      r_list[[i]] <- crop(Raster,e1)
      if((freq(r_list[[i]], value=NA)/ncell(r_list[[i]])) != 1){
        writeRaster(r_list[[i]],filename=paste("SplitRas",i,sep=""),
                    format="GTiff",datatype="FLT4S",overwrite=TRUE)
      }
      unlink(TempRast, recursive = T, force = T)
    } 
  } else if(nclus != 1){
    cl <- parallel::makeCluster(nclus)
    doParallel::registerDoParallel(cl)
    r_list <- foreach(i = 1:ncell(agg), .packages = c("raster")) %dopar% {
      dir.create(TempRast)
      rasterOptions(tmpdir=TempRast)
      e1          <- extent(agg_poly[agg_poly$polis==i,])
      Temp <- crop(Raster,e1)
      if((raster::freq(Temp, value=NA)/ncell(Temp)) != 1){
        writeRaster(Temp,filename=paste("SplitRas",i,sep=""),
                    format="GTiff",datatype="FLT4S",overwrite=TRUE)
      }
      unlink(TempRast, recursive = T, force = T)
      Temp
    }
    parallel::stopCluster(cl)
  }
}

create_subsets <- function(inputraster, targetsize, fixed = TRUE, targetdir, targetformat = ".tif"){
  
  # read files located at the path of inputraster as a brick
  temp_rstr <- brick(inputraster)
  
  # read dimensions of read raster file
  targetsizeX <- targetsize[1]
  targetsizeY <- targetsize[2]
  
  # if the generation of tiles in a fixed dimension (fixed = TRUE) is not forced, tiles in a close approximation of the target size will be generated
  if(fixed == FALSE){
    # determine next number of quadrates in x and y direction, by simple rounding
    nsx <- round(ncol(temp_rstr) / targetsizeX)
    nsy <- round(nrow(temp_rstr) / targetsizeY)
    
    # determine quadrate size using rounded number of cells
    aggfactorX <- ncol(temp_rstr)/nsx
    aggfactorY <- nrow(temp_rstr)/nsy
    
    # aggregate calculated infromation
    agg <- aggregate(temp_rstr[[1]],c(aggfactorX, aggfactorY))
    agg[] <- 1:ncell(agg)
    agg_poly <- rasterToPolygons(agg)
    names(agg_poly) <- "polis"
  }
  
  # if the generation of tiles in a fixed dimension (fixed = TRUE) is forced, no rounding is necessary
  if(fixed == TRUE){
    # aggregate given infromation
    agg <- aggregate(temp_rstr[[1]],c(targetsizeX, targetsizeY))
    agg[] <- 1:ncell(agg)
    agg_poly <- rasterToPolygons(agg)
    names(agg_poly) <- "polis"
  }
  
  # setting up progress reports
  print("Writing subset:")
  progress_count <- 0
  
  # use aggregated information from previous step to generate and write subset tiles
  for(i in 1:ncell(agg)) {
    e1  <- extent(agg_poly[agg_poly$polis==i,])
    subs <- crop(temp_rstr,e1)
    
    temp <- floor(i / (ncell(agg) / 100))
    
    if(temp %% 10 == 0 && temp > 0 && temp > progress_count){
      # print progress of execution
      print(paste0("Progress: ", temp, "%"))
      progress_count <- progress_count + 10
    }
    
    # if fixed size is desired, check whether both dimensions of the generated tile fit target size
    if(fixed == TRUE && (dim(subs)[1] != targetsize[1]) || (dim(subs)[2] != targetsize[2])){
      # case: fixed is forced but at least one dimension not equal to target
      # --> do not generate this tile
    }
    else{
      # case 1: fixed is forced and dimensions fit
      # case 2: fixed is not forced
      # --> generate this tile
      
      # determine amount of leading zeros for correct file name
      lead <- (nchar(length(agg[])) - nchar(i))
      zeros <- paste(replicate(lead, "0"), collapse = "")
      
      # write file
      writeRaster(subs,
                  filename = paste0(targetdir,"img_", zeros, i, targetformat),
                  overwrite = TRUE)
    }
    #}
    
    # rename files to fill gaps left by skipped tiles
    #if(fixed == TRUE){
    # save current working directory and switch to targetdir to circumvent writing errors
    #current_dir <- getwd()
    #setwd(targetdir)
    
    # get number of tiles generated previously
    #n_tiles <- length(list.files(".", pattern = "*.tif"))
    
    # build vector of old file names (with gaps)
    #old_files <- list.files(".", pattern = "*.tif", full.names = TRUE)
    
    # initiate vector of new file names (no gaps)
    #new_files <- vector(length = n_tiles)
    
    # fill vector with new file names
    #for(i in 1:n_tiles){
    #lead <- (nchar(n_tiles) - nchar(i))
    #zeros <- paste(replicate(lead, "0"), collapse = "")
    #new_files[i] <- paste0("img_", zeros, i, targetformat)
    #}
    
    # rename files
    #file.rename(from = old_files, to = new_files)
    
    # reset working directory
    #setwd(current_dir)
  }
}

SplitsToDataFrame <- function(Splits, ncores = 1){
  TempRast <- paste0(getwd(), "/Temp")
  if(ncores == 1){
    Temps <- list()
    for(i in 1:length(Splits)){
      dir.create(TempRast)
      rasterOptions(tmpdir=TempRast)
      Temp <- stack(Splits[i])
      Temp <- as.data.frame(Temp, row.names = NULL, col.names = NULL, xy =TRUE)
      colnames(Temp)[3:ncol(Temp)] <- paste0("Var", 1:length(3:ncol(Temp)))
      Temps[[i]] <- Temp[complete.cases(Temp),]
      gc()
      unlink(TempRast, recursive = T, force = T)
      message(i)
    }
    Temps <- data.table::rbindlist(Temps)
  } else if(ncores > 1){
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    Temps <- foreach(i = 1:length(Splits), .packages = c("raster", "data.table")) %dopar%{
      dir.create(TempRast)
      rasterOptions(tmpdir=TempRast)
      Temp <- stack(Splits[i])
      Temp <- as.data.frame(Temp, row.names = NULL, col.names = NULL, xy =TRUE)
      colnames(Temp)[3:ncol(Temp)] <- paste0("Var", 1:length(3:ncol(Temp)))
      gc()
      unlink(TempRast, recursive = T, force = T)
      Temp[complete.cases(Temp),]
    }
    Temps <- data.table::rbindlist(Temps)
    parallel::stopCluster(cl)
  }
  return(Temps)
}