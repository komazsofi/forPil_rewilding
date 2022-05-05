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