library(rgdal)
library(raster)
library(tidyverse)

# import

asreference=raster("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/_InputLayers/dk_dtm_32_SFD_TWI_10m.tif")

# import satellite data

canopy_height=raster("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/_InputLayers/raw/canopy_height.tif")
summer_shallow=raster("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/_InputLayers/raw/Summer_predict_shallow.tif")


# align with raster

canopy_height_resampl=resample(canopy_height,asreference)
summer_shallow_resampl=resample(summer_shallow,asreference)


# export

writeRaster(canopy_height_resampl,"O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/_InputLayers/canopy_height.tif")
writeRaster(summer_shallow_resampl,"O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forPil/_InputLayers/Summer_predict_shallow.tif")

