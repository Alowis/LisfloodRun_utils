library(rgdal)
library(raster)




workDir<-("//ies.jrc.it/H07/nahaUsers/tilloal/ERA5l_x_lisflood")

#import discharge at gauges from EFAS calibration and compare with LISFLOOD outputs

#Step one: Import EFAS-calibartion 

#Step 2: Select relevant stations

Stations_Metadata <- read.csv(paste(workDir,"/EFAS_calib_stations_metadata.csv", sep=""))


popMap_ssp1_2010<-raster(readGDAL(paste0(workDir,"/SSP1_1km/ssp1_total_2010.tif"),silent=TRUE)) 


popMap_ssp1_2020<-raster(readGDAL(paste0(workDir,"/SSP1_1km/ssp1_total_2020.tif"),silent=TRUE)) 

p2011=(popMap_ssp1_2020-popMap_ssp1_2010)/popMap_ssp1_2010
