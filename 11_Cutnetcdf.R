library(rgdal)
library(raster)
library(ncdf4)
library(lubridate)
library(ggplot2)
library(rasterVis)
#The aim of that script is to compute yearly statistics for e0, et and es maps from LISVAP
#First the statistics will be computed at the pixel level for each year
#The statistics could be computed by catchment using the cutmap utility of LISFLOOD

#Stats:

#-mean-median-Q2.5-Q97.5-std

history = 'Created Nov 2021' #####
Conventions = 'CF-1.6'
Source_Software = 'R netCDF4'
reference = 'A global daily high-resolution gridded meteorological data set for 1979-2019'  #####
title = 'Lisflood meteo maps 1981 for EUROPE setting Nov. 2021'
keywords = 'Lisflood, Global'
source = 'ERA5-land'
institution = 'European Commission - Economics of climate change Unit (JRC.C.6) : https://ec.europa.eu/jrc/en/research-topic/climate-change'
comment = 'The timestamp marks the end of the aggregation interval for a given map.'



workDir<-("//ies.jrc.it/H07/")

#import discharge at gauges from EFAS calibration and compare with LISFLOOD outputs


ncfile_obj= paste0(workDir,"/nahaUsers/tilloal/ERA5l_x_lisflood/variable_tests/rn.nc")

ncfile_cre= paste0(workDir,"/nahaUsers/tilloal/ERA5l_x_lisflood/variable_tests/et0_1981_2019_glofas.nc")

ncfile_st= paste0(workDir,"/nahaUsers/grimast/GloFAS3arcmin_evapo_fromSSRD/eT0_fromSSRD.nc")

nc0=nc_open(ncfile_obj)
nc0
nc1=nc_open(ncfile_cre)
nc1

# nc2=nc_open(ncfile_st)
# nc2


ncfile2= paste0(workDir,"/nahaUsers/grimast/GloFAS_forcings_ERA5_dailyBILint_3arcmin/t2dBIL_sf01z4_1979_2019.nc")

ncfile2= paste0(workDir,"/nahaUsers/grimast/GloFAS_forcings_ERA5_dailyBILint_3arcmin/ta2m_dailyERA5_1979_2019_v2.nc")


ncfile2= paste0(workDir,"/nahaUsers/grimast/GloFAS_forcings_ERA5_dailyBILint_3arcmin/ssrBIL_sf104z4_1979_2019.nc")

ncfile2= paste0(workDir,"/nahaUsers/grimast/GloFAS_forcings_ERA5_dailyBILint_3arcmin/u10BIL_sf01z4_1979_2019.nc")
#ncfile2= paste0(workDir,"/nahaUsers/grimast/GloFAS3arcmin_evapo_fromSSRD/eT0_fromSSRD.nc")

nc=nc_open(ncfile_st)

nv=names(nc[['var']])
etv=c("ET0","et0","eT0")
varid=which(!is.na(match(nv,etv)))
if (length(varid>0))
{
  print(nv[varid])
  t=nc$var[[varid]]
  name.var=names(nc$var)[varid]
}else{
  print("do a manual selection of the variables")
  print(nv)
}

tsize<-t$varsize
time <- ncvar_get(nc,"time")
t$units
tunits <- ncatt_get(nc,"time","units")
diff(time)
tunits2 <- as.character(tunits[2])
tt=unlist(strsplit(tunits2, "[ ]"))
timestamp <- as_datetime(c((time-time[1]+24)*60*60),origin=tt[3])
months=month(timestamp)
plot(timestamp)
longname=t$longname

name.lon="lon"
name.lat="lat"


lon=ncvar_get(nc,name.lon)
lat=ncvar_get(nc,name.lat)
lon=lon-0.025
lat=lat+0.025
lon=round(lon,2)
lat=round(lat,2)

# Initialize start and count to read one timestep of the variable.
start <- rep(1,3) # begin with start=(1,1,1,...,1)
# change to start=(1,1,1,...,i) to read timestep i
# start[1]<-lonb[1]
# start[2]<-latb[1]
count <- tsize # begin w/count=(nx,ny,nz,...,nt), reads entire var
count[1]<-length(lon)
count[2]<-length(lat)

dep=which(as.character(timestamp)==("1981-01-01"))
start[3]=dep

end=which(as.character(timestamp)==("2019-12-31"))
count[3]=end-dep
#cut the area to get the EFAS extent

diff(lon)
min(lon)
efas_lat=c(22.15,72.25)
efas_lon=c(-25.25, 50.35)

time2=seq(dep,end)

lolon=c(which(lon==efas_lon[1]),which(lon==efas_lon[2]))
lolat=c(which(lat==efas_lat[1]),which(lat==efas_lat[2]))
count1=lolon[2]-lolon[1]
count2=lolat[1]-lolat[2]

start[c(1,2)]=c(lolon[1],lolat[2])

count[c(1,2)]=c(count1,count2)



evapmap=list()
evapmap$data   = ncvar_get(nc,name.var,start = start, count= count)   
evapmap$lon    = ncvar_get(nc,name.lon,start = start[1], count= count[1]) 
evapmap$lat    = ncvar_get(nc,name.lat,start = start[2], count= count[2]) 



#now I create the netcdf---------------Needs to be finished


# path and file name, set dname
ncpath <- paste0(workDir,"/nahaUsers/tilloal/ERA5l_x_lisflood/variable_tests/")
ncname <- paste0(name.var,"_glofas_1990")  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- name.var  # note: tmp means temperature (not temporary)


# create and write the netCDF file -- ncdf4 version
# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(evapmap$lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(evapmap$lat)) 
timedim <- ncdim_def("time",tunits2,as.double(time2)*24)

arret0=as.array(evapmap$data)
# define variables
fillvalue <- -9999
dlname <- longname
tmp_def <- ncvar_def(name.var,t$units,list(londim,latdim,timedim),fillvalue,dlname,prec="double",compression=4)

# create netCDF file and put arrays
ncout <- nc_create(ncfname,list(tmp_def),force_v4=TRUE)

# put variables
ncvar_put(ncout,tmp_def,arret0)


# put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")


# add global attributes
ncatt_put(ncout,0,"title",title)
ncatt_put(ncout,0,"institution",institution)
ncatt_put(ncout,0,"source",source)
ncatt_put(ncout,0,"references",reference)
history <- paste("A.M. Tilloy", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"Conventions",Conventions)

# Get a summary of the created file:
ncout

nc_close(ncout)

