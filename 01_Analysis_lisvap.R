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


workDir<-("//ies.jrc.it/H07/")

#import discharge at gauges from EFAS calibration and compare with LISFLOOD outputs

world <- map_data("world")



ncfile= paste0(workDir,"/ClimateRun4/multi-hazard/alois/LISVAP/out_all/e0_1amin_e5l_1981_2020.nc")

ncfile2= paste0(workDir,"/ClimateRun4/multi-hazard/alois/LISVAP/et0_glofas_cut_all.nc")

opt='glofas'

if(opt=='glofas')nc=nc_open(ncfile2)
if(opt=='efas-e5l')nc=nc_open(ncfile)



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
tdims<-t$ndims
nt1<-tsize[tdims]

time <- ncvar_get(nc,"time")
time
tunits <- ncatt_get(nc,"time","units")
if(opt=='efas-e5l')timestamp <- as_datetime(c(time*60*60*24),origin="1981-01-02")
if(opt=='glofas')timestamp <- as_datetime(c(time*60*60),origin="1979-01-01")

Startdate="1981-01-01"
ids=which(as.character(timestamp)==Startdate)
# timestamp=timestamp[c(ids:length(time))]
months=month(timestamp)
dmon=diff(months)
change=which(dmon!=0)+1

years=year(timestamp)
dyr=diff(years)
change_yr=which(dyr!=0)+1


changid=which(change==ids)

if(opt=='glofas'){
  timeb<-c(ids,change[changid+1])
  change=change[c((changid+1):(length(change)))]
}
if(opt=='efas-e5l') timeb<-c(1,change[1])


name.lon="lon"
name.lat="lat"

lon=ncvar_get(nc,name.lon)
lat=ncvar_get(nc,name.lat)

# Initialize start and count to read one timestep of the variable.
start <- rep(1,tdims) # begin with start=(1,1,1,...,1)
# change to start=(1,1,1,...,i) to read timestep i
# start[1]<-lonb[1]
# start[2]<-latb[1]
count <- tsize # begin w/count=(nx,ny,nz,...,nt), reads entire var
count[1]<-length(lon)
count[2]<-length(lat)

startyr=c()
countyr=c()
for (im in 1:length(change_yr)){
  if(im>1)start[tdims]<- change_yr[im-1]
  cat(im)
  
  count[tdims] <- change_yr[im]-start[tdims] # change to count=(nx,ny,nz,...,1) to read 1 tstep
  startyr=c(startyr,start[tdims])
  countyr=c(countyr, count[tdims])
}

startcountyr=as.data.frame(cbind(startyr, countyr))
startcountyr$st=timestamp[startcountyr$startyr]
startcountyr$co=timestamp[startcountyr$startyr+startcountyr$countyr]

# Initialize start and count to read one timestep of the variable.
start <- rep(1,tdims) # begin with start=(1,1,1,...,1)
 # change to start=(1,1,1,...,i) to read timestep i
start[3]<-timeb[1]
# start[2]<-latb[1]
count <- tsize # begin w/count=(nx,ny,nz,...,nt), reads entire var
count[1]<-length(lon)
count[2]<-length(lat)

# count[1]<-2000
# count[2]<-2000
metadata=c()

startouf=c()
countouf=c()
yrmom=c()
for (im in 0:length(change)+1){
  if(im>1)start[tdims]<- change[im-1]
  cat(im)
  if (im==length(change)+1)count[tdims]=tsize[3]-start[tdims]+1
  else count[tdims] <- change[im]-start[tdims] # change to count=(nx,ny,nz,...,1) to read 1 tstep
  print(start[tdims])
  startouf=c(startouf,start[tdims])
  countouf=c(countouf, count[tdims])
  yrmom=c(yrmom, years[start[tdims]])
}

startcount=as.data.frame(cbind(startouf, countouf,yrmom))

max(timestamp)
c(timestamp[startcount$startouf[2]],timestamp[startcount$startouf[2]+startcount$countouf[2]])

tiver=ncvar_get(nc,"time",start = startcount$startouf[480], count= startcount$countouf[480]) 
tiver
length(time)
timestamp[tiver+1]

setwd(paste0(workDir,"/nahaUsers/tilloal/ERA5l_x_lisflood"))

if (opt=='efas-e5l')write.csv(startcount,'timeslices_V2.csv')
if (opt=='glofas')write.csv(startcount,'timeslices_gl.csv')




mylist=list()
for (iy in unique(startcount$yrmom)){
  chunk=startcount[which(startcount$yrmom==iy),]
  mylist=c(mylist,list(chunk))
}


metadata=as.data.frame(metadata)
plot(metadata$V1,type="h")




#om$cloc=paste0(om$Var1,"-",om$Var2)


r <- raster::raster(ncfile,band=100)
xmin<-r@extent[1]
xmax<-r@extent[2]
ymin<-r@extent[3]
ymax<-r@extent[4]
gplot(r) + 
  geom_raster(aes(fill=value),alpha=.8,interpolate = F)+
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "transparent", size = 0.1
  ) +
  coord_cartesian(xlim = c((xmin),(xmax)), ylim = c((ymin),(ymax)))+
  scale_fill_distiller(palette = "PuRd",na.value = "aliceblue",direction=1)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.key.size = unit(1, "cm"))

# rxy <- as.data.frame(r,xy=T)
# raster::plot(r, main = "ERA-5 Reanalysis Demo")
# maps::map("world", add = TRUE)

