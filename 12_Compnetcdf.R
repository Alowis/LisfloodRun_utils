library(rgdal)
library(raster)
library(ncdf4)
library(lubridate)
library(ggplot2)
library(rasterVis)

#In that file I am going to compare every input of glofas with goncalo's ones
workDir<-("//ies.jrc.it/H07/")

#import discharge at gauges from EFAS calibration and compare with LISFLOOD outputs

var= c("rn","ta","td","rgd","rg")
var2="et0"
name.lon="lon"
name.lat="lat"
for (vr in var2){
  
  if(vr!="et0"){
    ncfile_1= paste0(workDir,"/nahaUsers/tilloal/ERA5l_x_lisflood/variable_tests/compar/",vr,".nc")
    ncfile_2= paste0(workDir,"/nahaUsers/tilloal/ERA5l_x_lisflood/variable_tests/compar/e5ld_1min_lvap_",vr,"_1999.nc")
  }
  if(vr=="et0"){
    ncfile_1= paste0(workDir,"/nahaUsers/tilloal/ERA5l_x_lisflood/variable_tests/compar/",vr,"_1amin_1990_ws.nc")
    ncfile_2= paste0(workDir,"/nahaUsers/tilloal/ERA5l_x_lisflood/variable_tests/compar/",vr,"_1990_stef.nc")
  }
  nc0=nc_open(ncfile_1)
  nc0
  na=names(nc0[['var']])
  if(length(na)==1)
    {name.var=names(nc0$var)[1]
    t=nc0$var[[1]]}else{
    name.var=names(nc0$var)[3]
    t=nc0$var[[3]]}
  
  tsize<-t$varsize
  time <- ncvar_get(nc0,"time")
  t$units
  tunits <- ncatt_get(nc0,"time","units")
  diff(time)
  tunits2 <- as.character(tunits[2])
  tt=unlist(strsplit(tunits2, "[ ]"))
  timestamp <- as_datetime(c((time)*60*60*24),origin=tt[3])
  months=month(timestamp)
  
  
  lon=ncvar_get(nc0,name.lon)
  lat=ncvar_get(nc0,name.lat)
  #lon=lon-0.025
  #lat=lat+0.025
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
  
  dep=1
  start[3]=dep
  dur=length(nc0$dim$time$vals)
  if (dur<60){
  count[3]=length(nc0$dim$time$vals)
  }else{ count[3]=60}
  
  if(vr!="et0"){
  #cut the area to get the EFAS extent
    diff(lon)
    min(lon)
    efas_lat=c(22.15,72.25)
    efas_lon=c(-25.25, 50.35)
    
    time2=seq(dep,dep+364)
    
    lolon=c(which(lon==efas_lon[1]),which(lon==efas_lon[2]))
    lolat=c(which(lat==efas_lat[1]),which(lat==efas_lat[2]))
    count1=lolon[2]-lolon[1]
    count2=lolat[1]-lolat[2]
    
    start[c(1,2)]=c(lolon[1],lolat[2])
    
    count[c(1,2)]=c(count1,count2)
  }
  
  v0=list()
  v0$data   = ncvar_get(nc0,name.var,start = start, count= count)   
  v0$lon    = ncvar_get(nc0,name.lon,start = start[1], count= count[1]) 
  v0$lat    = ncvar_get(nc0,name.lat,start = start[2], count= count[2]) 
  
  
  
  nc1=nc_open(ncfile_2)
  nc1
  
  names(nc1[['var']])
  nat=names(nc1[['var']])
  if(length(nat)==1)
  {name.var=names(nc1$var)[1]
  t1=nc1$var[[1]]}else{
    name.var=names(nc1$var)[2]
    t1=nc1$var[[2]]}

  tsize<-t1$varsize
  time <- ncvar_get(nc1,"time")
  t$units
  tunits <- ncatt_get(nc1,"time","units")
  diff(time)
  tunits2 <- as.character(tunits[2])
  tt=unlist(strsplit(tunits2, "[ ]"))
  timestamp1 <- as_datetime(c((time)*60*60*24),origin=tt[3])
  months1=month(timestamp1)
  
  
  
  lon=ncvar_get(nc1,name.lon)
  lat=ncvar_get(nc1,name.lat)
  #lon=lon-0.025
  #lat=lat+0.025
  #lon=round(lon,2)
  #lat=round(lat,2)
  
  # Initialize start and count to read one timestep of the variable.
  start <- rep(1,3) # begin with start=(1,1,1,...,1)
  # change to start=(1,1,1,...,i) to read timestep i
  # start[1]<-lonb[1]
  # start[2]<-latb[1]
  count <- tsize # begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[1]<-length(lon)
  count[2]<-length(lat)
  
  dep=which(timestamp1==timestamp[1])
  start[3]=dep
  dur=length(nc0$dim$time$vals)
  if (dur<60){
    count[3]=length(nc0$dim$time$vals)
  }else{ count[3]=60}
  #cut the area to get the EFAS extent
  
  diff(lon)
  min(lon)

  
  # lolon=c(which(lon==efas_lon[1]),which(lon==efas_lon[2]))
  # lolat=c(which(lat==efas_lat[1]),which(lat==efas_lat[2]))
  # count1=lolon[2]-lolon[1]
  # count2=lolat[1]-lolat[2]
  # 
  # start[c(1,2)]=c(lolon[1],lolat[2])
  # 
  # count[c(1,2)]=c(count1,count2)
  # 
  v1=list()
  v1$data   = ncvar_get(nc1,name.var,start = start, count= count)   
  v1$lon    = ncvar_get(nc1,name.lon,start = start[1], count= count[1]) 
  v1$lat    = ncvar_get(nc1,name.lat,start = start[2], count= count[2]) 
  
  m0=c()
  m1=c()
  
  dd=count[3]
  for (ti in 1:dd)
  {
    v0t=as.vector(v0$data[,,ti])
    
    m0=c(m0,mean(na.omit(v0t)))
    
    v1t=as.vector(v1$data[,,ti])
    
    m1=c(m1,mean(na.omit(v1t)))
  }
  
  time=c(timestamp[1:dd])
  lt=length(time)
  if(min(m0)<0)plot(time,m0,type="o",col="blue",ylim=c(1.4*min(m0),0.6*max(m0)))
  if(min(m0)>0)plot(time,m0,type="o",col="blue",ylim=c(0.6*min(m0),1.4*max(m0)))
  points(time,m1,type="o",col="red")
  points(time[2:lt],m1[1:lt-1],type="o",col="green")
  points(time[1:lt-1],m1[2:lt],type="o",col="black")
  
  
  c1=cor.test(m0,m1,method="pearson",alternative="greater")
  print(paste0("no change to my data, correlation= ",c1$estimate))
  # +1 day
  c2=cor.test(m0[2:(lt-1)],m1[1:(lt-2)],alternative="greater")
  print(paste0("+1 day at my data, correlation= ",c2$estimate))
  #-1day
  c3=cor.test(m0[2:(lt-1)],m1[3:lt],alternative="greater")
  print(paste0("-1 day at my data, correlation= ",c3$estimate))
  
  if (max(c1$estimate,c2$estimate,c3$estimate)==c3$estimate) print("shift my value of -1 day to be aligned")
  if (max(c1$estimate,c2$estimate,c3$estimate)==c2$estimate) print("shift my value of +1 day to be aligned")
  if (max(c1$estimate,c2$estimate,c3$estimate)==c1$estimate) print("already aligned")
  
  a=mean(abs((m1-m0)/mean(m0)))
  plot((m1-m0)/m0,ylim=c(-1,1),type="o")
  points((m1[1:(lt-2)]-m0[2:(lt-1)])/m0[2:(lt-1)],type="o",col="red")
  b=mean(abs((m1[1:(lt-2)]-m0[2:(lt-1)])/mean(m0[2:(lt-1)])))
  print(a)
  print(b)
}
