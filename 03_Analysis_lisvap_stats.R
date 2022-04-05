#workDir<-("//ies.jrc.it/H07/nahaUsers/tilloal/ERA5l_x_lisflood")

source("Config_gen.R")

yearlist=seq(1981,2019)
vars=c("et0","es","e0")
monthlist=c("Jan","Fev","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


#The aim of that script is to compute yearly statistics for e0, et and es maps from LISVAP
#First the statistics will be computed at the pixel level for each year
#The statistics could be computed by catchment using the cutmap utility of LISFLOOD
#Stats:

#-mean-median-Q2.5-Q97.5-std



#Read metadata created from HPC script


#select variable
vx=vars[1]

#read yearly files with monthly metadata
totalishit=c()
for (iyr in 1:39){
  yrx=yearlist[iyr]
  timeslices=read.csv(paste0(workDir,"/lisvap_ana/",vx,"/metadata_yr",yrx,"_",vx,".csv"))
  timeslices$month=c(1:12)
  timeslices$year=rep(yrx,12)
  
  totalishit=rbind(totalishit,timeslices)
  
}
plot(totalishit$month,totalishit$mean,col=totalishit$year)


#initialize result matrices
q025=matrix(nrow=12,ncol=4)
q975=matrix(nrow=12,ncol=4)
median=matrix(nrow=12,ncol=4)
mean=matrix(nrow=12,ncol=4)

# I also compute the slope of change for each month
replots=list()
slope=c()
for (imo in 1:12){
  tsm=totalishit[which(totalishit$month==imo),]
  reg <- lm(mean ~ year, data = tsm)
  slope=rbind(slope,summary(reg)$coefficients[2,])
  print(summary(reg)$r.squared)
  label=monthlist[imo]
  replots[[imo]]=ggplot(tsm, aes(year, mean)) +
    geom_point() +
    stat_smooth(method = lm)+
    ggtitle(label)+
    theme_bw()
  q025[imo,]=c(quantile(tsm[,4],0.025),quantile(tsm[,4],0.975),median(tsm[,4]),imo)
  q975[imo,]=c(quantile(tsm[,3],0.025),quantile(tsm[,3],0.975),median(tsm[,3]),imo)
  mean[imo,]=c(quantile(tsm[,1],0.025),quantile(tsm[,1],0.975),median(tsm[,1]),imo)
  median[imo,]=c(quantile(tsm[,2],0.975),quantile(tsm[,2],0.975),median(tsm[,2]),imo)
  
}

replots[[8]]

q025=as.data.frame(q025)
names(q025)=c("q025","q975","median","month")

q975=as.data.frame(q975)
names(q975)=c("q025","q975","median","month")

median=as.data.frame(median)
names(median)=c("q025","q975","median","month")

mean=as.data.frame(mean)
names(mean)=c("q025","q975","median","month")

#final plot showing monthly vaslues for the 40yrs of record
ggplot(mean, aes(x = factor(month) , y = median, group = 1)) + 
  ggtitle(paste0( "mean ",vx," for the period",totalishit$year[1], "-",totalishit$year[468], " with E5-land in the EFAS domain")) +
  geom_line(data=totalishit, aes(x=factor(month),y=mean,group=year),col="lightblue",alpha=0.7)+
  geom_line(col='royalblue',lwd=1) + 
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.2,linetype=2,fill="grey60",color="darkblue")+
  theme_bw()+
  labs(y = "monthly potential evapotranspiration (mm/day) ",x = "Months")+
  scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Fev","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

  
  ###
  geom_line(data=totalishit, aes(x=factor(month),y=Q975,group=year),col="coral1",alpha=0.7)+
  geom_line(data=q975, aes(x = factor(month) , y = median, group = 1), col='firebrick',lwd=1) +
  geom_ribbon(data=q975,aes(ymin = q025, ymax = q975), alpha = 0.2,linetype=2,fill="coral",color="coral4")+
  
  geom_line(data=totalishit, aes(x=factor(month),y=Q025,group=year),col="darkolivegreen1",alpha=0.7)+
  geom_line(data=q025, aes(x = factor(month) , y = median, group = 1), col='darkolivegreen',lwd=1) +
  geom_ribbon(data=q025,aes(ymin = q025, ymax = q975), alpha = 0.2,linetype=2,fill="darkolivegreen1",color="darkolivegreen3")
  
##################Same process with GLOFAS EVT################
  
  workDir<-("//ies.jrc.it/H07/nahaUsers/tilloal/ERA5l_x_lisflood")
  yearlist=seq(1981,2019)
  vars=c("et0","es","e0")
  monthlist=c("Jan","Fev","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  vx=vars[1]
  totalishit_glo=c()
  for (iyr in 1:39){
    yrx=yearlist[iyr]
    timeslices=read.csv(paste0(workDir,"/lisvap_ana/",vx,"_glo/metadata_yr",yrx,"_",vx,".csv"))
    timeslices$month=c(1:12)
    timeslices$year=rep(yrx,12)
    
    totalishit_glo=rbind(totalishit_glo,timeslices)
    
  }
  plot(totalishit_glo$month,totalishit_glo$mean,col=totalishit_glo$year)
  
  q025_g=matrix(nrow=12,ncol=4)
  q975_g=matrix(nrow=12,ncol=4)
  median_g=matrix(nrow=12,ncol=4)
  mean_g=matrix(nrow=12,ncol=4)
  replots=list()
  slope=c()
  for (imo in 1:12){
    tsm=totalishit_glo[which(totalishit_glo$month==imo),]
    reg <- lm(mean ~ year, data = tsm)
    slope=rbind(slope,summary(reg)$coefficients[2,])
    print(summary(reg)$r.squared)
    label=monthlist[imo]
    replots[[imo]]=ggplot(tsm, aes(year, mean)) +
      geom_point() +
      stat_smooth(method = lm)+
      ggtitle(label)+
      theme_bw()
    q025_g[imo,]=c(quantile(tsm[,4],0.025),quantile(tsm[,4],0.975),median(tsm[,4]),imo)
    q975_g[imo,]=c(quantile(tsm[,3],0.025),quantile(tsm[,3],0.975),median(tsm[,3]),imo)
    mean_g[imo,]=c(quantile(tsm[,1],0.025),quantile(tsm[,1],0.975),median(tsm[,1]),imo)
    median_g[imo,]=c(quantile(tsm[,2],0.975),quantile(tsm[,2],0.975),median(tsm[,2]),imo)
    
  }
  
  replots[[8]]
  
  q025_g=as.data.frame(q025_g)
  names(q025_g)=c("q025","q975","median","month")
  
  q975_g=as.data.frame(q975_g)
  names(q975_g)=c("q025","q975","median","month")
  
  median_g=as.data.frame(median_g)
  names(median_g)=c("q025","q975","median","month")
  
  mean_g=as.data.frame(mean_g)
  names(mean_g)=c("q025","q975","median","month")
  
  
  
  ggplot(mean, aes(x = factor(month) , y = median, group = 1)) + 
    ggtitle(paste0( "mean ",vx," for the period",totalishit$year[1], "-",totalishit$year[468], " with E5-land in the EFAS domain")) +
    geom_line(data=totalishit, aes(x=factor(month),y=mean,group=year),col="lightblue",alpha=0.7)+
    geom_line(col='royalblue',lwd=1) + 
    geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.2,linetype=2,fill="grey60",color="darkblue")+
    theme_bw()+
    labs(y = "monthly potential open water evaporation (mm/day) ",x = "Months")+
    scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Fev","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
    
    geom_line(data=totalishit_glo, aes(x=factor(month),y=mean,group=year),col="coral1",alpha=0.7)+
    geom_line(data=mean_g, aes(x = factor(month) , y = median, group = 1), col='firebrick',lwd=1) +
    geom_ribbon(data=mean_g,aes(ymin = q025, ymax = q975), alpha = 0.2,linetype=2,fill="coral",color="coral4")
  
##########Other analysis using yearly averages created with cdo###################

#Here I want to spatially plot the change in evapotranspiration over the 40 yrs
  
  #load netcdf
  ncfile= paste0(dir.input,"/et0_40y_sum_new.nc")
  nc=nc_open(ncfile)
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
  
  #initialize variables
  t=nc$var[[2]]
  name.var=names(nc$var)[2]
  tsize<-t$varsize
  tdims<-t$ndims
  nt1<-tsize[tdims]
  time <- ncvar_get(nc,"time")
  time
  tunits <- ncatt_get(nc,"time","units")
  timestamp <- as_datetime(c(time*60*60*24),origin="1981-01-02")
  name.lon="lon"
  name.lat="lat"
  lon=ncvar_get(nc,name.lon)
  lat=ncvar_get(nc,name.lat)
  
  
  #####################
  
  start <- rep(1,3) # begin with start=(1,1,1,...,1)
  # change to start=(1,1,1,...,i) to read timestep i
  # start[1]<-lonb[1]
  # start[2]<-latb[1]
  count <- tsize # begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[1]<-1
  count[2]<-1
  llpairs= expand.grid(lon,lat)
  #create a vector of lon lat pairs
  #loop on lon lat values
  library(Kendall)
  #output = a map showing the kendall tau for each pixel and significance with points
  matmod=ncvar_get(nc,name.var,start = c(1,1,1), count= c(length(lon),length(lat),40))
  
  mat1=matrix(numeric(length(lon)*length(lat)),nrow = length(lon), ncol = length(lat))
  mat2=mat1
  mktot=list()
  slope=c()
  #filling two matrices with 
  ##1 kendal coeffincient (trend)
  ##2 pvalue of the coefficent (significance level)
  for (ilo in 1:length(lon)){
    print(ilo)
    start[1]=ilo
    system.time(
    for (ila in 1:length(lat)){
      start[2]=ila
      tspix=ncvar_get(nc,name.var,start = start, count= count)  
      if(!is.na(tspix[1])){
        mktest=MannKendall(tspix)
        
        mat1[ilo,ila]=mktest$tau
        mat2[ilo,ila]=mktest$sl
        
      }
      if(is.na(tspix[1])){
        mat1[ilo,ila]=NA
        mat2[ilo,ila]=NA
        
      }
    })
  }

  
  
  #save mat1 and mat2
  write.csv(mat1,'mat_kendal_corr.csv')
  write.csv(mat2,'mat_sig_level.csv')

  
  #trying to use a function and sapply...same speed
  para_co <- function(id1,id2) {
    
    ido=id1
    # print(ido)
    ida=id2
    start[1]=ido
    start[2]=ida
    tspix=ncvar_get(nc,name.var,start = start, count= count) 
    if(!is.na(tspix[1])){
      mktest=MannKendall(tspix)
      print(mktest$tau)
      ma1[id1,id2]=as.numeric(mktest$tau)
    }
    if(is.na(tspix[1])){
      ma1[id1,id2]=NA
    }
    #return(ma1)
  }
  id1=c(1:100)
  id2=rep(1,100)
  ma1=mat1
  system.time({
  mrd=mapply(para_co,id1,id2)
  })
  
  matmod=ncvar_get(nc,name.var,start = c(1,1,1), count= c(length(lon),length(lat),40))
  kend<-function(mat){
    mktest=MannKendall(tspix)
  }

 # Now I will have to plot the results with ggplot
  
  mat1=read.csv(paste0(dir.output,'/mat_kendal_corr.csv'),header=T)[-1]
  mat2=read.csv(paste0(dir.output,'/mat_sig_level.csv'))[-1]
  
  
#The grid is too thin (1arcmin) for an efficient use of ggplot
  llpairs= expand.grid(lon,lat)
  m1p=melt(mat1)
  m1p$Var1=llpairs$Var1
  m1p$Var2=llpairs$Var2
  m1p=m1p[,-1]
  
#transform the grid to a raster grid
  r1 <- rasterFromXYZ(m1p[,c("Var1","Var2","value")],crs=("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  rasterVis::levelplot(r1,contour=T,margin=F,maxpixel=100000)

  m1p$Var1=llpairs$Var1
  m1p$Var2=llpairs$Var2
  m1p$group=1
  #aggregate raster (upscaling with a factor 3)
  r1a=aggregate(r1, fact=3, fun=mean, expand=TRUE, na.rm=TRUE)
  
  mat2b=mat2
  mat2b[mat2b<=0.05 & mat2b>0.01]=1
  mat2b[mat2b<=0.01 & mat2b>0.001]=2
  mat2b[mat2b<=0.001]=3
  
  
  #now plot the points for CI
  
 # matsi=apply(mat2b,c(1,2),sigt)

  
  m2p=melt(mat2)
  m2p$Var1=llpairs$Var1
  m2p$Var2=llpairs$Var2
  
  rconf=rasterFromXYZ(m2p[,c("Var1","Var2","value")],crs=("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  #aggregate raster
  rconfa=aggregate(rconf, fact=20, fun=mean, expand=TRUE, na.rm=TRUE)

  #from raster back to ggplot
  mdown=rasterToPoints(r1a)
  mdown=as.data.frame(mdown)
  mdown$group=1
  
  magdow=rasterToPoints(rconfa)
  magdow=as.data.frame(magdow)
  magdow$group=1
  
  #test for trend in the data
  #add variable for statistical significance in the plot
  magdow$si=NA
  magdow$si[which(magdow$value<=0.05 & magdow$value>0.01)]=1
  magdow$si[which(magdow$value<=0.01 & magdow$value>0.001)]=2
  magdow$si[which(magdow$value<=0.001)]=3
  
  
  #final plot
  longlims=c(-22.5,47)
  latlims=c(27.5,70)
  ggplot(mdown) + 
    geom_contour_fill(aes(x=x,y=y,group=group,z=value),na.rm = TRUE)+
    scale_fill_distiller(palette = "RdYlGn",na.value="transparent",guide="colorsteps", breaks=breaks_extended(6), limits=c(-.6,.6),oob = scales::squish,"kendall tau")+
    geom_point(data=magdow,aes(x=x,y=y,group=group,size=si),na.rm = TRUE,alpha=0.4,shape=16)+
    scale_size(trans=scales::modulus_trans(1),range=c(0,0.7),  breaks = c(1,2,3),"significance level", labels = c(0.05, 0.01, 0.001))+
    scale_y_continuous(
      breaks = c(25,30,35,40,45,50,55,60,65,70),labels= c("25°N","30°N","35°N","40°N","45°N","50°N","55°N","60°N","65°N","70°N"),limits = c(25,71),"Latitude")+
    scale_x_continuous(
      breaks =c(-20,-10,0,10,20,30,40,50),labels= c("-20°E","-10°E","0°E","10°E","20°E","30°E","40°E","50°E"),"Longitude") +
    coord_fixed(xlim = longlims,  ylim = latlims, ratio = 1.3)+
    theme(axis.text=element_text(size=14),
          plot.title = element_text(size=18,face="bold"),
          axis.title=element_text(size=16),
          panel.background = element_rect(fill = "aliceblue", colour = "grey50"),
          legend.title = element_text(size=18),
          panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.text = element_text(size=14),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          legend.key.size = unit(1, "cm"))
  


