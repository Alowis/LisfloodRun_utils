library(rgdal)
#library(raster)
library(ncdf4)
library(lubridate)
library(ggplot2)
#library(rasterVis)
#library(metR)
#The aim of that script is to compute yearly statistics for e0, et and es maps from LISVAP
#First the statistics will be computed at the pixel level for each year
#The statistics could be computed by catchment using the cutmap utility of LISFLOOD
#Stats:

#-mean-median-Q2.5-Q97.5-std


workDir<-("/BGFS/CLIMEX/tilloal")

#import discharge at gauges from EFAS calibration and compare with LISFLOOD outputs


ncfile= paste0(workDir,"/e5land_test/out/e0_1amin_e5l_1981_2020.nc")

nc=nc_open(ncfile)

name.var=names(nc$var)[1]

t=nc$var[[1]]
tsize<-t$varsize
tdims<-t$ndims
nt1<-tsize[tdims]

time <- ncvar_get(nc,"time")
tunits <- ncatt_get(nc,"time","units")
timestamp <- as_datetime(c(time*60*60*24),origin="1981-01-02")
months=month(timestamp)


name.lon="lon"
name.lat="lat"

lon=ncvar_get(nc,name.lon)
lat=ncvar_get(nc,name.lat)
evapmap=list()


# Initialize start and count to read one timestep of the variable.
start <- rep(1,tdims) # begin with start=(1,1,1,...,1)
# start[1]<-lonb[1]
# start[2]<-latb[1]
count <- tsize # begin w/count=(nx,ny,nz,...,nt), reads entire var
count[1]<-length(lon)
count[2]<-length(lat)
######The idea is to use the file startcount as a .csv listing the location of my chunks of the netcdf

#load the csv file of the death
timeslices=read.csv(paste0(workDir,"/lisvap_ana/timeslices_ana_t.csv"))

my_image_subset = timeslices[1:12,] # Subset the first 10 elements
timeslices_ed = timeslices [-c(1:12),] # Remove the first 100 lines from the df

#workDir<-("//ies.jrc.it/H07/nahaUsers/tilloal/ERA5l_x_lisflood")
#then I save the new csv
write.csv(timeslices_ed,paste0(workDir,"/lisvap_ana/timeslices_ana_td.csv"),row.names = FALSE) # Write the file with 100 images less
          
          cat("jesuisla")
          
          for (im in 1:length(my_image_subset$startouf)){
            cat(im)
            start[tdims]=my_image_subset$startouf[im]
            count[tdims]=my_image_subset$countouf[im]
            evapmap$data   = ncvar_get(nc,name.var,start = start, count= count)   
            evapmap$lon    = ncvar_get(nc,name.lon,start = start[1], count= count[1]) 
            evapmap$lat    = ncvar_get(nc,name.lat,start = start[2], count= count[2]) 
            
            mov<-as.vector(evapmap$data[1:count[1],1:count[2],1])
            
            moncu=c(mean(na.omit(mov)),
                    median(na.omit(mov)),
                    quantile(na.omit(mov),0.975),
                    quantile(na.omit(mov),0.025),
                    sd(na.omit(mov)))
            metadata=rbind(metadata,moncu)
          }
          
          metadata=as.data.frame(metadata)
          names(metadata)= c("mean","median","Q975","Q025","st_dev")
          
          write.csv(metadata,paste0(workDir,"/lisvap_ana/metadata_yr",unique(my_image_subset$yrmom),".csv"))
          
          