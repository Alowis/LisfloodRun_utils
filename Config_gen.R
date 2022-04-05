################################################################################
## file of R environment configuration
################################################################################


###########
## Paths ##
###########

wd <- getwd()
setwd("..")

dir.study = getwd()  # exemple: setwd("~/Rstudy_RFA")

if (!exists("dir.data"))     dir.data    = paste0(dir.study,"/Data")

if (!exists("dir.input"))      dir.input     = paste0(dir.data,"/in")
if (!exists("dir.output"))     dir.output    = paste0(dir.data,"/out")


dir.create(dir.output,showWarnings = F,recursive = T)
dir.create(dir.input,showWarnings = F,recursive = T)


## downloading library
## """"""""""""""""""""""""

library(rgdal)
library(raster)
library(ncdf4)
library(lubridate)
library(ggplot2)
library(rasterVis)
library(metR)
library(reshape2)
library(scales)
# 
# library(igraph)
# library(chron)
# library(spatgraphs)
# library(stats)
# library(mapdata)
# library(chron)
# library(sp)
# library(geosphere)
# library(cartography)
# library(chron)
# library(EnvStats)
# library(EnvStats)
# library(geomorph)
# # library(in2extRemes)
# library(fExtremes)
# library(extRemes)
# library(ismev)
# library(POT)
# library(ks)
# library(scales)
# library(ggplot2)
# library(reshape2)
# library(plyr)
# library(ggmap)
# library(rgeos)
# library(rgdal)
# library(grid)
# library(gridExtra)
# library(lattice)
# library(ggpubr)
# library(dplyr)
# library(texmex)
# library("fitdistrplus")
# library(ismev)
# library(maptools)
# library(astsa)
# library(evd)
# library(ggmap)
# library(kdevine)
# library(kdecopula)
# library(copula)
# library(ncdf4)
# library(chron)
# library(lattice)
# library(RColorBrewer)
# library(lubridate)
# library(dbscan)
# library(dismo)
# library(maps)
# library(progress)
# library(fastmatch)
# library(viridis)
# library(ggplot2)
# library(ggnewscale)
# library(metR)
# library(lcopula)


# source functions directory
# """"""""""""""""""""""""""" 
# for (func in list.files(dir.functions)) {
#   print(func)
#   try(source(paste(dir.functions,func,sep="/")))
# }
