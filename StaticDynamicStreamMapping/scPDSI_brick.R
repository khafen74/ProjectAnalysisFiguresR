
# Create raster brick with all NetCDF data --------------------------------

rm(list=ls())
library(raster)

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\PDSI\\NETCDF")

bricks <- list()

for (i in 1:12)
{
  bricks[[i]] <- brick(paste("scpdsi_",i,"_PRISM.nc", sep=""))
}

#this will take a while . . . 
bigbrick <-brick(bricks)
