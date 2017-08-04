# Do setup ----------------------------------------------------------------

rm(list=ls())
library(raster)

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\PDSI\\NETCDF")
fn <- "scpdsi_wymean.nc"

raswy <- brick(fn)


# Mean and Std Dev --------------------------------------------------------

raswy_mean <- raster("scpdsi_wyOverallMean.tif")
#see https://stackoverflow.com/questions/33700755/how-can-i-find-the-pixel-wise-standard-deviation for a way to speed up sd
raswy_sd <- raster("scpdsi_wyOverallSD.tif")


# Zonal stats for individual quads ----------------------------------------

library(rgdal)

#polygons of quad map extents
zonepoly <- readOGR("C:\\konrad\\USGS\\PROSER_NHD\\data\\Quads", "Historical_Topo_Maps_subset")
#mean of all water years for quad map extent
zs_mean <- extract(raswy_mean, zonepoly, fun = mean, df = T, na.rm = T)
cnames <- c("ID", "mean", "FID")
zs_mean$FID <- zs_mean$ID-1
colnames(zs_mean) <- cnames
cnames <- c("ID", "sd", "FID")
#mean of sd (sd between mean water year scspdsi) for quad map extent
zs_sd <- extract(raswy_sd, zonepoly, fun = mean, df = T)
zs_sd$FID <- zs_sd$ID-1
colnames(zs_sd) <- cnames
zs_sd <- zs_sd[,2:3]
#combine mean and sd zonal stats into single dataframe
zs <- cbind(zs_mean, zs_sd[,1])
cnames <- c("ID", "mean", "FID", "sd")
colnames(zs) <- cnames
zs$quad_mean <- NA
zs$per <- NA
zs$year <- NA


# Get zonal stats for water-year in which quad was collected --------------

for (i in 1:nrow(zs))
{
  year <- as.integer(as.character(data.frame(zonepoly)[i, "Field_Chec"]))
  if (year == 0)
  {
    year <- as.integer(as.character(data.frame(zonepoly)[i, "Survey_Yea"]))
  }
  layer <- year - 1895
  if (layer >= 1 & layer <= 121)
  {
    zs[i,5] <- extract(subset(raswy,layer), zonepoly[i,], fun = mean)
    zs[i,7] <- year
  }else
  {
    print(year)
  }
  
}


# Calculate scPDSI percentile ---------------------------------------------

zs$per <- pnorm((zs$quad_mean - zs$mean) / zs$sd)*100


# Print zonal stats to csv ------------------------------------------------

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\FocusBasins\\csv")
fn <- "NationalQuads_scPDSI.csv"
write.csv(zs, fn, row.names = F)
