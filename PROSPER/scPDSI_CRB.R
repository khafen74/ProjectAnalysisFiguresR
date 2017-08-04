
# Do setup ----------------------------------------------------------------

rm(list=ls())

library(raster)
library(rgdal)

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\PDSI\\NETCDF")
fn <- "scpdsi_wymean.nc"
rasdat <- brick(fn)[]

# Get zonal stats ---------------------------------------------------------

df <- data.frame(year = 2004:2016, scpdsi = NA)
crb_shp <- readOGR("C:\\konrad\\USGS\\PROSPER\\data\\shp", "US_17_boundary_dissolved_wgs84")

for (i in 1:13)
{
  band <- 108 + i
  df[i,2] <- extract(subset(rasdat,band),crb_shp, fun = mean)
}


# Plot --------------------------------------------------------------------

plot(df$year, df$scpdsi, type = "l", xlab = "Water Year", ylab = "scPDSI", main = "Mean scPDSI for CRB")
lines(df$year, rep(0,nrow(df)), col="gray")


# Write files -------------------------------------------------------------

crb_shp <- readOGR("C:\\konrad\\USGS\\PROSPER\\data\\shp", "US_17_boundary_dissolved_wgs84")
year <- 2004
for (i in 109:121)
{
  #conterminous US
  writeRaster(subset(rasdat,i), paste("C:\\konrad\\USGS\\PROSPER\\data\\ras\\crb_",year,".tif",sep=""))
  
  #CRB
  writeRaster(mask(subset(rasdat,i), crb_shp),
              paste("C:\\konrad\\USGS\\PROSPER\\data\\ras\\crb_",year,"_clip.tif",sep=""))
  year <- year+1
}



# Clip to CRB -------------------------------------------------------------

crb_shp <- readOGR("C:\\konrad\\USGS\\PROSPER\\data\\shp", "US_17_boundary_dissolved_wgs84")
rasdat_clip <- mask(rasdat, crb_shp)

# Make histogram for each year --------------------------------------------

dev.off()

year <- 2004
titlesize = 1
axissize = 0.7
labelsize = 1
cexpoint = 1.0
tck = -0.025
yaline = -0.2
ylline = -0.8
ytline = 1.75
xaline = -0.2
xlline = -1.2

for (i in 109:121)
{
  width = 1.5
  height = 1.5
  units = 'in'
  pointsize = 12
  res = 600
  bg="white"
  filename = paste("hist_", year,".png", sep="")
  setwd("C:\\konrad\\USGS\\PROSPER\\figs\\map_hist")
  png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)
  #pdf(file=filename, width=width, height=height, pointsize=pointsize, bg=bg)
  
  par(mar=c(1,1,1,1))
  hist(rasdat_clip, i, maxpixels = 10000000, xlim = c(-8,8), breaks = seq(-15,15,0.5),
       axes = F, freq = F, main=NA, col="gray30", border=NA, ylim=c(0, 0.5), xlab=NA, ylab=NA)
  
  axis(side=1, labels=FALSE, at=seq(-8,8,2), tck=tck, cex.axis=axissize, line= xaline)
  axis(side=2, labels=FALSE, tck=tck, cex.axis=axissize, line = yaline)
  axis(side=2, labels=c(0.1,0.3,0.5), at=c(0.1,0.3,0.5), cex.axis=axissize, lwd = 0, line=ylline, las=1)
  axis(side=1, labels=seq(-8,8,2), at=seq(-8,8,2), cex.axis=axissize, lwd = 0, line=xlline, las=1)
  title(xlab = "scPDSI", outer = F, line = 2.0, cex.lab = labelsize)
  title(ylab = "Density", outer = F, line = 1.75, cex.lab = labelsize)
  
  year <- year+1
  
  dev.off()
}

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\PDSI\\NETCDF")
