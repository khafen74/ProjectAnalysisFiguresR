
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(raster)

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\PDSI\\NETCDF")
fn <- "scpdsi_wymean.nc"

raswy <- brick(fn)


# Mean and Std Dev --------------------------------------------------------

if (file.exists("scpdsi_wyOverallMean.tif"))
{
  raswy_mean <- raster("scpdsi_wyOverallMean.tif")
}else
{
  raswy_mean <- mean(raswy)
}

if (file.exists("scpdsi_wyOverallSD.tif"))
{
  raswy_sd <- raster("scpdsi_wyOverallSD.tif")
}else
{
  raswy_sd <- calc(raswy, fun=sd, na.rm = T)
}


# Save raster files -------------------------------------------------------

writeRaster(raswy_mean, "scpdsi_wyOverallMean.tif")
writeRaster(raswy_sd, "scpdsi_wyOverallSD.tif")


# Zonal stats for individual quads ----------------------------------------

library(rgdal)

#polygons of quad map extents
zonepoly <- readOGR("C:\\konrad\\USGS\\PROSER_NHD\\data\\FocusBasins\\shp", "QuadBasin_join")
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
zs$basin <- NA
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
  zs[i,5] <- extract(subset(raswy,layer), zonepoly[i,], fun = mean)
  zs[i,6] <- as.character(data.frame(zonepoly)[i, "Name_1"])
  zs[i,8] <- year
}


# Calculate scPDSI percentile ---------------------------------------------

zs$per <- pnorm((zs$quad_mean - zs$mean) / zs$sd)*100


# Save zonal stats data ---------------------------------------------------

fn = "C:\\konrad\\USGS\\PROSER_NHD\\data\\FocusBasins\\csv\\FocusBasinQuads_scPDSI_zonal.csv"
write.csv(zs, fn, row.names = F)


# Histograms of percentiles -----------------------------------------------

cnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Umpqua", "WillowWhitehorse")

width = 7.5
height = 7.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "hist_scPDSI_quads_Percentile.png"
setwd("C:\\konrad\\USGS\\PROSER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

par(mfrow=c(3,2),
    oma = c(0,0,0,0) + 0.1,
    mar = c(5,4,2,1) + 0.1)


freqval = T
breaks = seq(0, 100, by=5)

for (i in 1:length(cnames))
{
  plotdat <- subset(zs, zs$basin==cnames[i])
  hist(plotdat$per, breaks = breaks, main=cnames[i], xlab = "scPDSI Percentile", freq = freqval)
  
}

dev.off()

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\PDSI\\NETCDF")


# Histogram for all data --------------------------------------------------

width = 7.5
height =3.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "hist_scPDSI_studyArea_Percentile.png"
setwd("C:\\konrad\\USGS\\PROSER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

hist(zs$per, breaks = breaks, main="All Basins", xlab = "scPDSI Percentile", freq = freqval)

dev.off()

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\PDSI\\NETCDF")
