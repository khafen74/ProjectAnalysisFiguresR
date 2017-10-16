
# Do setup ----------------------------------------------------------------

rm(list=ls())

library(raster)
library(rgdal)

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\PDSI\\NETCDF")
fn <- "scpdsi_wymean.nc"
rasdat <- brick(fn)

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


# Figure ------------------------------------------------------------------

dev.off()

width = 4
height = 3.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "scPDSI_CRB.pdf"
setwd("C:\\konrad\\USGS\\PROSPER\\figs")
pdf(file=filename, width=width, height=height, bg=bg)

yearStart <- 2004
yearEnd <- 2016

datPlot <- df
x <- df$year
y <- df$scpdsi
for (i in 1:(length(y)-1))
{
  if ((y[i]<0.0&y[i+1]>0.0) | (y[i]>0.0&y[i+1]<0.0))
  {
    newx <- x[i] + (abs(y[i]))/(abs(y[i])+abs(y[i+1]))
    x <- append(x, newx, after=length(x))
    y <- append(y, 0.0, after=length(y))
  }
}
plotdf <- data.frame(x,y)
plotdf <- plotdf[order(x),]

plot(plotdf$x, plotdf$y, type="l", xlim = c(yearStart, yearEnd), col="gray", lwd=1,
     ylab = NA, xlab = NA, main=NA, ylim = c(-4,4), las=1)        

datPoly <-plotdf
datPoly <- subset(datPoly, datPoly$y >= 0)
ypos.poly <- c(datPoly$y, min(datPoly$y), min(datPoly$y))                     
ypos.poly <- c(datPoly$y, 0, 0)
# x.poly <- c(datPoly$x, datPoly$x[nrow(datPoly)], datPoly$x[1])
x.poly <- c(datPoly$x, yearEnd, yearStart)
polygon(x.poly, ypos.poly, col=rgb(30/255,144/255,1,alpha = 0.35), 
        border=rgb(30/255,144/255,1,alpha = 0.8), lwd = 0.5)

datPoly <-plotdf
datPoly <- subset(datPoly, datPoly$y <= 0)
yneg.poly <- c(datPoly$y, min(datPoly$y), min(datPoly$y))                     
yneg.poly <- c(datPoly$y, 0, 0)
# x.poly <- c(datPoly$x, datPoly$x[nrow(datPoly)], datPoly$x[1])
x.poly <- c(datPoly$x, yearEnd, yearStart)
polygon(x.poly, yneg.poly, col=rgb(220/255,20/255,60/255,alpha = 0.35), 
        border=rgb(220/255,20/255,60/255,alpha = 0.8), lwd = 0.5)
lines(plotdf$x, rep(0, nrow(plotdf)), lty=1, col = "gray")
  


sz = 0.85
mtext(text = "Water Year", side = 1, line = 0.5, outer = T, cex = sz)
mtext(text = "Mean Water Year scPDSI", side = 2, line = -1.75, outer = T, cex = sz)
dev.off()

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\PDSI\\NETCDF")


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
