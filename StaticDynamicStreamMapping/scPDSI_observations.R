# Do setup ----------------------------------------------------------------

rm(list=ls())
library(raster)
library(rgdal)

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\PDSI\\NETCDF")
fn <- "scpdsi_wyOverallMean.tif"
wymean <- raster(fn)
fn <- "scpdsi_wyOverallSD.tif"
wysd <- raster(fn)
fn <- "scpdsi_wymean.nc"
wybrick <- brick(fn)

monthbricks <- list()

for (i in 1:12)
{
  monthbricks[[i]] <- brick(paste("scpdsi_",i,"_PRISM.nc", sep=""))
}

# Extract scPDSI values ---------------------------------------------------

wd <- "C:\\konrad\\USGS\\PROSER_NHD\\data\\Observations"
fn <- "FlowPermanence_DataRelease_quadsExtent_wgs84"

pts <- readOGR(wd, fn)
zs_wymean <- extract(wymean, coordinates(pts)[,1:2], fun = mean, df = T, na.rm = T)
zs_wysd <- extract(wysd, coordinates(pts)[,1:2], fun = mean, df = T, na.rm = T)


# Update shapefile dataframe ----------------------------------------------

perm <- data.frame(pts)
perm$meanPDSI <- zs_wymean$scpdsi_wyOverallMean
perm$sdPDSI <- zs_wysd$scpdsi_wyOverallSD
perm$wyPDSI <- NA
perm$mPDSI <- NA


# Get scPDSI values for each observation ----------------------------------

for (i in 1:nrow(perm))
{
  year <- as.integer(as.character(perm[i, "Year"]))
  month <- as.integer(as.character(perm[i, "Month"]))
  #print(paste(year,month))
  if (year > 0)
  {
    layer <- year-1895
    #print(layer)
    perm[i, "wyPDSI"] <- extract(subset(wybrick, layer), matrix(coordinates(pts)[i,1:2], ncol=2))
    #print(perm[i, "wyPDSI"])
    
    if (month > 0)
    {
      perm[i, "mPDSI"] <- extract(subset(monthbricks[[month]], layer), matrix(coordinates(pts)[i,1:2], ncol=2))
      #print(perm[i, "mPDSI"])
    }
  }
}


# Calculate wy scPDSI percentile ------------------------------------------

perm$wyPer <- pnorm((perm$wyPDSI - perm$meanPDSI) / perm$sdPDSI)*100
perm$wet <- ifelse(perm$Category == "Wet", 1, 0)
perm$dry <- ifelse(perm$Category == "Dry", 1, 0)

#print to csv
setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\FocusBasins\\csv")
fn <- "FlowPerm_allQuadObs_scPDSI.csv"
write.csv(perm, fn, row.names = F)



