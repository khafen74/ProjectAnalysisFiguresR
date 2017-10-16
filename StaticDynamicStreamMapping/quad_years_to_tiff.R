
# Do setup ----------------------------------------------------------------

rm(list=ls())

library(raster)

fn <- "C:\\konrad\\USGS\\PROSPER_NHD\\data\\PDSI\\NETCDF\\crb_scpdsi_wymean_wgs84.tif"
raswy <- brick(fn)

fn2 <- "C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv\\crb_quads_scpdsi.csv"
crbquads <- read.csv(fn2)

fn3 <- "C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv\\flow_permanence.csv"
crbobs <- read.csv(fn3)

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\PDSI\\wyTIFF")


# Get list of years -------------------------------------------------------

years <- unique(crbquads$chck_year)
years <- c(years, seq(2004,2016,1))
years <- sort(years, decreasing = F)


# Create indiividual tiff for each year -----------------------------------

for (year in years)
{
  fntiff <- paste("scpdsi_", as.character(year),".tif", sep="")
  band <- dim(raswy)[3] - (2016-year)
  writeRaster(subset(raswy,band), fntiff, format="GTiff")
}
