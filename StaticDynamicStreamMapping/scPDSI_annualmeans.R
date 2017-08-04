
# Open all scPDSI files ---------------------------------------------------

rm(list=ls())
library(ncdf4)
library(raster)

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\PDSI\\NETCDF")
dat <- list()

for (i in 1:12)
{
  fn <- paste("scpdsi_",i,"_PRISM.nc", sep="")
  nc <- nc_open(fn)
  dat[[i]] <- ncvar_get(nc, attributes(nc$var)$names[1])
}

lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")

londim = ncdim_def("longitude","degrees",as.double(lon))
latdim = ncdim_def("latitude","degrees",as.double(lat))

nc_close(nc)


# Annual mean scPDSI for each pixel ---------------------------------------

#for years 1895 - 2016

anmean <- array(dim = c(dim(dat[[1]])[1],dim(dat[[1]])[2],122))
for (i in 1:122)
{
  raslist <- list()
  for (j in 1:12)
  {
    raslist[[j]] <- raster(dat[[j]][,,i])
  }
  rasstack <- stack(raslist)
  anmean[,,i] <- as.matrix(mean(rasstack))
}

fillval <- -9999
yrdim <- ncdim_def("year", "start 1985", 1:122)
varname = "annual mean scpdsi 1895-2016"
mean_def = ncvar_def("mean","pdsi",list(londim, latdim, yrdim), fillval, varname, prec = "double")

ofname = "scpdsi_anmean.nc"
ncout = nc_create(ofname, list(mean_def), force_v4 = T)

ncvar_put(ncout, mean_def, anmean)

nc_close(ncout)


# Water year mean for each pixel ------------------------------------------

#for 1895-1896 water year through 2015-2016 water year
wymean <- array(dim = c(dim(dat[[1]])[1],dim(dat[[1]])[2],121))
for (i in 1:121)
{
  raslist <- list()
  #months Jan - Sep from upcoming year
  for (j in 1:9)
  {
    raslist[[j]] <- raster(dat[[j]][,,i+1])
  }
  #months Oct - Dec from current year
  for (j in 10:12)
  {
    raslist[[j]] <- raster(dat[[j]][,,i])
  }
  rasstack <- stack(raslist)
  wymean[,,i] <- as.matrix(mean(rasstack))
}

fillval <- -9999
yrdim <- ncdim_def("water-year", "start 1895-1896", 1:121)
varname = "water-year mean scpdsi 1895-1896 through 2015-2016"
mean_def = ncvar_def("wymean","scpdsi",list(londim, latdim, yrdim), fillval, varname, prec = "double")

ofname = "scpdsi_wymean.nc"
ncout = nc_create(ofname, list(mean_def), force_v4 = T)

ncvar_put(ncout, mean_def, wymean)

nc_close(ncout)
