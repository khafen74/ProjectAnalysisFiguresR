
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(ncdf4)

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\PDSI\\NETCDF")
fn = "scpdsi_12_PRISM.nc"
nc = nc_open(fn)
pdsi = ncvar_get(nc, attributes(nc$var)$names[1])

lon = ncvar_get(nc, "longitude")
lat = ncvar_get(nc, "latitude")


# Make data frame of SD for each grid cell over all years -----------------

pdsi_min = array(dim=c(dim(pdsi)[1], dim(pdsi)[2]))
pdsi_max = array(dim=c(dim(pdsi)[1], dim(pdsi)[2]))
pdsi_mean = array(dim=c(dim(pdsi)[1], dim(pdsi)[2]))
pdsi_sd = array(dim=c(dim(pdsi)[1], dim(pdsi)[2]))

for(i in 1:dim(pdsi)[1])
{
  for(j in 1:dim(pdsi)[2])
  {
    pdsi_min[i,j] = min(pdsi[i,j,1:122], na.rm = F)
    pdsi_max[i,j] = max(pdsi[i,j,1:122], na.rm = F)
    pdsi_mean[i,j] = mean(pdsi[i,j,1:122], na.rm = F)
    pdsi_sd[i,j] = sd(pdsi[i,j,1:122], na.rm = F)
  }
}


# Write to NetCDF file ----------------------------------------------------

londim = ncdim_def("longitude","degrees",as.double(lon))
latdim = ncdim_def("latitude","degrees",as.double(lat))

fillval = -9999
varname = "minimum pdsi 1895-2016"
min_def = ncvar_def("min","pdsi",list(londim, latdim), fillval, varname, prec = "double")
varname = "maximum pdsi 1895-2016"
max_def = ncvar_def("max","pdsi",list(londim, latdim), fillval, varname, prec = "double")
varname = "mean pdsi 1895-2016"
mean_def = ncvar_def("mean","pdsi",list(londim, latdim), fillval, varname, prec = "double")
varname = "standard deviation of pdsi 1895-2016"
sd_def = ncvar_def("sd","pdsi",list(londim, latdim), fillval, varname, prec = "double")

ofname = "scpdsi_12_dstat.nc"
ncout = nc_create(ofname, list(min_def, max_def, mean_def, sd_def), force_v4 = T)

ncvar_put(ncout, min_def, pdsi_min)
ncvar_put(ncout, max_def, pdsi_max)
ncvar_put(ncout, mean_def, pdsi_mean)
ncvar_put(ncout, sd_def, pdsi_sd)

nc_close(ncout)

# Plots of arrays ---------------------------------------------------------

filled.contour(lon,lat,pdsi_mean)
