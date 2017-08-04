
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(raster)

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\PDSI\\NETCDF")
fn <- "scpdsi_wymean.nc"
fnzone <- "C:\\konrad\\USGS\\PROSPER_NHD\\data\\FocusBasins\\ras\\FocusBasins_paper.tif"
rasnc <- brick(fn)
raszone <- raster(fnzone)

# Calc zonal stats --------------------------------------------------------

zstat <- zonal(rasnc, raszone, fun = 'mean', digits = 5, na.rm = T)


# Manipulate data frame and name columns ----------------------------------

tzstat <- t(zstat)
tzstat <- data.frame(tzstat[2:nrow(tzstat),])
cnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Umpqua", "WillowWhitehorse")
cnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Willow Whitehorse")
colnames(tzstat) <- cnames
tzstat$watYear <- seq(1, 121, by=1) + 1895
write.csv(tzstat, "C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv\\FocusBasin_wyMeanScPDSI.csv", row.names = F)


# For annual (calendar year) means ----------------------------------------

tzstat <- t(zstat)
tzstat <- data.frame(tzstat[2:nrow(tzstat),])
cnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Umpqua", "WillowWhitehorse")
cnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Willow Whitehorse")
colnames(tzstat) <- cnames
tzstat$Year <- seq(0, 121, by=1) + 1895
write.csv(tzstat, "C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv\\FocusBasin_anMeanScPDSI.csv", row.names = F)

# Bring in quad map info --------------------------------------------------

quadfn <- "C:\\konrad\\USGS\\PROSPER_NHD\\data\\FocusBasins\\csv\\quads.csv"
quad <- read.csv(quadfn, header = T, sep=",")

#update field check date to survey data if field check does not exist
quad$Field_Chec <- ifelse(quad$Field_Chec>0, quad$Field_Chec, quad$Survey_Yea)

vars <- c("Name_1", "Field_Chec")
quad <- quad[vars]


# scPDSI values for year of quad field checks -----------------------------

quadslist <- list()
for (i in 1:length(cnames))
{
  quadslist[[i]] <- subset(quad, quad$Name_1==cnames[i] & quad$Field_Chec>0)
  quadslist[[i]]$scpdsi <- tzstat[as.character(quadslist[[i]]$Field_Chec), cnames[i]]
}


# Make overlapping histograms ---------------------------------------------
dev.off()

width = 7.5
height = 7.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "hist_scPDSI_quads_FocusBasins.png"
setwd("C:\\konrad\\USGS\\PROSER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

par(mfrow=c(3,2),
    oma = c(0,0,0,0) + 0.1,
    mar = c(5,4,2,1) + 0.1)

col = "black"
col2 = rgb(1,1,1,0.5)
freqval = T
breaks = seq(-5, 6, by=0.5)

for (i in 1:length(cnames))
{
  hist(tzstat[,cnames[i]], breaks = breaks, ylim = c(0,25), col=col, main=cnames[i], xlab = "scPDSI", freq = freqval)
  hist(quadslist[[i]]$scpdsi, breaks = breaks, col=col2, add=T, xlab=NA, ylab=NA, freq = freqval)
  
  if (i==length(cnames))
  {
    legend("topright", legend = c("Basin water-year means 1896-2016", "Mean of water-year in which topo maps were field checked", "Overlap"),
           fill = c("black", "white", "gray50"),
           border = c("black", "black", "black"),
           cex=0.8)
  }
}

dev.off()

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\PDSI\\NETCDF")


# Overlapping histogram with all data -------------------------------------

for (i in 1:nrow(quad))
{
  quad$scpdsi[i] <- tzstat[as.character(quad$Field_Chec[i]), as.character(quad$Name_1[i])]
}


dev.off()

width = 7.5
height = 3.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "hist_scPDSI_quads_all.png"
setwd("C:\\konrad\\USGS\\PROSER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

col = "black"
col2 = rgb(1,1,1,0.5)
freqval = T
breaks = seq(-5, 6, by=0.5)


hist(tzstat, breaks = breaks, col=col, main = "All Basins", xlab = "scPDSI", freq = freqval)
hist(quad$scpdsi, breaks = breaks, col=col2, add=T, xlab=NA, ylab=NA, freq = freqval)


legend("topright", legend = c("Basin water-year means", "Topo field-check year", "Overlap"),
       fill = c("black", "white", "gray50"),
       border = c("black", "black", "black"),
       cex=0.8)


dev.off()

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\PDSI\\NETCDF")

