# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\FocusBasins\\csv")
fn <- "FlowPerm_obs_scPDSI.csv"
fn2 <- "Obs_QuadsFocusBasins_scPDSI.csv"

permNA <- read.csv(fn, header=T)
perm <- subset(permNA, !is.na(permNA$wyPer))

wyHist <- subset(perm, as.integer(as.character(perm$Year)) > 0)
dryHist <- subset(wyHist, wyHist$Category == "Dry")
wetHist <- subset(wyHist, wyHist$Category == "Wet")

permProb <- aggregate(perm[c("wet","dry")], by=list(perm$REACHCODE), FUN = sum)
permProb <- merge(permProb, 
              aggregate(perm[c("FCODE", "wyPDSI", "meanPDSI", "sdPDSI")], by=list(perm$REACHCODE), 
                        FUN = mean), by.x = "Group.1", by.y = "Group.1")
permProb <-subset(permProb, permProb$wet+permProb$dry > 1)
permProb$prob <- permProb$wet/(permProb$wet + permProb$dry)


# Histograms of wyPDSI percentiles ----------------------------------------

width = 3.5
height = 7.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "hist_scPDSI_obs_Percentile.png"
setwd("C:\\konrad\\USGS\\PROSPER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

par(mfrow=c(3,1),
    oma = c(0,0,0,0) + 0.1,
    mar = c(5,4,2,1) + 0.1)


freqval = T
breaks = seq(0, 100, by=5)

hist(dryHist$wyPer, breaks = breaks, main = "Dry Observations", xlab = NA)
hist(wetHist$wyPer, breaks = breaks, main = "Wet Observations", xlab = NA)
hist(wyHist$wyPer, breaks = breaks, main = "All Observations", xlab = "scPDSI Percentile")

dev.off()
setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\PDSI\\NETCDF")


# Histograms with perennial/intermittent ----------------------------------

width = 7.5
height = 7.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "hist_scPDSI_obsClass_Percentile.png"
setwd("C:\\konrad\\USGS\\PROSPER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

par(mfrow=c(2,2),
    oma = c(0,0,0,0) + 0.1,
    mar = c(5,4,2,1) + 0.1)


freqval = T
breaks = seq(0, 100, by=5)

hist(subset(wyHist, wyHist$FCODE=="46006"&wyHist$Category=="Dry")$wyPer, breaks = breaks, main = "Perennial Streams, Dry Observations", xlab = NA)
hist(subset(wyHist, wyHist$FCODE=="46006"&wyHist$Category=="Wet")$wyPer, breaks = breaks, main = "Perennial Streams, Wet Observations", xlab = NA)
hist(subset(wyHist, wyHist$FCODE=="46003"&wyHist$Category=="Dry")$wyPer, breaks = breaks, main = "Intermittent Streams, Dry Observations", xlab = "scPDSI Percentile")
hist(subset(wyHist, wyHist$FCODE=="46003"&wyHist$Category=="Wet")$wyPer, breaks = breaks, main = "Intermittent Streams, Wet Observations", xlab = "scPDSI Percentile")

dev.off()
setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\PDSI\\NETCDF")


# Perennial misclassifications --------------------------------------------

permNA <- read.csv(fn2)
permPer <- subset(permNA, permNA$FCODE == "46006")
permPer$misclass <- ifelse(permPer$Category == "Dry", 1, 0)
permMisclass <- subset(permPer, permPer$misclass==1)
hist(permPer$wyPer, breaks = seq(0, 100, by=5), col="black")
hist(permMisclass$wyPer, breaks = seq(0,100, by=5), add = T, col="white")
hist(permMisclass$Year)

# Proportion incorrect by scPDSI ------------------------------------------

permNA <- read.csv(fn2)
permNA <- subset(permNA, permNA$Month>0)
dfplot <- subset(permNA, permNA$FCODE == "46006")
dfplot$perdif <- dfplot$wyPer - dfplot$perctl
dfplot$range <- cut(dfplot$perdif, breaks=seq(-100,100,by=10), labels=1:20)
agmis <- aggregate(dfplot$dry, by=list(dfplot$range), FUN=mean, na.rm=T)

# Barplot -----------------------------------------------------------------
dev.off()

fn2 <- "Obs_QuadsFocusBasins_scPDSI.csv"
permNA <- read.csv(fn2)

width = 7.5
height = 7.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "FocusBasins_perennial_misclass.png"
setwd("C:\\konrad\\USGS\\PROSPER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

dfplot <- subset(permNA, permNA$FCODE_1 == "46006"&permNA$Month>0)
dfplot$perdif <- dfplot$wyPer - dfplot$perctl
basinnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Umpqua", "WillowWhitehorse")

par(mfrow=c(3,2),
    oma = c(0,0,0,0) + 0.1,
    mar = c(4,4,2,0.5) + 0.1)

for (i in 1:length(basinnames))
{
  ylab=NA
  xlab=NA
  basindat <- subset(dfplot, dfplot$Name_1==basinnames[i])
  basindat$range <- cut(basindat$perdif, breaks=seq(-100,100,by=10), labels=1:20)
  agmis <- aggregate(basindat$dry, by=list(basindat$range), FUN=mean, na.rm=T)
  plotvals <- data.frame(group = 1:20, value = NA)
  
  for (j in 1:nrow(agmis))
  {
    plotvals[as.integer(agmis[j,1]),2] <- agmis[j,2]
  }
  
  if (i==3)
  {
    ylab = "Proportion Misclassified"
  }
  if (i>4)
  {
    xlab = "Difference in scPDSI Percentile"
  }
  
  barp <- barplot(t(as.matrix(plotvals$value)), space=0, main = basinnames[i], border=NA,
                  xlim=c(0,20), ylab=ylab, xlab=xlab)
  axis(side=1, at=seq(0,20,by=1), labels=F, tick = 0.1)
  axis(side=1, at=seq(0,20,by=1), labels=seq(-100, 100, by=10),
       line = -0.4, lwd = 0)
}

dev.off()
setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\FocusBasins\\csv")


# All observations --------------------------------------------------------

dev.off()

fn2 <- "Obs_Quads_scPDSI.csv"
permNA <- read.csv(fn2)

width = 7.5
height = 4.0
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "AllObs_perennial_misclass.png"
setwd("C:\\konrad\\USGS\\PROSPER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

dfplot <- subset(permNA, permNA$FCODE == "46006"&permNA$Month>7&permNA$Month<11)
dfplot$perdif <- dfplot$wyPer - dfplot$perctl


par(oma = c(0,0,0,0) + 0.1,
    mar = c(4,4,2,0.5) + 0.1)


ylab = "Proportion Misclassified"
xlab = "Difference in scPDSI Percentile"
  dfplot$range <- cut(dfplot$perdif, breaks=seq(-100,100,by=10), labels=1:20)
  agmis <- aggregate(dfplot$dry, by=list(dfplot$range), FUN=mean, na.rm=T)
  plotvals <- data.frame(group = 1:20, value = NA)
  
  for (j in 1:nrow(agmis))
  {
    plotvals[as.integer(agmis[j,1]),2] <- agmis[j,2]
  }
  
  
  barp <- barplot(t(as.matrix(plotvals$value)), space=0, main = "All Observations", border=NA,
                  xlim=c(0,20), ylab=ylab, xlab=xlab)
  axis(side=1, at=seq(0,20,by=1), labels=F, tick = 0.1)
  axis(side=1, at=seq(0,20,by=1), labels=seq(-100, 100, by=10),
       line = -0.4, lwd = 0)


dev.off()
setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\FocusBasins\\csv")


# All observations, abs value of difference -------------------------------

dev.off()

fn2 <- "Obs_Quads_scPDSI.csv"
permNA <- read.csv(fn2)

width = 7.5
height = 4.0
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "AllObs_perennial_misclass_abs.png"
setwd("C:\\konrad\\USGS\\PROSPER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

dfplot <- subset(permNA, permNA$FCODE == "46006"&permNA$Month>7&permNA$Month<11)
dfplot$perdif <- dfplot$wyPer - dfplot$perctl


par(oma = c(0,0,0,0) + 0.1,
    mar = c(4,4,2,0.5) + 0.1)


min <- 0
max <- 100
by <- 5
labmin <- 0
labmax <- 20
labby <- 1
ylab = "Proportion Misclassified"
xlab = "Difference in scPDSI Percentile"
dfplot$range <- cut(abs(dfplot$perdif), breaks=seq(min,max,by=by), labels=labmin+1:labmax)
agmis <- aggregate(dfplot$dry, by=list(dfplot$range), FUN=mean, na.rm=T)
plotvals <- data.frame(group = labmin:labmax, value = NA)

for (j in 1:nrow(agmis))
{
  plotvals[as.integer(agmis[j,1]),2] <- agmis[j,2]
}


barp <- barplot(t(as.matrix(plotvals$value)), space=0, main = "All Observations", border=NA,
                xlim=c(labmin,labmax), ylab=ylab, xlab=xlab)
axis(side=1, at=seq(labmin,labmax,by=labby), labels=F, tick = 0.1)
axis(side=1, at=seq(labmin,labmax,by=labby), labels=seq(min, max, by=by),
     line = -0.4, lwd = 0)


dev.off()
setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\FocusBasins\\csv")



