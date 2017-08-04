
# Load data ---------------------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\FocusBasins\\csv")

datBasin <- read.csv("FocusBasin_wyMeanScPDSI.csv")
datQuads <- read.csv("FocusBasinQuads_scPDSI_zonal.csv")
datObs <- read.csv("FlowPerm_obs_scPDSI.csv")
cnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Umpqua", "WillowWhitehorse", "watYear")
colnames(datBasin) <- cnames


# Plot quads with basin scPDSI --------------------------------------------

dev.off()

width = 7.5
height = 7.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "scPDSI_FocusBasins_hist_timeseries.png"
setwd("C:\\konrad\\USGS\\PROSER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

basinnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Umpqua", "WillowWhitehorse")

par(mfrow=c(3,2),
    oma = c(0,0,0,0) + 0.1,
    mar = c(4,4,2,4) + 0.1)

for (i in 1:length(basinnames))
{
  
  subQuad <- subset(datQuads, datQuads$basin==basinnames[i])
  subObs <- subset(datObs, datObs$Name==basinnames[i])
  subObs <- subset(subObs, subObs$Year>0)
  ymax <- max(max(table(subQuad$year))/nrow(subQuad), 
              max(table(subObs$Year))/nrow(subObs))
  #print(ymax)
  
  plot(1, type="n", xlab="", ylab="", xlim=c(1896, 2016), axes=F, 
       ylim = c(0,ymax))
  histobj <- hist(subQuad$year, xlim = c(1896, 2016), 
                  breaks = seq(1896, 2016, by=1), axes = F, col = "black", border = NA, add=T,
                  freq = F)
  histobj2 <- hist(subObs$Year, xlim = c(1896, 2016), col = rgb(0.5,0.5,0.5), border=NA, 
                  breaks = seq(1896, 2016, by=1), axes = F, add=T,
                  freq = F)
  
  axis(side=4, at = seq(0, round(ceiling(ymax*10)/10, digits = 2)
                        , by = round(ymax/3, digits=2)))
  mtext(text = "Density", side = 4, line = 2.5, cex = 0.7)
  
  # print(seq(0, round(ceiling(ymax*10)/10, digits = 2)
  #           , by = round(ymax/3, digits=2)))
  par(new=T)
  
  
  datPlot <- datBasin[,basinnames[i]]
  plot(datBasin$watYear, datBasin[,basinnames[i]], type="l", xlim = c(1896,2016), col=rgb(0,0,0,alpha=0.5), lwd=0.5,
       ylab = "scPDSI", xlab = "Water Year", main=basinnames[i])
  x.poly <- c(datBasin$watYear, datBasin$watYear[nrow(datBasin)], datBasin$watYear[1])         # Adjoin two x-coordinates
  y.poly <- c(datBasin[,basinnames[i]], min(datBasin[,basinnames[i]]), min(datBasin[,basinnames[i]]))                     # .. and the corresponding y-coordinates
  y.poly <- c(datBasin[,basinnames[i]], 0, 0)
  polygon(x.poly, y.poly, col=rgb(0.6,0.6,0.6,alpha = 0.5), border=NA, lwd = 0.5)
  
  if (i==1)
  {
    legend("topleft", legend = c("Topo maps field-checked", "Field Observations collected"),
           fill = c("black", "gray40"),
           border = c(NA,NA),
           cex=0.8)
  }
}

dev.off()

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\FocusBasins\\csv")


# Plot quads with basin scPDSI (color) ------------------------------------

dev.off()

width = 7.5
height = 7.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "scPDSI_FocusBasins_hist_timeseries_color.png"
setwd("C:\\konrad\\USGS\\PROSER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

basinnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Umpqua", "WillowWhitehorse")

par(mfrow=c(3,2),
    oma = c(0,0,0,0) + 0.1,
    mar = c(4,4,2,4) + 0.1)

for (i in 1:length(basinnames))
{
  
  subQuad <- subset(datQuads, datQuads$basin==basinnames[i])
  subObs <- subset(datObs, datObs$Name==basinnames[i])
  subObs <- subset(subObs, subObs$Year>0)
  ymax <- max(max(table(subQuad$year))/nrow(subQuad), 
              max(table(subObs$Year))/nrow(subObs))
  #print(ymax)
  
  plot(1, type="n", xlab="", ylab="", xlim=c(1896, 2016), axes=F, 
       ylim = c(0,ymax))
  histobj <- hist(subQuad$year, xlim = c(1896, 2016), 
                  breaks = seq(1896, 2016, by=1), axes = F, col = "black", border = NA, add=T,
                  freq = F)
  histobj2 <- hist(subObs$Year, xlim = c(1896, 2016), col = rgb(0.5,0.5,0.5), border=NA, 
                   breaks = seq(1896, 2016, by=1), axes = F, add=T,
                   freq = F)
  
  axis(side=4, at = seq(0, round(ceiling(ymax*10)/10, digits = 2)
                        , by = round(ymax/3, digits=2)))
  mtext(text = "Density", side = 4, line = 2.5, cex = 0.7)
  
  par(new=T)
  
  plot(datBasin$watYear, datBasin[,basinnames[i]], type="l", xlim = c(1896,2016), col="gray", lwd=1,
       ylab = "scPDSI", xlab = "Water Year", main=basinnames[i])
  x.poly <- c(datBasin$watYear, datBasin$watYear[nrow(datBasin)], datBasin$watYear[1])         # Adjoin two x-coordinates
  
  datPoly <-datBasin
  datPoly[,basinnames[i]] <- ifelse(datPoly[,basinnames[i]]>0, datPoly[,basinnames[i]], 0)
  ypos.poly <- c(datPoly[,basinnames[i]], min(datPoly[,basinnames[i]]), min(datPoly[,basinnames[i]]))                     # .. and the corresponding y-coordinates
  ypos.poly <- c(datPoly[,basinnames[i]], 0, 0)
  polygon(x.poly, ypos.poly, col=rgb(30/255,144/255,1,alpha = 0.35), 
          border=rgb(30/255,144/255,1,alpha = 0.8), lwd = 0.5)
  
  datPoly <-datBasin
  datPoly[,basinnames[i]] <- ifelse(datPoly[,basinnames[i]]<0, datPoly[,basinnames[i]], 0)
  yneg.poly <- c(datPoly[,basinnames[i]], min(datPoly[,basinnames[i]]), min(datPoly[,basinnames[i]]))                     # .. and the corresponding y-coordinates
  yneg.poly <- c(datPoly[,basinnames[i]], 0, 0)
  polygon(x.poly, yneg.poly, col=rgb(220/255,20/255,60/255,alpha = 0.35), 
          border=rgb(220/255,20/255,60/255,alpha = 0.8), lwd = 0.5)
  lines(datBasin$watYear, rep(0, nrow(datBasin)), lty=1, col = "gray")
  
  if (i==1)
  {
    legend("topleft", legend = c("Topo maps field-checked", "Field Observations collected"),
           fill = c("black", "gray40"),
           border = c(NA,NA),
           cex=0.8)
  }
  
}

dev.off()

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\FocusBasins\\csv")


# Plot single basin (color) -----------------------------------------------

dev.off()

width = 7.5
height = 7.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "scPDSI_Umpqua_hist_timeseries_color.png"
setwd("C:\\konrad\\USGS\\PROSER_NHD\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

basinnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Umpqua", "WillowWhitehorse")

par(oma = c(0,0,0,0) + 0.1, mar = c(4,4,2,4) + 0.1)

i = 5

subQuad <- subset(datQuads, datQuads$basin==basinnames[i])
subObs <- subset(datObs, datObs$Name==basinnames[i])
subObs <- subset(subObs, subObs$Year>0)
ymax <- max(max(table(subQuad$year))/nrow(subQuad), 
            max(table(subObs$Year))/nrow(subObs))
x1 = 1975
x2 = 2016
xlim = c(x1,x2)
#print(ymax)

plot(1, type="n", xlab="", ylab="", xlim=xlim, axes=F, 
     ylim = c(0,ymax))
histobj <- hist(subQuad$year, xlim = xlim, 
                breaks = seq(x1, x2, by=1), axes = F, col = "black", border = NA, add=T,
                freq = F)
histobj2 <- hist(subObs$Year, xlim = xlim, col = rgb(0.5,0.5,0.5), border=NA, 
                 breaks = seq(x1, x2, by=1), axes = F, add=T,
                 freq = F)

axis(side=4, at = seq(0, round(ceiling(ymax*10)/10, digits = 2)
                      , by = round(ymax/3, digits=2)))
mtext(text = "Density", side = 4, line = 2.5, cex = 0.7)

par(new=T)

plot(datBasin$watYear, datBasin[,basinnames[i]], type="l", xlim = xlim, col="gray", lwd=1,
     ylab = "scPDSI", xlab = "Water Year", main=basinnames[i])
x.poly <- c(datBasin$watYear, datBasin$watYear[nrow(datBasin)], datBasin$watYear[1])         # Adjoin two x-coordinates

datPoly <-datBasin
datPoly[,basinnames[i]] <- ifelse(datPoly[,basinnames[i]]>0, datPoly[,basinnames[i]], 0)
ypos.poly <- c(datPoly[,basinnames[i]], min(datPoly[,basinnames[i]]), min(datPoly[,basinnames[i]]))                     # .. and the corresponding y-coordinates
ypos.poly <- c(datPoly[,basinnames[i]], 0, 0)
polygon(x.poly, ypos.poly, col=rgb(30/255,144/255,1,alpha = 0.35), 
        border=rgb(30/255,144/255,1,alpha = 0.8), lwd = 0.5)

datPoly <-datBasin
datPoly[,basinnames[i]] <- ifelse(datPoly[,basinnames[i]]<0, datPoly[,basinnames[i]], 0)
yneg.poly <- c(datPoly[,basinnames[i]], min(datPoly[,basinnames[i]]), min(datPoly[,basinnames[i]]))                     # .. and the corresponding y-coordinates
yneg.poly <- c(datPoly[,basinnames[i]], 0, 0)
polygon(x.poly, yneg.poly, col=rgb(220/255,20/255,60/255,alpha = 0.35), 
        border=rgb(220/255,20/255,60/255,alpha = 0.8), lwd = 0.5)
lines(datBasin$watYear, rep(0, nrow(datBasin)), lty=1, col = "gray")

if (i==1)
{
  legend("topleft", legend = c("Topo maps field-checked", "Field Observations collected"),
         fill = c("black", "gray40"),
         border = c(NA,NA),
         cex=0.8)
}
  

dev.off()

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\FocusBasins\\csv")


