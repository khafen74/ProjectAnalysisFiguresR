# Load data ---------------------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv")

datBasin <- read.csv("FocusBasin_wyMeanScPDSI.csv")
#datQuads <- read.csv("FocusBasinQuads_scPDSI_zonal.csv")
datObs <- read.csv("Obs_FocusBasins.csv")
cnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Willow Whitehorse", "watYear")
colnames(datBasin) <- cnames

# Plot quads with basin scPDSI (color) ------------------------------------

dev.off()

width = 7.5
height = 7.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "scPDSI_FocusBasins_hist_timeseries_color.png"
setwd("C:\\konrad\\USGS\\PROSPER\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

basinnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Umpqua", "WillowWhitehorse")
basinnames <- c("Willow Whitehorse", "Methow", "Boise")

par(mfrow=c(3,2),
    oma = c(2,2,1,2) + 0.1,
    mar = c(2.5,3,1,3) + 0.1)

for (i in 1:length(basinnames))
{
  
  yearStart <- 2004
  yearEnd <- 2016
  
  subQuad <- subset(datQuads, datQuads$basin==basinnames[i])
  subObs <- subset(datObs, datObs$Name==basinnames[i])
  subObs <- subset(subObs, subObs$Year>=yearStart&subObs$Year<=yearEnd)
  ymax <- max(max(table(subQuad$year))/nrow(subQuad), 
              max(table(subObs$Year))/nrow(subObs))
  
  ymax = 0.45
  plot(1, type="n", xlab="", ylab="", xlim=c(yearStart, yearEnd), axes=F, 
       ylim = c(0,ymax), main = basinnames[i])
  
  if (i==1)
  {
    legend("topleft", legend = c("Field observations collected"),
           fill = c("gray40"),
           border = c(NA),
           cex=0.8)
  }
  # histobj <- hist(subQuad$year, xlim = c(1896, 2016), 
  #                 breaks = seq(1896, 2016, by=1), axes = F, col = "black", border = NA, add=T,
  #                 freq = F)
  histobj2 <- hist(subObs$Year, xlim = c(yearStart, yearEnd), col = rgb(0.5,0.5,0.5), border=NA, 
                   breaks = seq(yearStart, yearEnd, by=1), axes = F, add=T,
                   freq = F)
  print(sum(histobj2$density))
  
  axis(side=4, at = seq(0, round(ceiling(ymax*10)/10, digits = 2)
                        , by = round(ymax/3, digits=2)))
  #mtext(text = "Density", side = 4, line = 2.5, cex = 0.7)
  
  par(new=T)
  
  datPlot <- datBasin[,basinnames[i]]
  x <- datBasin$watYear
  y <- datBasin[,basinnames[i]]
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
       ylab = NA, xlab = NA, main=basinnames[i], ylim = c(-4,4))        
  
  datPoly <-plotdf
  datPoly <- subset(datPoly, datPoly$y >= 0)
  ypos.poly <- c(datPoly$y, min(datPoly$y), min(datPoly$y))                     
  ypos.poly <- c(datPoly$y, 0, 0)
  x.poly <- c(datPoly$x, datPoly$x[nrow(datPoly)], datPoly$x[1]) 
  polygon(x.poly, ypos.poly, col=rgb(30/255,144/255,1,alpha = 0.35), 
          border=rgb(30/255,144/255,1,alpha = 0.8), lwd = 0.5)
  
  datPoly <-plotdf
  datPoly <- subset(datPoly, datPoly$y <= 0)
  yneg.poly <- c(datPoly$y, min(datPoly$y), min(datPoly$y))                     
  yneg.poly <- c(datPoly$y, 0, 0)
  x.poly <- c(datPoly$x, datPoly$x[nrow(datPoly)], datPoly$x[1]) 
  polygon(x.poly, yneg.poly, col=rgb(220/255,20/255,60/255,alpha = 0.35), 
          border=rgb(220/255,20/255,60/255,alpha = 0.8), lwd = 0.5)
  lines(datBasin$watYear, rep(0, nrow(datBasin)), lty=1, col = "gray")
  
  
}

sz = 0.85
mtext(text = "Water Year", side = 1, line = 0.5, outer = T, cex = sz)
mtext(text = "Mean Water Year scPDSI", side = 2, line = 0.25, outer = T, cex = sz)
mtext(text = "Proportion of Observations Collected", side = 4, line = 0.25, outer = T, cex = sz)
dev.off()

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\FocusBasins\\csv")



# With observations divided by wet/dry ------------------------------------

dev.off()

width = 3.5
height = 6
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "scPDSI_FocusBasins_hist_timeseries_color_proportions.png"
setwd("C:\\konrad\\USGS\\PROSPER\\figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)

basinnames <- c("Boise", "John Day", "Lower Clark Fork", "Methow", "Umpqua", "WillowWhitehorse")
basinnames <- c("Methow", "Willow Whitehorse", "Boise")

par(mfrow=c(3,1),
    oma = c(2,2,1,2) + 0.1,
    mar = c(2.5,3,1,3) + 0.1)

for (i in 1:length(basinnames))
{
  
  yearStart <- 2004
  yearEnd <- 2016
  
  subObs <- subset(datObs, datObs$Name==basinnames[i])
  subObs <- subset(subObs, subObs$Year>=yearStart&subObs$Year<=yearEnd)
  wet <- subset(subObs, subObs$Category == "Wet")
  dry <- subset(subObs, subObs$Category == "Dry")
  
  ymax = 0.45
  plot(1, type="n", xlab="", ylab="", xlim=c(yearStart, yearEnd), axes=F, 
       ylim = c(0,ymax), main = basinnames[i])
  
  if (i==1)
  {
    legend("topleft", legend = c("Dry field observations", "wet field observations"),
           fill = c("gray60", "gray40"),
           cex=0.8,
           border = c(NA,NA),
           bty="o")
  }
  # histobj <- hist(subQuad$year, xlim = c(1896, 2016), 
  #                 breaks = seq(1896, 2016, by=1), axes = F, col = "black", border = NA, add=T,
  #                 freq = F)
  # histobj2 <- hist(subObs$Year, xlim = c(yearStart, yearEnd), col = rgb(0.5,0.5,0.5), border=NA, 
  #                  breaks = seq(yearStart, yearEnd, by=1), axes = F, add=T,
  #                  freq = F)
  
  xrange <- seq(yearStart-1, yearEnd, 1)
  ctall <- hist(subObs$Year, breaks = xrange, plot = F)$counts
  ctwet <- hist(wet$Year, breaks = xrange, plot=F)$counts
  ctdry <- hist(wet$Year, breaks = xrange, plot=F)$counts
  
  #total <- sum(ct1, ct2)
  par(new=T)
  barp <- barplot(ctall/sum(ctall), names.arg = xrange[-1], space = 0, 
                  yaxt='n', xaxt='n', ylim = c(0,ymax), xlim = c(0.5,12.5),
                  col = "gray60", border=NA)
  par(new=T)
  barp2 <- barplot(ctwet/(sum(ctall)), names.arg = xrange[-1], space = 0, 
                   yaxt='n', xaxt='n', ylim = c(0,ymax), xlim = c(0.5,12.5),
                   col = "gray40", border=NA)
  
  axis(side=4, at = seq(0, round(ceiling(ymax*10)/10, digits = 2)
                        , by = round(ymax/3, digits=2)))
  #mtext(text = "Density", side = 4, line = 2.5, cex = 0.7)
  
  par(new=T)
  
  datPlot <- datBasin[,basinnames[i]]
  x <- datBasin$watYear
  y <- datBasin[,basinnames[i]]
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
       ylab = NA, xlab = NA, main=basinnames[i], ylim = c(-4,4))        
  
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
  lines(datBasin$watYear, rep(0, nrow(datBasin)), lty=1, col = "gray")
  
  
}

sz = 0.85
mtext(text = "Water Year", side = 1, line = 0.5, outer = T, cex = sz)
mtext(text = "Mean Water Year scPDSI", side = 2, line = 0.25, outer = T, cex = sz)
mtext(text = "Proportion of Observations Collected", side = 4, line = 0.25, outer = T, cex = sz)
dev.off()

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\FocusBasins\\csv")


# Snippets ----------------------------------------------------------------

xrange <- seq(2003, 2016, 1)

test <- subset(datObs, datObs$Year > 2003)
test1 <- subset(test, test$wet == 0)
test2 <- subset(test, test$wet > 0)

ct1 <- hist(test1$Year, breaks = xrange, plot=F)$counts
ct2 <- hist(test2$Year, breaks = xrange, plot=F)$counts

Total <- sum(ct1, ct2)
barp <- barplot(rbind(ct2/Total, ct1/Total), names.arg = xrange[-1], space = 0)
