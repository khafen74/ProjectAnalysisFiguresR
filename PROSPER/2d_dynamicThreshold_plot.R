
# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSPER\\data\\csv")
fn <- "PredictionThresholds.csv"

dat <- read.csv(fn)


# Prep data ---------------------------------------------------------------

dat <- dat[,c(3,10,12:15)]
dat$PROSPER_Probability <- as.numeric(as.character(dat$PROSPER_Probability))
dat$DrySurface <- as.numeric(as.character(dat$DrySurface))
dat$WetSurface <- as.numeric(as.character(dat$WetSurface))
dat$Threshold <- as.numeric(as.character(dat$Threshold))
dat$SurfaceDifference <- as.numeric(as.character(dat$SurfaceDifference))
dat <- na.omit(dat)
pt1dat <- subset(dat, dat$Category=="Non-perennial")
pt2dat <- subset(dat, dat$Category=="Perennial")
sd = dat$SurfaceDifference
th = dat$Threshold

# Test plot ---------------------------------------------------------------


plot(pt1dat$SurfaceDifference, pt1dat$PROSPER_Probability,col = "red", ylim=c(0,1), 
     xlim=c(min(dat$SurfaceDifference),max(dat$SurfaceDifference)),
     ylab = "Probability", xlab = "Surface Difference")
points(pt2dat$SurfaceDifference, pt2dat$PROSPER_Probability,
       col="blue")
lines(sd[order(sd)], th[order(sd)])
points(dat$SurfaceDifference, dat$DrySurface, col = "red")
points(dat$SurfaceDifference, dat$WetSurface, col = "blue")
points(pt1dat$SurfaceDifference, pt1dat$PROSPER_Probability, col="red")
points(pt2dat$SurfaceDifference, pt2dat$PROSPER_Probability, col="blue")


# Create Plot -------------------------------------------------------------

redpts = rgb(1,0,0,0.2)
bluepts = rgb(0,0,1,0.2)
ptspch = 16

plot(sd[order(sd)], th[order(sd)], type = "l", ylim = c(0,1),
     xlim=c(min(dat$SurfaceDifference),max(dat$SurfaceDifference)),
     ylab = "Probability", xlab = "Surface Difference", lwd = 0.1,
     col = "gray50")
points(pt1dat$SurfaceDifference, pt1dat$PROSPER_Probability, col = redpts,
       pch = ptspch)
points(pt2dat$SurfaceDifference, pt2dat$PROSPER_Probability, col = bluepts,
       pch = ptspch)
