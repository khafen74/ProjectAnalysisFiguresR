# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv")
fn <- "MR_HR_join.csv"
dat <- read.csv(fn)


# Differences between MR and HR classifications ---------------------------

dat$perInt <- ifelse((dat$FCODE==46006 | dat$FCODE==55800)&
                       (dat$FCODE_1==46003 | dat$FCODE_1==46007), 1, 0)
dat$intPer <- ifelse((dat$FCODE==46003 | dat$FCODE==46007)&
                       (dat$FCODE_1==46006 | dat$FCODE_1==55800), 1, 0)

datsub <- subset(dat, dat$Month > 7 & dat$Month <11)

# Calculate classification conversions (MR ---> HR) -----------------------

perInt <- sum(datsub$perInt)/nrow(datsub)
intPer <- sum(datsub$intPer)/nrow(datsub)
