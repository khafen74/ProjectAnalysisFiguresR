
# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv")
fn <- "Obs_Quads_scPDSI.csv"

dat <- read.csv(fn)


# Calculate addtional metrics and subset data -----------------------------

dat$perDiff <- dat$wyPer - dat$perctl
dat$pdsiDiff <- dat$wyPDSI - dat$quad_mean
datPer <- subset(dat, dat$FCODE==46006&dat$Month>7&dat$Month<11)
datInt <- subset(dat, dat$FCODE==46003)
datPer$misclass <- ifelse(datPer$Category=="Dry", 1, 0)

# Models for perennial misclassification ----------------------------------

library(lme4)
library(bbmle)

#logisitc regression
test.log <- glm(datPer$misclass~datPer$perDiff, family="binomial")

#mixed models with reach as random effect
quad.rm <- glmer(wet~perctl+(1|REACHCODE), family=binomial, data=datPer)
obsPer.rm <- glmer(wet~wyPer+(1|REACHCODE), family=binomial, data=datPer)
obsPdsi.rm <- glmer(wet~wyPDSI+(1|REACHCODE), family=binomial, data=datPer)
pdsi.rm <- glmer(wet~pdsiDiff+(1|REACHCODE), family=binomial, data=datPer)
per.rm <- glmer(wet~perDiff+(1|REACHCODE), family=binomial, data=datPer)
sd.rm <- glmer(wet~sd+(1|REACHCODE), family=binomial, data=datPer)

AICctab(quad.rm, obsPer.rm, obsPdsi.rm, pdsi.rm, per.rm, sd.rm, nobs=nrow(datPer))


# Perennial in dry percentiles --------------------------------------------

perNeg <- subset(datPer, datPer$pdsiDiff <= 0)

quad.rm <- glmer(misclass~perctl+(1|REACHCODE), family=binomial, data=perNeg)
obs.rm <- glmer(misclass~wyPer+(1|REACHCODE), family=binomial, data=perNeg)
pdsi.rm <- glmer(misclass~pdsiDiff+(1|REACHCODE), family=binomial, data=perNeg)
per.rm <- glmer(misclass~perDiff+(1|REACHCODE), family=binomial, data=perNeg)


# Models of intermittent classifications ----------------------------------

quad.rm <- glmer(wet~perctl+(1|REACHCODE), family=binomial, data=datInt)
obsPer.rm <- glmer(wet~wyPer+(1|REACHCODE), family=binomial, data=datInt)
obsPdsi.rm <- glmer(wet~wyPDSI+(1|REACHCODE), family=binomial, data=datInt)
pdsi.rm <- glmer(wet~pdsiDiff+(1|REACHCODE), family=binomial, data=datInt)
per.rm <- glmer(wet~perDiff+(1|REACHCODE), family=binomial, data=datInt)

AICctab(quad.rm, obs.rm, pdsi.rm, per.rm, nobs=nrow(datInt))
