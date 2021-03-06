# Do setup ----------------------------------------------------------------

rm(list=ls())

library(lme4)
library(bbmle)

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv")
fn <- "FlowPerm_HRNHD_scPDSI.csv"
fnDA <- "FlowPerm_HRNHD_scPDSI_DA.csv"
dat <- read.csv(fnDA)


# Subset to months August to October --------------------------------------

#First calculate some additional metrics
#pdsi percentile at time of observation minus pdsi perctile at quad collection
dat$perDiff <- dat$wyPer - dat$perctl
#pdsi at time of observation minus pdsi at quad collection
dat$pdsiDiff <- dat$wyPDSI - dat$quad_mean
dat$misclass <- ifelse(((dat$FCODE==46006|dat$FCODE==55800)&dat$Category=="Dry")|
                         ((dat$FCODE==46003|dat$FCODE==46007)&dat$Category=="Wet"),1,0)

#subset data
datAugOct <- subset(dat, dat$Month > 7 & dat$Month < 11)
per <- subset(datAugOct, datAugOct$FCODE==46006 | datAugOct$FCODE==55800)



# Test models, probability of being classified wet ------------------------

test.log <- glm(per$wet~per$perDiff, family="binomial")

quad.rm <- glmer(wet~perctl+(1|REACHCODE), family=binomial, data=per)
obsPer.rm <- glmer(wet~wyPer+(1|REACHCODE), family=binomial, data=per)
obsPdsi.rm <- glmer(wet~wyPDSI+(1|REACHCODE), family=binomial, data=per)
pdsi.rm <- glmer(wet~pdsiDiff+(1|REACHCODE), family=binomial, data=per)
per.rm <- glmer(wet~perDiff+(1|REACHCODE), family=binomial, data=per)
sd.rm <- glmer(wet~sd+(1|REACHCODE), family=binomial, data=per)

AICctab(quad.rm, obsPer.rm, obsPdsi.rm, pdsi.rm, per.rm, sd.rm, nobs=nrow(per))



# Probability wet, with drainage area -------------------------------------

quad.rm <- glmer(wet~perctl+(1|DivDASqKM), family=binomial, data=per)
obsPer.rm <- glmer(wet~wyPer+(1|DivDASqKM), family=binomial, data=per)
obsPdsi.rm <- glmer(wet~wyPDSI+(1|DivDASqKM), family=binomial, data=per)
pdsi.rm <- glmer(wet~pdsiDiff+(1|DivDASqKM), family=binomial, data=per)
per.rm <- glmer(wet~perDiff+(1|DivDASqKM), family=binomial, data=per)
sd.rm <- glmer(wet~sd+(1|DivDASqKM), family=binomial, data=per)
pdsisd <- glmer(wet~pdsiDiff+sd+(1|DivDASqKM), family=binomial, data=per)

AICctab(quad.rm, obsPer.rm, obsPdsi.rm, pdsi.rm, per.rm, sd.rm, pdsisd, nobs=nrow(per))


# Probability misclass, with drainage area --------------------------------

quad.rm <- glmer(misclass~perctl+(1|DivDASqKM), family=binomial, data=datAugOct)
obsPer.rm <- glmer(misclass~wyPer+(1|DivDASqKM), family=binomial, data=datAugOct)
obsPdsi.rm <- glmer(misclass~wyPDSI+(1|DivDASqKM), family=binomial, data=datAugOct)
quadPdsi.rm <- glmer(misclass~quad_mean+(1|DivDASqKM), family=binomial, data=datAugOct)
pdsi.rm <- glmer(misclass~pdsiDiff+(1|DivDASqKM), family=binomial, data=datAugOct)
per.rm <- glmer(misclass~perDiff+(1|DivDASqKM), family=binomial, data=datAugOct)
sd.rm <- glmer(misclass~sd+(1|DivDASqKM), family=binomial, data=datAugOct)
pdsisd <- glmer(misclass~pdsiDiff+sd+(1|DivDASqKM), family=binomial, data=datAugOct)

AICctab(quadPdsi.rm, quad.rm, obsPer.rm, obsPdsi.rm, pdsi.rm, per.rm, sd.rm, pdsisd, nobs=nrow(per))
