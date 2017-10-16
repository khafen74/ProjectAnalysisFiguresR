
# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv")

fn <- "CRB_MR_PROSPER.csv"
fn2 <- "CRB_MR_PROSPER_median.csv"

nhd <- read.csv(fn)
med <- read.csv(fn2)
nhdmed <- merge(nhd[,1:19], med, by = "REACHCODE")


# Create new dataframe with wet/dry classifications -----------------------

years <- c("04","05","06","07","08","09","10","11","12","13","14","15","16")
newcols <- paste("p",years,sep="")
#if SPP greater than threshold classify as wet
pros <- data.frame(pAve = ifelse(nhd[,"ave_mean"] > nhd[,"th_mean"], 1, 0))
#determine if PROSPER classification is different than NHD classification
pros$mAve <- ifelse((((nhd$FCODE==46006 | nhd$FCODE==55800) & pros$pAve == 0) |
                       ((nhd$FCODE==46003 | nhd$FCODE==46007 | nhd$FCODE==46000) &
                          pros$pAve == 1)),1,0)

for (year in years)
{
  yearcol <- paste("y",year,"_mean",sep="")
  newcol <- paste("p",year,sep="")
  miscol <- paste("m",year,sep="")
  #if SPP greater than threshold classify as wet
  pros[,newcol] <- ifelse(nhd[,yearcol] > nhd[,"th_mean"], 1, 0)
  #determine if PROSPER classification is different than NHD classification
  pros[,miscol] <- ifelse((((nhd$FCODE==46006 | nhd$FCODE==55800) & pros[,newcol] == 0) |
                            ((nhd$FCODE==46003 | nhd$FCODE==46007 | nhd$FCODE==46000) & 
                               pros[,newcol] == 1)),1,0)
  
}

#calculate percent disagreement between PROSPER and NHD for each reach
miscols <- c("m04","m05","m06","m07","m08","m09","m10","m11","m12","m13","m14","m15","m16")
pros$disag <- rowSums(pros[,miscols])/length(miscols)

#add rcode so it can be joined in shapefile
pros$REACHCODE = as.character(nhd$REACHCODE)

# Wet/dry classifications from median values ------------------------------

years <- c("04","05","06","07","08","09","10","11","12","13","14","15","16")
newcols <- paste("p",years,sep="")
#if SPP greater than threshold classify as wet
prosmed <- data.frame(pAve = ifelse(nhdmed[,"ave_median"] > nhdmed[,"th_median"], 1, 0))
#determine if PROSPER classification is different than NHD classification
prosmed[is.na(prosmed)] <- 0
prosmed$mAve <- ifelse((((nhdmed$FCODE==46006 | nhdmed$FCODE==55800) & prosmed$pAve == 0) |
                       ((nhdmed$FCODE==46003 | nhdmed$FCODE==46007 | nhdmed$FCODE==46000) &
                          prosmed$pAve == 1)),1,0)

for (year in years)
{
  yearcol <- paste("y",year,"_median",sep="")
  newcol <- paste("p",year,sep="")
  miscol <- paste("m",year,sep="")
  #if SPP greater than threshold classify as wet
  prosmed[,newcol] <- ifelse(nhdmed[,yearcol] > nhdmed[,"th_median"], 1, 0)
  #determine if PROSPER classification is different than NHD classification
  prosmed[,miscol] <- ifelse((((nhdmed$FCODE==46006 | nhdmed$FCODE==55800) & prosmed[,newcol] == 0) |
                             ((nhdmed$FCODE==46003 | nhdmed$FCODE==46007 | nhdmed$FCODE==46000) &
                                prosmed[,newcol] == 1)),1,0)
  
}

#calculate percent disagreement between PROSPER and NHD for each reach
miscols <- c("m04","m05","m06","m07","m08","m09","m10","m11","m12","m13","m14","m15","m16")
prosmed$disag <- rowSums(prosmed[,miscols])/length(miscols)

#add rcode so it can be joined in shapefile
prosmed$REACHCODE = as.character(nhdmed$REACHCODE)

# Write to csv ------------------------------------------------------------

write.csv(pros, "pros_class.csv", row.names = T)
write.csv(prosmed, "pros_class_med.csv", row.names = T)


# Create new data frame by joining ----------------------------------------

full <- merge(nhd, pros, by="REACHCODE")
fullmed <- merge(nhdmed, prosmed, by="REACHCODE")


# Differences in perennial length by year ---------------------------------

lendf <- data.frame(year = seq(2004,2016,1), percent = 0)
lendfmed <- data.frame(year = seq(2004,2016,1), percent = 0)
years <- c("04","05","06","07","08","09","10","11","12","13","14","15","16")
fullmed[is.na(fullmed)] <- 0
i <- 1
for (year in years)
{
  yearcol <- paste("m",year,sep="")
  lendf[i,2] <- sum(full[,yearcol]*full$LENGTHKM)/sum(full$LENGTHKM)
  lendfmed[i,2] <- sum(fullmed[,yearcol]*fullmed$LENGTHKM)/sum(fullmed$LENGTHKM)
  i <- i+1
}


# Plot length by year -----------------------------------------------------

plot(lendf$year, lendf$percent, type="l", xlab = "Year", ylab = "Percent Wet", col = "blue", 
     ylim = c(0,0.5))
lines(lendfmed$year, lendfmed$percent, col = "red")
