# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv")

fn <- "CRB_MR_PROSPER.csv"
fn2 <- "pros_stitch_diff.csv"

nhd <- read.csv(fn)
pros <- read.csv(fn2)
nhdpros <- merge(nhd[,1:19], pros, by = "REACHCODE")


# Wet/dry classifications based on stitched data --------------------------

nhdpros$stPerm <- ifelse(nhdpros$st_mean > nhdpros$th_mean.x, 1, 0)
nhdpros$nhdPerm <- ifelse((nhdpros$FCODE==46006 | nhdpros$FCODE==55800), 1, 0)
nhdpros$misclass <- ifelse((((nhdpros$FCODE==46006 | nhdpros$FCODE==55800) & nhdpros$stPerm == 0) |
                       ((nhdpros$FCODE==46003 | nhdpros$FCODE==46007 | nhdpros$FCODE==46000) &
                          nhdpros$stPerm == 1)),1,0)
nhdpros$miscalss <- ifelse(nhdpros$nhdPerm != nhdpros$stPerm, 1, 0)
nhdpros$wetdry <- ifelse(nhdpros$stPerm == 0 & nhdpros$misclass == 1, 1, 0)


# Subset columns ----------------------------------------------------------

cols = c("REACHCODE", "st_mean", "th_mean.x", "stPerm", "nhdPerm", "misclass", "wetdry")
dfprint = nhdpros[cols]
write.csv(dfprint, "pros_class_stitch.csv", row.names = T)
