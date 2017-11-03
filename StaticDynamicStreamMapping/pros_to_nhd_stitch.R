# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSPER_NHD\\data\\csv")

fn <- "CRB_MR_PROSPER.csv"
fn2 <- "pros_stitch_diff.csv"

nhd <- read.csv(fn)
pros <- read.csv(fn2)
nhdpros <- merge(nhd[,1:19], pros, by = "REACHCODE")


# Wet/dry classifications based on stitched data --------------------------

nhdpros$sppPerm <- ifelse(nhdpros$spp_mean > nhdpros$th_mean.x, 1, 0)
nhdpros$nhdPerm <- ifelse((nhdpros$FCODE==46006 | nhdpros$FCODE==55800), 1, 0)

nhdpros$misclass <- ifelse(nhdpros$nhdPerm != nhdpros$sppPerm, 1, 0)
nhdpros$mistype <- ifelse(nhdpros$misclass == 1, 1+nhdpros$sppPerm, 0)


# Subset columns ----------------------------------------------------------

cols = c("REACHCODE", "spp_mean", "th_mean.x", "sppPerm", "nhdPerm", "misclass", "mistype")
dfprint = nhdpros[cols]
write.csv(dfprint, "pros_class_stitch.csv", row.names = T)
