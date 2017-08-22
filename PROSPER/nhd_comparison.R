
# Setup and read data -----------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSPER\\data\\csv")
fn1 <- "perm_prospermean.csv"
fn2 <- "perm_nhd.csv"

proDat <- read.csv(fn1)
nhdDat <- read.csv(fn2)


# Aggregate by HUC8 -------------------------------------------------------

proHuc8 <- aggregate(proDat$length, by=list(proDat$HUC_8), FUN = "sum")
colnames(proHuc8) <- c("HUC8", "pro_len")
nhdHuc8 <- aggregate(nhdDat$length, by=list(nhdDat$HUC_8), FUN = "sum")
colnames(nhdHuc8) <- c("HUC8", "nhd_len")

huc8 <- merge(x = proHuc8, y = nhdHuc8, by = "HUC8", all = T)
huc8$pro_dif <- huc8$pro_len - huc8$nhd_len
huc8$per_dif <- huc8$pro_dif/huc8$nhd_len*100.0


# Write to csv ------------------------------------------------------------

write.csv(huc8, "perm_huc8.csv")
