# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\FocusBasins\\csv")
fn <- "NationalQuads_scPDSI.csv"

datQuads <- read.csv(fn)


# Histogram of years checked ----------------------------------------------

hist(datQuads$year, xlab="Year", main = NA, col="black", border = NA)


# Histogram of mean WY scPDSI ---------------------------------------------

hist(datQuads$quad_mean, xlab = "scPDSI", main = NA, col = "black", border = NA)


# Histogram of scPDSI percentile ------------------------------------------

hist(datQuads$per, xlab = "Percentile", main = NA, col = "black", border = NA)
