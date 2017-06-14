library(foreign)
  library(dplyr)
library(reshape)

setwd("C:\\Users\\dcries\\github\\ra_dot\\data")
d <- read.csv('completedata_nona.csv')

#access entrance type
entrance <- read.dbf("\\\\my.files.iastate.edu\\Users\\dcries\\Desktop\\RA_DOT\\pri_tl_utm_t2_ruralprimary2lane_20150707s.dbf")
#
safety <- read.dbf("\\\\my.files.iastate.edu\\Users\\dcries\\Desktop\\RA_DOT\\SafetyFeatures_ruralprimarytwolane_20170602f.dbf")

d2 <- left_join(d,entrance)
d3 <- d2[!duplicated(d2),]
d3$EntTypeA[is.na(d3$EntTypeA)] <- 0
d3$EntTypeB[is.na(d3$EntTypeB)] <- 0
d3$EntTypeC[is.na(d3$EntTypeC)] <- 0

d <- d3

write.csv(d,'completedata_nona.csv',row.names=FALSE)


ms <- melt(safety[,-1],id.vars=c("TASLINKID"))
cs <- cast(ms,TASLINKID+variable~value)

d2 <- left_join(d,cs)
