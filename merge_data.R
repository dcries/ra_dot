library(foreign)
library(spikeSlabGAM)

setwd("\\\\my.files.iastate.edu\\Users\\dcries\\Desktop\\GIMS Rural Two-lane Primary")
setwd("C:\\Users\\dcries\\github\\ra_dot\\data")
setwd("C:\\Users\\dcries\\github\\ra_dot\\data")
setwd("/home/danny/Documents/github/ra_dot/data")
dot05 <- read.dbf("GIMS_2005.dbf")
dot06 <- read.dbf("GIMS_2006.dbf")
dot07 <- read.dbf("GIMS_2007.dbf")
dot08 <- read.dbf("GIMS_2008.dbf")
dot09 <- read.dbf("GIMS_2009.dbf")
dot10 <- read.dbf("GIMS_2010.dbf")
dot11 <- read.dbf("GIMS_2011.dbf")
dot12 <- read.dbf("GIMS_2012.dbf")
dot13 <- read.dbf("GIMS_2013.dbf")
dot14 <- read.dbf("GIMS_2014.dbf")
road <- read.dbf("roaddata.dbf")
crash <- read.dbf("crash_05-14.dbf")


all.equal(names(dot05),names(dot06))
which(names(dot05)!=names(dot06)) #7
match(names(dot05),names(dot06))
all.equal(names(dot07),names(dot06)) #true
all.equal(names(dot07),names(dot08)) #1 string mismatch
which(names(dot07)!=names(dot08)) #7
all.equal(names(dot07),names(dot09)) #true
all.equal(names(dot07),names(dot10)) #1 string mismatch

roadkeep <- c("AADT","COMPLETED","COUNTY","MSLink","NHS","TASLINKID","TRNNODE_F","TRNNODE_T","Miles")
crashkeep <- c("CRASH01YR","CRASH02YR","CRASH03YR","CRASH04YR","CRASH05YR","CRASH06YR","CRASH07YR","CRASH08YR",
               "CRASH09YR","CRASH10YR","CRASHES","FATCRASH","FATINJ","MAJCRASH","MAJINJ","MILES","MINCRASH",
               "MININJ","PDOCRASH","POSCRASH","POSSINJ","TASLINKID","UNKCRASH","UNKINJ","VMT","VOLUME","YEAR")

road <- road[,roadkeep]
crash <- crash[,crashkeep]

road$TASLINKID <- as.factor(road$TASLINKID)
crash$TASLINKID <- as.factor(crash$TASLINKID)

#is this correct?
road <- unique(road)

dotdata <- merge(crash,road,by="TASLINKID",all=TRUE)

f1 <- MAJINJ ~ AADT + PDOCRASH + POSCRASH + VOLUME + YEAR + TRNNODE_F + TRNNODE_T + COUNTY

m <- spikeSlabGAM(formula=f1,data=dotdata[1:3000,])
