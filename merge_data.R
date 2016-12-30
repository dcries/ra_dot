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


keep <- c("AADT","ACCESSCNTL","AUTOMOBILE","BUS","COMNETWORK","COMPLETED","CORPCITY","CRACKPATCH",
          "CURBEDL","CURBEDR","DIRECTION","FEDFUNC","FID_1","IRI","JURISDIC","LIMITMPH","MEDTYPE",
          "MEDWIDTH","NHS","NUMLANES","PICKUP","PSIRATING","RUMBLEL","RUMBLER","SINGLEUNIT",
          "SINGMULTTR","SURFTYPE","SURFWIDTH","SYSCODE","TERRAIN","TRANSCENTE","TRNLINKID","TRNNODE_F",
          "TRNNODE_T","TRUCKRTE","NATHWYSYS","NE","SHDTIEDL","SHDTIEDR","SHDTYPEL","SHDTYPER",
          "SHDWIDTHL","SHDWIDTHR","URBANAREA")
remove <- c("COMPLETED", "FID_1"  ,   "NHS" ,      "TRNLINKID", "TRNNODE_F", "TRNNODE_T", "NE")
keep <- keep[-which(keep %in% remove)]
keep[which(!keep %in% names(dot05))]

roadkeep <- c("COMPLETED","COUNTY","MSLink","NHS","TASLINKID","Miles")
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
#noroad <- c(128)
#dotdata <- dotdata[-noroad,]

#create df to cbind with dotdata that has corresponding road characteristics via MSLink
newdf <- data.frame(matrix(0,nrow=nrow(dotdata),ncol=length(keep)))
for(i in 1:nrow(dotdata)){
  if(dotdata$YEAR[i]==2005){
    ind <- which(dot05$MSLINK==dotdata[i,"MSLink"])
    if(sum(ind) > 0){
      newdf[i,] <- dot05[ind,keep]
    }
  }
  if(dotdata$YEAR[i]==2006){
    ind <- which(dot06$MSLINK==dotdata[i,"MSLink"])
    if(sum(ind) > 0){
      newdf[i,] <- dot06[ind,keep]
    }
  }
  if(dotdata$YEAR[i]==2007){
    ind <- which(dot07$MSLINK==dotdata[i,"MSLink"])
    if(sum(ind) > 0){
      newdf[i,] <- dot07[ind,keep]
    }
  }
  if(dotdata$YEAR[i]==2008){
    ind <- which(dot08$MSLINK==dotdata[i,"MSLink"])
    if(sum(ind) > 0){
      newdf[i,] <- dot08[ind,keep]
    }
  }
  if(dotdata$YEAR[i]==2009){
    ind <- which(dot09$MSLINK==dotdata[i,"MSLink"])
    if(sum(ind) > 0){
      newdf[i,] <- dot09[ind,keep]
    }
  }
  if(dotdata$YEAR[i]==2010){
    ind <- which(dot10$MSLINK==dotdata[i,"MSLink"])
    if(sum(ind) > 0){
      newdf[i,] <- dot10[ind,keep]
    }
  }
  if(dotdata$YEAR[i]==2011){
    ind <- which(dot11$MSLINK==dotdata[i,"MSLink"])
    if(sum(ind) > 0){
      newdf[i,] <- dot11[ind,keep]
    }
  }
  if(dotdata$YEAR[i]==2013){
    ind <- which(dot13$MSLINK==dotdata[i,"MSLink"])
    if(sum(ind) > 0){
      newdf[i,] <- dot13[ind,keep]
    }
  }
  if(dotdata$YEAR[i]==2014){
    ind <- which(dot14$MSLINK==dotdata[i,"MSLink"])
    if(sum(ind) > 0){
      newdf[i,] <- dot14[ind,keep]
    }
  }
  if(dotdata$YEAR[i]==2012){
    ind <- which(dot12$TASLINKID==dotdata[i,"TASLINKID"])
    if(sum(ind) > 0){
      newdf[i,] <- dot12[ind,keep]
    }
  }
  if(i %% 1000 == 0){
    print(i)
  }
}

names(newdf) <- keep


nomslink <- which(rowSums(newdf)==0)
which(dotdata[nomslink,"MSLink"] %in% dot05$MSLINK)

temp <- cbind(dotdata,newdf)

rhs <- cat(keep,sep="+")
f1 <- CRASHES ~ lin(AADT)+fct(ACCESSCNTL)+lin(AUTOMOBILE)+lin(BUS)+fct(COMNETWORK)+
  fct(CORPCITY)+lin(CRACKPATCH)+fct(CURBEDL)+fct(CURBEDR)+fct(DIRECTION)+
  fct(FEDFUNC)+lin(IRI)+fct(JURISDIC)+lin(LIMITMPH)+fct(MEDTYPE)+lin(MEDWIDTH)+
  lin(NUMLANES)+lin(PICKUP)+lin(PSIRATING)+fct(RUMBLEL)+fct(RUMBLER)+
  lin(SINGLEUNIT)+lin(SINGMULTTR)+fct(SURFTYPE)+lin(SURFWIDTH)+fct(SYSCODE)+
  fct(TERRAIN)+fct(TRANSCENTE)+fct(TRUCKRTE)+fct(NATHWYSYS)+fct(SHDTIEDL)+
  fct(SHDTIEDR)+fct(SHDTYPEL)+fct(SHDTYPER)+lin(SHDWIDTHL)+lin(SHDWIDTHR)+fct(URBANAREA)
  
#remove medtype,

m <- spikeSlabGAM(formula=f1,data=temp[1:100000,])
