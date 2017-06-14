setwd("C:\\Users\\dcries\\github\\ra_dot\\data")
d <- read.csv('completedata_nona.csv')


d$ACCESSCNTL <- as.factor(d$ACCESSCNTL)
d$TRANSCENTE <- as.factor(d$TRANSCENTE)
d$lVOLUME <- log(as.numeric(d$VOLUME))
d$FEDFUNC2 <- as.factor(d$FEDFUNC2)
d$MEDTYPE2 <- as.factor(d$MEDTYPE2)
d$SYSCODE <- as.factor(d$SYSCODE)
d$TERRAIN <- as.factor(d$TERRAIN)
d$COMNETWORK <- as.factor(d$COMNETWORK)
d$NATHWYSYS <- as.factor(d$NATHWYSYS)

d$SHDTYPEL <- as.factor(d$SHDTYPEL)
d$SHDTYPER <- as.factor(d$SHDTYPER)
d$SURFTYPE2 <- 0
d$SURFTYPE2 <- replace(d$SURFTYPE2 , d$SURFTYPE%in%c(60,65,69,92) , 1 )
d$SURFTYPE2 <- replace(d$SURFTYPE2 , d$SURFTYPE%in%c(70,74,76,77,79) , 2 )

#some speed limits >55
d$LIMITMPH2 <- as.factor(d$LIMITMPH)
#lots of 0s for PSI rating
d$RUMBLEL <- d$RUMBLEL - 1
d$RUMBLER <- d$RUMBLER - 1
#SHDTIEDR and L have 3 values, should just be yes/no
d$SHDTYPEL2 <- as.factor(d$SHDTYPEL)
d$SHDTYPER2 <- as.factor(d$SHDTYPER)

X <- as.data.frame(model.matrix(
  ~ TRANSCENTE+lVOLUME+FEDFUNC2+URBAN+
    SURFTYPE2+IRI+RUMBLEL+RUMBLER+SURFWIDTH+SHDTYPEL2+SHDTYPER2+SHDWIDTHL+SHDWIDTHR, data=d))
