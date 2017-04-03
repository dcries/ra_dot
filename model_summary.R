library(rstan)
load("C:/Users/dcries/workspace/fit_all.RData")
load("C:/Users/dcries/workspace/fit_min.RData")
load("C:/Users/dcries/workspace/fit_pdo.RData")
load("C:/Users/dcries/workspace/fit_fatmaj.RData")

setwd("C:\\Users\\dcries\\github\\ra_dot")
d <- read.csv('data/completedata_nona.csv')

d$ACCESSCNTL <- as.factor(d$ACCESSCNTL)
d$TRANSCENTE <- as.factor(d$TRANSCENTE)
d$lVOLUME <- log(as.numeric(d$VOLUME))
d$FEDFUNC <- as.factor(d$FEDFUNC)
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


Xall <- as.data.frame(model.matrix(
  ~ TRANSCENTE+lVOLUME+FEDFUNC+MEDTYPE2+SYSCODE+URBAN+MEDWIDTH+TERRAIN+PSIRATING+
    ACCESSCNTL+COMNETWORK+NATHWYSYS+
    SURFTYPE2, data=d))


fitmat <- as.matrix(fit)
fitmat <- fitmat[,-ncol(fitmat)]

meanest <- colMeans(fitmat)
sdpost <- apply(fitmat,2,sd)
quant <- t(apply(fitmat,2,quantile,probs=c(0.025,0.975)))
sumtable <- data.frame(cbind(meanest,sdpost,quant))

names(sumtable) <- c("Estimate","Posterior SD", "2.5q","97.5q")
rownames(sumtable) <- c()