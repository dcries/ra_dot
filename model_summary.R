library(rstan)
library(xtable)

load("C:/Users/dcries/workspace/fit_all.RData")
fit_all=fit
load("C:/Users/dcries/workspace/fit_min.RData")
fit_min=fit
load("C:/Users/dcries/workspace/fit_pdo.RData")
fit_pdo=fit
load("C:/Users/dcries/workspace/fit_fatmaj.RData")
fit_fatmaj=fit

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
  ~ TRANSCENTE+lVOLUME+FEDFUNC2+SYSCODE+URBAN+
    SURFTYPE2, data=d))

Xfatmaj <- as.data.frame(model.matrix(
  ~ TRANSCENTE+lVOLUME+
    SURFTYPE2, data=d))

Xmin <- as.data.frame(model.matrix(
  ~ TRANSCENTE+lVOLUME+FEDFUNC2+SYSCODE+URBAN+
    SURFTYPE2, data=d))

Xpdo <- as.data.frame(model.matrix(
  ~ TRANSCENTE+lVOLUME+FEDFUNC2+SYSCODE+URBAN+
    SURFTYPE2, data=d))


fit_all <- as.matrix(fit_all)
fit_min <- as.matrix(fit_min)
fit_pdo <- as.matrix(fit_pdo)
fit_fatmaj <- as.matrix(fit_fatmaj)

fit_all <- fit_all[,-ncol(fit_all)]
fit_min <- fit_min[,-ncol(fit_min)]
fit_pdo <- fit_pdo[,-ncol(fit_pdo)]
fit_fatmaj <- fit_fatmaj[,-ncol(fit_fatmaj)]


meanest_all <- colMeans(fit_all)
sdpost_all <- apply(fit_all,2,sd)
quant_all <- t(apply(fit_all,2,quantile,probs=c(0.025,0.975)))
sumtable_all <- data.frame(cbind(meanest_all,sdpost_all,quant_all))

meanest_min <- colMeans(fit_min)
sdpost_min <- apply(fit_min,2,sd)
quant_min <- t(apply(fit_min,2,quantile,probs=c(0.025,0.975)))
sumtable_min <- data.frame(cbind(meanest_min,sdpost_min,quant_min))

meanest_pdo <- colMeans(fit_pdo)
sdpost_pdo <- apply(fit_pdo,2,sd)
quant_pdo <- t(apply(fit_pdo,2,quantile,probs=c(0.025,0.975)))
sumtable_pdo <- data.frame(cbind(meanest_pdo,sdpost_pdo,quant_pdo))

meanest_fatmaj <- colMeans(fit_fatmaj)
sdpost_fatmaj <- apply(fit_fatmaj,2,sd)
quant_fatmaj <- t(apply(fit_fatmaj,2,quantile,probs=c(0.025,0.975)))
sumtable_fatmaj <- data.frame(cbind(meanest_fatmaj,sdpost_fatmaj,quant_fatmaj))

names(sumtable_all) <- c("Estimate","Posterior SD", "2.5q","97.5q")
rownames(sumtable_all) <- c(names(Xall),"sigmav")
names(sumtable_min) <- c("Estimate","Posterior SD", "2.5q","97.5q")
#rownames(sumtable_min) <- c(names(Xmin),"sigmav")
names(sumtable_pdo) <- c("Estimate","Posterior SD", "2.5q","97.5q")
rownames(sumtable_pdo) <- c(names(Xpdo),"sigmav")
names(sumtable_fatmaj) <- c("Estimate","Posterior SD", "2.5q","97.5q")
rownames(sumtable_fatmaj) <- c(names(Xfatmaj),"sigmav")

print(xtable(sumtable_all,digits=3,caption="Regression and variance parameter summaries for All Crashes Model",label="allcrash"))
print(xtable(sumtable_pdo,digits=3,caption="Regression and variance parameter summaries for PDO Crashes Model",label="pdocrash"))
print(xtable(sumtable_min,digits=3,caption="Regression and variance parameter summaries for Minimum injury Crashes Model",label="mincrash"))
print(xtable(sumtable_fatmaj,digits=3,caption="Regression and variance parameter summaries for Fatal/Major injury Crashes Model",label="fatmajcrash"))
