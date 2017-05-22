library(StanHeaders)
library(rstan)


#setwd("/home/dcries/ra_dot")
load("/home/dcries/ra_dot/neighbors_reduced.RData")

d <- read.csv('/home/dcries/ra_dot/data/completedata_nona.csv')

d$ACCESSCNTL <- as.factor(d$ACCESSCNTL)
d$TRANSCENTE <- as.factor(d$TRANSCENTE)
d$lVOLUME <- log(as.numeric(d$VOLUME))
d$FEDFUNC2 <- as.factor(d$FEDFUNC2)
d$MEDTYPE2 <- as.factor(d$MEDTYPE2)
d$SYSCODE <- as.factor(d$SYSCODE)
d$TERRAIN <- as.factor(d$TERRAIN)

d$SHDTYPEL <- as.factor(d$SHDTYPEL)
d$SHDTYPER <- as.factor(d$SHDTYPER)
d$SURFTYPE2 <- 0
d$SURFTYPE2 <- replace(d$SURFTYPE2 , d$SURFTYPE%in%c(60,65,69,92) , 1 )
d$SURFTYPE2 <- replace(d$SURFTYPE2 , d$SURFTYPE%in%c(70,74,76,77,79) , 2 )

X <- as.data.frame(model.matrix(
  ~ TRANSCENTE+lVOLUME+FEDFUNC2+URBAN+
    SURFTYPE2+lVOLUME*FEDFUNC2, data=d))



dataList = list(
  Y = d$PDOCRASH,
  N = length(d$FATMAJCRASHES),
  ljno0=neighbors$ljno0,
  lj0=neighbors$lj0 , 
  jno0 = neighbors$jno0,
  j0 = neighbors$j0,
  lu = neighbors$lu,
  log_l = log(d$MILES),
  X = X,
  K = length(X[1,]),
  Nseg=length(unique(d$TASLINKID)),
  j = neighbors$j, k=neighbors$k,
  w=neighbors$w,
  scolw = neighbors$scolw,
  mub=rep(0,ncol(X)),
  Vb=diag(ncol(X))*1000#,
  #omub=neighbors$omub,nomub=neighbors$nomub,
  #lomub=length(neighbors$omub),lnomub=length(neighbors$nomub)
)


### For paralellizing
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


fit <- stan(file = '/home/dcries/ra_dot/Model12_Fitting_5.stan',
            data = dataList,
            #pars="Beta",
            pars=c("Beta","sigmav","v"),
            chains = 4, iter=10000,thin=5)   ##Compiling the model

save(fit,file="fit_pdo.RData")
#----------------------------------------#







