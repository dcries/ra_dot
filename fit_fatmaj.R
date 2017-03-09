library(StanHeaders)
library(rstan)


setwd("/home/dcries/ra_dot")
load("/home/dcries/ra_dot/neighbors.RData")

d <- read.csv('/home/dcries/ra_dot/data/completedata_nona.csv')

d$ACCESSCNTL <- as.factor(d$ACCESSCNTL)
d$TRANSCENTE <- as.factor(d$TRANSCENTE)
d$VOLUME <- as.numeric(d$VOLUME)

X <- as.data.frame(model.matrix(
  ~ ACCESSCNTL+TRANSCENTE+VOLUME, data=d))



dataList = list(
  Y = d$FATMAJCRASHES,
  N = length(d$FATMAJCRASHES),
  ljno0=ljno0,
  lj0=lj0 , 
  lu = lu,
  log_l = log(d$MILES),
  X = X,
  K = length(X[1,]),
  Nseg=length(unique(d$TASLINKID)),
  j = j, k=k,
  w=w,
  scolw = scolw,
  omub=omub,nomub=nomub,
  lomub=length(omub),lnomub=length(nomub)
)

### For paralellizing
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


fit <- stan(file = 'Model12_Fitting_5.stan',
            data = dataList,
            #pars="Beta",
            pars=c("Beta","sigmav","sigmau"),
            chains = 4, iter=4000)   ##Compiling the model

save(fit,file="fit_fatmaj.RData")
#----------------------------------------#







