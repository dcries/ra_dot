library(rstan)


setwd("C:\\Users\\dcries\\github\\ra_dot\\data")
load("//my.files.iastate.edu/Users/dcries/Desktop/RA_DOT/neighbors.RData")

d <- read.csv('completedata_nona.csv')

d$ACCESSCNTL <- as.factor(d$ACCESSCNTL)
d$TRANSCENTE <- as.factor(d$TRANSCENTE)
d$VOLUME <- as.numeric(d$VOLUME)

X <- as.data.frame(model.matrix(
  ~ ACCESSCNTL+TRANSCENTE+VOLUME, data=d))



dataList = list(
  Y = d$FATMAJCRASHES,
  N = length(d$CRASHES),
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

fit <- stan(file = '..\\Model12_Fitting_5.stan',
            data = dataList,
            #pars="Beta",
            pars=c("Beta","lambda"),
            chains = 1, iter=100)   ##Compiling the model

#----------------------------------------#

chains <- 3   ## Number of chains when parapelizing (do not ask for to many or the computer will crash)
iter <- 3000  ## Length of the generated chains
warm <- 1000    ### Number using for the warming in the chains
thin <- 2 ## Thinning parameter for the chains


### For paralellizing
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

### Parallel will not work if "RMark", "snowfall" or "snow" are loades:
#unloadNamespace("RMark")
#unloadNamespace("snowfall")
#unloadNamespace("snow") 

fit <- stan(fit=fit,
            # pars="Beta",
            pars=c("Beta","tau2u","tau2v","u","v","lambda"),
            data = dataList, chains = chains,
            iter=iter,thin=thin,warmup = warm,
            algorithm = "NUTS",refresh=10,  ## max_treedepth is for NUTS
            control = list(adapt_delta = .9,max_treedepth= 12))
