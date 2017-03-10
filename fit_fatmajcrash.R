library(StanHeaders)
library(rstan)


setwd("C:\\Users\\dcries\\github\\ra_dot")
setwd("/home/danny/Documents/github/ra_dot/")
load("neighbors.RData")
load("neighbors_subset.RData")

d <- read.csv('data/completedata_nona.csv')
d <- d[1:2000,]

d$ACCESSCNTL <- as.factor(d$ACCESSCNTL)
d$TRANSCENTE <- as.factor(d$TRANSCENTE)
d$VOLUME <- as.numeric(d$VOLUME)
d$lVOLUME <- log(d$VOLUME)

X <- as.data.frame(model.matrix(
  ~ ACCESSCNTL+TRANSCENTE+lVOLUME, data=d))



dataList = list(
  N = length(d$CRASHES),
  Y = d$FATMAJCRASHES,
  ljno0=ljno0,
  lj0=lj0 , 
  lu = lu,
  log_l = log(d$MILES),
  X = as.matrix(X),
  K = length(X[1,]),
  Nseg=length(unique(d$TASLINKID)),
  j = j, k=k,
  w=w,
  scolw = scolw,
  omub=omub,nomub=nomub,
  lomub=length(omub),lnomub=nrow(w)
)

### For paralellizing
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


model <- stan_model(file='Model12_Fitting_5.stan')
fit <- sampling(model,data=dataList,chains=4,iter=1000,pars=c("Beta","sigmav","sigmau"))#,iter=1000,control = list(adapt_delta = .9))

fitdf <- as.data.frame(fit)
names(fitdf) <- tolower(names(fitdf))


fit <- stan(file = 'Model12_Fitting_5.stan',
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
