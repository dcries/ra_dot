library(rjags)


setwd("C:\\Users\\dcries\\github\\ra_dot\\data")
load("//my.files.iastate.edu/Users/dcries/Desktop/RA_DOT/neighbors.RData")

d <- read.csv('completedata_nona.csv')
#d <- d[1:2000,]

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
  lomub=length(omub),lnomub=length(nomub),
  m0=rep(0,ncol(X)),
  V0=diag(ncol(X))/100,
  jno0=jno0,j0=j0
)

crash <- "
model{
  for(i in 1:N){
    XB[i] <- inprod(X[i,],beta)
    Y[i] ~ dpois(lambda[i])
    v[i] ~ dnorm(0,tau2v)
    #lambda[i] <- exp(XB[i] + log_l[i] ) 
  }
  for(i in 1:lj0){
    lambda[j0[i]] <- exp(XB[j0[i]]  + log_l[j0[i]] + v[j[j0[i]]])
  }

  for(i in 1:ljno0){
    lambda[jno0[i]] <-  exp(XB[jno0[i]]  + log_l[jno0[i]] + v[j[jno0[i]]] +u[k[i]]) #not sure about this
  }
  for(i in 1:lu){
    mub[i] <- inprod(w[i,],u)/sum(w[i,])   #not sure about this division
    tau2b[i] <- tau2u/sum(w[i,])         #not sure about this
    u[i] ~ dnorm(mub[i],tau2b[i])
  }

  beta ~ dmnorm(m0,V0)
  tau2v ~ dgamma(0.5,0.05)
  tau2u ~ dgamma(0.5, 0.05)

  sigmav <- 1/sqrt(tau2v)
  sigmau <- 1/sqrt(tau2u)

}"

mjc <- jags.model(textConnection(crash),dataList,n.chains=1,n.adapt=10000)
crashsamp <- coda.samples(mjc,c("beta","sigmav"),n.iter=100)
