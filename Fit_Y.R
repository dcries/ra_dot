### Fitting best model for Y

#setwd("C:/Users/Eduardo/Desktop/DOT_2015/Data_Complete/Y1")

library("rstan")
library("dplyr")
#library("snow")
library("markovchain")
#source( "C:\\Users\\Eduardo\\Desktop\\tesis\\openGraphSaveGraph.R")
#source("C:\\Users\\Eduardo\\Desktop\\DOT_2015\\sflist2stanfit.txt")

#r <- 5 ##region#

d <- read.table("C:\\Users\\Eduardo\\Desktop\\DOT2016\\Data_Ready_2016.txt",header = T)
###### Taking subsets###################
#cont <- d$REGION==r
#####################################################################
#d <- d[cont,]
d <- d[order(d$TASLINKID,d$YEAR),]

names(d)


tr <- d$AUTOMOBILE + d$MOTORCYCLE +d$PICKUP + d$BUS + d$SINGLEUNIT + d$SINGMULTTR

d$log_VOLUME <- log(d$VOLUME)

d$AUTOMOBILE_r <- d$AUTOMOBILE/tr
d$BUS_r<- d$BUS / tr
d$MOTORCYCLE_r <- d$MOTORCYCLE / tr
d$SINGLEUNIT_r <- d$SINGLEUNIT / tr
d$PICKUP_r <- d$PICKUP / tr

#d$MEDTYPE <- as.factor( replace(d$MEDTYPE , !d$MEDTYPE%in%2,"no_2" )  ) ## Cuidado: casi todos son 2
# d$MEDTYPE <- replace(d$MEDTYPE , d$MEDTYPE<3 , "_PV_SL(nBR)"  )
# d$MEDTYPE <- replace(d$MEDTYPE , d$MEDTYPE%in%c("3","4") , "_PV_SL(BR)"  )
# d$MEDTYPE <- as.factor(replace(d$MEDTYPE , d$MEDTYPE%in%c("5") , "_BR"  ))
d$MEDTYPE <- replace(d$MEDTYPE , d$MEDTYPE<3 , "_no_PV_SL(BR)"  )
d$MEDTYPE <- replace(d$MEDTYPE , d$MEDTYPE%in%c("3","4") , "_PV_SL(BR)"  )
d$MEDTYPE <- as.factor(replace(d$MEDTYPE , d$MEDTYPE%in%c("5") , "_no_PV_SL(BR)"  ))

#d$LIMITMPH1 <- d$LIMITMPH
d$LIMITMPH[d$LIMITMPH<70] <- "_less_70"
d$LIMITMPH[d$LIMITMPH=="70"] <- "_70"
d$LIMITMPH <- as.factor(d$LIMITMPH)

#d$LIMITMPH <- as.factor(d$LIMITMPH)
d$log_MEDWIDTH <-  log(d$MEDWIDTH)

d$NUMLANES <- replace(d$NUMLANES  , d$NUMLANES<=5 , "_5_less")
d$NUMLANES[d$NUMLANES=="6"] <- "_6"
d$NUMLANES <- replace(d$NUMLANES  , d$NUMLANES=="7" | d$NUMLANES=="8" | d$NUMLANES=="9", "_7_more")

d$NUMLANES2 <- replace(d$NUMLANES  , d$NUMLANES=="_6" , "_6_more")
d$NUMLANES2 <- replace(d$NUMLANES2  , d$NUMLANES2=="_7_more" , "_6_more")

d$NUMLANES <- as.factor(d$NUMLANES)
d$NUMLANES2 <- as.factor(d$NUMLANES2)

d$PSIRATING <- d$PSIRATING

d$SURFTYPE <- replace(d$SURFTYPE , d$SURFTYPE%in%c(60,61,65,69,92) , "_asphalt" )
d$SURFTYPE <- as.factor(replace(d$SURFTYPE , d$SURFTYPE%in%c(70,74,76,77,79) , "_concrete" ))

#d$SURFWIDTH <- d$SURFWIDTH/100

X <- as.data.frame(model.matrix(
  ~ log_VOLUME+NUMLANES2+MEDTYPE+log_MEDWIDTH  +SURFTYPE+ 
      LIMITMPH  , data=d))
# X <- as.data.frame(model.matrix(
#   d$CRASHES ~ log_VOL + AUTOMOBILE_r +
#     BUS_r+MOTORCYCLE_r+ SINGLEUNIT_r + PICKUP_r +
#     MEDTYPE + LIMITMPH + log_MEDWIDTH + NUMLANES +
#     PSIRATING + SURFTYPE , data=d))



############################# Computing the matrix of neighbors w
d <- as.data.frame(d)
d$ord <- 1:length(d$VOLUME)
g <- group_by(d,TASLINKID)
s <- summarise_each(g , funs(min) , ord)   ### if there are repeated ROADLINK, I take the first set of data
ft <- d[s$ord,c("TRNNODE_F","TRNNODE_T")]
ft <- as.matrix(ft)

f <- function(x,v){
  apply(v , 1 , FUN = function(z) sum(x%in%z))
}

## mATRIX
w <- apply(X=matrix(1:length(ft[,1])), MARGIN = 1 ,
           FUN = function(x) f( ft[x,],ft )  )
w <- w-diag(2,length(w[,1]))
scolw = rowSums(w)
table(scolw)

j <- 1:length(unique(d$TASLINKID))
aux <- table(d$TASLINKID)
j <- rep(j,aux)

### sub_Indices of j que me dicen cuales son las que tienen vecinos 
# They tell me which ones have neighbors
aux_scolw <- rep(scolw,aux)
j0 <- (1:length(d$VOLUME))[aux_scolw==0] 
jno0 <- (1:length(d$VOLUME))[aux_scolw>0]
lj0 <- length(j0)
ljno0 <- length(jno0)

k <- j[jno0]
k <- rep( 1:length(unique(k)),table(k))

lu <- sum(scolw>0)  ## number of sections with neighbprs
w <- w[scolw>0,scolw>0]
scolw = scolw[scolw>0]

#########################

transition <- sweep(w,1,scolw,FUN = "/")
colnames(transition) <- 1:length(transition[1,])
rownames(transition) <- 1:length(transition[1,])
markov <- new("markovchain" , transitionMatrix = transition)
com <- communicatingClasses(markov)  ### the sections wich are communicated

omub <- numeric(0)
for(i in 1:length(com))
  omub <- c(omub,com[[i]][1])
omub <- as.numeric(omub)          ## the first element of each of the comunicating classes
nomub <- (1:lu)[! (1:lu)%in% omub]

###########################



dataList = list(
  Y = d$CRASHES,
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




fit <- stan(file = 'C:\\Users\\Eduardo\\Desktop\\DOT_2015\\Tri_Pois_Restric_Cov\\Model12\\Model12_Fitting_5.stan',
            data = dataList,
            #pars="Beta",
            pars=c("Beta","tau2u","tau2v","u","v","lambda"),
            chains = 1, iter=1)   ##Compiling the model

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
# fit <- stan(fit=fit,
#             pars=c("u","v","Beta","tau2u","tau2v","lambda"),
#      data = dataList, chains = chains,
#      iter=iter,thin=thin,warmup = warm,
#      algorithm = "NUTS",refresh=2,  ## max_treedepth is for NUTS
#      control = list(adapt_delta = .9,max_treedepth= 12))

#fit2 <- sflist2stanfit(fit)
na <- names(X)
r <- "All"
save(na,r,fit, file = paste("Model_Y_REGION_",r,"_VOL_MPH_NUMLANES2_MEDTYPE_MEDWIDTH_SURFTYPE.RData",sep = ""))


#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())
