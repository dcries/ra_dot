

setwd("C:\\Users\\dcries\\github\\ra_dot\\data")
d <- read.csv('completedata_nona.csv')
load("\\\\my.files.iastate.edu\\Users\\dcries\\Desktop\\RA_DOT\\workspace\\fit_all.RData")
example <- read.csv('example.csv')

# d$ACCESSCNTL <- as.factor(d$ACCESSCNTL)
# d$TRANSCENTE <- as.factor(d$TRANSCENTE)
# d$lVOLUME <- log(as.numeric(d$VOLUME))
# d$FEDFUNC2 <- as.factor(d$FEDFUNC2)
# d$MEDTYPE2 <- as.factor(d$MEDTYPE2)
# d$SYSCODE <- as.factor(d$SYSCODE)
# d$TERRAIN <- as.factor(d$TERRAIN)
# d$COMNETWORK <- as.factor(d$COMNETWORK)
# d$NATHWYSYS <- as.factor(d$NATHWYSYS)
# 
# d$SHDTYPEL <- as.factor(d$SHDTYPEL)
# d$SHDTYPER <- as.factor(d$SHDTYPER)
# d$SURFTYPE2 <- 0
# d$SURFTYPE2 <- replace(d$SURFTYPE2 , d$SURFTYPE%in%c(60,65,69,92) , 1 )
# d$SURFTYPE2 <- replace(d$SURFTYPE2 , d$SURFTYPE%in%c(70,74,76,77,79) , 2 )
# 
# 
# 
# X <- as.data.frame(model.matrix(
#   ~ TRANSCENTE+lVOLUME+FEDFUNC2+URBAN+
#     SURFTYPE2, data=d))
# 
# #to get prediction intervals
# fit <- as.matrix(fit)
# fit <- fit[,1:10]
# big <- fit%*%t(as.matrix(X))
# cis <- matrix(0,nrow=2,ncol=ncol(big))
# for(i in 1:ncol(big)){
#   cis[,i] <- quantile(big[,i],probs=c(0.025,0.975))
# }
# 
# 
# out <- data.frame(X)
# out$year <- d$YEAR
# out$taslinkid <- d$TASLINKID
# out$xb <- as.matrix(X)%*%c(-7.302,-.118,-.058,-.054,.209,.26,.971,.066,.443,-.005)
# out$v <- 0
# out$xbv <- out$xb+out$v
# out$lambda <- exp(out$xbv)
# out$miles <- d$MILES
# out$expcrash <- out$lambda*out$miles
# out$lb_lambda <- exp(cis[1,])
# out$ub_lambda <- exp(cis[2,])
# out$lb_expcrash <- out$lb_lambda*out$miles
# out$ub_expcrash <- out$ub_lambda*out$miles
# 
# write.csv(out,file="example.csv",row.names=FALSE)


