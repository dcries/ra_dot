data {
int<lower=0> N; // number of data items
int<lower=0> K; // number of predictors including teh interceptor
int<lower=0> Nseg;   // Number of sections characterized by TASLINKID
int<lower=0> ljno0;
int<lower=0> lj0;
int<lower=0> lu;   // ### Number of sections that has neighbors
int<lower=0> lnomub ; 
int<lower=0> lomub ; 
matrix[N,K] X; // predictor matrix
matrix[lu,lu] w;
vector<lower=0> [lu] scolw ;
int<lower=0> Y[N]; // outcome vector
vector[N] log_l;
int<lower=0> j[N]; // index for taslinkid, use to give random effect for segment
int<lower=0> jno0[ljno0]; //index for segments with neighbors
int<lower=0> j0[lj0]; //index for segments without neighbors
int<lower=0> k[ljno0];
int <lower=0> nomub [lnomub] ;
int <lower=0> omub [lomub] ;
}

parameters {
  real<lower=0> tau2u; //  precision for the variance of u
  real<lower=0> tau2v; //  precision for the variance of v
  vector[Nseg] v;   // random interceptor for TASLINID
  vector[lu] u; // random variables: contains the spatial correlation
  vector[K] Beta;   /// fixed effects coefficients
  
}

transformed parameters {
//generated quantities{
  vector[N] lambda;
  vector[lu] mub;
  vector<lower=0>[lu] tau2b;
  vector[N] XB;
  vector<lower=0>[lu] sigmab;
  real<lower=0> sigmav;
  
  for(i in 1:lu){
    sigmab[i] <- 1/sqrt(tau2b[i]);
  }
  sigmav <- 1/sqrt(tau2v);
  
 //  for(i in 1:lomub)
  //    mub[omub[i]] <-0 ;
    
  //  for(i in 1:lnomub)
  //  mub[nomub[i]] <-  dot_product(w[nomub[i]],u) /scolw[nomub[i]];
  
  for(i in 1:lu){
       tau2b[i] <- tau2u / scolw[i]; // ??? is this correct --> tau2u * scolw? writeup says 
   //differently, ie. to divide
  }

  
  XB <- X*Beta;
  
 // for(i in 1:lj0)
 //  lambda[j0[i]] <- exp(XB[j0[i]] + v[j[j0[i]]] + log_l[j0[i]]) ;
  
 // for(i in 1:ljno0)
 //   lambda[jno0[i]] <-  exp(XB[jno0[i]] + v[j[jno0[i]]]+u[ k[i] ] + log_l[jno0[i]]);
 // }
 
 
  for(i in 1:lj0)
    lambda[j0[i]] <- exp(XB[j0[i]] + v[j[j0[i]]] + log_l[j0[i]]) ;
  
  for(i in 1:ljno0)
    lambda[jno0[i]] <-  exp(XB[jno0[i]] + v[j[jno0[i]]] + log_l[jno0[i]]);
  
 
}

model {
  Beta ~ normal(0,.1);
  tau2v ~ gamma(0.5, 0.05);
  tau2u ~ gamma(0.5, 0.05);
  
  u ~ normal(mub,sigmab); //normal(mub,tau2b);
  v ~ normal(0,sigmav);//normal(0,tau2v);
 
  Y  ~ poisson(lambda);
}
