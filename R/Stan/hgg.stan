data {
  int<lower=1> N; // number of labs
  vector[N] x; // lab sample means 
  vector<lower=0>[N] u2; // lab sample variances
  vector<lower=1>[N] dof; // degrees of freedom
  real<lower=0> med_abs_dif; // median absolute difference
}

parameters {
  real mu; // overall mean of lab effects (lambda)
  real<lower=0> tau; // std deviation of lab effects (lambda)
  real lambda[N]; // lab effects
  real<lower=0> sigma[N]; // lab specific std error of x
}


model {
  
  // priors
  mu ~ normal(0,10^5); // overall mean
  tau ~ cauchy(0,med_abs_dif)T[0,]; // std dev of true lab means
  
  sigma ~ cauchy(0,med_abs_dif); // std dev of observed mean, given true lab mean
  lambda ~ normal(mu,tau); // true lab means
  
  for(ii in 1:N) {


    // likelihood
    u2[ii] ~ gamma(dof[ii]/2,dof[ii]/(2*sigma[ii]^2) ); // observed sample std dev
    x[ii] ~ normal(lambda[ii],sigma[ii]);               // observed sample mean
    
  }
}

