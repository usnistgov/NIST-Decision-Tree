data {
  int<lower=1> N;
  vector[N] x;
  vector<lower=0>[N] u2;
  vector<lower=1>[N] dof;
  real<lower=0> med_abs_dif;
}

parameters {
  real mu;
  real<lower=0> tau;
  real lambda[N];
  real<lower=0> sigma[N];
}


model {
  
  // priors
  mu ~ normal(0,10^5); // overall mean
  tau ~ cauchy(0,med_abs_dif); // std dev of true lab means

  sigma ~ cauchy(0,med_abs_dif); // std dev of observed mean, given true lab mean
  lambda ~ double_exponential(mu,tau/sqrt(2)); // true lab means
  
  for(ii in 1:N) {

    // likelihood
    u2[ii] ~ gamma(dof[ii]/2,dof[ii]/(2*sigma[ii]^2) ); // observed sample std dev
    x[ii] ~ normal(lambda[ii],sigma[ii]);               // observed sample mean
    
  }
}
