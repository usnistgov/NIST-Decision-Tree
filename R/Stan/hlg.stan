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
  tau ~ cauchy(0,med_abs_dif)T[0,]; // std dev of true lab means
  
  for(ii in 1:N) {
    sigma[ii] ~ cauchy(0,med_abs_dif)T[0,]; // std dev of observed mean, given true lab mean
    lambda[ii] ~ double_exponential(0,tau); // true lab means

    // likelihood
    u2[ii] ~ gamma(dof[ii]/2,dof[ii]/(2*sigma[ii]^2) ); // observed sample std dev
    x[ii] ~ normal(mu + lambda[ii],sigma[ii]);               // observed sample mean
    
  }
}
