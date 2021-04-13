

data {
  int<lower=1> N;
  vector[N] x;
  vector<lower=0>[N] u;
  vector<lower=1>[N] dof;
  real<lower=0> med_abs_dif;
}

parameters {
  real<lower=0> tau;
  vector<lower=0>[N] sigma;
  vector[N] lambda;
  real mu;
}


model {
  for(ii in 1:N) {

    // priors
    mu ~ normal(0,10^5);                // overall mean
    tau ~ cauchy(0,med_abs_dif);        // std dev of true lab means
    sigma[ii] ~ cauchy(0,med_abs_dif);  // std dev of observed mean, given true lab mean
    lambda[ii] ~ normal(0,tau);         // true lab means

    // likelihood
    u[ii]^2 ~ gamma(dof[ii]/2,dof[ii]/(2*sigma[ii]^2) ); // observed sample std dev
    x[ii] ~ normal(mu + lambda[ii],sigma);               // observed sample mean
  }

}


