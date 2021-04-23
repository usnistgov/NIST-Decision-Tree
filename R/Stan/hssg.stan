functions {
   real skew_student_t_lpdf(real x, real nu, real xi,
                            real omega, real alpha) {
      // nu    = Number of degrees of freedom
      // xi    = Location parameter
      // omega = Scale parameter
      // alpha = Skewness parameter
      real z; real zc;
      if (omega <= 0) reject(omega);
      z = (x-xi)/omega;
      zc = alpha*z*sqrt((nu+1)/(nu+square(z)));
      return log(2) - log(omega) + student_t_lpdf(z | nu, 0, 1) +
             student_t_lcdf(zc | nu+1, 0, 1);
   }

   // Azzalini & Capitanio (2014, (2.6), (4.15)-(4.17))
   // delta = alpha/sqrt(1+alpha^2)
   // b.nu = sqrt(nu/pi)*gamma((nu-1)/2)/gamma(nu/2)
   // Mean of the random effects distribution
   // mu = xi + omega * b.nu * delta
   // Standard deviation of the random effects distribution
   // tau = omega*sqrt(nu/(nu-2) - (b.nu*delta)^2)

   real delta_fn (real alpha) {return alpha/sqrt(1+square(alpha));}

   real bnu_fn (real nu) {return sqrt(nu/pi())*tgamma((nu-1)/2)/tgamma(nu/2);}

   // xi is the location parameter of the skew t distribution
   // xi does not appear explicitly in this code
   // omega is the scale parameter of the skew t distribution
   // omega does not appear explicitly in this code:
   //       it is computed when needed using omega_fn
   real omega_fn (real tau, real nu, real alpha) {
      return tau / sqrt(nu/(nu-2) - square(bnu_fn(nu)*delta_fn(alpha))); }
}

data {
  int<lower=1> N;       // Number of participants
  vector[N] x; // Measured values
  vector<lower=0>[N] u2; // Std. uncertainties associated with measured values
  vector<lower=1>[N] dof;
  real<lower=0> med_abs_dif; // Prior median for tau
}

parameters {
  real mu; // Measurand
  real<lower=0> tau; // SD of random effects
  real lambda[N]; // Means of measured values
  vector<lower=0>[N] sigma; // within lab true std dev
  
  // Ensure finite 3rd moment for random effects distribution
  real<lower=3> nu; // Tail heaviness for random effects distribution
  real alpha; // Skewness parameter of random effects distribution
}

model {
  nu ~ gamma(3, 0.25)T[3,]; // Number of DF for random effects distribution
  tau ~ cauchy(0, med_abs_dif)T[0,]; // Half Cauchy prior for SD of random effects
  mu ~ normal(0.0, 1.0E5); // Prior for measurand 
  alpha ~ normal(0,4); // Prior for skewness parameter
  
  for (ii in 1:N) {
    // Azzalini & Capitanio (2014, (2.6), (4.15)-(4.17))
    // delta = alpha/sqrt(1+alpha^2)
    // b.nu = sqrt(nu/pi)*gamma((nu-1)/2)/gamma(nu/2)
    // mu = xi + omega * b.nu * delta
    // tau = omega*sqrt(nu/(nu-2) - (b.nu*delta)^2)
    lambda[ii] ~ skew_student_t(nu,
                // xi
                mu - omega_fn(tau, nu, alpha) * bnu_fn(nu) * delta_fn(alpha),
                // omega
                omega_fn(tau, nu, alpha),
                alpha);
    
    u2[ii] ~ gamma(dof[ii]/2,dof[ii]/(2*sigma[ii]^2) ); // observed sample std dev;
    x[ii] ~ normal(lambda[ii], sigma[ii]);
  }
}

