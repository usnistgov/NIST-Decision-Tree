# Decision Tree Methods

library(symmetry)
library(R2jags)

source('../R/utils.R')

#source('../R/DOE/DoEUnilateralDL.R')
#source('../R/DOE/sampleFromTau2Dist.R')
source('../R/DOE/symmetricalBootstrapCI.R')
#source('../R/DOE/KCplotDoEplot.R')

# 1. DSL

run_method = function(method,x,u,dof) {
  
  res = list()
  
  # AWA
  if(method == 'AWA') {
    
    DLres = metafor::rma(yi=x, 
                         sei=u,
                         level=95,
                         method="DL")
    
    res$method = "Adaptive Weighted Average"
    res$mu = DLres$beta
    res$mu_upper = DLres$ci.ub
    res$mu_lower = DLres$ci.lb
    res$tau = sqrt(DLres$tau2)
    res$se = DLres$se
    
    return(res)
    
  # WM
  } else if(method == 'WM') {
    
    res$mu = spatstat.geom::weighted.median(x, 1/u^2)
    
    if(length(x) >= 12) {
      
      res$method = "Weighted Median with Nonparametric Bootstrap"
      
      xm = function (xw,i) {
        spatstat::weighted.median(xw[i,1], xw[i,2])
      }
      
      xw = cbind(x, 1/u^2)
      withProgress({
        xm.boot = boot(xw, xm, R=input$num_median_bootstrap)
        bootCI = boot.ci(xm.boot, conf=0.95,type='perc')
      },
      value=.5,
      message='Running Bootstrap...')
      
      res$se = sd(xm.boot$t)
      res$mu_lower = bootCI$percent[4]
      res$mu_upper = bootCI$percent[5]
      res$tau = NULL
      
    } else {
      
      res$method = "Weighted Median with Parametric (Laplace) Bootrap"
      
      mu_laplace = res$mu
      weights = 1/u^2
      weights = weights/sum(weights)
      b_laplace = sum(abs(x - mu_laplace)*weights)
      
      nboot = 1000
      boot_samples = rmutil::rlaplace(n=nboot,m=mu_laplace,s=b_laplace)
      
      res$se = sd(boot_samples)
      hw = symmetricalBootstrapCI(boot_samples,res$mu,.95)
      res$mu_upper = res$mu + hw
      res$mu_lower = res$mu - hw
      res$tau = NULL
      res$boot_samples = boot_samples
      
    } 
    
    return(res)
   
  # Bayes
    
  } else {
    
    if(method == 'HGG') {
    
      res$method = "Hierarchical Guass-Gauss"
      stan_filename = '../R/Stan/hgg.stan'
      jags_filename = '../R/Jags/hgg.txt'
      
      
    } else if(method == 'HLG') {
      
      res$method = "Hierarchical Laplace-Gauss"
      stan_filename = '../R/Stan/hlg.stan'
      jags_filename = '../R/Jags/hlg.txt'
      
      
    } else if(method == 'HSSG') {
      
      res$method = "Skew Student-Gauss"
      stan_filename = '../R/Stan/hssg.stan'
      jags_filename = '../R/Jags/hssg2.txt'
      
    }
    
    tps = mad(x)
    sps = median(u)
    
    model_inits = function() {
      list(mu = mean(x),
           tau = sqrt(var(x)),
           lambda = x,
           sigma=u)
    }
    
    model_data = list(N=length(x), 
                      x=x, 
                      u2=u^2, 
                      dof=dof, 
                      med_abs_dif=tps,
                      sigma_prior_scale = sps)
    
    parameters_to_save = c('mu','tau','lambda','sigma')
    
    if(method == 'HSSG') {
      
      model_inits = function() {
        list(mu = mean(x),
             tau = sqrt(var(x)),
             sigma=u)
      }
      
      model_data$nu_prior_scale = 1
      
      parameters_to_save = c(parameters_to_save,'delta','nu')
      
    }
    
    jags_out = jags(data = model_data,
                    inits = model_inits,
                    model.file=jags_filename,
                    parameters.to.save = parameters_to_save,
                    n.chains = 4,
                    n.iter = 20000,
                    n.burnin = 5000)
    
    p_samples = jags_out$BUGSoutput$sims.list
    
    res$mu = mean(p_samples$mu)
    res$mu_samples = p_samples$mu
    
    hw = symmetricalBootstrapCI(p_samples$mu,res$mu,.95)
    res$mu_upper = res$mu + hw
    res$mu_lower = res$mu - hw
    res$se = sd(p_samples$mu)
    res$tau = mean(p_samples$tau)
    res$tau_samples = p_samples$tau
    res$tau_lower = quantile(p_samples$tau,.025)
    res$tau_upper = quantile(p_samples$tau,.975)
    #res$p_samples = p_samples
    
    return(res)
  
  }
  
}

# example

n = 5
tau = 2
lambda = rnorm(n,0,tau)
u = runif(n,1,2)
x = lambda + u
dof = rep(50,n)

res = run_method('AWA',x,u,df) 

res = run_method("WM",x,u,dof)

res = run_method("HGG",x,u,dof)

res = run_method("HLG",x,u,dof)

res = run_method("HSSG",x,u,dof)


  
