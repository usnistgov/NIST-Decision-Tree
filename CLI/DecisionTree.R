options(warn=-1)

#library(shiny)
#library(shinythemes)
library(symmetry)
library(ggplot2)
#library(rstan)
#library(shinyjs)
library(R2jags)
#library(rhandsontable)
library(tinytex)

# decision tree
source('../R/utils.R')
#source('../R/inputAndDTmodule.R')
#source('../R/resultsModule.R')
source('../R/dt_choice.R')

source('../R/DOE/DoEUnilateralDL.R')
source('../R/DOE/sampleFromTau2Dist.R')
source('../R/DOE/symmetricalBootstrapCI.R')
source('../R/DOE/KCplotDoEplot.R')

options(warn=0)

args = commandArgs(trailingOnly = TRUE)

args_list = list()

for(ii in 1:length(args)) {
  strs = strsplit(args[ii],'=')[[1]]
  param_name = strs[1]
  param_val = strs[2]
  args_list[param_name] = param_val
}

data = read.csv(args_list$file)

dtres = get_DT_decision(x=data$MeasuredValue,
                        u=data$StandardUncertainty,sizeHetero=0.10, sizeGauss=0.05, sizeSym=0.05,exclude=NULL)

# now compute the result
res = list()

if(dtres == 'AWA') {
  
  DLres = metafor::rma(yi=data$MeasuredValue, 
                       sei=data$StandardUncertainty,
                       level=95,
                       method="DL")
  
  res$method = "Adaptive Weighted Average"
  res$mu = DLres$beta
  res$mu_upper = DLres$ci.ub
  res$mu_lower = DLres$ci.lb
  res$tau = sqrt(DLres$tau2)
  res$se = DLres$se
  
  doe_res = DoEUnilateralDL(data$Result,
                            data$Uncertainty,
                            data$DegreesOfFreedom,
                            data$Laboratory,
                            1000, # number bootstrap
                            FALSE, # LOO
                            .95, # coverage prob
                            DLres) # dl res
  
  
  
} else if(dtres == 'WM') {
  
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
    
  }
  
  
} else {
  
  if(dtres == 'HGG') {
    
    res$method = "Hierarchical Guass-Gauss"
    stan_filename = '../R/Stan/hgg.stan'
    jags_filename = '../R/Jags/hgg.txt'
    
    
  } else if(dtres == 'HLG') {
    
    res$method = "Hierarchical Laplace-Gauss"
    stan_filename = '../R/Stan/hlg.stan'
    jags_filename = '../R/Jags/hlg.txt'
    
    
  } else if(dtres == 'HSSG') {
    
    res$method = "Skew Student-Gauss"
    stan_filename = '../R/Stan/hssg.stan'
    jags_filename = '../R/Jags/hssg2.txt'
    
  }
  
  x = data$MeasuredValue
  u = data$StandardUncertainty
  dof = data$DegreesOfFreedom
  
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
  
  if(dtres == 'HSSG') {
    
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
                  n.iter = 8000,
                  n.burnin = 3000)
  
  p_samples = jags_out$BUGSoutput$sims.list
  
  res$mu = mean(p_samples$mu)
  hw = symmetricalBootstrapCI(p_samples$mu,res$mu,.95)
  res$mu_upper = res$mu + hw
  res$mu_lower = res$mu - hw
  res$se = sd(p_samples$mu)
  res$tau = mean(p_samples$tau)
  res$tau_lower = quantile(p_samples$tau,.025)
  res$tau_upper = quantile(p_samples$tau,.975)
  #res$p_samples = p_samples
  
}

rmarkdown::render(input="../R/ResultsDownload.Rmd", 
                  output_file='DecisionTreeResults.pdf',
                  params = list(res = res,
                                vars_in = vars_in,
                                the_proc = the_proc,
                                doe_res = doe_res))

