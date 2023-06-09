select_test = function(chosen_test,rvs) {
  # changes all reactive values to 0 except for the selected proceudure
  # e.g. select_test('hgg', <reactive values>)
  test_names = c('awa','wmed','hgg','hlg','hssg')
  match = chosen_test == test_names
  
  for(ii in 1:length(test_names)) {
    if(match[ii]) {
      rvs[[ test_names[ii] ]] = 1
    } else {
      rvs[[ test_names[ii] ]] = 0
    }
  }
  
  return(rvs)
}

get_test_name = function(which_test) {
  test_names = c('awa','wmed','hgg','hlg','hssg')
  
  if(which_test[[ test_names[1] ]] == 1) {
    return("Adaptive Weighted Average")
  } else if(which_test[[ test_names[2] ]] == 1) {
    return("Weighted Median")
  } else if(which_test[[ test_names[3] ]] == 1) {
    return("Hierarchical Gauss-Gauss")
  } else if(which_test[[ test_names[4] ]] == 1) {
    return("Hierarchical Laplace-Gauss")
  } else if(which_test[[ test_names[5] ]] == 1) {
    return("Hierarchical Skew Student-Gauss")
  } else {
    return("No test recommended.")
  }
}

all_false = function(which_test) {
  test_names = c('awa','wmed','hgg','hlg','hssg')
  
  if(which_test[[ test_names[1] ]] == 1) {
    return(FALSE)
  } else if(which_test[[ test_names[2] ]] == 1) {
    return(FALSE)
  } else if(which_test[[ test_names[3] ]] == 1) {
    return(FALSE)
  } else if(which_test[[ test_names[4] ]] == 1) {
    return(FALSE)
  } else if(which_test[[ test_names[5] ]] == 1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

unselect_test = function(test,rvs) {
  rvs[[ test ]] = 0
  return(rvs)
}

clear_selections = function(rvs) {
  test_names = c('awa','wmed','hgg','hlg','hssg')
  
  for(ii in 1:length(test_names)) {
    rvs[[ test_names[ii] ]] = 0
  }
  
  return(rvs)
}

has_correct_format = function(input) {
  grepl("^\\d+,\\d+(,\\d+)+$",input)
}

# from NICOB_app_public/server.R 5/9/22
bootDL = function(K,thedat,themle){
  
  muB = numeric(K)
  n=nrow(thedat)
  
  x = thedat$x
  u = thedat$u
  dof = thedat$dof
  
  for (k in 1:K) {
    
    tau2B = sampleFromTau2Dist(x,u)
    xB = rnorm(n, mean=themle$mu, sd=sqrt(tau2B+u^2))
    uB=rep(NA,n)
    
    if (is.null(dof)) {
      uB = u
      
      } else {
        uB = u*sqrt(dof/rchisq(n, df=dof))
        
      } #simulate new uncertainties
    
    
    muB[k] = rma(yi=xB, sei=uB, method="DL")$beta ### don't need modified version, only outputting b
  }
  
  return(muB)
}

# from NICOB_app_public/sampleFromTau2Dist.R 5/9/22
sampleFromTau2Dist = function(y,sigma){
  ######################################################################
  ## Data specific variables
  ######################################################################
  K=length(y)
  w=sigma^(-2)
  
  S_r=function(w,r){
    sum(w^r)
  }
  
  S1=S_r(w,1)
  S2=S_r(w,2)
  S3=S_r(w,3)
  
  c=S1-S2/S1
  muhat=sum(w*y)/sum(w)
  Q=sum(w*(y-muhat)^2)
  if ((S1-S2/S1)!=0)
    tau2_hat_M=(Q-(K-1))/(S1-S2/S1)
  else
    tau2_hat_M=0
  tau2_hat_DL=max(0,tau2_hat_M)
  
  ######################################################################
  ## Functions for expected value and variance of Cochran's Q, given in 
  ## equations (5) and (7) of Biggerstaff and Tweedie (1997).
  ######################################################################
  
  E_Q=function(tau2,weight_vec){
    S1=S_r(weight_vec,1)
    S2=S_r(weight_vec,2)
    S3=S_r(weight_vec,3)
    
    K=length(weight_vec)
    
    (K-1)+(S1-S2/S1)*tau2
  }
  
  Var_Q=function(tau2,weight_vec){
    S1=S_r(weight_vec,1)
    S2=S_r(weight_vec,2)
    S3=S_r(weight_vec,3)
    
    K=length(weight_vec)
    
    2*(K-1)+4*(S1-S2/S1)*tau2+2*(S2-2*S3/S1+S2^2/S1^2)*tau2^2
  }
  
  ######################################################################
  ## Functions to calculate the shape and scale parameters for gamma 
  ## distribution approximation to distribution of Q.
  ######################################################################
  ### This function was returning Inf when all of the SEs were equal, making Var_Q = 0, 
  ### this conditional fixes that but makes the tau sampler always return 0, might not be what we want
  lambda_tau2=function(tau2,data_w){ 
    
    if(Var_Q(tau2,weight_vec=data_w)==0){
      E_Q(tau2,weight_vec=data_w)/.0000000001
    }else{
      E_Q(tau2,weight_vec=data_w)/Var_Q(tau2,weight_vec=data_w)
    }
  }
  r_tau2=function(tau2,data_w){
    if(Var_Q(tau2,weight_vec=data_w)==0){
      (E_Q(tau2,weight_vec = data_w))^2/.0000000001
    }else{
      (E_Q(tau2,weight_vec = data_w))^2/Var_Q(tau2,weight_vec = data_w)
    }
  }
  
  
  ######################################################################
  ## Simulate from approximate gamma distribution for tau^2_M to generate 
  ## a sample from the approximate distribution for tau^2.
  ######################################################################
  
  r=r_tau2(tau2_hat_M,w)
  lambda=lambda_tau2(tau2_hat_M,w)
  
  transformed_tau2_M=rgamma(n=1,shape=r,scale=1/lambda)
  if (c!=0)
    r_tau2_DL=max(0,(transformed_tau2_M-(K-1))/c)
  else
    r_tau2_DL=0
  return(r_tau2_DL)
  
}

run_ndt_method = function(x,
                          u,
                          dof,
                          the_proc,
                          num_bootstrap,
                          jags_params,
                          seed,
                          priors) {
  
  set.seed(abs(round(seed)))
  res = list()
  n = length(x)
  
  if(is.null( the_proc )) {
    return(NULL)
  }
  
  # adaptive weighted average
  if(grepl('average',the_proc,TRUE)) {
    DLres = DerSimonianLaird(x=x,ux=u)
    
    rma_res = metafor::rma(yi=x,sei=u,method="DL")
    
    res$method = "Adaptive Weighted Average"
    
    bootDL_res = bootDL(num_bootstrap,
                        thedat=data.frame(x=x,u=u,dof=dof),
                        themle=list(mu=DLres$beta))
    
    
    
    
    
    res$se_dslbs = sd(bootDL_res)
    hw_dslbs = symmetricalBootstrapCI(bootDL_res,as.numeric(DLres$beta),.95)
    res$mu_upper_dslbs = DLres$beta + hw_dslbs
    res$mu_lower_dslbs = DLres$beta - hw_dslbs
    
    res$mu = DLres$beta
    res$mu_upper = rma_res$ci.ub
    res$mu_lower = rma_res$ci.lb
    res$tau = sqrt(DLres$tau2)
    res$se = DLres$se
    
    res$diagnostics = NULL
    res$proc_complete = TRUE
    
    # weighted median
  } else if(grepl('median',the_proc,TRUE)) {
    
    if(length(x) >= 15) {
      
      res$method = "Weighted Median with Nonparametric Bootstrap"
      
      
    } else {
      
      res$method = "Weighted Median with Parametric (Laplace) Bootstrap"
      
    } 
    
    
    bt_res = weightedMedian(x=x, ux=u, nux=dof, K=5000, conf.level=0.95,
                            bootstrap=NULL, print=FALSE)
    
    
    res$mu = bt_res$m
    res$se = bt_res$um
    res$mu_upper = bt_res$Upr
    res$mu_lower = bt_res$Lwr
    res$tau = NULL
    res$boot_samples = bt_res$muB
    res$diagnostics = NULL
    res$proc_complete = TRUE
    
    
  } else { 
    # all Bayesian procedures 
    
    # get mcmc file name
    if(grepl('gauss.+gauss',the_proc,TRUE)) {
      res$method = "Hierarchical Gauss-Gauss"
      stan_filename = 'R/Stan/hgg.stan'
      jags_filename = 'R/Jags/hgg.txt'
      
    } else if(grepl('laplace',the_proc,TRUE)){
      res$method = "Hierarchical Laplace-Gauss"
      stan_filename = 'R/Stan/hlg.stan'
      jags_filename = 'R/Jags/hlg.txt'
      
    } else if(grepl('skew',the_proc,TRUE)) {
      res$method = "Skew Student-Gauss"
      stan_filename = 'R/Stan/hssg.stan'
      jags_filename = 'R/Jags/hssg2.txt'
    }
    
    
    loaded_packages = .packages()
    
    if('rstan' %in% loaded_packages) {
      mcmc_sampler = 'stan'
    } else{
      mcmc_sampler = 'jags'
    }
    
    model_inits = function() {
      list(mu = mean(x),
           tau = mad(x),
           lambda = x,
           sigma = u)
    }
    
    model_data = list(N=n, 
                      x=x, 
                      u2=u^2, 
                      dof=dof, 
                      mu_prior_loc = mean(x),
                      mu_prior_scale = (1/sqrt(3))*sd(x),
                      tau_prior_scale = priors$tau_prior_scale,
                      sigma_prior_scale = priors$sigma_prior_scale)
    
    parameters_to_save = c('mu','tau','lambda','sigma')
    
    if(grepl('skew',the_proc,TRUE) && mcmc_sampler == 'jags') {
      
      model_inits = function() {
        list(mu = mean(x),
             tau = sqrt(var(x)),
             sigma=u)
      }
      
      model_data$nu_prior_shape = priors$nu_prior_shape
      model_data$nu_prior_scale = priors$nu_prior_scale
      model_data$alpha_prior_scale = priors$alpha_prior_scale
      
      parameters_to_save = c(parameters_to_save,'delta','nu')
      
    }
    
    if(mcmc_sampler == 'stan') {
      
      # Run MCMC
     
      stan_out = stan(file=stan_filename,
                      data=model_data,
                      init=model_inits,
                      iter=3000,
                      warmup=1000,
                      chains=4)
    
      
      stan_out = extract(stan_out)
      
      res$mu = mean(stan_out$mu)
      res$mu_upper = quantile(stan_out$mu,.975)
      res$mu_lower = quantile(stan_out$mu,.025)
      res$se = sd(stan_out$mu)
      res$tau = mean(stan_out$tau)
      
    } else if(mcmc_sampler == 'jags') {
      
      
      jags_out = jags(data = model_data,
                      inits = model_inits,
                      model.file=jags_filename,
                      parameters.to.save = parameters_to_save,
                      n.chains = 4,
                      n.iter = jags_params$n_iter,
                      n.burnin = jags_params$burn_in,
                      n.thin = jags_params$thin)

      
      p_samples = jags_out$BUGSoutput$sims.list
      
      res$mu = mean(p_samples$mu)
      hw = symmetricalBootstrapCI(p_samples$mu,res$mu,.95)
      res$mu_upper = res$mu + hw
      res$mu_lower = res$mu - hw
      res$se = sd(p_samples$mu)
      res$tau = mean(p_samples$tau)
      res$tau_lower = quantile(p_samples$tau,.025)
      res$tau_upper = quantile(p_samples$tau,.975)
      res$p_samples = p_samples
      res$diagnostics = as.data.frame(jags_out$BUGSoutput$summary[,c('Rhat','n.eff')])
      
    }
    
    res$proc_complete = TRUE
    
  } 
  
  return(res)
  
}

compute_doe_table = function(the_proc,
                             vars_in,
                             res,
                             num_bootstrap) {
  
  if(grepl('average',the_proc,TRUE)) {
    
    data = vars_in$the_data
    which_to_compute = vars_in$which_to_compute
    
    DLres = DerSimonianLaird(x=vars_in$measured_vals, 
                             ux=vars_in$standard_unc)
    
    doe_res = DoEUnilateralDL(data$Result,
                              data$Uncertainty,
                              data$DegreesOfFreedom,
                              as.character(data$Laboratory),
                              num_bootstrap, # number bootstrap
                              FALSE, # LOO
                              .95, # coverage prob
                              DLres,
                              exclude = !vars_in$which_to_compute) # dl res
    
    
    return(doe_res)
    
  } else if (grepl('hierarchical',the_proc,TRUE)) {
    
    p_samples = res$p_samples
    data = vars_in$the_data
    
    distances_pred = matrix(0,nrow=length(p_samples$mu),ncol=nrow(vars_in$the_data))
    distances_trade = matrix(0,nrow=length(p_samples$mu),ncol=nrow(vars_in$the_data))
    colnames(distances_pred) = data$Laboratory
    colnames(distances_trade) = data$Laboratory
    
    included_inds = which(vars_in$which_to_compute)
    counter = 1 # indexes the mcmc lab effects
    
    # go through each lab
    for(jj in 1:ncol(distances_pred)) {
      
      # if lab was included in MCMC, use MCMC samples 
      if(vars_in$which_to_compute[jj]) {
        
        # lab random effect - KCRV
        sd_vec_pred = sqrt(p_samples$tau^2 + p_samples$sigma[,counter]^2)
        sd_vec_trade = sqrt(p_samples$sigma[,counter]^2)
        
        distances_pred[,jj] = data$Result[jj] - p_samples$mu + rnorm(length(p_samples$mu),mean=0,sd=sd_vec_pred)
        distances_trade[,jj] = data$Result[jj] - p_samples$mu + rnorm(length(p_samples$mu),mean=0,sd=sd_vec_trade)
        counter = counter + 1
        
        # if lab not included in model simulate from input data  
      } else {
        
        sd_vec_pred = sqrt(p_samples$tau^2 + data$Uncertainty[jj]^2)
        sd_vec_trade = sqrt(data$Uncertainty[jj]^2)
        distances_pred[,jj] = data$Result[jj] - p_samples$mu + rnorm(length(p_samples$mu),mean=0,sd=sd_vec_pred)
        distances_trade[,jj] = data$Result[jj] - p_samples$mu + rnorm(length(p_samples$mu),mean=0,sd=sd_vec_trade)
        
      }
      
      
    }
    
    DoE.x = data$Result - res$mu 
    
    # standard uncertainty
    DoE.U.pred = apply(distances_pred,2,sd)
    DoE.U.trade = apply(distances_trade,2,sd)
    
    # 95% intervals 
    quants_lwr_pred = rep(0,ncol(distances_pred))
    quants_upr_pred = rep(0,ncol(distances_trade))
    quants_lwr_trade = rep(0,ncol(distances_pred))
    quants_upr_trade = rep(0,ncol(distances_trade))
    
    # expanded uncertainty
    hw_pred_vec = rep(0,ncol(distances_pred))
    hw_trade_vec = rep(0,ncol(distances_trade))
    
    
    for(ii in 1:ncol(distances_pred)) {
      hw_pred = symmetricalBootstrapCI(distances_pred[,ii],DoE.x[ii],.95)
      hw_trade = symmetricalBootstrapCI(distances_trade[,ii],DoE.x[ii],.95)
      
      quants_lwr_pred[ii] = DoE.x[ii] - hw_pred
      quants_upr_pred[ii] = DoE.x[ii] + hw_pred
      
      quants_lwr_trade[ii] = DoE.x[ii] - hw_trade
      quants_upr_trade[ii] = DoE.x[ii] + hw_trade
      
      hw_pred_vec[ii] = hw_pred
      hw_trade_vec[ii] = hw_trade
    }
    
    outdf = data.frame(Lab=data$Laboratory,
                       DoE.x=DoE.x, 
                       DoE.U.Pred=DoE.U.pred,
                       DoE.U.Trade=DoE.U.trade,
                       DoE.U95.Pred=hw_pred_vec,
                       DoE.U95.Trade=hw_trade_vec,
                       DoE.Lwr.Pred=quants_lwr_pred, 
                       DoE.Upr.Pred=quants_upr_pred,
                       DoE.Lwr.Trade=quants_lwr_trade, 
                       DoE.Upr.Trade=quants_upr_trade)
    
    return(list(DoE=outdf))
    
  } else if(grepl('median',the_proc,TRUE)) {
    
    data = vars_in$the_data
    
    distances = matrix(0,nrow=length(res$boot_samples),ncol=nrow(data))
    colnames(distances) = data$Laboratory
    
    for(jj in 1:ncol(distances)) {
      
      sd_vec = data$Uncertainty
      
      distances[,jj] = data$Result[jj] - res$boot_samples + rnorm(length(res$boot_samples),mean=0,sd=sd_vec[jj])
      
    }
    
    DoE.x = data$Result - res$mu
    DoE.U = apply(distances,2,sd)
    
    quants_lwr = rep(0,ncol(distances))
    quants_upr = rep(0,ncol(distances))
    
    hw_vec = rep(0,ncol(distances))
    
    for(ii in 1:ncol(distances)) {
      hw = symmetricalBootstrapCI(distances[,ii],DoE.x[ii],.95)
      quants_lwr[ii] = DoE.x[ii] - hw
      quants_upr[ii] = DoE.x[ii] + hw
      hw_vec[ii] = hw
    }
    
    
    outdf = data.frame(Lab=data$Laboratory,
                       DoE.x=DoE.x, 
                       DoE.U.Pred=DoE.U,
                       DoE.U.Trade=DoE.U,
                       DoE.U95.Pred=hw_vec,
                       DoE.U95.Trade=hw_vec,
                       DoE.Lwr.Pred=quants_lwr, 
                       DoE.Upr.Pred=quants_upr,
                       DoE.Lwr.Trade=quants_lwr,
                       DoE.Upr.Trade=quants_upr)
    
    return(list(DoE=outdf))
    
  } else {
    
    return(NULL)
    
  }
  
}

# David's function
get_DT_decision <- function(x,u,
                            sizeHetero=0.10, sizeGauss=0.05, sizeSym=0.05) {
  
  # x: observed lab means
  # u: observed lab uncertainties (std errors of x)
  # size*: alpha levels for various test
  
  # this function assumes excluded labs are not in x and u
  
  
  if(length(x) < 3) {
    stop("Number of included labs is less than 3.")
    
  }
  
  # run all of the tests at once
  homogeneity = NA
  normality = NA
  symmetry = NA
  
  ### 1: homogeneity
  res = metafor::rma(yi = x,sei = u,method = "DL")
  pval = res$QEp
  homogeneity = ifelse(pval < sizeHetero, FALSE, TRUE)
  
  ### 2: normality
  res = stats::shapiro.test((x-median(x))/u) 
  pval = res$p.value
  normality = ifelse(pval < sizeGauss, FALSE, TRUE)
  
  ### 3: symmetry
  res = symmetry::symmetry_test(x,stat='MGG',bootstrap=TRUE, B=5000) 
  pval = res$p.value
  symmetry = ifelse(pval < sizeSym, FALSE, TRUE)
  
  # left side of tree
  if(homogeneity) {
    if(normality) {
      the_proc = "AWA"
    } else{
      the_proc = "WM"
    }
    
    # right side of tree 
  } else {
    if(symmetry) {
      
      if(normality) {
        the_proc = "HGG"
        
      } else{
        the_proc = "HLG"
      }
      
    } else {
      the_proc = "HSSG"
    }
  }
  
  return(the_proc)
  
}


# Antonio's function
DecisionTree = function (x, ux, lab,
                         sizeHetero=0.10, sizeGauss=0.05, sizeSym=0.05,
                         exclude=NULL)
{
  ## INPUTS
  
  ## x   = Numerical vector with measured values
  ## ux  = Numerical vector with standard uncertainties
  ## lab = Character vector of labels for measurement results, often
  ##       denoting laboratories where the measurements were made
  
  ## sizeHetero, sizeGauss, sizeSym = Sizes (Type I errors) for
  ## Cochran's Q test of homogeneity, for the Anderson-Darling or
  ## Shapiro-Wilk tests of Gaussian shape, and for the test of
  ## symmetry, respectively
  
  ## exclude = Vector of character strings (either NULL or including
  ## a subset of vector lab) with labels of measured values
  ## that the Decision Tree should exclude from consideration when
  ## it makes a recommendation of model for the measurement results
  
  ## OUTPUT
  
  ## A printed recommendation of the name of the model (from among
  ## those represented as leaves in the Decision Tree) that seems
  ## most appropriate for the measurement results, for the purpose
  ## of computing a key comparison reference value (consensus value)
  ## and degrees of equivalence
  
  z = data.frame(w=x, uw=ux, lab=lab)
  
  ## STEP 1: Data Selection
  keep = setdiff(as.character(z$lab), exclude)
  w = z$w; uw = z$uw; lab = z$lab
  names(w) = names(uw) = names(lab) = lab
  if (!is.null(z$nu)) {nu = z$nu; names(nu) = lab; nu = nu[keep]}
  x = w[keep]
  u = uw[keep]
  lab = lab[keep]
  n = length(x)
  
  if (is.null(exclude)) {
    xExclude = NULL; uExclude = NULL; labExclude = NULL
    
    if (!is.null(z$nu)) {
      nuExclude = NULL
    }
  } else {
    
    iExclude = match(exclude, z$lab)
    xExclude = z$w[iExclude] 
    uExclude = z$uw[iExclude]
    labExclude = z$lab[iExclude]
    names(xExclude) = names(uExclude) = names(labExclude) = labExclude
    
    if (!is.null(z$nu)) {
      nuExclude = z$nu[iExclude]
      names(nuExclude) = z$lab[iExclude]
      nuExclude = z$nu[iExclude]
    }
  }
  
  ## STEP 2: Heterogeneity
  require(metafor)
  x.rma = rma(yi=x, sei=u, slab=lab, method="DL")
  cat("\n## Cochran's Q Test p-value =", x.rma$QEp, "\n")
  
  ## STEP 3: Symmetry
  require(symmetry)
  x.sym = symmetry_test(x, stat="MGG", bootstrap=TRUE, B=500000)
  cat("## Symmetry Test p-value =", x.sym$p.value, "\n")
  
  ## STEP 4: Gaussian Shape
  require(nortest)
  x.ad = tryCatch(ad.test((x-median(x))/u), error=function(e){})
  if (class(x.ad) == "NULL") {
    cat("## Anderson-Darling Test not possible\n")
    x.ad = data.frame(p.value=NA)
  } else {
    cat("## Anderson-Darling Test p-value =", x.ad$p.value, "\n") }
  x.sw = tryCatch(shapiro.test((x-median(x))/u), error=function(e){})
  if (class(x.sw) == "NULL") {
    cat("## Shapiro-Wilk Test not possible\n")
  } else {
    cat("## Shapiro-Wilk Test p-value =", x.sw$p.value, "\n") }
  
  if ((x.rma$QEp > sizeHetero) &
      (min(c(x.ad$p.value, x.sw$p.value), na.rm=TRUE) > sizeGauss)) {
    cat("## ==>> Adaptive Weighted Average\n") 
    if (is.null(exclude)) { cat("##", paste(lab, collapse=", "),"\n")
    } else {
      cat("##", paste(c(lab, paste("-", labExclude, sep="")),
                      collapse=", "),"\n") }
    cat("##", paste(c(x, xExclude), collapse=", "), "\n")
    cat("##", paste(c(u, uExclude), collapse=", "), "\n")
    if (!is.null(z$nu)) {
      cat("##", paste(round(c(nu, nuExclude),1), collapse=", "), "\n")}
  }
  if ((x.rma$QEp > sizeHetero) &
      (min(c(x.ad$p.value, x.sw$p.value), na.rm=TRUE) <= sizeGauss)) {
    
    cat("## ==>> Weighted Median\n") 
  }
  
  if ((x.rma$QEp <= sizeHetero) &
      (x.sym$p.value > sizeSym) &
      (min(c(x.ad$p.value, x.sw$p.value), na.rm=TRUE) > sizeGauss)) {
    
    cat("## ==>> Hierarchical Gauss+Gauss\n")
    if (is.null(exclude)) { cat("##", paste(lab, collapse=", "),"\n")
    } else {
      cat("##", paste(c(lab, paste("-", labExclude, sep="")),
                      collapse=", "),"\n") }
    cat("##", paste(c(x, xExclude), collapse=", "), "\n")
    cat("##", paste(c(u, uExclude), collapse=", "), "\n")
    if (!is.null(z$nu)) {
      cat("##", paste(round(c(nu, nuExclude),1), collapse=", "), "\n")}
  }
  
  if ((x.rma$QEp <= sizeHetero) &
      (x.sym$p.value > sizeSym) &
      (min(c(x.ad$p.value, x.sw$p.value), na.rm=TRUE) <= sizeGauss)) {
    cat("## ==>> Hierarchical Laplace+Gauss\n") 
    
    if (is.null(exclude)) { 
      cat("##", paste(lab, collapse=", "),"\n")
      
    } else {
      cat("##", paste(c(lab, paste("-", labExclude, sep="")),
                      collapse=", "),"\n") 
    }
    cat("##", paste(c(x, xExclude), collapse=", "), "\n")
    cat("##", paste(c(u, uExclude), collapse=", "), "\n")
    if (!is.null(z$nu)) {
      cat("##", paste(round(c(nu, nuExclude),1), collapse=", "), "\n")}
  }
  if ((x.rma$QEp <= sizeHetero) &
      (x.sym$p.value <= sizeSym)) {
    cat("## ==>> Hierarchical SkewStudent+Gauss\n") 
    decision = 'hgg'
    
  }
  return(decision)
}


convert_acronyms_to_full_names = function(acronym) {
  
  if(grepl('awa',acronym,TRUE)) {
    return("Adaptive Weighted Average")
  }
  
  if(grepl('wm',acronym,TRUE)) { 
    return("Weighted Median")
  }
    
  if(grepl('hgg',acronym,TRUE)) { 
    return("Hierarchical Gauss-Gauss")
  }
    
  if(grepl('hlg',acronym,TRUE)) { 
    return("Hierarchical Laplace-Gauss")
  }
    
  if(grepl('hssg',acronym,TRUE)) { 
    return("Hierarchical Skew Student-Gauss")
  }
  
  stop("Error")
  
  
}

get_prior_default = function(x,u,which_param) {
  
  if(which_param == 'tau_scale') {
    return(mad(x))
    
  } else if(which_param == 'sigma_scale') {
    return(median(u))
    
  } else if(which_param == 'nu_shape') {
    return(3)
    
  } else if(which_param == 'nu_scale') {
    return(.25)
    
  } else if(which_param == 'alpha_scale') {
    return(4)
    
  }
  
  
  
  
}

get_sigma_prior_default = function(u) {
  
  
  
}

get_jags_default_priors = function(acronym,x,u) {
  
  if(acronym %in% c('AWA','WM')) {
    
    return(NULL)
    
  }
  
  priors = list(
    tau_prior_scale = get_prior_default(x,u,'tau_scale'),
    sigma_prior_scale = get_prior_default(x,u,'sigma_scale')
  )
  
  
  if(acronym == 'HSSG') {
    
    priors$nu_prior_shape = get_prior_default(x,u,'nu_shape')
    priors$nu_prior_scale = get_prior_default(x,u,'nu_scale')
    priors$alpha_prior_scale = get_prior_default(x,u,'alpha_scale')
    
    
  } 
  
  return(priors)
  
  
}

get_mcmc_params = function(n_iter = 25000,
                           burn_in = 12000,
                           thin = 5) {
  
  return(
    list(n_iter = n_iter,
         burn_in = burn_in, 
         thin = thin)
  )
  
}


run_full_ndt = function(dataset,
                        exclude,
                        procedure = "Recommended", 
                        num_bootstrap = 1000,
                        seed = 123,
                        n_iter = 50000,
                        burn_in = 25000,
                        thin = 10) {
  
  dataset$Include = !exclude
  
  dataset = dataset[,c('Include','Laboratory','MeasuredValues','StdUnc','DegreesOfFreedom')]
  colnames(dataset) = c('Include','Laboratory','Result','Uncertainty','DegreesOfFreedom')
  
  
  vars_in = list(measured_vals=dataset$Result[!exclude],
                 standard_unc=dataset$Uncertainty[!exclude],
                 dof=dataset$DegreesOfFreedom[!exclude],
                 which_to_compute=!exclude,
                 the_data=dataset)
  
  if(procedure == "Recommended") { 
    
    the_proc = get_DT_decision(x=vars_in$measured_vals,
                               u=vars_in$standard_unc,
                               sizeHetero=0.10, 
                               sizeGauss=0.05, 
                               sizeSym=0.05) 
  } else {
    the_proc = procedure
    
  }
  
  print(paste0("Selected Method: ",convert_acronyms_to_full_names(the_proc)))
  print("Running method...")
  
  
  ndt_res = run_ndt_method(x = vars_in$measured_vals,
                           u = vars_in$standard_unc,
                           dof = vars_in$dof,
                           the_proc = convert_acronyms_to_full_names(the_proc),
                           num_bootstrap = num_bootstrap,
                           jags_params = list(n_iter = n_iter,
                                              burn_in = burn_in, 
                                              thin = thin),
                           seed = seed,
                           priors = get_jags_default_priors(the_proc,vars_in$measured_vals,vars_in$standard_unc)) 
  
  print("Method complete, computing DoE...")
  
  doe_table_res = compute_doe_table(the_proc = convert_acronyms_to_full_names(the_proc),
                                    vars_in = vars_in,
                                    res = ndt_res,
                                    num_bootstrap = num_bootstrap)
  
  KCplot(val=vars_in$the_data$Result, 
         unc=vars_in$the_data$Uncertainty, 
         tau=ndt_res$tau,
         kcrv=ndt_res$mu, 
         kcrv.unc=ndt_res$se,
         lab=vars_in$the_data$Laboratory, 
         title=paste("KCRV Estimation:",the_proc), 
         title.position="left",
         ylab=NULL, 
         exclude=vars_in$the_data$Laboratory[!vars_in$which_to_compute])
  
  print("All analysis completed.")
  
  return(list(ndt_res = ndt_res, doe_table_res = doe_table_res))
}