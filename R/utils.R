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
sampleFromTau2Dist=function(y,sigma){
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