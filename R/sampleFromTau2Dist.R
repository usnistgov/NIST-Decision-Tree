######################################################################
##
## FILE        : sampleFromTau2Dist.R
##
## AUTHOR      : Amanda Koepke & Antonio Possolo
## MODIFICATION: 2016-Mar-31
##
## INPUT : y = Numeric vector with values measured by the labs
##         sigma = Numeric vector with standard uncertainties
##               associated with measured values
##
## OUTPUT: Returns one sample from the approximate distribution for tau^2, 
## sampled by simulating tau^2_M from its approximate distribution, a
## location-shifted, scaled gamma (derived in Biggerstaff and Tweedie (1997)) 
## and then the simulated tau^2=max(0,tau^2_M).
######################################################################

sampleFromTau2Dist=function(y,sigma){
  ######################################################################
  ## Data specific variables
  ######################################################################
  K=length(y)
  w=sigma^-2
  
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