######################################################################
##
## FILE        : DoEUnilateralDerSimonianLaird.R

## AUTHOR      : Amanda Koepke & Antonio Possolo
## MODIFICATION: 2020-May-01

## INPUT : x.All   = Numeric vector with values measured by the labs (required)
##         u.All   = Numeric vector with standard uncertainties
##               associated with measured values (required)
##         nu.All  = Numeric vector with (positive) numbers of
##               degrees of freedom that the elements of u are
##               based on; if NULL, then the elements of u are
##               assumed to be based on infinitely many degrees
##               of freedom (optional)
##         lab.All = Character vector with laboratory labels (optional)
##         K   = Number of bootstrap replicates (optional)
##         LOO = TRUE/FALSE indicating whether to base the DoE on
##               differences to leave-one-out estimates of the
##               consensus value, or on differences to the
##               consensus value that has been derived from all
##               measurement results (optional)
##         coverageProb = Coverage probability for the interval
##               (DoE.Lwr, DoE.Upr), and for the interval DoE.x +/-
##               DoE.U (required)
##         DLRes = The results of the DerSimonianLaird function

## OUTPUT: List with three elements: (D) used in bilateral DoE function; 
## (DoE) data frame with values and expanded uncertainties
## for unilateral degrees of equivalence, and endpoints of coverage
## intervals for these values; (DoEwarn) any warnings

######################################################################

DoEUnilateralDL = function (x.All, u.All, nu.All, lab.All, K,LOO, coverageProb, DLRes, exclude)
{
  testWarn = ""
  
  n.All = length(x.All)
  
  if (is.null(lab.All)) {lab.All=paste("L", 1:n.All, sep="")}
  
  # Check if any lab was exempt from prior computation
  #sanitize = !startsWith( lab.All,"-")
  sanitize = !exclude
  
  nu.All[is.na(nu.All)] = 100000
  nu.All[is.null(nu.All)] = 100000
  
  ## Analysis for included labs 
  x=x.All[sanitize]
  u=u.All[sanitize]
  nu=nu.All[sanitize]
  lab=lab.All[sanitize]
  # DLRes = rma(yi=x, sei=u, method="DL")
  
  #Number of labs taken into account for the consensus
  nI = sum(sanitize)
  if (nI != n.All && nI<= 2 && LOO){
    testWarn="WARNING: Not enough labs were including in the consensus for DoE with LOO to be meaningful<br/>"
  }
  
  if (LOO) {
    ## DoEs based on leave-one-out estimates
    
    DoE.x = numeric(nI)
    D = array(dim=c(K,nI))
    names(DoE.x) = dimnames(D)[[2]] = lab
    for (j in 1:nI)
    {
      cat (j, "of", nI, "\n")
      tau2B = numeric(K)
      for (k in 1:K) {
        tau2B[k] = sampleFromTau2Dist(x[-j],u[-j])
      }
      
      ## AP-AK: We avoid doing the "full" bootstrap for
      ## each mu[-j], and rely instead on a modified
      ## version of the Hartung-Knapp-Sidik-Jonkman
      ## approximation for the distribution of each
      ## leave-one-out estimate of the consensus value,
      ## as described by Rover, Knapp, and Friede (2015,
      ## DOI 10.1186/s12874-015-0091-1)
      xj = x[-j]; uj = u[-j]
      nj = length(xj)
      wj0 = 1/uj^2
      xj0 = sum(wj0*xj)/sum(wj0)
      tauj2 = max(0, (sum(wj0*(xj-xj0)^2)-nj+1) /
                    (sum(wj0) - sum(wj0^2)/sum(wj0)))
      wj = 1/(uj^2+tauj2)
      muj = sum(wj*xj)/sum(wj)
      qSTAR = max(1, sum(wj*(xj-muj)^2)/(nj-1))
      muj.u = sqrt(qSTAR/sum(wj))
      muj.df = nj-1
      
      ## AP-AK: Addressing possibility of no
      ## more than 2 degrees of freedom
      ## Alternatives include sqrt(3) and
      ## qt((1+coverageProb)/2, df=nu[j]) /
      ## qt((1+coverageProb)/2, df=Inf)
      muj.phi = if (muj.df <= 2) {
        a = 0.4668994; b = -0.3998882
        1.5*((1 - (3/4)*(a - 4*b *
                           (muj.df^(-3/4)-1)/3))^(-4/3))
      } else {sqrt(muj.df/(muj.df-2))}
      
      mujB = as.vector(muj) + muj.u*rt(K, df=muj.df)/muj.phi
      DoE.x[j] = x[j] - muj
      ## AP-AK: The same sample of size K in tau2B
      ## is used for all j=1,...,n
      sigmaj = sqrt(tau2B + u[j]^2)
      if (is.null(nu[j]) || (nu[j] > 10000)) {
        ej = rnorm(K, mean=0, sd=sigmaj)
      } else {
        ## AP-AK: Addressing possibility of no
        ## more than 2 degrees of freedom (same approach as above)
        phi = if (nu[j] <= 2) {
          a = 0.4668994; b= -0.3998882
          1.5*((1 - (3/4)*(a - 4*b *
                             (nu[j]^(-3/4)-1)/3))^(-4/3))
        } else {sqrt(nu[j]/(nu[j]-2))}
        
        ej = sigmaj*rt(K, df=nu[j])/phi }
      
      ## Following Jones & Spiegelhalter (2011) --
      ## Approach 2: Identify Outliers to the Random
      ## Effects Distribution.  Note that D[,j] is used
      ## only to evaluate the uncertainty of (not to
      ## assign a value to) the difference in DoE.x
      D[,j] = x[j] + ej - mujB
    }
    
  } else {
    ## DoEs computed according to MRA
    
    z = DLRes
    mu = z$b
    
    indexInf = (nu > 10000)
    ## Column j of D will have a sample from the
    ## bootstrap distribution of x[j]-mu
    D_pred = array(dim=c(K,nI))
    D_trade = array(dim=c(K,nI))
    dimnames(D_pred)[[2]] = lab
    dimnames(D_trade)[[2]] = lab
    muB = numeric(K)
    for (k in 1:K)
    {
      tau2B = sampleFromTau2Dist(x,u) 
      
      xB_pred = rnorm(nI, mean=mu, sd=sqrt(tau2B + u^2))
      xB_trade = rnorm(nI, mean=mu, sd=sqrt(u^2))
      
      uB = rep(NA, nI)
      
      if (is.null(nu)) {
        uB = u
        
      } else {
        uB[indexInf] = u[indexInf]
        uB[!indexInf] = u[!indexInf]*sqrt(nu[!indexInf]/rchisq(sum(!indexInf), df=nu[!indexInf]))
      }
      
      muB[k] = metafor::rma(yi=xB_pred, sei=uB, method="DL",
                            knha=FALSE)$b
      
      
      D_pred[k,] = xB_pred - as.vector(muB[k])
      D_trade[k,] = xB_trade - as.vector(muB[k])
    }
    
    DoE.x = x - as.vector(mu)
    names(DoE.x) = lab
    ## AP 2019-Mar-30: First center each column of D at zero, then
    ## shift it to become centered at the corresponding value of DoE.x
    ## Needs to be done only for the MRA version of the DoEs
    DC_pred = sweep(D_pred, 2, apply(D_pred, 2, mean))
    D_pred = sweep(DC_pred, 2, -DoE.x)
    
    DC_trade = sweep(D_trade, 2, apply(D_trade, 2, mean))
    D_trade = sweep(DC_trade, 2, -DoE.x)
  }                 ############################This is the end of the LOO if/else
  
  
  
  DoE.U.Pred = rep(NA,nI)
  DoE.U.Trade = rep(NA,nI)
  
  DoE.Lwr.Pred = rep(NA,nI)
  DoE.Upr.Pred = rep(NA,nI)
  
  DoE.Lwr.Trade = rep(NA,nI)
  DoE.Upr.Trade = rep(NA,nI)
  
  hw_vec_pred = rep(NA,nI)
  hw_vec_trade = rep(NA,nI)
  
  for (j in 1:nI) {
    
    hw_vec_pred[j] = symmetricalBootstrapCI(D_pred[,j], DoE.x[j], coverageProb) 
    hw_vec_trade[j] = symmetricalBootstrapCI(D_trade[,j], DoE.x[j], coverageProb)
    
    # expanded uncertainties
    DoE.Lwr.Pred[j] = DoE.x[j] - hw_vec_pred[j]
    DoE.Upr.Pred[j] = DoE.x[j] + hw_vec_pred[j]
    
    DoE.Lwr.Trade[j] = DoE.x[j] - hw_vec_trade[j]
    DoE.Upr.Trade[j] = DoE.x[j] + hw_vec_trade[j]
    
    # standard uncertainties
    DoE.U.Pred[j] = sd(D_pred[,j])
    DoE.U.Trade[j] = sd(D_trade[,j])
    
  }
  
  DoE.U95.Pred = hw_vec_pred
  DoE.U95.Trade = hw_vec_trade
  
  
  ### Analysis for excluded labs
  
  if(n.All>nI){
    x.excluded=x.All[!sanitize]
    u.excluded=u.All[!sanitize]
    nu.excluded=nu.All[!sanitize]
    lab.excluded=lab.All[!sanitize]
    
    DoE.U.excluded.pred = rep(NA,n.All-nI)
    D.excluded.pred = array(NA,dim=c(K,n.All-nI)) 
    
    DoE.U.excluded.trade = rep(NA,n.All-nI)
    D.excluded.trade = array(NA,dim=c(K,n.All-nI)) 
    
    DoE.x.excluded = x.excluded-as.numeric(DLRes$beta)
    names(DoE.x.excluded) = lab.excluded
    
    for(j in 1:(n.All-nI)){
      if (LOO) {
        DoE.U.excluded.pred[j]=sqrt(u.excluded[j]^2 + var(mujB) + DLRes$tau2) ### AP CHECK: is mujB right here?
        DoE.U.excluded.trade[j]=sqrt(u.excluded[j]^2 + var(mujB))
        
      }else{
        # muB is bootstrapped mu's 
        DoE.U.excluded.pred[j]=sqrt(u.excluded[j]^2 + var(muB) + DLRes$tau2) 
        DoE.U.excluded.trade[j]=sqrt(u.excluded[j]^2 + var(muB)) 
      }
      
      D.excluded.pred[,j]=rnorm(K, mean=DoE.x.excluded[j], sd=sqrt(u.excluded[j]^2 + DLRes$tau2))
      D.excluded.trade[,j]=rnorm(K, mean=DoE.x.excluded[j], sd=sqrt(u.excluded[j]^2))
    }  
    
    # D.excluded = array(rep(DoE.x.excluded,each=K),dim=c(K,n.All-nI)) ## This didn't seem to have enough variability
    dimnames(D.excluded.pred)[[2]] = as.list(lab.excluded)
    dimnames(D.excluded.trade)[[2]] = as.list(lab.excluded)
    
    DoE.x=c(DoE.x,DoE.x.excluded)
    
    DoE.U.Pred = c(DoE.U.Pred,DoE.U.excluded.pred)
    DoE.U.Trade = c(DoE.U.Trade,DoE.U.excluded.trade)
    
    DoE.U95.Pred = c(DoE.U95.Pred,1.96*DoE.U.excluded.pred)
    DoE.U95.Trade = c(DoE.U95.Trade,1.96*DoE.U.excluded.trade)
    
    DoE.Lwr.Pred = c(DoE.Lwr.Pred,DoE.x.excluded-1.96*DoE.U.excluded.pred) 
    DoE.Upr.Pred = c(DoE.Upr.Pred,DoE.x.excluded+1.96*DoE.U.excluded.pred)
    
    DoE.Lwr.Trade = c(DoE.Lwr.Trade,DoE.x.excluded-1.96*DoE.U.excluded.trade) 
    DoE.Upr.Trade = c(DoE.Upr.Trade,DoE.x.excluded+1.96*DoE.U.excluded.trade)
    
    lab.outlabel = c(lab,lab.excluded)
    
    D_pred = cbind(D_pred,D.excluded.pred)
    D_trade = cbind(D_trade,D.excluded.trade)
    
  }else{
    lab.outlabel=lab.All
  }
  
  
  ### Add results for excluded labs 
  results = data.frame(Lab=lab.outlabel, 
                       DoE.x=DoE.x, 
                       DoE.U95.Pred=DoE.U95.Pred,
                       DoE.U95.Trade=DoE.U95.Trade,
                       DoE.U.Pred=DoE.U.Pred,
                       DoE.U.Trade=DoE.U.Trade,
                       DoE.Lwr.Pred=DoE.Lwr.Pred, 
                       DoE.Lwr.Trade=DoE.Lwr.Trade, 
                       DoE.Upr.Pred=DoE.Upr.Pred,
                       DoE.Upr.Trade=DoE.Upr.Trade)
  
  return(invisible(list(D_pred=D_pred,D_trade=D_trade,DoE=results, DoEwarn=testWarn)))
}
