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
  require(metafor)
  testWarn = ""
  
  n.All = length(x.All)
  
  if (is.null(lab.All)) {lab.All=paste("L", 1:n.All, sep="")}
  
  # Check if any lab was exempt from prior computation
  #sanitize = !startsWith( lab.All,"-")
  sanitize = !exclude
  
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
      if (is.null(nu[j]) || (nu[j]==Inf)) {
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
    
    indexInf = (nu == Inf)
    ## Column j of D will have a sample from the
    ## bootstrap distribution of x[j]-mu
    D = array(dim=c(K,nI))
    dimnames(D)[[2]] = lab
    muB = numeric(K)
    for (k in 1:K)
    {
      tau2B = sampleFromTau2Dist(x,u) 
      
      xB = rnorm(nI, mean=mu, sd=sqrt(tau2B + u^2))
      uB = rep(NA, nI)
      if (is.null(nu)) {uB = u
      } else {
        uB[indexInf] = u[indexInf]
        uB[!indexInf] = u[!indexInf] *
          sqrt(nu[!indexInf] /
                 rchisq(sum(!indexInf), df=nu[!indexInf]))}
      muB[k] = rma(yi=xB, sei=uB, method="DL",
                   knha=FALSE)$b
      D[k,] = xB - as.vector(muB[k])
    }
    DoE.x = x - as.vector(mu)
    names(DoE.x) = lab
    ## AP 2019-Mar-30: First center each column of D at zero, then
    ## shift it to become centered at the corresponding value of DoE.x
    ## Needs to be done only for the MRA version of the DoEs
    DC = sweep(D, 2, apply(D, 2, mean))
    D = sweep(DC, 2, -DoE.x)
  }                 ############################This is the end of the LOO if/else
  
  DoE.Lwr = apply(D, 2, function (x) {quantile(x, probs=(1-coverageProb)/2)})
  DoE.Upr = apply(D, 2, function (x) {quantile(x, probs=(1+coverageProb)/2)})
  DoE.U = rep(NA,nI)
  for (j in 1:nI) {
    DoE.U[j] = symmetricalBootstrapCI(D[,j], DoE.x[j], coverageProb) }
  
  
  
  
  ### Analysis for excluded labs
  
  if(n.All>nI){
    x.excluded=x.All[!sanitize]
    u.excluded=u.All[!sanitize]
    nu.excluded=nu.All[!sanitize]
    lab.excluded=lab.All[!sanitize]
    
    DoE.U.excluded = rep(NA,n.All-nI)
    D.excluded = array(NA,dim=c(K,n.All-nI)) 
    
    DoE.x.excluded = x.excluded-as.numeric(DLRes$beta)
    names(DoE.x.excluded) = lab.excluded
    for(j in 1:(n.All-nI)){
      if (LOO) {
        DoE.U.excluded[j]=1.96*sqrt(u.excluded[j]^2 + var(mujB) + DLRes$tau2) ### AP CHECK: is mujB right here?
      }else{
        DoE.U.excluded[j]=1.96*sqrt(u.excluded[j]^2 + var(muB) + DLRes$tau2) 
      }
      D.excluded[,j]=rnorm(K, mean=DoE.x.excluded[j], sd=sqrt(u.excluded[j]^2 + DLRes$tau2)) ### AP CHECK: Need to fill this in for bilateral DoE code, what with?
    }  
    
    # D.excluded = array(rep(DoE.x.excluded,each=K),dim=c(K,n.All-nI)) ## This didn't seem to have enough variability
    dimnames(D.excluded)[[2]] = as.list(lab.excluded)
    
    DoE.x=c(DoE.x,DoE.x.excluded)
    DoE.U=c(DoE.U,DoE.U.excluded)
    DoE.Lwr=c(DoE.Lwr,DoE.x.excluded-DoE.U.excluded) ### might modify the 1.96 later 
    DoE.Upr=c(DoE.Upr,DoE.x.excluded+DoE.U.excluded)
    
    lab.outlabel=c(lab,lab.excluded)
    
    D=cbind(D,D.excluded)
    
  }else{
    lab.outlabel=lab.All
  }
  
  
  ### Add results for excluded labs 
  results = data.frame(Lab=lab.outlabel, DoE.x=DoE.x, DoE.U95=DoE.U,
                       DoE.Lwr=DoE.Lwr, DoE.Upr=DoE.Upr)
  
  return(invisible(list(D=D,DoE=results, DoEwarn=testWarn)))
}
