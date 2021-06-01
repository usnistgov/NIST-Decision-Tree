######################################################################
##
## FILE        : DoEUnilateralBayes.R

## AUTHOR      : Amanda Koepke & Antonio Possolo
## MODIFICATION: 2016-Aug-02

## INPUT : x.All    = Numeric vector with values measured by the labs
##         u.All    = Numeric vector with standard uncertainties
##                associated with measured values
##         nu.All   = Numeric vector with (positive) numbers of
##                degrees of freedom that the elements of u are
##                based on; if NULL, then the elements of u are
##                assumed to be based on infinitely many degrees
##                of freedom (optional)
##         lab.All  = Character vector with laboratory labels (optional)
##         LOO  = TRUE/FALSE indicating whether to base the DoE on
##                differences to leave-one-out estimates of the
##                consensus value, or on differences to the
##                consensus value that has been derived from all
##                measurement results (optional)
##         mcmc = MCMC output from bayesGelman is used, if
##                supplied, only when LOO=FALSE (optional)
##         ni   = Number of total MCMC draws for each chain
##         nb   = Number of MCMC draws to discard at the beginning
##                of each chain
##         nt   = Thinning rate (positive integer) for each chain

## OUTPUT: List with two elements: (D) Kxn array whose jth column
## has K bootstrap replicates of the unilateral DoE for lab j;
## (DoE) data frame with values and 95 % expanded uncertainties
## for unilateral degrees of equivalence, and 2.5th and 97.5th
## percentiles of the bootstrap distributions of these values

######################################################################

DoEUnilateralBayes = function (x.All, u.All, nu.All, lab.All, LOO, mcmc,
                               ni, nb, nt,coverageProb,
                               UItauPriorScale, UIsigmaPriorScale)
{
  source("bayesGelman.R")
  
  n.All = length(x.All)
  
  ## check convergence of LOO runs
  bayesRunsConv=c()
  
  consensusValueName="mu"
  StudyEffectName="xi"
  
  if (is.null(lab.All)) {lab.All=paste("L", 1:n.All, sep="")}
  
  # Check if any lab was exempt from prior Mcmc computation
  sanitize = !startsWith( lab.All,"-")
  
  ## Analysis for included labs
  x=x.All[sanitize]
  u=u.All[sanitize]
  nu=nu.All[sanitize]
  lab=lab.All[sanitize]
  
  #Number of labs taken into account for the consensus ## AP CHECK: What should min number of labs be?
  nI = sum(sanitize)
  if (nI != n.All && nI <= 2){
    testWarn="WARNING: Not enough labs were including in the consensus for DoE with LOO to be meaningful<br/>"
  }
  
  
  if (LOO) {
    nc = length(mcmc)
    ## DoEs based on leave-one-out estimates
    cat("## DoEUnilateralBayes (Leave-One-Out Version) ---\n")
    
    ### ### DO WE STILL NEED THIS HERE??? ### ###
    # ##Add Warning if LOO is one with only one lab used for the consensusV
    # if (nI != n && nI<= 1){
    #   testWarnLab="WARNING: Not enough labs were including in the consensus for DoE with LOO to be meaningful<br/>"
    # }
    ### ### ### ###
    
    # ##### Code below is from Antonio's 2016-Nov-02 modification of the DoE code
    # ##### I don't use sigmaMCMC in the LOO version of the DoE, per "Questions and Answers" emails (August 2016).
    # ## We begin by populating sigmaMCMC that will be used
    # ## below (to define gammaj) using the results of a
    # ## Bayes-Gelman model fitted to all of the data
    #
    #
    #
    # if (is.null(nu)) {
    #   ## When nu is NULL we take sigma=u, as if all elements
    #   ## of u were based on infinitely many degrees of
    #   ## freedom, and the sigma will not have been estimated
    #   K = sum(sapply(mcmc, nrow))
    #   sigmaMCMC =  t(array(rep(u, K), dim=c(n,K)))
    # } else {
    #   ## When nu is not NULL, the sigma will have
    #   ## been estimated and there are samples from
    #   ## their posterior distributions
    #   sigmaMCMC = NULL
    #   sigmaNAMEs = paste("sigma[", 1:n,"]", sep="")
    #
    #   for (jc in 1:nc)
    #   {
    #     is = match(sigmaNAMEs, dimnames(mcmc[[jc]])[[2]])
    #     sigmaMCMC = rbind(sigmaMCMC, mcmc[[jc]][,is])
    #   }
    # }
    
    ## For each j=1,...,n, BG[[j]] is a matrix with two
    ## columns and K rows: the columns have samples of the
    ## posterior distributions of mu and tau when the
    ## measurement results from lab j were st aside
    BG = list()
    for (j in 1:nI)
    {
      cat(j, "of", nI, "\n")
      zj = bayesGelman(x=x[-j], u=u[-j], nu=nu[-j],
                       tauPriorScale=UItauPriorScale,
                       sigmaPriorScale=UIsigmaPriorScale,
                       nc=1,
                       ni=ni, nb=nb, nt=nt)
      bayesRunsConv=c(bayesRunsConv,(zj$warn!=""))
      
      BG[[j]] = rbind(zj$mcmc[[1]][, c(consensusValueName, "tau")])
      
    }
    
    K = nrow(BG[[1]])
    D = array(dim=c(K, nI))
    DoE.x = DoE.U = DoE.Lwr = DoE.Upr = numeric(nI)
    for (j in 1:nI)
    {
      muMCMC = BG[[j]][,consensusValueName]
      tauMCMC = BG[[j]][,"tau"]
      
      ######## Used below to generate ej, different from 2016-Nov-02 update
      sigmaj = sqrt(tauMCMC^2 + u[j]^2)
      if (is.null(nu[j]) || (nu[j]==Inf)) {
        # if (is.null(nu[j]) | (isTRUE(nu[j]==Inf))) {
        ej = rnorm(K, mean=0, sd=sigmaj)
      } else {
        ## AP-AK: Addressing possibility of no
        ## more than 2 degrees of freedom
        phi = if (nu[j] <= 2) {## other options: ## sqrt(3) ##qt(.975,df=nu[j])/qt(.975,df=Inf)
          a = 0.4668994; b= -0.3998882
          1.5*((1 - (3/4)*(a - 4*b *
                             (nu[j]^(-3/4)-1)/3))^(-4/3))
        } else {sqrt(nu[j]/(nu[j]-2))}
        # ej = sigmaj*u[j]*rt(K, df=nu[j])/phi ##AK change## typo
        ej = sigmaj*rt(K, df=nu[j])/phi
        
      }
      ########
      
      # ######## Generation of ej from 2016-Nov-02 update
      # gammaj = sqrt(tauMCMC^2 + sigmaMCMC[,j]^2)
      # ej = rnorm(K, mean=0, sd=gammaj)
      # ########
      
      DoE.x[j] = x[j] - mean(muMCMC)
      D[,j] = x[j] - muMCMC + ej
      DoE.U[j] =
        symmetricalBootstrapCI(D[,j], mean(D[,j]), coverageProb)
      DoE.Lwr[j] = quantile(D[,j], probs=(1-coverageProb)/2)
      DoE.Upr[j] = quantile(D[,j], probs=(1+coverageProb)/2)
    }
    
  } else {
    
    ## DoEs computed according to MRA
    
    
    nc = length(mcmc)
    if (is.null(nu)) {
      ## When nu is NULL we take sigma=u, as if all elements
      ## of u were based on infinitely many degrees of
      ## freedom, and the sigma will not have been estimated
      muMCMC = tauMCMC = NULL
      
      for (jc in 1:nc)
      {
        muMCMC = c(muMCMC, mcmc[[jc]][,consensusValueName])
        tauMCMC = c(tauMCMC, mcmc[[jc]][,"tau"])
      }
      
      K = length(muMCMC)
      sigmaMCMC =  t(array(rep(u, K), dim=c(nI,K)))
      
      
    } else {
      ## When nu is not NULL, the sigma will have
      ## been estimated and there are samples from
      ## their posterior distributions
      muMCMC = thetaMCMC = tauMCMC = sigmaMCMC = NULL
      
      thetaNAMEs = paste(StudyEffectName, "[", 1:nI,"]", sep="")
      sigmaNAMEs = paste("sigma[", 1:nI,"]", sep="")
      
      
      for (jc in 1:nc)
      {
        muMCMC = c(muMCMC, mcmc[[jc]][,consensusValueName])
        it = match(thetaNAMEs, dimnames(mcmc[[jc]])[[2]])
        thetaMCMC = rbind(thetaMCMC, mcmc[[jc]][,it])
        tauMCMC = c(tauMCMC, mcmc[[jc]][,"tau"])
        is = match(sigmaNAMEs, dimnames(mcmc[[jc]])[[2]])
        sigmaMCMC = rbind(sigmaMCMC, mcmc[[jc]][,is])
      }
    }
    K = length(muMCMC)
    muHAT = mean(muMCMC)
    
    ## A drawing from the predictive distribution for lab j is
    ## obtained by first making a drawing from the joint
    ## posterior distribution of (mu, tau, sigma[j]), to
    ## obtain (M,T,S), and then making a drawing Y from a
    ## Gaussian distribution with mean M and standard
    ## deviation sqrt(T^2+S^2). The corresponding value of D
    ## computed below is Y-M.  The unilateral DoE for lab j is
    ## x[j]-muHAT, where muHAT is the mean of the posterior
    ## distribution of mu. The U for this DoE is U(D)
    D = array(dim=c(K, nI))
    DoE.x = DoE.U = DoE.Lwr = DoE.Upr = numeric(nI)
    
    ### Commented out Thomas' addition, added this from the old, commit on Apr 2, 2019
    ### Computing the DoE for the labs included in the consensus first
    for (j in 1:nI)
    {
      D[,j] = x[j] - muMCMC +
        rnorm(K, mean=0, sd=sqrt(tauMCMC^2 + sigmaMCMC[,j]^2))
      DoE.x[j] = x[j] - muHAT
      DoE.U[j] =
        symmetricalBootstrapCI(D[,j], mean(D[,j]), coverageProb)
      DoE.Lwr[j] = quantile(D[,j], probs=(1-coverageProb)/2)
      DoE.Upr[j] = quantile(D[,j], probs=(1+coverageProb)/2)
    }
    #   ### AP CHECK: Thomas wrote this section below, until the next ###
    #   #We compute the DoE for the lab included in the consensus first
    #   for (i in 1:nI)
    #   {
    #
    #     j = (1:n)[sanitize][i]
    #     D[,j] = x[j] - muMCMC +
    #       rnorm(K, mean=0, sd=sqrt(tauMCMC^2 + sigmaMCMC[,i]^2))
    #     DoE.x[j] = x[j] - muHAT
    #     DoE.U[j] =
    #       symmetricalBootstrapCI(D[,j], mean(D[,j]), coverageProb)
    #     DoE.Lwr[j] = quantile(D[,j], probs=(1-coverageProb)/2)
    #     DoE.Upr[j] = quantile(D[,j], probs=(1+coverageProb)/2)
    #   }
    #
    #   #We compute the DoE for the lab exclude from the consensus
    #   #We use a formula in case nu is specified that requires 3 or more labs to be included in the consensus
    #
    #   if(nI<n)
    #   {
    #     if (is.null(nu))
    #       nu=rep(Inf,n)
    #
    #     for (i in 1:(n-nI))
    #     {
    #
    #       j = (1:n)[!sanitize][i]
    #       nutau= nI-1
    #       nudj=( u[j]^2/(nu[j]+1) + tauMCMC^2/(nutau+1) )^2 / (  (u[j]^2/(nu[j]+1))^2/nu[j]  + (tauMCMC^2/(nutau+1))^2/nutau   )
    #       D[,j] = x[j] - muMCMC +      sqrt(tauMCMC^2 +u[j]^2)/sqrt(nudj/(nudj-2))*rt(K, nudj)
    #       DoE.x[j] = x[j] - muHAT
    #       DoE.U[j] =
    #         symmetricalBootstrapCI(D[,j], mean(D[,j]), coverageProb)
    #       DoE.Lwr[j] = quantile(D[,j], probs=(1-coverageProb)/2)
    #       DoE.Upr[j] = quantile(D[,j], probs=(1+coverageProb)/2)
    #
    #
    #     }
    #
    #   }
    # ### ### ### ###
    # ### ### ### ###
    
  }
  
  
  ### ### Need to add analysis for excluded labs here
  if(n.All>nI){
    x.excluded=x.All[!sanitize]
    u.excluded=u.All[!sanitize]
    nu.excluded=nu.All[!sanitize]
    lab.excluded=lab.All[!sanitize]
    
    DoE.U.excluded = rep(NA,n.All-nI)
    D.excluded = array(NA,dim=c(K,n.All-nI)) ### AP CHECK: Need to fill this in for bilateral DoE code, what with?
    
    for(j in 1:(n.All-nI)){
      
      D.excluded[,j]=x.excluded[j] - muMCMC + rnorm(K, mean=0, sd=sqrt(u.excluded[j]^2 + tauMCMC^2))
      DoE.U.excluded[j]=symmetricalBootstrapCI(D.excluded[,j],
                                               estimate=mean(D.excluded[,j]),
                                               coverage=coverageProb)
      
    }
    dimnames(D.excluded)[[2]] = as.list(lab.excluded)
    
    DoE.x.excluded = x.excluded-mean(muMCMC)
    names(DoE.x.excluded) = lab.excluded
    
    DoE.x=c(DoE.x,DoE.x.excluded)
    DoE.U=c(DoE.U,DoE.U.excluded)
    DoE.Lwr=c(DoE.Lwr,DoE.x.excluded-DoE.U.excluded) ### AP CHECK
    DoE.Upr=c(DoE.Upr,DoE.x.excluded+DoE.U.excluded)
    
    lab.outlabel=c(lab,lab.excluded)
    
    # DC.excluded = sweep(D.excluded, 2, apply(D.excluded, 2, mean))
    # D.excluded = sweep(DC.excluded, 2, -DoE.x.excluded)
    
    D=cbind(D,D.excluded)
  }else{
    lab.outlabel=lab.All
  }
  
  
  ### Add results for excluded labs
  results = data.frame(Lab=lab.outlabel, DoE.x=DoE.x, DoE.U95=DoE.U,
                       DoE.Lwr=DoE.Lwr, DoE.Upr=DoE.Upr)
  
  
  ### Send warning if MCMC didn't converge
  if (sum(bayesRunsConv)>0) {
    testWarn=paste("WARNING: MCMC may not have reached equilibrium<br/>",
                   "WARNING: Results are unreliable<br/>",
                   paste("SUGGESTION: Re-run bayesGelman with ",
                         "ni = ", 10*ni, ", nb = ", 2*nb,
                         ", and nt = ", ceiling(0.001*ni), "<br/>", sep=""),sep=" ")
    
  }else{
    testWarn=""
  }
  if (exists("testWarnLab"))
  {
    testWarn = paste(testWarnLab, testWarn)
  }
  
  return(invisible(list(D=D, DoE=results,DoEwarn=testWarn)))
}