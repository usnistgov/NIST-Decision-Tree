######################################################################

## weightedMedian.R
##
## AUTHOR:            Antonio Possolo and David Newton
## MODIFICATION DATE: 2022 Apr 05

######################################################################

DTweightedMedian = function (x, ux, nux=NULL, K=10000, conf.level=0.95,
                           bootstrap=NULL, print=TRUE)
{
  
  ## Fits common median model x[j] = mu + epsilon[j] to measurement
  ## results for which {(x[j]-mu)/ux[j]} can reasonably be regarded
  ## as a sample from a Laplace distribution with mean 0
  
  ## INPUTS
  ## x          = Numerical vector with measured values
  ## ux         = Numerical vector with associated standard uncertainties
  ## K          = Number of bootstrap replicates
  ## conf.level = Coverage probability desired for interval for mu
  ## bootstrap  = NULL, "parametric", "nonparametric".
  ##              NULL and n > 11 : nonparametric bootstrap
  ##              NULL and n =< 11: parametric bootstrap
  ## print      = Logical indicating whether results should be printed
  ##              in addition to being returned invisibly
  
  ## OUTPUT
  ## An invisible list with the weighted median estimate of mu, the
  ## associated standard uncertainty, and a coverage interval for mu
  
  require(spatstat.geom)
  require(extraDistr)
  require(boot)
  
  if (!is.null(bootstrap) &&
      !(bootstrap %in% c("parametric", "nonparametric"))) {
    cat(paste0("## weightedMedian: argument 'bootstrap' must be one of ",
               "NULL, 'parametric', or 'nonparametric'\n"))
    return(invisible(list(m=NA, um=NA, Lwr=NA, Upr=NA)))
  }
  iNA = (is.na(x) | is.na(ux))
  x = x[!iNA]; ux = ux[!iNA]
  n = length(x)
  
  wx = 1/ux^2
  muHAT = spatstat.geom::weighted.median(x, wx)
  
  ## There are (2*n-1 choose n) different samples of size n that can
  ## be drawn, with replacement, from a set of size n. This is
  ## 2^{2*n-1}/sqrt(n*pi) approximately. The bootstrap distribution
  ## of a statistic t(x) determined based on K bootstrap samples may
  ## be a reasonable approximation of the sampling distribution of
  ## t(x) if the probability is high that all K bootstrap samples
  ## will be different (even if the values of t(x) for these samples
  ## will not all be different). For n=11, there are 352716
  ## bootstrap samples.
  
  if ((is.null(bootstrap) && (n > 11)) ||
      (!is.null(bootstrap) && (bootstrap=="nonparametric"))) {
    xm = function (xw,i) {spatstat.geom::weighted.median(xw[i,1], xw[i,2])}
    xw = cbind(x, 1/ux^2)
    xm.boot = boot(xw, xm, R=K)
    muHAT.u = sd(xm.boot$t)
    bootCI = quantile(xm.boot$t,
                      probs=c((1-conf.level)/2, (1+conf.level)/2))
    muB = xm.boot$t
  } 
  
  if ((is.null(bootstrap) && (n < 12)) ||
      (!is.null(bootstrap) && (bootstrap=="parametric"))) {
    muB = numeric(K)
    beta = ux/sqrt(2)
    for (k in 1:K)
    {
      ## Suppose y is a vector of m observations from a Laplace
      ## distribution with median mu and scale beta, hence with
      ## standard deviation beta*sqrt(2) (each of the elements
      ## of the vector x that is an input to this function can
      ## be regarded as median of such a y)
      
      ## (2/beta)*sum(abs(y-mu)) ~ chi-square(2m)
      ## beta = 2*sum(abs(x-mu))/x2(2m)
      ## Since betaHAT = sum(abs(y-mu))/m, beta = 2*m*betaHAT/x2(2m).
      ## Now, let x denote the median of y and ux its standard
      ## uncertainty. Replace betaHAT with ux/sqrt(2), and use
      ## sqrt(2)*m*ux/chi2(2m) as parametric bootstrap replicate
      ## of beta, where m = nux+1
      
      if (is.null(nux)) {
        betaB = beta
      } else {
        betaB = sqrt(2)*(nux+1)*beta/rchisq(n, df=2*(nux+1))
      }
      xB = rlaplace(n, mu=x, sigma=betaB)
      muB[k] = spatstat.geom::weighted.median(xB, 1/(2*betaB^2))
    }
    muHAT.u = sd(muB)
    bootCI = quantile(muB, probs=c((1-conf.level)/2, (1+conf.level)/2))
  } 
  
  if (print) {print(c(m=muHAT, "u(m)"=muHAT.u,
                      Lwr=bootCI[1], Upr=bootCI[2]))}
  return(invisible(list(m=muHAT, um=muHAT.u, Lwr=bootCI[1], Upr=bootCI[2],
                        muB=muB)))
}



