######################################################################

## weightedMedian.R
##
## AUTHOR:            Antonio Possolo
## MODIFICATION DATE: 2022 May 20

######################################################################

symmetricalBootstrapCI = function (x, estimate, coverage) {

    ## AUTHOR      : Thomas Lafarge & Antonio Possolo
    ## MODIFICATION: 2022-May-20
    ##
    ## INPUTS:
    ##   x        = Numeric vector with boostrap replicates of a statistic
    ##   estimate = Scalar with value of statistic where the
    ##              coverage interval should be centered at
    ##   coverage = Coverage probability (between 0 and 1)
    ## OUTPUT: Expanded uncertainty for specified coverage

    if (abs(coverage-0.5) >= 0.5) {
        stop(paste0("ERROR: specified coverage must be greater than 0 ",
                    "and smaller than 1")) }
    m = length(x)
    Umax = max(abs(x - estimate))
    Udelta = (max(x)-min(x))/m
    x1 = Umax
    x0 = 0
    iteration = 1
    while ((iteration<1e7) && ((x1-x0)>Udelta))
	{
            iteration = iteration + 1
            x3 = (x1 +x0) /2
            if(sum((estimate-x3 <= x) &
                       (x <= estimate+x3))/m < coverage) {
                x0 = x3 } else { x1 = x3 }
	}
    return((x1 + x0)/2)
}

weightedMedian = function (x, ux, nux=NULL, K=10000, conf.level=0.95,
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
    ##              NULL and there are 15 or more numerically
    ##                 different values in x: nonparametric bootstrap
    ##              NULL and there are fewer than 15 numerically
    ##                 different values in x: parametric bootstrap
    ## print      = Logical indicating whether results should be printed
    ##              in addition to being returned invisibly

    ## OUTPUT
    ## An invisible list with the weighted median estimate of mu, the
    ## associated standard uncertainty, a coverage interval for mu,
    ## and the boostrap replicates of the weighted median

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
    nUnique = length(unique(x))

    wx = 1/ux^2
    muHAT.x = spatstat.geom::weighted.median(x, wx)

    ## There are (2*n-1 choose n) different samples of size n that can
    ## be drawn, with replacement, from a set of size n. This is
    ## 2^{2*n-1}/sqrt(n*pi) approximately. The bootstrap distribution
    ## of a statistic t(x) determined based on K bootstrap samples may
    ## be a reasonable approximation of the sampling distribution of
    ## t(x) if the probability is high that all K bootstrap samples
    ## will be different (even if the values of t(x) for these samples
    ## will not all be different). For n=15, there are 77,558,760
    ## bootstrap samples.
    if ((is.null(bootstrap) && (nUnique > 14)) ||
        (!is.null(bootstrap) && (bootstrap=="nonparametric"))) {
        xm = function (xw,i) {spatstat.geom::weighted.median(xw[i,1], xw[i,2])}
        xw = cbind(x, 1/ux^2)
        xm.boot = boot(xw, xm, R=K)
        muB = xm.boot$t
    } 

    if ((is.null(bootstrap) && (nUnique < 15)) ||
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
            betaB = if (is.null(nux)) {beta
                    } else {sqrt(2)*(nux+1)*beta/rchisq(n, df=2*(nux+1))}
            xB = rlaplace(n, mu=x, sigma=betaB)
            muB[k] = spatstat.geom::weighted.median(xB, 1/(2*betaB^2))
        }
    } 

    gamma = pnorm(1)-pnorm(-1)
    muHAT.u = symmetricalBootstrapCI(muB, estimate=muHAT.x, coverage=gamma)
    bootCI = quantile(muB, probs=c((1-conf.level)/2, (1+conf.level)/2))

    if (print) {print(c(m=muHAT.x, "u(m)"=muHAT.u,
                        Lwr=bootCI[1], Upr=bootCI[2]))}
    return(invisible(list(m=muHAT.x, um=muHAT.u,
                          Lwr=bootCI[1], Upr=bootCI[2], muB=muB)))
}

## ######################################################################
## ##
## ## EXAMPLE: IAEA (2021, Table 3: Data for GRESP and for std13)
## ## Delta 2H expressed in permil
## 
## ## @TechReport{      iaea-2021,
## ##   author        = {{IAEA}},
## ##   year          = {2021},
## ##   title         = {Certification Report on Value Assignment for the
## ##                   $\delta{}^{2}\mbox{H}$ and $\delta{}^{18}\mbox{O}$ 
## ##                   Stable Isotopic Composition in the
## ##                   Water Reference Material GRESP
## ##                   (Greenland Summit Precipitation)},
## ##   institution   = {International Atomic Energy Agency},
## ##   type = {IAEA Analytical Quality in Nuclear Applications Series},
## ##   number        = {63},
## ##   note          = {IAEA/AQ/63},		  
## ##   address       = {Vienna, Austria}}
## 
# lab = c("G-FD", "G-LGR", "G-PiT", "G-Pi1", "G-PiSo", "G-PiS",
#         "G-CrTCEA", "G-UFZ", "S-FD", "S-LGR", "S-Pi1", "S-PiSo")
# x = c(-259.2, -257.9, -257.9, -258.3, -257.8, -257.7, -258, -257.5,
#       -259.6, -257.8, -257.8, -259)
# ux = c(0.9, 0.9, 0.3, 0.7, 0.9, 1.1, 0.8, 1, 0.8, 0.8, 1.1, 0.5)
# nux = c(11, 119, 19, 941, 261, 3618, 49, 49, 2, 107, 220, 19)
# 
# wM.NP = weightedMedian(x, ux, bootstrap="nonparametric")
# ##            m         u(m)     Lwr.2.5%    Upr.97.5%
# ## -257.9500000    0.4500137 -259.1000000 -257.8500000
# ## -257.9500000    0.4999863 -259.1000000 -257.8500000
# 
# wM.P = weightedMedian(x=x, ux=ux, nux=nux, K=100000, bootstrap="parametric")
# ##            m         u(m)     Lwr.2.5%    Upr.97.5%
# ## -257.9500000    0.2415312 -258.8043333 -257.6754742
# ## -257.9500000    0.2424106 -258.8018402 -257.6748316
# 
# wM = weightedMedian(x=x, ux=ux, nux=nux)
# ##            m         u(m)     Lwr.2.5%    Upr.97.5%
# ## -257.9500000    0.2371064 -258.8066491 -257.6760749
# ## -257.9500000    0.2406318 -258.7909657 -257.6754067
# 
# plot(density(wM$muBOOT), col="Red")
# lines(density(wM.NP$muBOOT), col="Green")
# lines(density(wM.P$muBOOT), col="Blue")

######################################################################
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ##
######################################################################

    
