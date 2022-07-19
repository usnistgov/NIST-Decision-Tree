## ------------------------------------------------------------
## ISSUE WITH METAFOR

# require(metafor)
# 
# y = c(6.62607013, 6.62607006, 6.62606993)
# uy = c(6.00e-08, 1.65e-07, 8.90e-08)
# 
# options(digits=5)
# for (i in list(c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1)))
# {
#     y.rma = rma(yi=round(y[i],8), sei=uy[i], method="DL")
#     cat(i, ":",  y.rma$QE, y.rma$QEp, "\n")
# }

## 1 2 3 : 3.8362 0.14689 
## 1 3 2 : 4.1468 0.12576 
## 2 1 3 : 3.8362 0.14689 
## 2 3 1 : 4.3021 0.11636 
## 3 1 2 : 11.135 0.0038196 
## 3 2 1 : 11.135 0.0038196 

## ------------------------------------------------------------
## EXAMPLE: Data

# PCB28 = data.frame(lab=c("IRMM", "KRISS", "NARL", "NIST", "NMIJ", "NRC"),
#     x=c(34.3, 32.9, 34.53, 32.42, 31.9, 35.8),
#     u=c(1.03, 0.69, 0.83, 0.29, 0.4, 0.38),
#     nu=c(60, 4, 18, 2, 13, 60))

## ------------------------------------------------------------
## EXAMPLE: metafor

# require(metafor)
# rma(yi=x, sei=u, data=PCB28, method="DL")

## Random-Effects Model (k = 6; tau^2 estimator: DL)
## tau^2 (estimated amount of total heterogeneity): 2.9289 (SE = 2.4363)
## tau (square root of estimated tau^2 value):      1.7114
## I^2 (total heterogeneity / total variability):   92.67%
## H^2 (total variability / sampling variability):  13.64
## Test for Heterogeneity: Q(df = 5) = 68.2154, p-val < .0001
## estimate      se     zval    pval    ci.lb    ci.ub     ​ 
##  33.6004  0.7450  45.1014  <.0001  32.1403  35.0606  *** 

## ------------------------------------------------------------
## EXAMPLE: metRology

# require(metRology)
# dsl(x=PCB28$x, s=PCB28$u)
## Location estimate: Method=DerSimonian-Laird
##             Value         u
## estimate 33.60043 0.7449979

## ------------------------------------------------------------
## Cochran's Q

# cochranQ = function (x, ux, digits=5)
# {
#     w = 1/ux^2
#     mu = sum(w*x)/sum(w)
#     Q = sum(w*(x-mu)^2)
#     Qnu = length(x)-1
#     Qp = 1-pchisq(Q, df=Qnu)
#     return(signif(c(Q=Q, nu=Qnu, p=Qp), digits))
# }

#cochranQ(x=PCB28$x, ux=PCB28$u, digits=5)
##          Q         nu          p 
## 6.8215e+01 5.0000e+00 2.4092e-13 

## ------------------------------------------------------------
## DerSimonian-Laird Estimate

# DerSimonianLaird = function (x, ux)
# {
#     n = length(x)
#     w0 = 1/ux^2
#     x0 = sum(w0*x)/sum(w0)
#     Q = sum(w0*(x-x0)^2)
#     Qp = 1-pchisq(Q, df=length(x)-1)
#     tau = sqrt(max(0, (Q-n+1) /
#                       (sum(w0) - sum(w0^2)/sum(w0))))
#     w = 1/(tau^2+ux^2)
#     mu.x = sum(w*x)/sum(w)
#     mu.u = 1/sqrt(sum(w))
#     return(list(beta=mu.x,
#                 se=mu.u,
#                 Q=Q,
#                 Qp = Qp,
#                 tau2=tau^2))
# }

#DerSimonianLaird(x=PCB28$x, ux=PCB28$u)
##   estimate     stdunc 
## 33.6004326  0.7449979 

## ------------------------------------------------------------
