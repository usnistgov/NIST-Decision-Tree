######################################################################
##
## FILE        : symmetricalBootstrapCI.R

## AUTHOR      : Thomas Lafarge
## MODIFICATION: 2016-Mar-23
##
## INPUT : x        = Numeric vector with boostrap replicates of a statistic
##         estimate = Scalar with value of statistic where the
##                    coverage interval should be centered at
##         coverage = Coverage probability (between 0 and 1)

symmetricalBootstrapCI = function (x, estimate, coverage) {
  m = length(x)
  Umax = max(abs(x - estimate)) #maximum absolute deviation, measure of spread
  Udelta = (max(x)-min(x))/m #range of data/length of data, rough spacing of points? if all m points were evenly spaced would be the spacing
  x1 = Umax
  x0 = 0
  iteration = 1
  while ((iteration<1e3) && ((x1-x0)>Udelta))
  {
    iteration = iteration + 1
    x3 = (x1 +x0) /2
    if(sum((estimate-x3 <= x) & (x <= estimate+x3))/m < coverage) {
      x0 = x3
    } else {
      x1 = x3
      
    }
  }
  return((x1 + x0)/2)
}