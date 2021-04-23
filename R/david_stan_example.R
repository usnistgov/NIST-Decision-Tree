# library(rstan)
# 
# x = c(1,2,3,6)
# 
# the_data = list(
#   N=4,
#   x=x,
#   u2=c(1,1,1,3),
#   dof=c(10,10,10,10),
#   med_abs_dif = mad(x))
# 
# stan_inits = function() {
#   list(mu = mean(x),
#        tau = 1,
#        lambda = x,
#        sigma=c(1,1,1,3))
# }
# 
# fit <- stan(file = 'hgg.stan',
#             data = the_data,
#             chains = 1,
#             init=stan_inits,
#             warmup = 1000,
#             iter = 2000)
# 
# 
# fit = extract(fit)
# 
# # basic example to see if stan is even working
# 
# example(stan_model, package = "rstan", run.dontrun = TRUE)
