#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
source('R/utils.R')
source('R/KCplotDoEplot_6_22.R')
source('R/sampleFromTau2Dist.R')
source('R/symmetricalBootstrapCI.R')
source('R/weightedMedian.R')

#* @apiTitle NDT API
#* @apiDescription Plumber example description.

#* Echo back the input
#* @get /test
function(res) {
  return("Server is up and running.")
}

#* Test post
#* @param msg
#* @post /test
function(msg) {
  return(msg)
}


#* Run selected NDT procedure
#* @param laboratory:[str]
#* @param measured_values:[double] Measurement results
#* @param std_unc:[double] Measurement uncertainties
#* @param dof:[int] Degrees of freedom
#* @param exclude:[bool] Vector of labs to be excluded (optional)
#* @param procedure Selected procedure
#* @param num_bootstrap:int Number of bootstrap samples for DSL
#* @param seed:int Random number seed
#* @param n_iter:int Number Jags Iterations
#* @param burn_in:int Number of burn-in samples
#* @post /run_ndt_query_params
function(req,res,
         laboratory=c("Lab1","Lab2","Lab3","Lab4","Lab5"),
         measured_values=c(1,3,5,4,2),
         std_unc=c(1,2,1,2,1),
         dof=rep(30,5),
         exclude=rep(FALSE,5),
         procedure="Recommended",
         num_bootstrap=1000,
         seed=123,
         n_iter=50000,
         burn_in=25000) {
  
  dataset = data.frame(
    Laboratory = laboratory,
    MeasuredValues = as.numeric(measured_values),
    StdUnc = as.numeric(std_unc),
    DegreesOfFreedom = as.numeric(dof)
  )
  
  exclude = exclude == 'true'
  num_bootstrap = as.numeric(num_bootstrap)
  seed = as.numeric(seed)
  n_iter = as.numeric(n_iter)
  burn_in = as.numeric(burn_in)
  
  res = run_full_ndt(dataset,
                     exclude,
                     procedure = "Recommended",
                     num_bootstrap = num_bootstrap,
                     seed = seed,
                     n_iter = n_iter,
                     burn_in = burn_in,
                     thin = 10)
  
  res$ndt_res$p_samples = NULL
  
  return(res)
}

