library(plumber)
source('R/utils.R')
source('R/KCplotDoEplot_6_22.R')
source('R/sampleFromTau2Dist.R')
source('R/symmetricalBootstrapCI.R')
source('R/weightedMedian.R')

# default parameters
default_params = list(
  laboratory=c("Lab1","Lab2","Lab3","Lab4","Lab5"),
  measured_values=c(1,3,5,4,2),
  std_unc=c(1,2,1,2,1),
  dof=rep(30,5),
  exclude=rep(FALSE,5),
  procedure="Recommended",
  num_bootstrap=1000,
  seed=123,
  n_iter=50000,
  burn_in=25000
)


#* @apiTitle NDT API
#* @apiDescription Implements NDT as a REST API

#* Echo back the input
#* @get /test
function(res) {
  return("Server is up and running.")
}

#* Run selected NDT procedure
#* @param req
#* @post /run_ndt
function(req) {
  
  # get post body
  body <- jsonlite::fromJSON(req$postBody)

  # fill in body arguments with defaults where missing
  for(n in names(default_params)) {
    if(is.null(body[[n]])) {
      body[[n]] = default_params[[n]]
    }
  }
  
  dataset = data.frame(
    Laboratory = body$laboratory,
    MeasuredValues = as.numeric(body$measured_values),
    StdUnc = as.numeric(body$std_unc),
    DegreesOfFreedom = as.numeric(body$dof)
  )
  
  exclude = body$exclude == 'true'
  num_bootstrap = as.numeric(body$num_bootstrap)
  seed = as.numeric(body$seed)
  n_iter = as.numeric(body$n_iter)
  burn_in = as.numeric(body$burn_in)
  
  # run ndt
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

