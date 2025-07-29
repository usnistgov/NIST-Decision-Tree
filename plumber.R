library(plumber)
source('R/utils.R')
source('R/KCplotDoEplot_6_22.R')
source('R/sampleFromTau2Dist.R')
source('R/symmetricalBootstrapCI.R')
source('R/weightedMedian.R')

# default parameters
default_params_run_ndt = list(
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

default_params_run_tests = list(
  measured_values = NA,
  std_unc = NA,
  homogeneity_alpha = 0.10,
  normality_alpha = 0.05,
  symmetry_alpha = 0.05
)


#* @apiTitle NDT API
#* @apiDescription Implements NDT as a REST API

#* Echo back the input
#* @get /health-check
function(res) {
  return("Server is up and running.")
}

#* Run hypothesis tests
#* @param req
#* @post /run-tests
function(req) {
  
  body <- jsonlite::fromJSON(req$postBody)
  
  # required values
  if(!(('measured_values') %in% names(body))) {
    return("Error: no field named 'measured_values' in POST request.")
  }
  
  if(!(('standard_uncertainties') %in% names(body))) {
    return("Error: no field named 'std_unc' in POST request.")
  }
  
  # optional parameters 
  if('homogeneity_alpha' %in% names(body)) {
    homogeneity_alpha = as.numeric(body$homogeneity_alpha)
  } else {
    homogeneity_alpha = default_params_run_tests$homogeneity_alpha
  }
  
  if('normality_alpha' %in% names(body)) {
    normality_alpha = as.numeric(body$normality_alpha)
  } else {
    normality_alpha = default_params_run_tests$normality_alpha
  }
  
  if('symmetry_alpha' %in% names(body)) {
    symmetry_alpha = as.numeric(body$symmetry_alpha)
  } else {
    symmetry_alpha = default_params_run_tests$symmetry_alpha
  }
  
  res = get_DT_decision(x=as.numeric(body$measured_values),
                        u=as.numeric(body$standard_uncertainties),
                        sizeHetero=homogeneity_alpha,
                        sizeGauss=normality_alpha,
                        sizeSym=symmetry_alpha)
  
  return(res)
  
}

#* Run selected NDT procedure
#* @param req
#* @post /run-ndt
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

