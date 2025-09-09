library(plumber)
library(jsonvalidate)
library(jsonlite)

source('R/utils.R')
source('R/KCplotDoEplot_6_22.R')
source('R/sampleFromTau2Dist.R')
source('R/symmetricalBootstrapCI.R')
source('R/weightedMedian.R')

# schema validators
run_tests_validator = json_validator('example_post_requests/run_tests_json_schema.json',engine='ajv')
run_ndt_validator = json_validator('example_post_requests/run_ndt_json_schema.json',engine='ajv')

# default parameters
default_params_run_ndt = list(
  dof=100,
  exclude=FALSE,
  procedure="Recommended",
  num_bootstrap=1000,
  seed=123,
  n_iter=50000,
  burn_in=25000
)

default_params_run_tests = list(
  measured_values = NA,
  standard_uncertainties = NA,
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
function(req, res) {
  
  body = jsonlite::fromJSON(req$postBody)

  # Validate request based on json schema
  if (!run_tests_validator(req$postBody)) {
    res$status = 400
    return(list(
      error = "Invalid request",
      details = attr(run_tests_validator(req$postBody, verbose = TRUE), "errors")
    ))
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
function(req, res) {
  
  # get post body
  body = jsonlite::fromJSON(req$postBody)
  
  # validate request
  if (!run_ndt_validator(req$postBody)) {
    res$status = 400
    return(list(
      error = "Invalid request",
      details = attr(run_ndt_validator(req$postBody, verbose = TRUE), "errors")
    ))
  }

  # fill in body arguments with defaults where missing
  for(n in names(default_params_run_ndt)) {
    if(is.null(body[[n]])) {
      
      # for dof and exclude, need to make them same length as other inputs
      if(n %in% c('dof','exclude')) {
        body[[n]] = rep(default_params_run_ndt[[n]],length(body$measured_values))
        
      } else {
        body[[n]] = default_params_run_ndt[[n]]
        
      }
    }
  }
  
  dataset = data.frame(
    Laboratory = body$laboratory,
    MeasuredValues = as.numeric(body$measured_values),
    StdUnc = as.numeric(body$standard_uncertainties),
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
  
  res$ndt_res$p_samples = NULL # don't need to send all the posterior samples to client
  
  return(res)
}

