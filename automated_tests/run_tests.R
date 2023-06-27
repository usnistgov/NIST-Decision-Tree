library(decisiontree)

set.seed(123)

dataset = read.csv('example_dataset.csv')

# test all methods
methods_to_run = c("AWA","WM","HGG","HLG","HSSG")

# test a variety of exclusions
excludes = matrix( FALSE, ncol=nrow(dataset), nrow=5)
excludes[2,1] = TRUE
excludes[3,nrow(dataset)] = TRUE
excludes[4,c(1,nrow(dataset))] = TRUE
excludes[5,3] = TRUE

for(ii in 1:length(methods_to_run)) {
  
  for(jj in 1:nrow(excludes)) {
    
    dataset$DegreesOfFreedom = sample(c(NA,dataset$DegreesOfFreedom),replace=TRUE,size=nrow(dataset))
    
    res = run_full_ndt(dataset = dataset,
                       exclude = excludes[jj,],
                       procedure = methods_to_run[ii], 
                       num_bootstrap = 1000,
                       seed = 123,
                       n_iter = 25000,
                       burn_in = 12500,
                       thin = 10)
    
    summary_table(res)
    doe_plot(res)
    get_doe_table(res)
    get_MCMC_diagnostics(res)
    get_KCplot(res)
    get_MCMC_diagnostics(res)
    
  }
  
}



