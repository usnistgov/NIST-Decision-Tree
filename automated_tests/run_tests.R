library(ggplot2)

setwd("~/ByScientist/AntonioPossolo/CODES/CODES/decisiontree")

source('R/utils.R')
source('R/KCplotDoEplot_6_22.R')
source('R/sampleFromTau2Dist.R')
source('R/DoEUnilateralDL.R')
source('R/symmetricalBootstrapCI.R')
source('R/weightedMedian.R')
source('R/CochranDerSimonianLaird.R')

set.seed(123)

dataset = read.csv('data/example_dataset.csv')

# test all methods
methods_to_run = c("AWA","WM","HGG","HLG","HSSG")

# test a variety of exclusions
excludes = matrix( FALSE, ncol=nrow(dataset), nrow=5)
excludes[2,1] = TRUE
excludes[3,nrow(dataset)] = TRUE
excludes[4,c(1,nrow(dataset))] = TRUE
excludes[5,2] = TRUE

for(ii in 1:length(methods_to_run)) {
  
  for(jj in 1:nrow(excludes)) {
    
    dataset$DegreesOfFreedom = sample(c(NA,dataset$DegreesOfFreedom),replace=TRUE,size=nrow(dataset))
    
    res = run_full_ndt(dataset = dataset,
                       exclude = excludes[jj,],
                       procedure = methods_to_run[ii], 
                       num_bootstrap = 500,
                       seed = 123,
                       n_iter = 25000,
                       burn_in = 12500,
                       thin = 10)
    
    print(summary_table(res))         # returns table of lab uncertainties
    print(get_doe_plot(res,'1'))              # displays the DoE plot
    print(get_doe_plot(res,'2'))
    print(get_doe_table(res,'1'))         # returns DoE Table
    print(get_doe_table(res,'2'))
    print(get_MCMC_diagnostics(res))  # returns MCMC Diagnostics Table
    print(get_KCplot(res))            # displays KCRV and lab plot
    
  }
  
}



