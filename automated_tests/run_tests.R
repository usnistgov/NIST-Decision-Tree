library(symmetry)
library(ggplot2)
library(R2jags)
library(rhandsontable)
library(metafor)
library(rmutil)
library(shinycssloaders)
# also need boot, extraDistr, rmarkdown, knitr

setwd("~/ByScientist/AntonioPossolo/CODES/CODES/decisiontree/")

source('R/utils.R')
source('R/weightedMedian.R')
source('R/CochranDerSimonianLaird.R')
source('R/DoEUnilateralDL.R')
source('R/sampleFromTau2Dist.R')
source('R/symmetricalBootstrapCI.R')
source('R/KCplotDoEplot_6_22.R')

dataset = read.csv('example_dataset.csv')

methods_to_run = c("Recommended",
                   "AWA",
                   "WM",
                   "HGG",
                   "HLG",
                   "HSSG")

methods_to_run = 'AWA'

for(ii in 1:length(methods_to_run)) {
  
  res = run_full_ndt(dataset = dataset,
                     exclude = rep(FALSE,length(dataset$MeasuredValues)),
                     procedure = methods_to_run[ii], 
                     num_bootstrap = 1000,
                     seed = 123,
                     n_iter = 50000,
                     burn_in = 25000,
                     thin = 10)
  
  res = run_full_ndt(dataset = dataset,
                     exclude = c(TRUE,rep(FALSE,length(dataset$MeasuredValues)-1)),
                     procedure = methods_to_run[ii], 
                     num_bootstrap = 1000,
                     seed = 123,
                     n_iter = 50000,
                     burn_in = 25000,
                     thin = 10)
  
  
  res = run_full_ndt(dataset = dataset,
                     exclude = c(rep(FALSE,length(dataset$MeasuredValues)-1),TRUE),
                     procedure = methods_to_run[ii], 
                     num_bootstrap = 1000,
                     seed = 123,
                     n_iter = 50000,
                     burn_in = 25000,
                     thin = 10)
  
}









