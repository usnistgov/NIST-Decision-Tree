library(shiny)
library(shinythemes)
library(symmetry)
library(ggplot2)
#library(rstan)
#library(shinyjs)
library(R2jags)
library(rhandsontable)

#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)

source('./R/utils.R')
source('./R/inputAndDTmodule.R')
source('./R/resultsModule.R')

source('./R/DOE/DoEUnilateralDL.R')
source('./R/DOE/sampleFromTau2Dist.R')
source('./R/DOE/symmetricalBootstrapCI.R')
source('./R/DOE/KCplotDoEplot.R')


ui <- fluidPage(theme=shinytheme('spacelab'),
    
    titlePanel("Decision Tree for Key Comparisons"),
    br(),
    
    tabsetPanel(
        
        tabPanel("1. Data Input", inputUI('input')),
        
        tabPanel("2. Decision Tree",DT_UI('DT')),
        
        tabPanel("3. Fit Model",resultsUI('results'))
        
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    vars_in <- input_server('input')
    
    selected_procedure <- DT_server('DT', vars_in)
    
    resultsServer('results', vars_in, selected_procedure)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
