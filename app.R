library(shiny)
library(shinythemes)
library(symmetry)
library(ggplot2)
#library(rstan)
library(R2jags)

#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)

source('./R/utils.R')
source('./R/inputAndDTmodule.R')
source('./R/resultsModule.R')


ui <- fluidPage(theme=shinytheme('spacelab'),
    
    titlePanel("Decision Tree for Key Comparisons"),
    
    tabsetPanel(
        
        tabPanel("Data Input",inputUI('input')),
        
        tabPanel("Decision Tree",DT_UI('DT')),
        
        tabPanel("Results",resultsUI('results'))
        
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    vars_in <- input_server('input')
    
    selected_procedure <- DT_server('DT',vars_in)
    
    resultsServer('results',vars_in,selected_procedure)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
