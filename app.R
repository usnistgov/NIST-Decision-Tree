library(shiny)
library(shinythemes)
library(symmetry)
library(ggplot2)

source('./R/utils.R')
source('./R/inputAndDTmodule.R')

ui <- fluidPage(theme=shinytheme('spacelab'),
    
    titlePanel("Decision Tree for Key Comparisons"),
    
    tabsetPanel(
        
        tabPanel("Data Input",inputUI('input')),
        
        tabPanel("Decision Tree",DT_UI('DT')),
        
        navbarMenu("Run Procedure",
                   tabPanel("Adaptive Weighted Average",
                            awa_UI('awa')),
                   tabPanel("Weighted Median"),
                   tabPanel("Hierarchical Gauss-Gauss"),
                   tabPanel("Hierarchical Laplace-Gauss"),
                   tabPanel("Hierarchical Skew Student-Gauss"))
        
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    vars_in <- input_server('input')
    
    DT_server('DT',vars_in)
    
    awa_server('awa',vars_in)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
