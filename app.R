library(shiny)
library(shinythemes)
library(symmetry)
library(ggplot2)
#library(rstan)
#library(shinyjs)
library(R2jags)
library(rhandsontable)
library(tinytex)

#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)

source('./R/utils.R')
source('./R/inputAndDTmodule.R')
source('./R/resultsModule.R')

source('./R/DOE/DoEUnilateralDL.R')
source('./R/DOE/sampleFromTau2Dist.R')
source('./R/DOE/symmetricalBootstrapCI.R')
source('./R/DOE/KCplotDoEplot.R')


ui <- fluidPage(id='fullpage',#shinytheme('spacelab'),
    
    ### styling
    tags$link(rel="stylesheet",href="my_style.css"),
    
    tags$head(tags$link(rel="shortcut icon", href="./favicon.ico")),
    tags$head(HTML("<title>NIST Decision Tree</title>")),
    
    tags$div(HTML('<class="nist-header">')),
    
    tags$div(HTML("<div class='nist-header'>
                    <div class='nist-header__logo'>
                      <a href='https://www.nist.gov/' title='National Institute of Standards and Technology' class='nist-header__logo-link' rel='home'>
                        <img src='./nist_logo_reverse.svg'  alt=''>
                      </a>
                    </div>
                    <div class='nist-header__title'>
                    </div>
                  </div>")),
    
    ### end styling
    
    column(width=11,
    
    titlePanel("Decision Tree for Key Comparisons"),
    br(),
    
    tabsetPanel(
        
        tabPanel("1. Data Input", inputUI('input')),
        
        tabPanel("2. Decision Tree",DT_UI('DT')),
        
        tabPanel("3. Fit Model",resultsUI('results'))
        
    ),
    
    br()
    ),
    
    ### footer ###
    
    tags$div(HTML("<footer id='footer' class='nist-footer'>
    <div class='nist-footer__inner'>
      <div class='nist-footer__menu' role='navigation'>
        <ul>
          <li class='nist-footer__menu-item'>
            <a href='https://www.nist.gov/privacy-policy'>Privacy Statement</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.nist.gov/privacy-policy#privpolicy'>Privacy Policy</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.nist.gov/privacy-policy#secnot'>Security Notice</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.nist.gov/privacy-policy#accesstate'>Accessibility Statement</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.nist.gov/privacy'>NIST Privacy Program</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.nist.gov/no-fear-act-policy'>No Fear Act Policy</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.nist.gov/disclaimer'>Disclaimer</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.nist.gov/office-director/freedom-information-act'>FOIA</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.nist.gov/environmental-policy-statement'>Environmental Policy Statement</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.nist.gov/privacy-policy#cookie'>Cookie Disclaimer</a>
          </li>
          <li class='nist-footer__menu-item '>
            <a href='https://www.nist.gov/summary-report-scientific-integrity'>Scientific Integrity Summary</a>
          </li>
          <li class='nist-footer__menu-item '>
            <a href='https://www.nist.gov/nist-information-quality-standards'>NIST Information Quality Standards</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://business.usa.gov/'>Business USA</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.commerce.gov/'>Commerce.gov</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='https://www.healthcare.gov/'>Healthcare.gov</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='http://www.science.gov/'>Science.gov</a>
          </li>
          <li class='nist-footer__menu-item'>
            <a href='http://www.usa.gov/'>USA.gov</a>
          </li>
        </ul>
      </div>
    </div>
  </footer>"))
    
 ### footer end ###
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    vars_in <- input_server('input')
    
    selected_procedure <- DT_server('DT', vars_in)
    
    resultsServer('results', vars_in, selected_procedure)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
