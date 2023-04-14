library(shiny)
library(shinythemes)
library(symmetry)
library(ggplot2)
#library(rstan)
#library(shinyjs)
library(R2jags)
library(rhandsontable)
library(tinytex)
library(metafor)
library(rmutil)
# also need boot, extraDistr, rmarkdown, knitr


source('./R/utils.R')
source('./R/inputAndDTmodule.R')
source('./R/resultsModule.R')
source('./R/weightedMedian.R')
source('./R/CochranDerSimonianLaird-2022May24.R')


source('./R/DOE/DoEUnilateralDL.R')
source('./R/DOE/sampleFromTau2Dist.R')
source('./R/DOE/symmetricalBootstrapCI.R')
source('./R/DOE/KCplotDoEplot_6_22.R')

version = "1.0.0"


ui <- fluidPage(id='fullpage',#shinytheme('spacelab'),
    
    ### styling
    tags$link(rel="stylesheet",href="my_style.css"),
    tags$link(rel='stylesheet',href='nist-style.css'),

    tags$head(tags$link(rel="shortcut icon", href="./favicon.ico")),
    tags$head(HTML("<title>NIST Decision Tree</title>")),
    tags$head(HTML(
      '<!-- Global site tag (gtag.js) - Google Analytics -->
        <script async src="https://www.googletagmanager.com/gtag/js?id=G-4XRM4LDBLT"></script>
        <script>
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag("js", new Date());

          gtag("config", "G-4XRM4LDBLT");
        </script>'
    )),

    #tags$div(HTML('<class="nist-header">')),

    tags$div(HTML('<header class="nist-header" id="nist-header" role="banner">
                  <a href="https://www.nist.gov/" title="National Institute of Standards and Technology" class="nist-header__logo-link" rel="home">
                    <svg aria-hidden="true" class="nist-header__logo-icon" version="1.1" xmlns="http://www.w3.org/2000/svg" width="24" height="32" viewBox="0 0 24 32">
                      <path d="M20.911 5.375l-9.482 9.482 9.482 9.482c0.446 0.446 0.446 1.161 0 1.607l-2.964 2.964c-0.446 0.446-1.161 0.446-1.607 0l-13.25-13.25c-0.446-0.446-0.446-1.161 0-1.607l13.25-13.25c0.446-0.446 1.161-0.446 1.607 0l2.964 2.964c0.446 0.446 0.446 1.161 0 1.607z"></path>
                    </svg>
                    <svg class="nist-header__logo-image" version="1.1" xmlns="http://www.w3.org/2000/svg" viewBox="-237 385.7 109.7 29.3">
                      <title>National Institute of Standards and Technology</title>
                      <g>
                        <path class="st0" d="M-231,415h-6v-23.1c0,0,0-4.4,4.4-5.8c4-1.3,6.6,1.3,6.6,1.3l19.7,21.3c1,0.6,1.4,0,1.4-0.6v-22h6.1V409
                                    c0,1.9-1.6,4.4-4,5.3c-2.4,0.9-4.9,0.9-7.9-1.7l-18.5-20c-0.5-0.5-1.8-0.6-1.8,0.4L-231,415L-231,415z"/>
                        <path class="st0" d="M-195,386.1h6.1v20.7c0,2.2,1.9,2.2,3.6,2.2h26.8c1.1,0,2.4-1.3,2.4-2.7c0-1.4-1.3-2.8-2.5-2.8H-176
                                    c-3,0.1-9.2-2.7-9.2-8.5c0-7.1,5.9-8.8,8.6-9h49.4v6.1h-12.3V415h-6v-22.9h-30.2c-2.9-0.2-4.9,4.7-0.2,5.4h18.6
                                    c2.8,0,7.4,2.4,7.5,8.4c0,6.1-3.6,9-7.5,9H-185c-4.5,0-6.2-1.1-7.8-2.5c-1.5-1.5-1.7-2.3-2.2-5.3L-195,386.1
                                    C-194.9,386.1-195,386.1-195,386.1z"/>
                      </g>
                    </svg>
                  </a>
                  
                  </header>')),

    ### end styling
  
    column(width=11,
    
    titlePanel("NIST Decision Tree for Key Comparisons"),
    h4(paste("Version",version),style="text-align:right"),
    
    tabsetPanel(
      
        tabPanel("0. About", aboutUI('about')),
        
        tabPanel("1. Data Input", inputUI('input')),
        
        tabPanel("2. Decision Tree",DT_UI('DT')),
        
        tabPanel("3. Fit Model",resultsUI('results')),
        
        selected='1. Data Input',
        
    ),
    
    br()
    ),
    
    ### footer ###

    tags$div(HTML(
      '<footer class="nist-footer">
        <div class="nist-footer__inner">
        <div class="nist-footer__menu" role="navigation">
        <ul>
        <li class="nist-footer__menu-item">
        <a href="https://www.nist.gov/privacy-policy">Site Privacy</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://www.nist.gov/oism/accessibility">Accessibility</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://www.nist.gov/privacy">Privacy Program</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://www.nist.gov/oism/copyrights">Copyrights</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://www.commerce.gov/vulnerability-disclosure-policy">Vulnerability Disclosure</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://www.nist.gov/no-fear-act-policy">No Fear Act Policy</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://www.nist.gov/foia">FOIA</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://www.nist.gov/environmental-policy-statement">Environmental Policy</a>
        </li>
        <li class="nist-footer__menu-item ">
        <a href="https://www.nist.gov/summary-report-scientific-integrity">Scientific Integrity</a>
        </li>
        <li class="nist-footer__menu-item ">
        <a href="https://www.nist.gov/nist-information-quality-standards">Information Quality Standards</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://www.commerce.gov/">Commerce.gov</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://www.science.gov/">Science.gov</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://www.usa.gov/">USA.gov</a>
        </li>
        <li class="nist-footer__menu-item">
        <a href="https://vote.gov/">Vote.gov</a>
        </li>
        </ul>
        </div>
        </div>
        <div class="nist-footer__logo">
        <a href="https://www.nist.gov/" title="National Institute of Standards and Technology" class="nist-footer__logo-link" rel="home">
        <img src="./nist_logo_brand_white.svg" alt="National Institute of Standards and Technology logo" />
        </a>
        </div>
        </footer>'
    ))

 ### footer end ###
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    about_server('about')
    
    vars_in <- input_server('input')
    
    selected_procedure <- DT_server('DT', vars_in)
    
    resultsServer('results', vars_in, selected_procedure, version)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
