library(shiny)
library(ggplot2)
library(dplyr,include.only = "%>%")
library(symmetry,include.only = "MGG")

version = "1.0.4"

ui <- fluidPage(id='fullpage',#shinytheme('spacelab'),

                ### styling
                tags$link(rel="stylesheet",href='my_style.css'),
                tags$link(rel='stylesheet',href='nist-style.css',),
                tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
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

                tags$div(HTML(nist_header_html)),

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

                tags$div(HTML(nist_footer_html))

                ### footer end ###



)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  about_server('about')

  vars_in <- input_server('input')

  selected_procedure <- DT_server('DT', vars_in)

  resultsServer('results', vars_in, selected_procedure, version)

}

shinyApp(ui=ui,server=server)





