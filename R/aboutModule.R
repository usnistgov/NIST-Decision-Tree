aboutUI = function(id) {
  
  ns = NS(id)
  
  tagList(
    br(),
    
    markdown(
      
      "
      ### About the NIST Decision Tree:
      <br>
      
      The NIST Decision Tree (NDT) is a web application that implements the 
      [Decision Tree for Key Comparisons](https://www.nist.gov/publications/decision-tree-key-comparisons)
      (Possolo et al. 2021), which is intended for use as an aid for scientists who carry
      out interlaboratory studies aimed at generating Key Comparison Reference Values (KCRV). 
      The NDT guides users through a series of hypothesis tests intended
      to help them in deciding upon an appropriate statistical model for their 
      particular data.
      The NDT then carries out the preferred statistical procedure, and displays
      the results via plots, tables, and a downloadable report. Results include the
      KCRV estimate and associated uncertainties (standard and expanded), 
      an estiamte of dark uncertainty (where applicable), and the MRA 
      degrees of equivalence and their uncertainties, among other results.
      For detailed instructions regarding the application, please click the 
      button below to download the User Manual.
      
      "
    ),
    br(),
    downloadButton(ns('manual'),"Download PDF Manual"),
    br(),
    br(),br(),br(),br(),br(),br(),br(),br()
  )
  
  
}

about_server <- function(id) {
  # when 'go' is clicked, this module server 
  # saves the data outside of the NS used for the input and 
  # decision trees
  moduleServer(
    id,
    function(input,output,session) {
      
      output$manual = downloadHandler(
        filename = "NDT_Manual.pdf",
        content = function(file) {
          file.copy("./NISTDecisionTree-UserManual.pdf",file)
        }
      )
      
    }
  )
}