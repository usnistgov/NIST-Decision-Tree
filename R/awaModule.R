awa_UI <- function(id) {
  ns = NS(id)
  
  tagList(
    uiOutput(ns('model_text_output')),
    br(),
    br(),
    h4("Consensus and Lab Estimates"),
    fluidRow(plotOutput(ns('model_plot'),width="80%"))
  )
  
}

awa_server <- function(id,vars_in) {
  moduleServer(
    id,
    function(input,output,session) {
      
      dl_res = eventReactive(vars_in,{
        
        res = metafor::rma(yi=vars_in()$measured_vals, 
                           sei=vars_in()$standard_unc,
                           level=95,
                           method="DL")
        res
      })
      
      output$model_text_output = renderUI({
        
        res = dl_res()
        
        tagList(
          br(),
          h3(paste("DerSimonian-Laird Results:")),
          h5(paste("Consensus estimate:",round(res$beta,3))),
          h5(paste("Standard uncertainty:", round(res$se,3))),
          h5(paste("95% coverage interval: (",round(res$ci.lb,3),", ",round(res$ci.ub,3),")",sep='')),
          h5(paste("Dark uncertainty (tau): ",round(sqrt(res$tau2),3) ))
             
        )
        
      })
      
      output$model_plot = renderPlot({
        
        res = dl_res()
        vars_in = as.data.frame(vars_in())
        vars_in$lab = factor(1:nrow(vars_in))
        vars_in$lower = vars_in$measured_vals - vars_in$standard_unc
        vars_in$upper = vars_in$measured_vals + vars_in$standard_unc
        
        se_all = sqrt(res$tau2 + res$se**2)
        
        gmrib_data = data.frame(x=0:(nrow(vars_in)+1),ymin = res$beta - se_all,ymax=res$beta+se_all)
        
        ggplot(vars_in,aes(x=lab,y=measured_vals)) + 
          geom_point() +
          geom_hline(yintercept = res$beta) +
          geom_errorbar(aes(ymin=lower,ymax=upper),width=.1,size=1) +
          geom_ribbon(data=gmrib_data,
                      aes(x=x,ymin=ymin,ymax=ymax),inherit.aes = FALSE,alpha=.2) +
          ylab("Measured Value") + 
          xlab("Lab")
        
      })
      
      
    }
  )
}