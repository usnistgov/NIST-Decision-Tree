resultsUI <- function(id) {
  ns = NS(id)
  
  tagList(
    uiOutput(ns('model_text_output')),
    br(),
    br(),
    h4("Consensus and Lab Estimates"),
    fluidRow(plotOutput(ns('model_plot'),width="80%"))
  )
  
}

resultsServer <- function(id,vars_in,selected_procedure) {
  moduleServer(
    id,
    function(input,output,session) {
      

      res = eventReactive(selected_procedure(),{
        # do stats
        
        the_proc = selected_procedure()
        res = list()
        x = vars_in()$measured_vals
        u = vars_in()$standard_unc
        dof = vars_in()$dof
        n = length(x)
        
        if(is.null( the_proc )) {
          return(NULL)
        }
        
        # adaptive weighted average
        if(grepl('average',the_proc,TRUE)) {
          DLres = metafor::rma(yi=x, 
                               sei=u,
                               level=95,
                               method="DL")
          
          res$method = "Adaptive Weighted Average"
          
          res$mu = DLres$beta
          res$mu_upper = DLres$ci.ub
          res$mu_lower = DLres$ci.lb
          res$tau2 = DLres$tau2
          res$se = DLres$se
          
          res$proc_complete = TRUE
          

        } else if(grepl('median',the_proc,TRUE)) {
          
          res$mu = spatstat::weighted.median(x, 1/u^2)
          
          if(length(x) >= 12) {
            
            res$method = "Weighted Median with Nonparametric Bootstrap"
            
            xm = function (xw,i) {
              spatstat::weighted.median(xw[i,1], xw[i,2])
            }
            
            xw = cbind(x, 1/u^2)
            xm.boot = boot(xw, xm, R=5000)
            bootCI = boot.ci(xm.boot, conf=0.95,type='perc')
            res$se = sd(xm.boot$t)
            res$mu_lower = bootCI$percent[4]
            res$mu_upper = bootCI$percent[5]
          
          } else {
            
            res$method = "Weighted Median with Parametric (Laplace) Bootrap"
            
            mu_laplace = res$mu
            weights = 1/u
            weights = weights/sum(weights)
            b_laplace = sum(abs(x - mu_laplace)*weights)
            
            nboot = 5000
            boot_samples = rmutil::rlaplace(n=nboot,m=mu_laplace,s=b_laplace)
            
            res$se = sd(boot_samples)
            res$mu_upper = quantile(boot_samples,0.975)
            res$mu_lower = quantile(boot_samples,0.025)
            
          } 
          
          
          res$proc_complete = TRUE
          
          
        } else { # all Bayesian procedures
          
          # mu, mu_upper, mu_lower, se (of mu), tau2, se (of x)
          # x, u
        
        if(grepl('gauss.+gauss',the_proc,TRUE)) {
          res$method = "Heirarchical Guass-Gauss"
          filename = 'R/hgg.stan'
        } else if(grepl('laplace',the_proc,TRUE)){
          res$method = "Heirarchical Laplace-Gauss"
          filename = 'R/hlg.txt'
        } else if(grepl('skew',the_proc,TRUE)) {
          res$method = "Skew Student-Gauss"
          filename = 'R/hssg.txt'
        }

        # jags setup
        tauPriorScale=mad(x)
        jags_data = list(n=n, x=x, s2=u^2, dof=dof, tauPriorScale=tauPriorScale, u_prior_scale=median(u))
        
        if(res$method == "Skew Student-Gauss") {
          # for "zero's trick"
          jags_data$z = rep(0,n)
        }
        
        jags_inits = function() {
          list(mu = mean(x),
               tau = 1,
               xi = x,
               ui = u) 
        }
        
        # Parameters to estimate
        jags_params = c("mu", "tau", "xi", "ui")
        
        # Run MCMC
        
        jags_out = jags(jags_data,
                        inits=jags_inits,
                        parameters.to.save=jags_params,
                        model.file=filename,
                        n.iter=4000,
                        n.chains=1)
        
        jags_out = as.mcmc(jags_out)
        jags_out = jags_out[[1]]
        
        res$mu = mean(jags_out[,'mu'])
        res$mu_upper = quantile(jags_out[,'mu'],.975)
        res$mu_lower = quantile(jags_out[,'mu'],.025)
        res$se = sqrt(var(jags_out[,'mu']))
        res$tau2 = mean(jags_out[,'tau']**2)
        
        res$proc_complete = TRUE
        
        } 
        
        return(res)

      })
      
      output$model_text_output = renderUI({
        # report the basics
        
        if(is.null(res()$proc_complete)) {
          return(NULL)
        }
        
        the_proc = selected_procedure()
        
        res = res()
        if(grepl('average',the_proc,TRUE)) {
          return(
            tagList(
              br(),
              h3(paste(res$method)),
              h5(paste("Consensus estimate:",round(res$mu,3))),
              h5(paste("Standard uncertainty:", round(res$se,3))),
              h5(paste("95% coverage interval: (",round(res$mu_lower,3),", ",round(res$mu_upper,3),")",sep='')),
              h5(paste("Dark uncertainty (tau): ",round(sqrt(res$tau2),3) ))
              
            )
          )
        } else if(grepl('median',the_proc,TRUE)) {
          return(
            tagList(
              br(),
              h3(paste(res$method)),
              h5(paste("Consensus estiamte"),round(res$mu,3)),
              h5(paste("Standard uncertainty:", round(res$se,3))),
              h5(paste("95% coverage interval: (",round(res$mu_lower,3),", ",round(res$mu_upper,3),")",sep=''))
            )
          )
        } else {
          return(
            tagList(
              br(),
              h3(paste(res$method)),
              h5(paste("Consensus estimate:",round(res$mu,3))),
              h5(paste("Standard uncertainty:", round(res$se,3))),
              h5(paste("95% coverage interval: (",round(res$mu_lower,3),", ",round(res$mu_upper,3),")",sep='')),
              h5(paste("Dark uncertainty (tau): ",round(sqrt(res$tau2),3) ))
            )
          )
        }

        
      })
      
      output$model_plot = renderPlot({
        # make plot of values
        
        res = res()
        vars_in = as.data.frame(vars_in())
        vars_in$lab = factor(1:nrow(vars_in))
        vars_in$lower = vars_in$measured_vals - vars_in$standard_unc
        vars_in$upper = vars_in$measured_vals + vars_in$standard_unc
        
        the_proc = selected_procedure()
        
        # consensus value +/- standard error of mu
        gmrib_data = data.frame(x=0:(nrow(vars_in)+1),ymin = res$mu - res$se,ymax=res$mu+res$se)
        
        # plot the raw data
        p = ggplot(vars_in,aes(x=lab,y=measured_vals)) + 
              geom_point() +
              geom_hline(yintercept = res$mu) + # consensus estimate
              geom_errorbar(aes(ymin=lower,ymax=upper),width=.1,size=1) + # user data
              geom_ribbon(data=gmrib_data, 
                          aes(x=x,ymin=ymin,ymax=ymax),inherit.aes = FALSE,alpha=.2) + # KCV confidence interval
              ylab("Measured Value") + 
              xlab("Lab")
        
        return(p)

        
      })
      
      
    }
  )
}