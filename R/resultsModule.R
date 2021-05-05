resultsUI <- function(id) {
  ns = NS(id)
  
  tagList(
    br(),
    uiOutput(ns('method_name')),
    br(),
    uiOutput(ns('prior_options')),
    actionButton(ns('run_method'),"Run Method"),
    helpText("Click the above button to run the selected method"),
    br(),
    uiOutput(ns('model_text_output')),
    br(),
    fluidRow(plotOutput(ns('model_plot_v2'),width="50%"),
             plotOutput(ns('doe_plot')))
    
  )
  
}

resultsServer <- function(id,vars_in,selected_procedure) {
  moduleServer(
    id,
    function(input,output,session) {
      
      output$method_name = renderUI({
        
        the_proc = selected_procedure()
        
        if(is.null(the_proc)) {
          return(NULL)
        }
        
        else{
          
          return(
            tagList(
              h3(the_proc)
            )
          )
          
        }

        
      })
      
      output$prior_options = renderUI({
        
        the_proc = selected_procedure()
        
        if(is.null(the_proc)) {
          return(NULL)
        }
        
        if(grepl('average',the_proc,TRUE)) {
          return(NULL)
          
        } else if(grepl('median',the_proc,TRUE)) {
          return(NULL)
          
        } else {
          
          default = mad(vars_in()$measured_vals)
          
          return(
            tagList(
              numericInput(session$ns('tau_prior_scale'),"Tau Prior Scale",value=default)
            )
          )
        }

        
      })
      
      res = eventReactive(input$run_method,{
        # do stats
        
        # res expects the following scalar values:
        # mu, mu_upper, mu_lower
        # tau, se (of the mean)
        
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
          res$tau = sqrt(DLres$tau2)
          res$se = DLres$se
          
          res$proc_complete = TRUE
          

        } else if(grepl('median',the_proc,TRUE)) {
          
          res$mu = spatstat.geom::weighted.median(x, 1/u^2)
          
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
            res$tau = NULL
          
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
            res$tau = NULL
            
          } 
          
          
          res$proc_complete = TRUE
          
          
        } else { # all Bayesian procedures
          
          # mu, mu_upper, mu_lower, se (of mu), tau, se (of x)
          # x, u
        
        if(grepl('gauss.+gauss',the_proc,TRUE)) {
          res$method = "Hierarchical Guass-Gauss"
          stan_filename = 'R/Stan/hgg.stan'
          jags_filename = 'R/Jags/hgg.txt'
          
        } else if(grepl('laplace',the_proc,TRUE)){
          res$method = "Hierarchical Laplace-Gauss"
          stan_filename = 'R/Stan/hlg.stan'
          jags_filename = 'R/Jags/hlg.txt'
          
        } else if(grepl('skew',the_proc,TRUE)) {
          res$method = "Skew Student-Gauss"
          stan_filename = 'R/Stan/hssg.stan'
          jags_filename = 'R/Jags/hssg.txt'
        }
          
          
        loaded_packages = .packages()
        
        if('rstan' %in% loaded_packages) {
          mcmc_sampler = 'stan'
        } else{
          mcmc_sampler = 'jags'
        }
        
        model_inits = function() {
          list(mu = mean(x),
               tau = sqrt(var(x)),
               lambda = x,
               sigma=u)
        }
        
        model_data = list(N=n, 
                          x=x, 
                          u2=u^2, 
                          dof=dof, 
                          med_abs_dif=isolate(input$tau_prior_scale))
        
        if(mcmc_sampler == 'stan') {

          # Run MCMC
          withProgress({
            stan_out = stan(file=stan_filename,
                            data=model_data,
                            init=model_inits,
                            iter=2000,
                            warmup=1000,
                            chains=1)
          },
          value=.5,
          message="Running MCMC...")
          
          stan_out = extract(stan_out)
          
          res$mu = mean(stan_out$mu)
          res$mu_upper = quantile(stan_out$mu,.975)
          res$mu_lower = quantile(stan_out$mu,.025)
          res$se = sqrt(var(stan_out$mu))
          res$tau = mean(stan_out$tau)
          
        } else if(mcmc_sampler == 'jags') {
          
          
          withProgress({
            jags_out = jags(data = model_data,
                            inits = model_inits,
                            model.file=jags_filename,
                            parameters.to.save = c('mu','tau','lambda','sigma'),
                            n.chains = 4,
                            n.iter = 3000)
          },
          value=.5,
          message="Running MCMC...")

          
          p_samples = jags_out$BUGSoutput$sims.list
          
          res$mu = mean(p_samples$mu)
          res$mu_upper = quantile(p_samples$mu,.975)
          res$mu_lower = quantile(p_samples$mu,.025)
          res$se = sqrt(var(p_samples$mu))
          res$tau = mean(p_samples$tau)
          
        }



        
        res$proc_complete = TRUE
        
        } 
        
        return(res)

      })
      
      observeEvent(res, {
        
        output$model_text_output = renderUI({
          # report the basics
          
          if(is.null(res()$proc_complete)) {
            return(NULL)
          }
          
          the_proc = isolate(selected_procedure())
          
          res = res()
          if(grepl('average',the_proc,TRUE)) {
            return(
              tagList(
                h3(paste("Results:",res$method)),
                h5(paste("Consensus estimate:",round(res$mu,3))),
                h5(paste("Standard uncertainty:", round(res$se,3))),
                h5(paste("95% coverage interval: (",round(res$mu_lower,3),", ",round(res$mu_upper,3),")",sep='')),
                h5(paste("Dark uncertainty (tau): ",round(sqrt(res$tau),3) ))
                
              )
            )
          } else if(grepl('median',the_proc,TRUE)) {
            return(
              tagList(
                br(),
                h3(paste("Results:",res$method)),
                h5(paste("Consensus estiamte"),round(res$mu,3)),
                h5(paste("Standard uncertainty:", round(res$se,3))),
                h5(paste("95% coverage interval: (",round(res$mu_lower,3),", ",round(res$mu_upper,3),")",sep=''))
              )
            )
          } else {
            return(
              tagList(
                br(),
                h3(paste("Results:",res$method)),
                h5(paste("Consensus estimate:",round(res$mu,3))),
                h5(paste("Standard uncertainty:", round(res$se,3))),
                h5(paste("95% coverage interval: (",round(res$mu_lower,3),", ",round(res$mu_upper,3),")",sep='')),
                h5(paste("Dark uncertainty (tau): ",round(sqrt(res$tau),3) ))
              )
            )
          }
          
        })
        
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
      
      output$model_plot_v2 = renderPlot({
        
        res = res()
        vars_in = vars_in()
        the_proc = isolate(selected_procedure())
        
        if(grepl('recommend',the_proc,ignore.case = T)) {
          the_proc = strsplit(the_proc,'\\(')[[1]][1]
        }
        
        KCplot(val=vars_in$measured_vals, 
               unc=vars_in$standard_unc, 
               tau=res$tau,
               kcrv=res$mu, 
               kcrv.unc=res$se,
               lab=paste('Lab',1:length(vars_in$measured_vals)), 
               title=paste("KCV Estimation:",the_proc), 
               title.placement="left",
               ylab=NULL, 
               exclude=NULL)
        
      })
      
      
    }
  )
}