resultsUI <- function(id) {
  ns = NS(id)
  
  tagList(
    br(),
    uiOutput(ns('method_name')),
    br(),
    uiOutput(ns('prior_options')),
    actionButton(ns('run_method'),"Run Method"),
    helpText("Click the button above to run the selected method. The results will be displayed below."),
    br(),
    hr(),
    br(),
    uiOutput(ns('model_text_output')),
    br(),
    uiOutput(ns('download_button_ui')),
    br(),
    fluidRow(
      column(6,plotOutput(ns('model_plot_v2'))),
      column(6,plotOutput(ns('doe_plot')))
    ),
    br(),
    h4("Unilateral Degrees of Equivalence Table"),
    br(),
    DT::dataTableOutput(ns('doe_table'),width='60%'),
    br()
    
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
          
          return(
            tagList(
              numericInput(session$ns('num_DL_DOE_bootstrap'),"Number Bootstrap for DOE",value=1000)
            )
          )
          
        } else if(grepl('median',the_proc,TRUE)) {
          
          return(
            tagList(
              numericInput(session$ns('num_median_bootstrap'),"Number Bootstrap Runs",value=1000)
            )
          )
          
        } else if(grepl('(laplace)|(gauss.+gauss)',the_proc,TRUE)) {
          
          default_tps = mad(vars_in()$measured_vals)
          default_sps = median(vars_in()$standard_unc)
          
          return(
            tagList(
              numericInput(session$ns('tau_prior_scale'),"Tau Prior Scale (Default: mad(x))",value=default_tps),
              numericInput(session$ns('sigma_prior_scale'),'Sigma Prior Scale (Default: median(u))',value=default_sps)
            )
          )
          
        } else {
          
          default_tps = mad(vars_in()$measured_vals)
          default_sps = median(vars_in()$standard_unc)
          
          return(
            tagList(
              numericInput(session$ns('tau_prior_scale'),"Tau Prior Scale",value=default_tps),
              numericInput(session$ns('nu_prior_scale'),"Nu Prior Scale",value=1),
              numericInput(session$ns('sigma_prior_scale'),'Sigma Prior Scale',value=default_sps)
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
            withProgress({
              xm.boot = boot(xw, xm, R=input$num_median_bootstrap)
              bootCI = boot.ci(xm.boot, conf=0.95,type='perc')
            },
            value=.5,
            message='Running Bootstrap...')

            res$se = sd(xm.boot$t)
            res$mu_lower = bootCI$percent[4]
            res$mu_upper = bootCI$percent[5]
            res$tau = NULL
            res$boot_samples = xm.boot
          
          } else {
            
            res$method = "Weighted Median with Parametric (Laplace) Bootrap"
            
            mu_laplace = res$mu
            weights = 1/u^2
            weights = weights/sum(weights)
            b_laplace = sum(abs(x - mu_laplace)*weights)
            
            nboot = input$num_median_bootstrap
            boot_samples = rmutil::rlaplace(n=nboot,m=mu_laplace,s=b_laplace)
            
            res$se = sd(boot_samples)
            hw = symmetricalBootstrapCI(boot_samples,res$mu,.95)
            res$mu_upper = res$mu + hw
            res$mu_lower = res$mu - hw
            res$tau = NULL
            res$boot_samples = boot_samples
            
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
          jags_filename = 'R/Jags/hssg2.txt'
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
                          med_abs_dif=isolate(input$tau_prior_scale),
                          sigma_prior_scale = isolate(input$sigma_prior_scale))
        
        parameters_to_save = c('mu','tau','lambda','sigma')
        
        if(grepl('skew',the_proc,TRUE) && mcmc_sampler == 'jags') {
          
          model_inits = function() {
            list(mu = mean(x),
                 tau = sqrt(var(x)),
                 sigma=u)
          }
          
          model_data$nu_prior_scale = isolate(input$nu_prior_scale)
          
          parameters_to_save = c(parameters_to_save,'delta','nu')
          
        }
        
        if(mcmc_sampler == 'stan') {

          # Run MCMC
          withProgress({
            stan_out = stan(file=stan_filename,
                            data=model_data,
                            init=model_inits,
                            iter=3000,
                            warmup=1000,
                            chains=4)
          },
          value=.5,
          message="Running MCMC...")
          
          stan_out = extract(stan_out)
          
          res$mu = mean(stan_out$mu)
          res$mu_upper = quantile(stan_out$mu,.975)
          res$mu_lower = quantile(stan_out$mu,.025)
          res$se = sd(stan_out$mu)
          res$tau = mean(stan_out$tau)
          
        } else if(mcmc_sampler == 'jags') {
          
          
          withProgress({
            jags_out = jags(data = model_data,
                            inits = model_inits,
                            model.file=jags_filename,
                            parameters.to.save = parameters_to_save,
                            n.chains = 4,
                            n.iter = 8000,
                            n.burnin = 3000)
          },
          value=.5,
          message="Running MCMC...")

          
          p_samples = jags_out$BUGSoutput$sims.list
          
          res$mu = mean(p_samples$mu)
          hw = symmetricalBootstrapCI(p_samples$mu,res$mu,.95)
          res$mu_upper = res$mu + hw
          res$mu_lower = res$mu - hw
          res$se = sd(p_samples$mu)
          res$tau = mean(p_samples$tau)
          res$tau_lower = quantile(p_samples$tau,.025)
          res$tau_upper = quantile(p_samples$tau,.975)
          res$p_samples = p_samples
          
        }

        res$proc_complete = TRUE
        
        } 
        
        return(res)

      })
      
      # Text output heading results
      observeEvent(res, {
        
        output$model_text_output = renderUI({
          # report the basics
          
          if(is.null(res()$proc_complete)) {
            return(NULL)
          }
          
          the_proc = isolate(selected_procedure())
          
          res = res()

          # DSL
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
            
          # weighted median
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
            
          # bayes
          } else {
            return(
              tagList(
                br(),
                h3(paste("Results:",res$method)),
                h5(paste("Consensus estimate:",round(res$mu,3))),
                h5(paste("Standard uncertainty:", round(res$se,3))),
                h5(paste("95% coverage interval: (",round(res$mu_lower,3),", ",round(res$mu_upper,3),")",sep='')),
                h5(paste("Dark uncertainty (tau) : ",round(sqrt(res$tau),3) )),
                h5(paste("Tau postererior 0.025 and 0.975 quantiles: ",'(',signif(res$tau_lower,3),',',signif(res$tau_upper,3),')',sep=''))
              )
            )
          }
          
        })
        
      })
      
      # degrees of equivalence
      doe_res = eventReactive(input$run_method, {
        
        vars_in = vars_in()
        the_proc = isolate(selected_procedure())
        
        if(grepl('average',the_proc,TRUE)) {
          
          data = vars_in()$the_data
          
          DLres = metafor::rma(yi=data$Result, 
                               sei=data$Uncertainty,
                               level=95,
                               method="DL")
          
          withProgress({
          
          doe_res = DoEUnilateralDL(data$Result,
                                    data$Uncertainty,
                                    data$DegreesOfFreedom,
                                    data$Laboratory,
                                    isolate(input$num_DL_DOE_bootstrap), # number bootstrap
                                    FALSE, # LOO
                                    .95, # coverage prob
                                    DLres) # dl res
          
          },
          value=.5,
          message='Computing DoE...')
          
          return(doe_res)
          
        } else if (grepl('hierarchical',the_proc,TRUE)) {
          
          p_samples = res()$p_samples
          vars_in = vars_in()
          data = vars_in()$the_data
          
          distances = matrix(0,nrow=length(p_samples$mu),ncol=nrow(vars_in()$the_data))
          colnames(distances) = data$Laboratory
          
          included_inds = which(vars_in$which_to_compute)
          counter = 1
          
          # go through each lab
          for(jj in 1:ncol(distances)) {
            
            # if lab was included in MCMC, use MCMC samples 
            if(vars_in$which_to_compute[jj]) {
              
              # lab random effect - KCV 
              distances[,jj] = p_samples$lambda[,counter] - p_samples$mu
              counter = counter + 1
            
            # if lab not included in model simulate from input data  
            } else {
            
              sd_vec = sqrt(data$Uncertainty[jj]^2)
              distances[,jj] = data$Result[jj] - p_samples$mu + rnorm(length(p_samples$mu),mean=0,sd=sd_vec)
            
            }
            
            
          }
          
          DoE.x = apply(distances,2,mean)
          DoE.U = apply(distances,2,sd)
          
          quants_lwr = rep(0,ncol(distances))
          quants_upr = rep(0,ncol(distances))
          
          for(ii in 1:ncol(distances)) {
            hw = symmetricalBootstrapCI(distances[,ii],DoE.x[ii],.95)
            quants_lwr[ii] = DoE.x[ii] - hw
            quants_upr[ii] = DoE.x[ii] + hw
          }
          
          outdf = data.frame(Lab=data$Laboratory,
                             DoE.x=DoE.x, 
                             DoE.U95=DoE.U,
                             DoE.Lwr=quants_lwr, 
                             DoE.Upr=quants_upr)
          
          return(list(DoE=outdf))
          
        } else if(grepl('median',the_proc,TRUE)) {
          
          res = res()
          data = vars_in()$the_data
          
          distances = matrix(0,nrow=length(res$boot_samples),ncol=nrow(data))
          colnames(distances) = data$Laboratory
          
          for(jj in 1:ncol(distances)) {
            
            sd_vec = data$Uncertainty
            
            distances[,jj] = data$Result[jj] - res$boot_samples + rnorm(length(res$boot_samples),mean=0,sd=sd_vec)
            
          }
          
          DoE.x = apply(distances,2,mean)
          DoE.U = apply(distances,2,sd)
          
          quants_lwr = rep(0,ncol(distances))
          quants_upr = rep(0,ncol(distances))
          
          for(ii in 1:ncol(distances)) {
            hw = symmetricalBootstrapCI(distances[,ii],DoE.x[ii],.95)
            quants_lwr[ii] = DoE.x[ii] - hw
            quants_upr[ii] = DoE.x[ii] + hw
          }
          
          
          outdf = data.frame(Lab=data$Laboratory,
                             DoE.x=data$Result, 
                             DoE.U95=DoE.U,
                             DoE.Lwr=quants_lwr, 
                             DoE.Upr=quants_upr)
          
          return(list(DoE=outdf))
          
        } else {
          
          return(NULL)
        }
        
        
      })
      
      # KCplot
      observeEvent(res, {
        
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
                 lab=vars_in$the_data$Laboratory[vars_in$which_to_compute], 
                 title=paste("KCV Estimation:",the_proc), 
                 title.placement="left",
                 ylab=NULL, 
                 exclude=NULL)
          
        })

      })
      
      # DoE Table
      observeEvent(doe_res, {
      
        output$doe_table = DT::renderDataTable({
          
          if(is.null(doe_res())) {
            return(NULL)
          }
          
          data = doe_res()$DoE
          
          data[,2:5] = signif(data[,2:5])
          
          return(data)
          
          },
          options=list(searching=FALSE,paging=FALSE),
          rownames = FALSE)
        
      })
      
      # DoE Plot
      observeEvent(doe_res, {
        
        output$doe_plot = renderPlot({
          
          if(is.null(doe_res())) {
            return(NULL)
          }
          
          data = doe_res()$DoE
          
          return(DoEplot(data,'Unilateral Degrees of Equivalence'))
          
        
        })
      })
      
      output$download_button_ui <- renderUI({
        
        res = res()
        
        if(is.null(res)) {
          return(NULL)
        }
        
        downloadButton(session$ns('download_all'),'Download .pdf Report')
      })
      
      output$download_all <- downloadHandler(
        filename = function() {
          paste("ResultsDownload.pdf")
        },
        
        content = function(file) {
          
          res = res()
          vars_in = vars_in()
          the_proc = isolate(selected_procedure())
          
          doe_res = doe_res()
          
          if(grepl('recommend',the_proc,ignore.case = T)) {
            the_proc = strsplit(the_proc,'\\(')[[1]][1]
          }
          
          withProgress(message = 'Preparing file for download...', {
            rmarkdown::render("./R/ResultsDownload.Rmd", 
                              params = list(res = res,
                                            vars_in = vars_in,
                                            the_proc = the_proc,
                                            doe_res = doe_res))
          })
          
          file.copy('./R/ResultsDownload.pdf',file)
          
        }
      )
      
    }
  )
}