resultsUI <- function(id) {
  ns = NS(id)
  
  tagList(
    br(),
    uiOutput(ns('init_message')),
    uiOutput(ns('method_name')),
    br(),
    uiOutput(ns('prior_options')),
    uiOutput(ns('run_text')),
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
      
      output$run_text = renderUI({
        
        the_proc = selected_procedure()
        
        if(is.null(the_proc)) {
          return(NULL)
        } 
        tagList(
          hr(),
          actionButton(session$ns('run_method'),"Run Method"),
          helpText("Click the button above to run the selected method. The results will be displayed below."),
        )
      })
      
      output$method_name = renderUI({
        
        the_proc = selected_procedure()
        
        if(is.null(the_proc)) {
          return(NULL)
        }
        
        else{
          
          return(
            tagList(
              h3(paste0("Selected Procedure: ",the_proc)),
              p(paste0("After you have confirmed your selections for the parameters below, ",
                       "click the 'Run Method' button to run the analysis. ",
                       "Once finished, you may download a .pdf report with the results of the analysis."))
            )
          )
          
        }

        
      })
      
      output$init_message = renderUI({
        
        sp = selected_procedure()
        
        if(is.null(sp)) {
          return(p("Data has not been validated or model has not been selected. Please complete Tabs 1-2 first."))
        }
        
        return(p(""))
        
        
      })
      
      output$prior_options = renderUI({
        
        the_proc = selected_procedure()
        
        if(is.null(the_proc)) {
          return(NULL)
        }
        
        if(grepl('average',the_proc,TRUE)) {
          
          return(
            tagList(
              hr(),
              br(),
              h3("General Parameters",style='text-align:center'),
              br(),
              fluidRow(
                column(3,
                  numericInput(session$ns('random_seed'),"Random Number Seed",value=sample(1:1000,size=1)),
                ),
                column(3,
                  numericInput(session$ns('nsd'),"Number of Significant Digits Reported",
                               value=4,
                               min=1,
                               step=1)
                )
                  
              ),
              br(),
              hr(),
              br(),
              h3("Model Estimation Parameters",style='text-align:center'),
              br(),
              numericInput(session$ns('num_DL_DOE_bootstrap'),"Number of Boostrap Replicates for Uncertainty Evaluations",value=5000)
            )
          )
          
        } else if(grepl('median',the_proc,TRUE)) {
          
          return(
            tagList(
              hr(),
              br(),
              h3("General Parameters",style='text-align:center'),
              br(),
              fluidRow(
                column(3,
                       numericInput(session$ns('random_seed'),"Random Number Seed",value=sample(1:1000,size=1)),
                ),
                column(3,
                       numericInput(session$ns('nsd'),"Number of Significant Digits Reported",
                                    value=4,
                                    min=1,
                                    step=1)
                )
                
              ),
              br(),
              hr(),
              br(),
              h3("Model Estimation Parameters",style='text-align:center'),
              br(),
              numericInput(session$ns('num_median_bootstrap'),"Number Bootstrap Runs",value=5000)
            )
          )
          
        } else if(grepl('(laplace)|(gauss.+gauss)',the_proc,TRUE)) {
          
          default_tps = mad(vars_in()$measured_vals)
          default_sps = median(vars_in()$standard_unc)
          
          return(
            tagList(
              h3("General Parameters",style='text-align:center'),
              br(),
              fluidRow(
                column(3,
                       numericInput(session$ns('random_seed'),"Random Number Seed",value=sample(1:1000,size=1)),
                ),
                column(3,
                       numericInput(session$ns('nsd'),"Number of Significant Digits Reported",
                                    value=4,
                                    min=1,
                                    step=1)
                )
                
              ),
              hr(),
              h3("MCMC Parameters",style='text-align:center'),
              br(),
              fluidRow(
                column(3,numericInput(session$ns('total_mcmc'),"Total Number of MCMC Steps",value=250000)),
                column(3,numericInput(session$ns('burnin_mcmc'),"Number of MCMC Warm-Up Steps",value=125000)),
                column(3,numericInput(session$ns('thin_mcmc'),"Keep an MCMC Draw Every ____ Steps",value=10)),
              ),
              hr(),
              br(),
              h3("Prior Distribution Parameters",style='text-align:center'),
              br(),
              fluidRow(
                column(3,numericInput(session$ns('tau_prior_scale'),"Tau Prior Median (Default: mad(x))",value=default_tps)),
                column(3,numericInput(session$ns('sigma_prior_scale'),'Sigma Prior Median (Default: med(u))',value=default_sps))
              ),
              br()
              
            )
          )
          
        } else {
          
          default_tps = mad(vars_in()$measured_vals)
          default_sps = median(vars_in()$standard_unc)
          
          return(
            tagList(
              
              h3("General Parameters",style='text-align:center'),
              br(),
              fluidRow(
                column(4,
                       numericInput(session$ns('random_seed'),"Random Number Seed",value=sample(1:1000,size=1)),
                ),
                column(4,
                       numericInput(session$ns('nsd'),"Number of Significant Digits Reported",
                                    value=4,
                                    min=1,
                                    step=1)
                )
                
              ),
              hr(),
              br(),
              h3("MCMC Parameters",style='text-align:center'),
              br(),
              fluidRow(
                column(3,numericInput(session$ns('total_mcmc'),"Total Number of MCMC Steps",value=250000)),
                column(3,numericInput(session$ns('burnin_mcmc'),"Number of MCMC Warm-Up Steps",value=125000)),
                column(3,numericInput(session$ns('thin_mcmc'),"Keep an MCMC Draw Every ____ Steps",value=10))
              ),
              br(),
              hr(),
              br(),
              h3("Prior Distribution Parameters",style='text-align:center'),
              br(),
              fluidRow(
                column(3,numericInput(session$ns('tau_prior_scale'),"Tau Prior Scale (Default: mad(x))",value=default_tps)),
                column(3,numericInput(session$ns('nu_prior_shape'),"Gamma Shape for Nu Prior Scale",value=3)),
                column(3,numericInput(session$ns('nu_prior_scale'),"Gamma Scale for Nu Prior Scale",value=0.25))
              ),
              fluidRow(
                column(3,numericInput(session$ns('sigma_prior_scale'),'Sigma Prior Scale (Default: med(x))',value=default_sps)),
                column(3,numericInput(session$ns('alpha_prior_scale'),'Alpha (Skewness) Prior Scale',value=4))
              )
            )
          )
        }

        
      })
      
      res = eventReactive(input$run_method,{
        # do stats
        
        # res expects the following scalar values:
        # mu, mu_upper, mu_lower
        # tau, se (of the mean)
        
        
        
        validate(
          need(is.numeric(input$random_seed),
               'Random Seed must be a positive integer.'),
          need(input$nsd %in% 2:10,
               "Number of significant digits must be between 2 and 10.")
        )
        
        if(grepl('(gauss)|(laplace)|(skew)',selected_procedure(),TRUE)) {
          
          jags_params = list(n_iter = round(input$total_mcmc),
                             burn_in = round(input$burnin_mcmc), 
                             thin = round(input$thin_mcmc))
          
          validate(
            need( (jags_params$n_iter > 0) && (jags_params$burn_in > 0) && (jags_params$thin > 0),
                  "MCMC parameters must all be positive."),
            need( jags_params$burn_in < jags_params$n_iter,
                  "Number of warm up MCMC draws must be less than the total number of MCMC draws."),
            need(jags_params$n_iter <= 500000,
                 "Total MCMC draws must be less than or equal to 500000."),
            need(jags_params$thin <= 50,
                 "Thinning rate for MCMC draws must be less than or equal to 50.")
          )
          
        }
        

        set.seed(abs(round(input$random_seed)))
        
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
          
          withProgress({
            
            bootDL_res = bootDL(input$num_DL_DOE_bootstrap,
                                thedat=data.frame(x=x,u=u,dof=dof),
                                themle=list(mu=DLres$beta))
            
          },value=.33,
          message = "Running Bootstrap for KCRV 1/2")
        
          
          
          res$se_dslbs = sd(bootDL_res)
          hw_dslbs = symmetricalBootstrapCI(bootDL_res,as.numeric(DLres$beta),.95)
          res$mu_upper_dslbs = DLres$beta + hw_dslbs
          res$mu_lower_dslbs = DLres$beta - hw_dslbs
          
          res$mu = DLres$beta
          res$mu_upper = DLres$ci.ub
          res$mu_lower = DLres$ci.lb
          res$tau = sqrt(DLres$tau2)
          res$se = DLres$se
          
          res$proc_complete = TRUE
          

        } else if(grepl('median',the_proc,TRUE)) {
          
          if(length(x) >= 15) {
            
            res$method = "Weighted Median with Nonparametric Bootstrap"
  
          
          } else {
            
            res$method = "Weighted Median with Parametric (Laplace) Bootstrap"
          
          } 
          
          withProgress({
            bt_res = weightedMedian(x=x, ux=u, nux=dof, K=5000, conf.level=0.95,
                                      bootstrap=NULL, print=FALSE)
          },
          value=.33,
          message="Running Bootstrap for KCRV")
          
          res$mu = bt_res$m
          res$se = bt_res$um
          res$mu_upper = bt_res$Upr
          res$mu_lower = bt_res$Lwr
          res$tau = NULL
          res$boot_samples = bt_res$muB
          
          
          res$proc_complete = TRUE
          
          
        } else { # all Bayesian procedures
          
          # mu, mu_upper, mu_lower, se (of mu), tau, se (of x)
          # x, u
        
        if(grepl('gauss.+gauss',the_proc,TRUE)) {
          res$method = "Hierarchical Gauss-Gauss"
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
               tau = mad(x),
               lambda = x,
               sigma = u)
        }
        
        model_data = list(N=n, 
                          x=x, 
                          u2=u^2, 
                          dof=dof, 
                          tau_prior_scale=isolate(input$tau_prior_scale),
                          sigma_prior_scale = isolate(input$sigma_prior_scale))
        
        parameters_to_save = c('mu','tau','lambda','sigma')
        
        if(grepl('skew',the_proc,TRUE) && mcmc_sampler == 'jags') {
          
          model_inits = function() {
            list(mu = mean(x),
                 tau = sqrt(var(x)),
                 sigma=u)
          }
          
          model_data$nu_prior_shape = isolate(input$nu_prior_shape)
          model_data$nu_prior_scale = isolate(input$nu_prior_scale)
          model_data$alpha_prior_scale = isolate(input$alpha_prior_scale)
          
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
                            n.iter = jags_params$n_iter,
                            n.burnin = jags_params$burn_in,
                            n.thin = jags_params$thin)
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
          
          nsd = input$nsd
          
          the_proc = isolate(selected_procedure())
          
          res = res()

          # DSL
          if(grepl('average',the_proc,TRUE)) {
            return(
              tagList(
                h3(paste("Results:",res$method)),
                h5(paste("Consensus estimate:",signif(res$mu,nsd))),
                h5(paste("Standard uncertainty:", signif(res$se,nsd))),
                h5(paste("Standard uncertainty (using parametric bootstrap):", signif(res$se_dslbs,nsd))),
                h5(paste("95% coverage interval: (",signif(res$mu_lower,nsd),", ",signif(res$mu_upper,nsd),")",sep='')),
                h5(paste("95% coverage interval (using parametric bootstrap): (",signif(res$mu_lower_dslbs,nsd),", ",signif(res$mu_upper_dslbs,nsd),")",sep='')),
                h5(paste("Dark uncertainty (tau): ",signif(res$tau,nsd) ))
                
              )
            )
            
          # weighted median
          } else if(grepl('median',the_proc,TRUE)) {
            return(
              tagList(
                br(),
                h3(paste("Results:",res$method)),
                h5(paste("Consensus estimate"),signif(res$mu,nsd)),
                h5(paste("Standard uncertainty:", signif(res$se,nsd))),
                h5(paste("95% coverage interval: (",signif(res$mu_lower,nsd),", ",signif(res$mu_upper,nsd),")",sep=''))
              )
            )
            
          # bayes
          } else {
            return(
              tagList(
                br(),
                h3(paste("Results:",res$method)),
                h5(paste("Consensus estimate:",signif(res$mu,nsd))),
                h5(paste("Standard uncertainty:", signif(res$se,nsd))),
                h5(paste("95% coverage interval: (",signif(res$mu_lower,nsd),", ",signif(res$mu_upper,nsd),")",sep='')),
                h5(paste("Dark uncertainty (tau) : ",signif(res$tau,nsd) )),
                h5(paste("Tau posterior 0.025 and 0.975 quantiles: ",'(',signif(res$tau_lower,nsd),',',signif(res$tau_upper,nsd),')',sep=''))
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
          
          withProgress({
          
          DLres = metafor::rma(yi=data$Result[data$Include], 
                               sei=data$Uncertainty[data$Include],
                               level=95,
                               method="DL")
          

          
          doe_res = DoEUnilateralDL(data$Result,
                                    data$Uncertainty,
                                    data$DegreesOfFreedom,
                                    as.character(data$Laboratory),
                                    isolate(input$num_DL_DOE_bootstrap), # number bootstrap
                                    FALSE, # LOO
                                    .95, # coverage prob
                                    DLres,
                                    exclude = !data$Include) # dl res
          
          },
          value=.66,
          message='Running Bootstrap for DoE 2/2')
          
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
              
              # lab random effect - KCRV
              sd_vec = sqrt(p_samples$tau^2 + p_samples$sigma[,counter]^2)
              
              distances[,jj] = data$Result[jj] - p_samples$mu + rnorm(length(p_samples$mu),mean=0,sd=sd_vec)
              counter = counter + 1
            
            # if lab not included in model simulate from input data  
            } else {
            
              sd_vec = sqrt(p_samples$tau^2 + data$Uncertainty[jj]^2)
              distances[,jj] = data$Result[jj] - p_samples$mu + rnorm(length(p_samples$mu),mean=0,sd=sd_vec)
            
            }
            
            
          }
          
          DoE.x = data$Result - res()$mu 
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
                             DoE.U95=DoE.U*2,
                             DoE.Lwr=quants_lwr, 
                             DoE.Upr=quants_upr)
          
          return(list(DoE=outdf))
          
        } else if(grepl('median',the_proc,TRUE)) {
          
          res = res()
          data = vars_in()$the_data
          
          distances = matrix(0,nrow=length(res$boot_samples),ncol=nrow(data))
          colnames(distances) = data$Laboratory
          
          
          withProgress({
            
            for(jj in 1:ncol(distances)) {
              
              sd_vec = data$Uncertainty
              
              distances[,jj] = data$Result[jj] - res$boot_samples + rnorm(length(res$boot_samples),mean=0,sd=sd_vec[jj])
              
            }
            
          },
          message='Computing DoE',
          value = .66)
          
          
          
          DoE.x = data$Result - res$mu
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
                             DoE.U95=DoE.U*2,
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
          
          if(grepl('average',the_proc,TRUE)) {
            kcrv.unc = max(res$se,res$se_dslbs)
          } else {
            kcrv.unc = res$se
          }
          
          KCplot(val=vars_in$the_data$Result, 
                 unc=vars_in$the_data$Uncertainty, 
                 tau=res$tau,
                 kcrv=res$mu, 
                 kcrv.unc=kcrv.unc,
                 lab=vars_in$the_data$Laboratory, 
                 title=paste("KCRV Estimation:",the_proc), 
                 title.position="left",
                 ylab=NULL, 
                 exclude=vars_in$the_data$Laboratory[!vars_in$which_to_compute])
          
        })

      })
      
      # DoE Table
      observeEvent(doe_res, {
      
        output$doe_table = DT::renderDataTable({
          
          if(is.null(doe_res())) {
            return(NULL)
          }
          
          data = doe_res()$DoE
          
          data[,2:5] = signif(data[,2:5],input$nsd)
          
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
          vars_in = vars_in()
          
          return(DoEplot(data,
                         'Unilateral Degrees of Equivalence',
                         exclude=vars_in$the_data$Laboratory[!vars_in$which_to_compute]))
          
        
        })
      })
      
      output$download_button_ui <- renderUI({
        
        res = res()
        
        if(is.null(res)) {
          return(NULL)
        }
        
        downloadButton(session$ns('download_all'),'Download Report (PDF File)')
      })
      
      output$download_all <- downloadHandler(
        filename = function() {
          paste("ResultsDownload.pdf")
        },
        
        content = function(file) {
          
          res = res()
          vars_in = vars_in()
          the_proc = isolate(selected_procedure())
          nsd = input$nsd
          doe_res = doe_res()
          
          if(grepl('recommend',the_proc,ignore.case = T)) {
            the_proc = strsplit(the_proc,'\\(')[[1]][1]
          }
          
          withProgress(message = 'Preparing file for download...', {
            rmarkdown::render("./R/ResultsDownload.Rmd", 
                              params = list(res = res,
                                            vars_in = vars_in,
                                            the_proc = the_proc,
                                            doe_res = doe_res,
                                            nsd = nsd))
          })
          
          file.copy('./R/ResultsDownload.pdf',file)
          
        }
      )
      
    }
  )
}