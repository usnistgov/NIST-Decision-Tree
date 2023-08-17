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
    uiOutput(ns('doe_type_radio_buttons')),
    br(),
    fluidRow(
      column(6,plotOutput(ns('model_plot_v2'))),
      column(6,plotOutput(ns('doe_plot')))
    ),
    br(),
    h3("Unilateral Degrees of Equivalence Table"),
    br(),
    DT::dataTableOutput(ns('doe_table'),width='60%'),
    br(),
    h3("Table of Uncertainties for Each Lab"),
    DT::dataTableOutput(ns('results_table_out'),width='100%'),
    br(),
    uiOutput(ns('download_table_button_ui')),
    br(),
    uiOutput(ns('table_heading')),
    fluidRow(
      column(6,uiOutput(ns("table_explanation_left"))),
      column(6,uiOutput(ns("table_explanation_right")))
    ),
    br(),
    uiOutput(ns('jags_diagnostics_header')),
    DT::dataTableOutput(ns('diagnostic_table'),width='50%'),
    br()

  )

}

resultsServer <- function(id,vars_in,selected_procedure,version) {
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

      output$doe_type_radio_buttons = renderUI({

        if(is.null(res())) {
          return(NULL)
        }
        return(tagList(
          radioButtons(session$ns("doe_type"), label = "Type of DoEs to Display",
                       choices = list("DoEs Recognizing Dark Uncertainty"=1,
                                      "DoEs Ignoring Dark Uncertainty"=2))
        ))

      })

      output$init_message = renderUI({

        sp = selected_procedure()

        if(is.null(sp)) {
          return(p("Data has not been validated or model has not been selected. Please complete Tabs 1-2 first."))
        }

        return(p(""))


      })

      output$jags_diagnostics_header = renderUI({

        if(is.null(res()) | is.null(res()$diagnostics)){
          return(NULL)
        }

        return(tagList(h4("MCMC Sampler Diagnostics"),
                       br(),
                       p("If one of the Bayesian models is run (Hierarchical Gauss-Gauss, Hierarchical Laplace-Gauss, or Hierarchical Skew-Student-t), then diagnostics for the MCMC sampler will be given below.
                         As a general recommendation, if any of the R-hat values are greater than 1.05, then the sampler may not have reached equilibrium, and the 'Total Number of MCMC Steps' should be increased, and the run repeated.
                         The 'Number of MCMC Warm-Up Steps' should be about half of the 'Total Number of MCMC Steps.' The 'Effective Sample Size' (n.eff) is approximately the size of the MCMC sample that the results are based on.")))


      })

      output$table_heading = renderUI({

        if(is.null(res())) {
          return(NULL)
        }

        h4("Column Name Descriptions",align="center")

      })

      output$table_explanation_left = renderUI({

        if(is.null(res())) {
          return(NULL)
        }

        return(tagList(
          p(
            get_table_descriptions_left()
          )
        ))

      })

      output$table_explanation_right = renderUI({

        if(is.null(res())) {
          return(NULL)
        }

        return(tagList(
          p(
            get_table_descriptions_right()
          )
        ))

      })

      output$prior_options = renderUI({

        the_proc = selected_procedure()

        if(is.null(the_proc)) {
          return(NULL)
        }

        x = vars_in()$measured_vals
        u = vars_in()$standard_unc
        
        def_priors = get_default_priors(x,u)

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
              numericInput(session$ns('num_bootstrap'),"Number of Boostrap Replicates for Uncertainty Evaluations",value=5000)
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
                column(3,numericInput(session$ns('mu_prior_loc'),"Mu Prior Location (Default: mean(x))",value=def_priors$mu_prior_loc)),
                column(3,numericInput(session$ns('mu_prior_scale'),"Mu Prior Scale (Default: sd(x)/sqrt(3))",value=def_priors$mu_prior_scale)),
                column(3,numericInput(session$ns('tau_prior_scale'),"Tau Prior Median (Default: mad(x))",value=def_priors$tau_prior_scale)),
                column(3,numericInput(session$ns('sigma_prior_scale'),'Sigma Prior Median (Default: med(u))',value=def_priors$sigma_prior_scale))
              ),
              br()

            )
          )

        } else {
          # HSSG
          
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
                column(3,numericInput(session$ns('mu_prior_loc'),"Mu Prior Location (Default: mean(x))",value=def_priors$mu_prior_loc)),
                column(3,numericInput(session$ns('mu_prior_scale'),"Mu Prior Scale (Default: sd(x)/sqrt(3))",value=def_priors$mu_prior_scale)),
                column(3,numericInput(session$ns('tau_prior_scale'),"Tau Prior Scale (Default: mad(x))",value=def_priors$tau_prior_scale)),
                column(3,numericInput(session$ns('nu_prior_shape'),"Gamma Shape for Nu Prior Scale",value=def_priors$nu_prior_shape)),
                column(3,numericInput(session$ns('nu_prior_scale'),"Gamma Scale for Nu Prior Scale",value=def_priors$nu_prior_scale))
              ),
              fluidRow(
                column(3,numericInput(session$ns('sigma_prior_scale'),'Sigma Prior Scale (Default: med(x))',value=def_priors$sigma_prior_scale)),
                column(3,numericInput(session$ns('alpha_prior_scale'),'Alpha (Skewness) Prior Scale',value=def_priors$alpha_prior_scale))
              )
            )
          )
        }


      })

      res = eventReactive(input$run_method,{

        validate(
          need(is.numeric(input$random_seed),
               'Random Seed must be a positive integer.'),
          need(input$nsd %in% 2:10,
               "Number of significant digits must be between 2 and 10.")
        )

        jags_params = NULL

        if(grepl('(gauss)|(laplace)|(skew)',selected_procedure(),TRUE)) {

          jags_params = list(n_iter = round(isolate(input$total_mcmc)),
                             burn_in = round(isolate(input$burnin_mcmc)),
                             thin = round(isolate(input$thin_mcmc)))

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

        x = vars_in()$measured_vals
        u = vars_in()$standard_unc
        dof = vars_in()$dof
        the_proc = selected_procedure()
        seed = input$random_seed
        num_bootstrap = input$num_bootstrap

        priors = list(
          mu_prior_loc = input$mu_prior_loc,
          mu_prior_scale = input$mu_prior_scale,
          tau_prior_scale = input$tau_prior_scale,
          sigma_prior_scale = input$sigma_prior_scale,
          nu_prior_shape = input$nu_prior_shape,
          nu_prior_scale = input$nu_prior_scale,
          alpha_prior_scale = input$alpha_prior_scale
        )

        withProgress({


          res = run_ndt_method(x=x,
                               u=u,
                               dof=dof,
                               the_proc = the_proc,
                               num_bootstrap = num_bootstrap,
                               jags_params = jags_params,
                               seed = seed,
                               priors = priors)

        },value = 1,message="Running selected procedure...")

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

      # jags diagnostics
      observeEvent(res,{

        output$diagnostic_table = DT::renderDataTable({

          if(is.null(res()) | is.null(res()$diagnostics)) {
            return(NULL)
          }

          diagnostics = res()$diagnostics
          diagnostics$Rhat = round(diagnostics$Rhat,3)

          return(diagnostics)

        },options=list(searching=FALSE,paging=FALSE))

      })

      results_table = eventReactive(res(), {

        if(is.null(res())) {
          return(NULL)
        }

        vars_in = vars_in()
        the_proc = isolate(selected_procedure())
        res = res()
        num_bootstrap = input$num_bootstrap

        doe_table = compute_doe_table(the_proc=the_proc,
                                                         vars_in=vars_in,
                                                         res=res,
                                                         num_bootstrap=num_bootstrap)$DoE

        out_table = summary_table(ndt_res=res,
                                  vars_in=vars_in,
                                  doe_table=doe_table)

        out_table[,2:ncol(out_table)] = round(out_table[,2:ncol(out_table)],input$nsd)

        return(out_table)


      })

      observeEvent(results_table, {

        output$results_table_out = DT::renderDataTable({

          if(is.null(results_table())) {
            return(NULL)
          }

          return(results_table())


        },options=list(searching=FALSE,paging=FALSE))

      })

      # degrees of equivalence
      doe_res = eventReactive(res(), {

        vars_in = vars_in()
        the_proc = isolate(selected_procedure())
        res = res()
        num_bootstrap = input$num_bootstrap

        withProgress({

          out = compute_doe_table(the_proc=the_proc,
                                                     vars_in=vars_in,
                                                     res=res,
                                                     num_bootstrap=num_bootstrap)

        },value=1,message="Computing DoEs...")


        return(out)

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

          if(is.null(doe_res()) | is.null(input$doe_type)) {
            return(NULL)
          }

          doe_data = doe_res()$DoE

          doe_data[,2:ncol(doe_data)] = signif(doe_data[,2:ncol(doe_data)],input$nsd)

          doe_type = input$doe_type

          if(doe_type == "1") {
            doe_data$DoE.Lwr = doe_data$DoE.Lwr.Pred
            doe_data$DoE.Upr = doe_data$DoE.Upr.Pred
            doe_data$DoE.U95 = doe_data$DoE.U95.Pred
            doe_data$DoE.U = doe_data$DoE.U.Pred

          } else if(doe_type == "2") {
            doe_data$DoE.Lwr = doe_data$DoE.Lwr.Trade
            doe_data$DoE.Upr = doe_data$DoE.Upr.Trade
            doe_data$DoE.U95 = doe_data$DoE.U95.Trade
            doe_data$DoE.U = doe_data$DoE.U.Trade
          }

          outdf = doe_data[,c("Lab","DoE.x","DoE.U","DoE.U95","DoE.Lwr","DoE.Upr")]

          return(outdf)

          },
          options=list(searching=FALSE,paging=FALSE),
          rownames = FALSE)

      })

      # DoE Plot
      observeEvent(doe_res, {

        output$doe_plot = renderPlot({

          if(is.null(doe_res()) | is.null(input$doe_type)) {
            return(NULL)
          }

          doe_data = doe_res()$DoE
          vars_in = vars_in()
          doe_type = input$doe_type

          cdt_res = condense_doe_table(doe_data,doe_type)
          doe_table = cdt_res$doe_data
          doe_plot_title = cdt_res$doe_plot_title

          return(DoEplot(doe_table,
                         doe_plot_title,
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

      output$download_table_button_ui <- renderUI({

        res = results_table()

        if(is.null(res)) {
          return(NULL)
        }

        downloadButton(session$ns('download_results_table'),'Download Lab Uncertainties Table (.csv File)')
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
          the_seed = input$random_seed
          doe_res = doe_res()
          doe_type = input$doe_type
          diagnostics = res$diagnostics
          results_table = results_table()

          if(grepl('recommend',the_proc,ignore.case = T)) {
            the_proc = strsplit(the_proc,'\\(')[[1]][1]
          }

          withProgress(message = 'Preparing file for download...', {
            rmarkdown::render("./R/ResultsDownload.Rmd",
                              params = list(res = res,
                                            vars_in = vars_in,
                                            the_proc = the_proc,
                                            doe_res = doe_res,
                                            nsd = nsd,
                                            doe_type = doe_type,
                                            diagnostics = diagnostics,
                                            version = version,
                                            results_table = results_table,
                                            the_seed = the_seed))
          })

          file.copy('./R/ResultsDownload.pdf',file)

        }
      )

      output$download_results_table <- downloadHandler(
        filename = function() {
          paste("Lab-Uncertainties-Table.csv")
        },

        content = function(file) {

          results_table = results_table()

          write.csv(x=results_table,file=file,row.names=F)

        }
      )

    }
  )
}
