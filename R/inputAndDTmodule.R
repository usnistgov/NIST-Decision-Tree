inputUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    br(),
    h3("Data Entry"),
    p("Directions: Enter your data using the interactive table below.",
      "The table functions similarly to common spreadsheet software.",
      "For example, you can copy and paste data from a spreadsheet",
      "directly into the table below.",
      "To manually add/remove rows, right click on the table and select the",
      "desired option.",
      "Labs in rows with a the option checked will be taken into account for the",
      "KCRV computation. Labs in rows left unchecked will not be used in the KCRV",
      "computation, but their degrees of equivalence will be computed."),
    br(),
    p("When you have finished entering the data into the table,",
      "click the 'Validate Data' button to confirm your selections. Then proceed to the",
      "'Decision Tree' tab."),
    br(),
    fluidRow(column(1,actionButton(ns('validate'),"Validate Data"))),
    fluidRow(column(4,uiOutput(ns('validate_msg')))),
    br(),
    fluidRow(column(7,br(),br(),br(),br(),rHandsontableOutput(ns("hot")),offset=.5),
             column(5,plotOutput(ns('raw_data_plot')))),
    br(),
    hr(),
    hr(),
    p("Alternatively, you may upload a .csv file following the same format as the above table.",
      '(Column names should read "Laboratory","MeasuredValues","StdUnc","DegreesOfFreedom".)'),
    fileInput(ns('file_input'),'Upload .csv File',accept='.csv'),
    hr(),
    #fluidRow(column(4,textOutput(ns('go_message'))))
    br()
  )

}


input_server <- function(id) {
  # when 'go' is clicked, this module server 
  # saves the data outside of the NS used for the input and 
  # decision trees
  moduleServer(
    id,
    function(input,output,session) {
      
      file_data <- eventReactive(input$file_input,{
        
        if(is.null(input$file_input)) {
          return(NULL)
        }
        
        the_data = read.csv(input$file_input$datapath,fileEncoding = "UTF-8-BOM")
        
        colnames(the_data)[1] = sub('^.\\.\\.','',colnames(the_data)[1])
        
        validate(
          need(colnames(the_data) == c('Laboratory','MeasuredValues','StdUnc','DegreesOfFreedom'),
               paste('Column names of the .csv file do not match the expected column names.',
                     'Columns headings should read "Laboratory","MeasuredValues","StdUnc","DegreesOfFreedom".')),
          need(all(is.numeric(the_data$MeasuredValues)),
               "All MeasuredValues must be numeric."),
          need(all(is.numeric(the_data$StdUnc)),
               "All StdUnc must be numeric."),
          need(all(the_data$StdUnc > 0),
               "All StdUnc must be positive.")
        )
        
        return(the_data)
        
      },ignoreNULL = FALSE)
      
      
      output$hot <- renderRHandsontable({
        
        #cat('here \n')
        
        the_data = file_data()
        
        if(is.null(the_data)) {
          
          init.df = data.frame(include=c(T,T,T,T,T,T),
                               lab = c('IRMM',
                                       'KRISS',
                                       'NARL',
                                       'NIST',
                                       'NMIJ',
                                       'NRC'), 
                               result=c(34.3,32.9,34.53,32.42,31.9,35.8), 
                               uncertainty=c(1.03,0.69,0.83,0.29,0.4,0.38),
                               dof=c(60,4,18,2,13,60))
          
        } else {
          
        
          init.df = data.frame(include=rep(T,nrow(the_data)),
                               lab = the_data$Laboratory, 
                               result=the_data$MeasuredValues, 
                               uncertainty=the_data$StdUnc,
                               dof=the_data$DegreesOfFreedom)
          
          
          
        }
        
        min_val = min(abs(init.df[,3:4]))
        decs = min_val - round(min_val,0)
        decs = format(decs,scientific = FALSE)
        # get number of decimal places; need to subtract 2 from the length for "0."
        num_dec = length(strsplit(decs,'')[[1]]) - 2
        
        if(num_dec < 1) {
          num_dec = 1
        }
        
        digits_pattern = paste0("0.",paste0(rep('0',num_dec),collapse=''))
          
        DF = init.df 
        names(DF) <- c('Include','Laboratory','MeasuredValues','StdUnc','DegreesOfFreedom')
        
        myindex = which(DF[,1]==F)-1
        
        rhandsontable(DF, readOnly = FALSE, selectCallback = TRUE,digits=num_dec) %>%
          hot_context_menu(allowColEdit = FALSE) %>%
          #hot_validate_numeric(cols=3:4) %>%
          hot_col(2, format = '', halign = 'htCenter', valign = 'htTop') %>%
          hot_col(1, halign = 'htCenter') %>%
          hot_col(c(3,4), format=digits_pattern,halign = 'htCenter') %>%
          hot_col(5, type = 'numeric', format = '0',halign = 'htCenter') %>%
          #hot_col(c(3,4), format = paste0("0.", paste0(rep(0, 5), collapse='')), halign = 'htCenter') %>%
        #   hot_col(c(2,3,4,5), renderer = "function(instance, td, row, col, prop, value, cellProperties) {
        #     Handsontable.renderers.TextRenderer.apply(this, arguments);
        #     if (instance.params) {
        #     hrows = instance.params.myindex
        #     hrows = hrows instanceof Array ? hrows : [hrows] 
        #   }
        #   if (instance.params && hrows.includes(row)) {td.style.background = 'lightpink';}
        #   else {td.style.background = 'darkseagreen';  }
        # }
        # ") %>%
          hot_col(1, type = 'checkbox') %>% 
          hot_cols(manualColumnResize = TRUE,
                   colWidths = c(75,100,150,150,100))
        
      })
      
      output$raw_data_plot <- renderPlot({
        
        data = hot_to_r(input$hot)
        
        if(is.null(data)) {
          return(NULL)
        }
        
        colnames(data) = c('Include','Laboratory','Result','Uncertainty','DegreesOfFreedom')
        
        data$upper = data$Result + data$Uncertainty
        data$lower = data$Result - data$Uncertainty
        data$Include = as.character(as.logical(data$Include))
        
        p = ggplot(data,aes(x=Laboratory,y=Result,color=Include)) + 
          geom_point() +
          geom_errorbar(aes(ymin=lower,ymax=upper),width=.2) +
          ylab("Measured Value +/- Std. Uncertainty") + 
          scale_color_manual(values=c("TRUE"="black","FALSE"="grey50"))
        
        p
      })

      # format input variables
      init <- eventReactive(input$validate, {

        the_data = hot_to_r(input$hot)
        colnames(the_data) = c('Include','Laboratory','Result','Uncertainty','DegreesOfFreedom')
        
        which_to_compute = as.logical(the_data$Include)
        measured_vals = as.numeric(the_data$Result)[which_to_compute]
        standard_unc = as.numeric(the_data$Uncertainty)[which_to_compute]
        dof = as.numeric(the_data$DegreesOfFreedom)[which_to_compute]
        dof[is.na(dof)] = 10000
        the_data$DegreesOfFreedom[is.na(the_data$DegreesOfFreedom)] = 10000
        
        
        
        if(any(dof < 1)) {
          return("Degrees of freedom cannot be less than 1.")
        }
        
        if(any(standard_unc <= 0)) {
          return("Standard uncertainties must be positive.")
        }
        
        n1 = length(measured_vals)
        n2 = length(standard_unc)
        n3 = length(dof)
        
        if(any(n1!=n2,n1!=n3,n2!=n3)){
          return("Unequal number of entries. Each field should have the same number of input quantities.")
        }
        
        if(sum(which_to_compute) < 3) {
          return("Need at least 3 observations to use the decision tree.")
        }
        
        return(list(measured_vals=measured_vals,
                    standard_unc=standard_unc,
                    dof=dof,
                    which_to_compute=which_to_compute,
                    the_data=the_data))
      })
      
      vars_in <- eventReactive(init(), {
        
        if(is.list(init())) {
          return(init())
        } else {
          return(NULL)
        }
        
      })
      
      observeEvent(init, {
        
        output$validate_msg = renderUI({
          
          
          if(is.list(init())) {
            return(
              h5("Valid inputs. Proceed to the next tab (Decision Tree).",style='color:#009900')
              )
            
          } else{
            return(
              h5(init(),style="color:#cc0000")
            )
          }
        })
        
      })

      return(vars_in)
    }
  )
}


DT_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(4,
             uiOutput(ns('Q_test_heading')),
             verbatimTextOutput(ns('Q_output')),
             uiOutput(ns('Q_prompt')),
             br(),
             uiOutput(ns('step_2_heading')),
             verbatimTextOutput(ns('step_2_output')),
             uiOutput(ns('step_2_prompt')),
             br(),
             uiOutput(ns('step_3_heading')),
             verbatimTextOutput(ns('step_3_output')),
             uiOutput(ns('step_3_prompt')),
             textOutput(ns('plc_step3')),
             br()
      ),
      column(width=6,offset=1,
             fluidRow(imageOutput(ns('dt'),inline=TRUE)),
             br(),
             fluidRow(uiOutput(ns("recommendation"))),
             br(),
             uiOutput(ns('procedure_prompt'))
      )),
  
  br(),
  br(),
  br(),
  br()
    
  )
}


DT_server <- function(id,vars_in) {
  moduleServer(
    id,
    function(input,output,session) {
      
      # 'state' is variable to keep track of the app's state
      #
      # Q = -1: user has not made an assumption about homogeneity
      # Q = 0/1: user has accepted/rejected assumption of homogeneity
      #
      # (The following follow the same pattern as Q:)
      # QY_norm: indicates status of normality assumption following a "yes" to homogeneity
      # QN_sym: indicates status of symmetry assumption following a "no" to homogeneity
      # NY_norm: indicates status of normality assumption following a "no,yes" path

      state = reactiveValues(Q=-1,QY_norm=-1,QN_sym=-1,NY_norm=-1)
      
      observeEvent(vars_in(), {
        # reset when vars_in changes
        state$Q = -1
        state$QY_norm = -1
        state$QN_sym = -1
        state$NY_norm = -1
      })
      
      # the test based on the user's selections
      which_test = reactiveValues(awa=0,wmed=0,hgg=0,hlg=0,hssg=0)
      
      # recommendation based on the selected test
      # updates when 'which_test' is updated
      output$recommendation = renderUI({
        test_names = c('awa','wmed','hgg','hlg','hssg')
        
        if(which_test[[ test_names[1] ]] == 1) {
          return(h4("Decision Tree recommends Adaptive Weighted Average."))
        } else if(which_test[[ test_names[2] ]] == 1) {
          return(h4("Decision Tree recommends Weighted Median."))
        } else if(which_test[[ test_names[3] ]] == 1) {
          return(h4("Decision Tree recommends Hierarchical Gauss-Gauss."))
        } else if(which_test[[ test_names[4] ]] == 1) {
          return(h4("Decision Tree recommends Hierarchical Laplace-Gauss."))
        } else if(which_test[[ test_names[5] ]] == 1) {
          return(h4("Decision Tree recommends Hierarchical Skew Student-Gauss."))
        } else {
          return(NULL)
        }
      })
      
      output$procedure_prompt = renderUI({
        
        if(all_false(which_test)) {
          return(NULL)
        }
        
        recommended_test = get_test_name(which_test)
        
        all_tests = c('Adaptive Weighted Average',
                      'Weighted Median',
                      'Hierarchical Gauss-Gauss',
                      'Hierarchical Laplace-Gauss',
                      'Hierarchical Skew Student-Gauss')
        
        not_recommended = all_tests[all_tests != recommended_test]
        
        tagList(
          h5("Choose the desired procedure below, then proceed to the 'Fit Model' Tab above."),
          selectInput(session$ns('user_selected_procedure'),
                      label=NULL,
                      choices=c(paste(recommended_test,'(recommended)'),
                                not_recommended),
                      selected=paste(recommended_test,'(recommended)'),
                      width='100%')
          #actionButton(session$ns('run_proc'),'Go')
        )

        
        
      })

      
      # loads decision tree image based on state variables updating
      output$dt <- renderImage({
        
        if(state$Q == -1) {
          return(list(src = 'www/DT_start.png'))
        }
        
        if(state$Q == 1) {
          if(state$QY_norm == -1) {
            return(list(src = 'www/DT_Y.png'))
            
          } else if (state$QY_norm == 1) {
            return(list(src = 'www/DT_YY.png'))
            
          } else if (state$QY_norm == 0) {
            return(list(src = 'www/DT_YN.png'))
          }
          
        } else if(state$Q == 0) {
          if(state$QN_sym == -1) {
            return(list(src = 'www/DT_N.png'))
            
          } else if (state$QN_sym == 1) {
            
            if(state$NY_norm == -1) {
              return(list(src = 'www/DT_NY.png'))
              
            } else if(state$NY_norm == 0) {
              return(list(src = 'www/DT_NYN.png'))
              
            } else if(state$NY_norm == 1) {
              return(list(src = 'www/DT_NYY.png'))
              
            }
            
            
            
          } else if (state$QN_sym == 0) {
            return(list(src = 'www/DT_NN.png'))
          }
        }
        
        
      },deleteFile=FALSE)
      
      #### update state variables based on user decisions
      observeEvent(input$Qyes, {
        state$Q = 1
        state$QY_norm = -1
        state$QN_sym = -1
        state$NY_norm = -1
      })
      
      observeEvent(input$Qno, {
        state$Q = 0
        state$QY_norm = -1
        state$QN_sym = -1
        state$NY_norm = -1
      })
      
      observeEvent(input$QY_norm_no, {
        state$QY_norm = 0
      })
      
      observeEvent(input$QY_norm_yes, {
        state$QY_norm = 1
      })
      
      observeEvent(input$QN_sym_yes, {
        state$QN_sym = 1
        state$NY_norm = -1
      })
      
      observeEvent(input$QN_sym_no, {
        state$QN_sym = 0
        state$NY_norm = -1
      })
      
      observeEvent(input$step_3_yes, {
        state$NY_norm = 1
      })
      
      observeEvent(input$step_3_no, {
        state$NY_norm = 0
      })
      
      
      #### initial Q test output (printed by default)
      output$Q_test_heading <- renderUI({
        
        if(is.null(vars_in())) {
          return(NULL)
        }
        
        h5("Step 1: Cochran's Q-Test for homogeneity.")
        
      })
        
      output$Q_prompt <- renderUI({
        
        if(is.null(vars_in())) {
          return(NULL)
        }
        
        tagList(
          h5("Assume Homogeneity?"),
          fluidRow(column(2,actionButton(session$ns('Qyes'),'Yes')),
                   column(2,actionButton(session$ns('Qno'),'No')))
        )
        
      })
      
      output$Q_output <- renderText({
        
        if(is.null(vars_in())) {
          return(NULL)
        }
        
        measured_vals = vars_in()$measured_vals
        standard_unc = vars_in()$standard_unc
        dof = vars_in()$dof
        
        res = metafor::rma(yi = measured_vals,
                           sei = standard_unc,
                           method = "DL")
        
        pval = signif(res$QEp,2)
        
        if(pval < .001) {
          pval = 'p < 0.001'
        }
        
        Q_test_dof = length(measured_vals) - 1
        
        paste("p-value = ",pval,' (A low p-value suggests heterogeneity).', '\n',
              "Q = ",signif(res$QE,4),' (Reference Distribution: Chi-Square with ',
              Q_test_dof,' Degrees of Freedom)','\n',
              "tau est. = ",signif(sqrt(res$tau2),4),'\n',
              "tau/median(x) = ",signif(sqrt(res$tau2)/median(measured_vals),4),'\n',
              "tau/median(u) = ",signif(sqrt(res$tau2)/median(standard_unc),4),sep='')
        
      })

      #### changing displays based on user decisions
      observeEvent(state$Q, {
        
        if(state$Q == -1) {
          # reset all    
          
          output$Q_user_decision = renderUI({
            return(NULL)
          })
          
          output$step_2_output = renderText({
            return(NULL)
          })
          
          output$step_2_prompt = renderUI({
            return(NULL)
          })
          
          output$step_2_heading = renderUI({
            return(NULL)
          })
          
          which_test = clear_selections(which_test)
          
        } else if(state$Q == 1) {
          
          output$go_message = renderText({
            return(NULL)
          })
          
          output$step_2_heading = renderUI({
            tagList(
              #h5("Homogeneity assumed."),
              h5("Step 2: Shapiro-Wilk test of normality:")
            )
          })
          
          output$step_2_output = renderText({
            mvs = isolate(vars_in()$measured_vals)
            sus = isolate(vars_in()$standard_unc)
            res = shapiro.test((mvs-median(mvs))/sus) 
            paste("p: ", signif(res$p.value,2),' (A low p-value suggests non-normality.)', sep='')
          })
          
          output$step_2_prompt <- renderUI({
            
            tagList(
              h5("Assume Normality?"),
              fluidRow(column(2,actionButton(session$ns('QY_norm_yes'),'Yes')),
                       column(2,actionButton(session$ns('QY_norm_no'),'No')))
            )
            
          })
          
        } else if(state$Q == 0) {
          
          output$go_message = renderText({
            return(NULL)
          })
          
          output$step_2_heading= renderUI({
            tagList(
              #h5("Homogeneity not assumed."),
              h5("Step 2: Miao-Gel-Gastwirth test of symmetry:")
            )
          })
          
          output$step_2_output = renderText({
            
            mvs = isolate(vars_in()$measured_vals)
            res = symmetry::symmetry_test(mvs,stat='MGG',bootstrap=TRUE, B=10000) 
            paste("p: ", signif(res$p.value,2), ' (A low p-value suggests asymmetry.)' ,sep='')
          })
          
          output$step_2_prompt <- renderUI({
            
            tagList(
              h5("Assume Symmetry?"),
              fluidRow(column(2,actionButton(session$ns('QN_sym_yes'),'Yes')),
                       column(2,actionButton(session$ns('QN_sym_no'),'No')))
            )
          })
          
        }
        
      })
      
      observeEvent(state$QY_norm, {
        
        if(state$QY_norm == -1) {
          which_test = unselect_test('awa',which_test)
          which_test = unselect_test('wmed',which_test)
          
        } else if(state$QY_norm == 1) {
          which_test = select_test('awa',which_test)
          
        } else if(state$QY_norm == 0) {
          which_test = select_test('wmed',which_test)
        }
        
      })
      
      observeEvent(state$QN_sym, {
        
        if(state$QN_sym == -1) {
          
          output$norm_user_dec <- renderUI({
            return(NULL)
          })
          
          output$step_3_heading <- renderUI({
            return(NULL)
          })
          
          output$step_3_output <- renderText({
            return(NULL)
          })
          
          output$step_3_prompt <- renderUI({
            return(NULL)
          })
          
          which_test = unselect_test('hgg',which_test)
          which_test = unselect_test('hlg',which_test)
          which_test = unselect_test('hssg',which_test)
          
        } else if(state$QN_sym == 0) {
          # skew student gauss
          
          which_test = select_test('hssg',which_test)
          
          output$norm_user_dec <- renderUI({
            return(NULL)
          })
          
          output$step_3_heading <- renderUI({
            return(NULL)
          })
          
          output$step_3_output <- renderText({
            return(NULL)
          })
          
          output$step_3_prompt <- renderUI({
            return(NULL)
          })
          
        } else if(state$QN_sym == 1) {
          # normality test
          
          which_test = clear_selections(which_test)
          
          
          output$step_3_heading <- renderUI({
            tagList(
              #h5("Symmetry assumed."),
              h5("Step 3: Shapiro-Wilk test of normality:")
            )
          }) 
          
          output$step_3_output <- renderText({
            mvs = isolate(vars_in()$measured_vals)
            sus = isolate(vars_in()$standard_unc)
            res = shapiro.test((mvs-median(mvs))/sus) 
            paste("p:", signif(res$p.value,2))
          })
          
          output$step_3_prompt <- renderUI({
            
            tagList(
              h5("Assume Normality?"),
              fluidRow(column(2,actionButton(session$ns('step_3_yes'),'Yes')),
                       column(2,actionButton(session$ns('step_3_no'),'No')))
            )
          })
          
        }
        
        
        
      })
      
      observeEvent(state$NY_norm, {
        
        if(state$NY_norm == 1) {
          which_test = select_test('hgg',which_test)
          
        } else if(state$NY_norm == 0) {
          which_test = select_test('hlg',which_test)
          
        } else if(state$NY_norm == -1) {
          which_test = unselect_test('hgg',which_test)
          which_test = unselect_test('hlg',which_test)
        }
      })
      
      return(reactive(input$user_selected_procedure))
    }
  )
}