inputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(column(4,textInput(ns('mv'),'Measured Values',value='1,2,3'))),
    fluidRow(column(4,textInput(ns('su'),"Standard Uncertainties",value='1,1,1'))),
    fluidRow(column(4,textInput(ns('df'),"Degrees of Freedom",value='5,6,7'))),
    fluidRow(column(1,actionButton(ns('go'),"Go")))
    #fluidRow(column(4,textOutput(ns('go_message'))))
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
             textOutput(ns('plc_step3'))
      ),
      column(width=6,offset=1,
             fluidRow(imageOutput(ns('dt'),inline=TRUE)),
             br(),
             fluidRow(uiOutput(ns("recommendation")))))
    
  )
}


input_server <- function(id) {
  # when 'go' is clicked, this module server 
  # saves the data outside of the NS used for the input and 
  # decision trees
  moduleServer(
    id,
    function(input,output,session) {

      # format input variables
      vars_in <- eventReactive(input$go, {
          measured_vals = eval(parse(text=paste('c(',input$mv,')')))
          standard_unc = eval(parse(text=paste('c(',input$su,')')))
          dof = eval(parse(text=paste('c(',input$df,')')))
          
          return(list(measured_vals=measured_vals,
                      standard_unc=standard_unc,
                      dof=dof))
      })
      
      observeEvent(input$go, {
        #output$go_message <- renderText("Data saved. Proceed to next tab.")
        
        observeEvent(input$go, {
          showModal(modalDialog(
            title = "Data Saved.",
            paste("Your data has been submitted. Please proceed to the next tab.")
          ))
        })
      })

      
      return(vars_in)
    }
  )
}

DT_server <- function(id,vars_in) {
  moduleServer(
    id,
    function(input,output,session) {
      
      # 'state' is variable to keep track of the app's state
      #
      # start = 0: user has not submitted data; 
      # start = 1: user has submitted data 
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
        state$Q = -1
        state$QY_norm = -1
        state$QN_sym = -1
        state$NY_norm = -1
      })
      
      # the test based on the user's selections
      which_test = reactiveValues(awa=0,wmed=0,hgg=0,hlg=0,hssg=0)
      
      output$recommendation = renderUI({
        test_names = c('awa','wmed','hgg','hlg','hssg')
        
        if(which_test[[ test_names[1] ]] == 1) {
          return(h4("Decision Tree recommends Adapted Weighted Average."))
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
      
      
      #### initial Q test output
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
        
        paste("p = ",round(res$QEp,5),"; Q = ",res$QE,sep='')
        
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
            paste("p:", round(res$p.value,5))
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
              h5("Step 2: Miao, Gel and Gastwirth test of symmetry:")
            )
          })
          
          output$step_2_output = renderText({
            
            mvs = isolate(vars_in()$measured_vals)
            res = symmetry::symmetry_test(mvs,stat='MGG',bootstrap=TRUE, B=10000) 
            paste("p:", round(res$p.value,5))
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
            paste("p:", round(res$p.value,5))
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
    }
  )
}