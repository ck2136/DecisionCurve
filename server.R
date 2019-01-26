#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# shiny packages
library(shiny)
library(shinydashboard)

# for summary statistics
library(DT)
library("psych")
library(dplyr)
library(formattable)

# prediction models
library(rpart)
library(randomForest)
library(e1071)
library(xgboost)

# plotting
library(ggfortify)
library(plotly)
library(pROC)
library(rpart.plot)
library(caret)

# fit stats
library(stargazer)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  #This function is repsonsible for loading in the selected file
  
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
    
  })
  
  # This allows user to select variables in the filedata
  output$Variables <- renderUI({
    selectInput('vars', 'Select the variables to view the summary statistics', names(filedata()) , multiple = TRUE)
  })
  
  
  redfile <- reactive({
    select(filedata(), input$vars)
  })
  
  #This previews the CSV data file
  output$filetable <- renderDataTable({
    validate(
      need(redfile(),
           "Please insert a .csv file")
    )
    
    redfile()
  })
  
  output$sum <- renderDataTable({
    
    validate(
      need(redfile(), "Sorry, there is no data for your requested summary table. 
           Please insert the data."
      )
      )
    
    datatable(describe(redfile())) %>%
      formatRound(., columns = c(colnames(describe(redfile()))), digits = 2)
  })
  
  # R session info
  output$info <- renderPrint({
    sessionInfo()
  })
  
  output$varnames <- renderPrint({
    names(filedata())
  })
  
  
  # correlation
  
  correl <- reactive({
    round(cor(cbind(redfile()), use = "complete"),3)
  })
  
  
  
  makecorPlot <- function(){
    pairs.panels(redfile())
  }
  
  output$corPlot <- renderPlot({
    print(makecorPlot())
  })
  
  
  # Predictive Modeling
  
  output$outcomevariable <- renderUI({
    selectInput('outvar', 'Select the Outcome Variable (only one)', names(filedata()) , multiple = FALSE)
  })
  
  output$independentvariable <- renderUI({
    selectInput('indvar', 'Select independent variables (may be more than one)', names(filedata()) , multiple = TRUE)
  })
  
  output$Variablespm <- renderUI({
    selectInput('vars2', 'Variables', names(filedata()) , multiple = TRUE)
  })
  
  
  est <- reactive({
    # set input data
    dat <- filedata()
    # initialize list
    fit_stats <- list()
    
    fit_glm <- glm(formula = as.formula(paste0(input$outvar, " ~ ", paste0(c(input$indvar), collapse = " + "))), data=dat, family =binomial("logit"))
    dat$pred_glm <- fit_glm$fitted.values
    fit_stats$fit_glm <- fit_glm
     
    fit_dt <- rpart(as.formula(paste0(input$outvar, " ~ ", paste0(c(input$indvar), collapse = " + "))), data=dat)
    dat$pred_dt <- predict(fit_dt, dat)
    fit_stats$fit_dt <- fit_dt
    
    fit_rf <- randomForest(as.formula(paste0("as.factor(",input$outvar,")", " ~ ", paste0(c(input$indvar), collapse = " + "))), data=dat, ntree = input$ntree, mtry = input$mtry)
    dat$pred_rf <- predict(fit_rf, dat, type = "prob")[,2]# later will need to revise this section
    fit_stats$fit_rf <- fit_rf
    
    fit_svm <- svm(as.formula(paste0(input$outvar, " ~ ", paste0(c(input$indvar), collapse = " + "))),  data=dat, kernel = input$svmkernel, gamma = input$gamma)
    dat$pred_svm <- predict(fit_svm, dat)
    fit_stats$fit_svm <- fit_svm
    
    list(dat = dat, fit_stats = fit_stats)
    
  })
  
  output$preddat <- renderDataTable({
    
    dat <- est()$dat
    sel_dat <- dat[,grepl("^pred_|^pat", colnames(dat))]
    datatable(sel_dat) %>%
      formatRound(., columns = c(colnames(sel_dat)), digits = 2)
  })
  
  # glm outputs
  
  ## glm fit summary
  output$glm_sum <- renderUI(HTML(stargazer(est()$fit_stats$fit_glm, dep.var.labels = input$outvar, type="html")))
  
  ## glm cm plot
  output$glmcmplot <- renderPlot({
    fourfoldplot(confusionMatrix(if_else(est()$dat$pred_glm >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$table)
  })
  
  ### Plotting 
  
  #### Decision Tree plots
  
  output$dtplot <- renderPlot({
    rpart.plot(est()$fit_stats$fit_dt)
  })
  
  output$dtcmplot <- renderPlot({
    fourfoldplot(confusionMatrix(if_else(est()$dat$pred_dt >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$table)
  })
  
  #### Random Forest summary
  
  output$rfsum <- renderPrint({
    print(est()$fit_stats$fit_rf)
  })
  
  output$rfcmplot <- renderPlot({
    fourfoldplot(confusionMatrix(if_else(est()$dat$pred_rf >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$table)
  })
  
  #### SVM summary
  
  output$svmsum <- renderPrint({
    print(est()$fit_stats$fit_svm)
  })
  
  output$svmcmplot <- renderPlot({
    fourfoldplot(confusionMatrix(if_else(est()$dat$pred_svm >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$table)
  })

  
  #### ROC plots
  
  makeROCplot <- reactive({
    dat <- est()$dat
    glmROC <- roc(response = dat[,input$outvar], predictor = dat$pred_glm)
    rfROC <- roc(response = dat[,input$outvar], predictor = dat$pred_rf)
    dtROC <- roc(response = dat[,input$outvar], predictor = dat$pred_dt)
    svmROC <- roc(response = dat[,input$outvar], predictor = dat$pred_svm)
    list(glm = glmROC, rf = rfROC, dt = dtROC, svm = svmROC)
  })
  
  output$roc <- renderPlotly({
    ggplotly(ggroc(list(glm=makeROCplot()$glm, rf=makeROCplot()$rf, dt=makeROCplot()$dt, svm=makeROCplot()$svm)), aes = "linetype", color = "red")
    
  })
  
  #### Prediction Performance Table
  #This previews the CSV data file
  output$pred_perf <- renderDataTable({
    validate(
      need(est()$dat,
           "Please insert a .csv file")
    )
    perf_tab <- cbind(
      c(confusionMatrix(if_else(est()$dat$pred_glm >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$overall, confusionMatrix(if_else(est()$dat$pred_glm >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$byClass),
      c(confusionMatrix(if_else(est()$dat$pred_dt >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$overall, confusionMatrix(if_else(est()$dat$pred_dt >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$byClass),
      c(confusionMatrix(if_else(est()$dat$pred_rf >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$overall, confusionMatrix(if_else(est()$dat$pred_rf >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$byClass),
      c(confusionMatrix(if_else(est()$dat$pred_svm >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$overall, confusionMatrix(if_else(est()$dat$pred_svm >= input$prob_thresh, as.factor(names(table(est()$dat[,input$outvar])))[1], as.factor(names(table(est()$dat[,input$outvar])))[2]), as.factor(est()$dat[,input$outvar]))$byClass)
    )
    
    colnames(perf_tab) <- c("glm","dt","rf","svm")
    
    datatable(perf_tab) %>%
      formatRound(., columns = c(colnames(perf_tab)), digits = 2)
  })
  
  
})
