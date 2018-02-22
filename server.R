#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library("psych")
library(dplyr)
library(lavaan)
library("semPlot")
library(formattable)
library(plspm)
library(ggfortify)
library(pander)
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
    redfile()
  })
  
  output$sum <- renderDataTable({
    datatable(describe(redfile())) %>%
      formatRound(., columns = c(colnames(describe(redfile()))), digits = 2)
  })
  
  # R session info
  output$info <- renderPrint({
    sessionInfo()
  })
  
  
  # correlation
  
  correl <- reactive({
    round(cor(cbind(redfile()), use = "complete"),3)
  })
  
  output$correl <- renderPrint({
    correl()
  })
  
  
  makecorPlot <- function(){
    pairs.panels(redfile())
  }
  
  output$corPlot <- renderPlot({
    print(makecorPlot())
  })
  
  
  # EFA, CFA, PATH, GCM Analysis
  
  get.text <- reactive({
    input$model
  })
  
  
  output$Variablesefa <- renderUI({
    selectInput('vars2', 'Variables', names(filedata()) , multiple = TRUE)
  })
  
  output$Variablepcagroup <- renderUI({
    selectInput('vars3', 'Grouping Variable', names(redfileefa()) , multiple = FALSE)
  })
  
  
  # EFA data selection of the indicators for factor analysis
  redfileefa <- reactive({
    select(filedata(), input$vars2)
  })
  
  
  est <- reactive({
    
    dat <- filedata()
    
    model <- get.text()
    
    if(input$anal == "efa" & input$efalysisopt == "ML"){
      fit <- factanal(~., data=redfileefa(), factors = input$numfactor, rotation = input$rotfactor)
    }
    
    if(input$anal == "efa" & input$efalysisopt == "pa"){
      fit <- fa.parallel(x = redfileefa(), fa=input$pafaoption, fm = input$pafmoption)
    }
    
    if(input$anal == "efa" & input$efalysisopt == "paf"){
      fit <- fa(redfileefa(), nfactors = input$numfactor, rotate = input$rotfactor, residuals = TRUE, SMC = TRUE, fm = input$pafmoption)
    }
    
    if(input$anal == "efa" & input$efalysisopt == "pca"){
      fit <- princomp(redfileefa())
    }
    
    if(input$anal == "cfa"){
      fit <- cfa(model, data=dat, estimator = input$estimatoroptions, se = input$seoptions,
                 bootstrap = input$bootstrapoptions, orthogonal = input$orthogonaloptions)
    }
    if(input$anal == "sem"){
      fit <- sem(model, data=dat, estimator = input$estimatoroptions, se = input$seoptions,
                 bootstrap = input$bootstrapoptions, orthogonal = input$orthogonaloptions)
    }
    if(input$anal == "growth"){
      fit <- growth(model, data=dat, estimator = input$estimatoroptions, se = input$seoptions,
                    bootstrap = input$bootstrapoptions, orthogonal = input$orthogonaloptions)
    }
    
    list(fit = fit)
    
  })
  
  # PLS analysis
  
  get.textpls <- reactive({
    input$plsinner
  })
  
  output$Variablespls <- renderUI({
    selectInput('vars1', 'Variables', names(filedata()) , selected = c('PEW1','PEW2','PEW3','PEW4','PUW1','PUW2','PUW3','PUW4','PEUW1','PEUW2','PEUW3','PEA1','PEA2','PEA3','PEA4','PUA1','PUA2','PUA3','PUA4','PEUA1','PEUA2','PEUA3','IU1','IU2','IU3') ,multiple = TRUE)
  })
  
  redfilepls <- reactive({
    select(filedata(), input$vars1)
  })
  
  plsmod <- reactive({
    
    dat <- redfilepls()
    
    
    mod <- eval(
      parse(
        text =  get.textpls()
      )
    )
    
    mod
    
  })
  
  # Again select Variables for PLS analysis 
  # This allows user to select variables in the filedata
  
  
  
  
  
  
  
  # Make Plots
  
  makeplot <- function(){
    if(input$anal != 'pls' & input$anal != 'efa'){
      res <- est()$fit
      semPaths(res, input$pltopt, style=input$pltstyl, layout = input$lay, edge.label.cex=.8, fade=F, gray=T)
    } else if(input$anal == 'pls') {
      res <- plsmod()
      plot(plsmod())
    } 
  }
  
  makeplot2 <- function(){
    if(input$anal == 'efa' & input$efalysisopt == 'pa'){
      fa.parallel(x = redfileefa(), fa=input$pafaoption, fm = input$pafmoption)
    }
  }
  # For Principale Component Analysis Plots
  makeplot3 <- function(){
    if(input$anal == 'efa' & input$efalysisopt == 'pca'){
      autoplot(princomp(~., redfileefa()), colour = input$vars3)
    }
  }
  makeplot4 <- function(){
    if(input$anal == 'efa' & input$efalysisopt == 'pca'){
      plot(princomp(~., redfileefa()), main = "Principal Component Variance Plot")
    }
  }
  
  output$plot <- renderPlot({
    print(makeplot())
  })
  
  output$plot2 <- renderPlot({
    makeplot2()
  })
  output$plot3 <- renderPlot({
    makeplot3()
  })
  output$plot4 <- renderPlot({
    makeplot4()
  })
  
  output$plsinner <- renderPlot({
    print(plot(plsmod()))
  })
  output$plsouter <- renderPlot({
    print(plot(plsmod(), what = "loadings", arr.width = 0.1))
  })
  
  # Tabular Numbers
  
  ## Standardized Solutions
  
  output$stdsol <- renderFormattable({
    formattable(standardizedsolution(est()$fit),  digits = 4)
  })
  
  ## Overall Summary of Model
  
  result <- reactive({
    if(input$anal == 'pls'){
      res <- plsmod()
      res <- summary(res)
    } else {
      res <- est()$fit
      res <- summary(res, standardized=TRUE, fit.measures=TRUE)
    } 
    res
  })
  result1 <- reactive({
    if(input$anal == 'sem' || input$anal == 'growth' || input$anal == 'cfa'){
      res <- est()$fit
      res <- summary(res, standardized=TRUE, fit.measures=TRUE)
    }
    res
  })
  
  result2 <- reactive({
    if(input$anal == 'efa'){
      res <- est()
    }
    res
  })
  
  resultpca <- reactive({
    if(input$anal == 'efa' && input$efalysisopt == 'pca'){
      res <- est()
    }
    res$fit$loadings
  })
  
  output$result <- renderPrint({
    result()
  })
  output$result1 <- renderPrint({
    result1()
  })
  output$resultefa <- renderPrint({
    result2()
  })
  output$resultefapca <- renderPrint({
    resultpca()
  })
  
  outputOptions(output, 'result', suspendWhenHidden=FALSE)
  # Rsquared
  
  output$r2 <- renderPrint({
    if(input$anal == 'pls'){
      r2 <- plsmod()$inner_summary[, "R2", drop = FALSE]
    } else {
      r2 <- inspect(est()$fit, "rsquare")
    }
    r2
  })
  
  # Fit measures
  
  output$fm <- renderPrint({
    if(input$anal == 'pls'){
      fm <- plsmod()$gof
    } else {
      fm <- fitmeasures(est()$fit)
    }
    fm
  })
  
})
