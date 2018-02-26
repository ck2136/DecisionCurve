## ui.R ##
library(shinydashboard)
library(DT)
library(shinyAce)
library(formattable)
library(shinyjs)
library(plotly)

useShinyjs()

# this below will suppress the output-error
tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

dashboardPage(
  
  # Header
  dashboardHeader(title = "Decision Curve Analysis dashboard"),
  
  
  # Sidebar Menus
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data and Summary", tabName = "data", icon = icon("th")),
      menuItem("Model Specification", tabName = "model", icon = icon("th")),
      menuItem('Result', tabName = 'results', icon = icon('th'), 
               collapsible = 
                 menuSubItem('random', tabName = 'random'),
               menuSubItem('Prediction Model Summary', tabName = 'regresult'),
               menuSubItem("ROC and Confusion Matrix", tabName = "roc"),
               menuSubItem('Decision Curve Plot', tabName = 'plot')
      )
      
      
    )
  ),
  
  # Body of the Dashboard
  dashboardBody(
    tabItems(
      
      # First tab content DashBoard
      tabItem(tabName = "dashboard",
              
              h2("Introduction"),
              fluidRow(
                box(
                  h4('This is a dashboard for determining the predictive utility of decision models.'),
                  h4('This dashboard is based on', a("Vickers et al.", href="http://journals.sagepub.com/doi/abs/10.1177/0272989x06295361", target="_blank"), "'s work on determining the clinical utility of prediction models. Vicker's et al. have used a formula to weigh the impact of false positive and false negatives into a 'Net Benefit' equation whereby a predictive model with higher 'Net Benefit' is preferred."),
                  h4("There are several important key points to remember"),
                  h4(strong("Number 1:"),"User needs to have his/her own .csv file to input into the 'Data and Summary' Tab (Second tab on the left"),
                  h4(strong("Number 2:"),"User needs to have enough domain knowledge and information regarding the relationship between variables to set up the model equation in the 'Model Specification' Tab (Fourth tab on the left"),
                  h4(strong("Number 3:"),"In addition to the model specification, user needs to be familiar with the R syntax in setting up the equations for predictive modeling (i.e. logistic regression, decision tree, random forest, svm)"),
                  h4(strong("Number 4:"),"The outputs that are generated in the 'Result' tab (Bottom tab on the left) needs the user to be comfortable with the 'dca' function output. (There is a link in the model specification to the dca() vignette)")
                  
                  ,width = 13)
              )
              
              
      ),
      
      # Second Tab content: Data and SUmmary
      tabItem(tabName = "data",
              
              h2("Please Enter CSV Data"),
              
              fileInput('datafile', 'Choose CSV file',
                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
              
              h3("Descriptive Statistics: Select variables"),
              uiOutput("Variables"),
              
              
              # View summary statistics
              h3("1. Summary Statistics"),
              
              fluidRow( 
                box(
                dataTableOutput("sum"),
                width = 12
              ) ),
              
              h3("2. Correlation"),
              
              fluidRow(
                box(
                  plotOutput("corPlot"),
                  width = 12
                )
                ),
              
              # Create row to check the data
              h3("3. Data View"),
              
              fluidRow(
                box(
                  dataTableOutput("filetable"),
                  width = 12
                )
                ),
              
              h3("4. R session info"),
              
              fluidRow(
                box(
                  verbatimTextOutput("info"),
                  width = 12
                )
              )
      ),
      
      
      # Third Tab Content
      tabItem(tabName = "model",
              
              h4(strong("*Note"),'Estimation may take a few seconds to minutes depending on the dataset.'),
              
              h3("Predictive Model Specification"),
              
              h4("Specify the model formula with variables"),
              
              fluidRow(
                box(
                  width = 4,
                  uiOutput("outcomevariable")
                ),
                box(
                  width = 4,
                  uiOutput("independentvariable")
                )
                ),
              
              fluidRow(
                box(width = 12,
                    h2("Predicted Probabilities")),
                hr(),
                br(),
                box(
                  width = 12,
                  dataTableOutput("preddat")
                )
              ),
              
              h3("Specify the modeling options"),
              
              fluidRow(
                box(
                  h3("GLM Option"),
                  numericInput("prob_thresh","Pr. Threshold for classification", min = 0.01, max = 1, value = 0.5)
                  ,width = 2
                ),
                box(
                    h3("RandomForest Options"),
                    sliderInput("mtry","Number of randomly sampled variables", min = 1, max = 100, value = 2, step = 1),
                    sliderInput("ntree","Number of trees to grow", min = 1, max = 100, value = 5, step = 1),
                    width = 3
                    
                  ),
                box(
                    h3("Decision Tree Options"),
                    selectInput("rpartmethod","Method", choices = c("anova","poisson","class","exp"), selected = "class"),
                    sliderInput("minsplit","Min # of obs in node to split", min = 1, max = 100, value = 20, step = 1),
                    sliderInput("cp","Min # of obs in node to split", min = 0.000001, max = 1, value = 0.01, step = 0.01),
                    width = 3
                  ),
                box(
                    h3("SVM Options"),
                    selectInput("svmkernel","Kernel", choices = c("linear","polynomial","radial","sigmoid"), selected = "radial"),
                    numericInput("gamma","Gamma", min = 0.00000001, max = 1, value = 0.01),
                    numericInput("cp","Complexity Parameter", min = 0.000001, max = 1, value = 0.01),
                    width = 3
                  )
                ),
              hr(),
              br(),br(),br(),br(),
              fluidRow(
                box(
                  p('For Decision Curve Analysis See',
                    a("dca", href="https://www.mskcc.org/departments/epidemiology-biostatistics/health-outcomes/decision-curve-analysis-01", target="_blank"),
                    'for the documentation, tutorial, and more.')
                )
                
              )
              
            
              ),
      
     
      # Fourth tab content
      tabItem(tabName = "regresult",
              
              h2("Analysis Result"),
              fluidRow(
                column(6,
                       box(title = "GLM regression table", uiOutput("glm_sum"))
                ),
                column(6,
                       box(title = "Decision Tree summary", plotOutput("dtplot"))
                       )
                  ),
              fluidRow(
                column(6,
                       box(title = "Random Forest summary", verbatimTextOutput("rfsum")
                           ,
                           width = 4)
                       ),
                column(6,
                       box(title = "SVM summary", verbatimTextOutput("svmsum")
                           ,
                           width = 4)
                       ) 
                )
      ),
      
      # Fifth tab content
      tabItem(tabName = 'roc',
              
              h3('ROC curve'),
              
              fluidRow(
                box(
                  plotlyOutput("roc")
                  ,width = 12
                )
              ),
              br(),
              hr(),
              h3("Confusion Matrices"),
              
              fluidRow(
                box(
                  h3("GLM confusion matrix"),
                  plotOutput("glmcmplot")
                  ,width = 3
                ),
                box(
                  h3("Decision Tree confusion matrix"),
                  plotOutput("dtcmplot")
                  ,width = 3
                ),
                box(
                  h3("Random Forest confusion matrix"),
                  plotOutput("rfcmplot")
                  ,width = 3
                ),
                box(
                  h3("SVM confusion matrix"),
                  plotOutput("svmcmplot")
                  ,width = 3
                )
                
              ),
              br(),br(), hr(),
              fluidRow(
                box(
                  h3("Prediction Performance Measures"),
                  dataTableOutput("pred_perf")
                  ,width = 6
                )
              )
              
              
              
      )
      
      
      
      
      
      
                )
                )
                )

