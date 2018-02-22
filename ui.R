## ui.R ##
library(shinydashboard)
library(DT)
library(shinyAce)
library(formattable)


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
               menuSubItem('Regression Output', tabName = 'regresult'),
               menuSubItem('Plots', tabName = 'plot')
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
              
              h3(strong("*Note"),'Estimation may take a few seconds to minutes depending on the dataset.'),
              
              h3("Analysis"),
              
              fluidRow(
                box(
                  selectInput('anal','Choose an Analysis',
                              choices =  c(
                                "Exploratory Factor Analysis" = 'efa',
                                "Confirmatory Factor Analysis" = 'cfa',
                                "Structural Equation Model: SEM or Path Analysis" = 'sem',
                                "Growth Curve Analysis" = 'growth',
                                "Partial Least Square SEM" = "pls"), selected = 'pls')
                )),
              
              
              h3("Specify the model and/or variables"),
              
              p('For CBSEM See',
                a("lavaan", href="http://lavaan.ugent.be/", target="_blank"),
                'for the syntax.'),
              
              
              # Conditional Panel if Choosing Exploratory Factor Analysis
              conditionalPanel(
                condition = "input.anal == 'sem' || input.anal == 'growth' || input.anal == 'cfa'",
                
                fluidRow(
                  box(
                    width = 8,
                    aceEditor("model", mode="r", value="# Model 1
                              IU ~ PEW + PUW + PEUW + PEA + PUA + PEUA
                              UseDay ~ IU
                              Workoutweek ~ UseDay"
                              , 
                              height = "200px")
                  )
                  
                )
                
                
                ) ,
              
              # Options for EFA
              conditionalPanel(
                condition = "input.anal == 'efa'",
                fluidRow(
                  box(
                    width = 8,
                    h3("Select Measurement Variables"),
                    uiOutput("Variablesefa")
                  ),
                  box(
                    radioButtons("efalysisopt", strong("Analysis Options"),
                                 c("Maximum likelihood" = "ML",
                                   "Parallel Analysis" = "pa",
                                   "Principal Axis Factoring" = "paf",
                                   "Principal Component Analysis (PCA)" = "pca"
                                 ), selected = "ML")
                    
                  ),
                  box(
                    sliderInput("numfactor", strong("Number of Factors"), step = 1, 
                                min = 1, max = 10,  value = 3)
                  )
                ),
                fluidRow(
                  box(
                    radioButtons("rotfactor", strong("Factor Rotation"),
                                 c("Pro Max" = "promax",
                                   "VariMax" = "varimax",
                                   "None" = "none"
                                 ), selected = "promax")
                  ),
                  box(
                    radioButtons("pafmoption", strong("Factor Extraction Method"),
                                 c("Maximum likelihood" = "ML",
                                   "Minimum Residual" = "minres",
                                   "Generalized least squares" = "gls",
                                   "Weighted least squares (sometimes called ADF estimation)" = "wls",
                                   "Unweighted least squares" = "uls",
                                   "Principal Axis Factoring" = "pa"
                                 ), selected = "ML")
                  )
                  
                  
                ),
                fluidRow(
                  box(
                    radioButtons("pafaoption", strong("PC or FA: for Parallel Analysis"),
                                 c(
                                   "Factor Analysis (FA)" = "fa",
                                   "Principal Componenta ANalysis (PCA)" = "pc",
                                   "Both" = "both"
                                 ), selected = "fa")
                  )
                )
              ),
              
              
              # If using PLS then specify measurement and structural model
              conditionalPanel(
                condition = "input.anal == 'pls'",
                
                fluidRow(
                  box( width = 8, 
                       aceEditor("plsinner", mode="r", 
                                 value="
                                 PEW <- PUW <- PEUW <- PEA <- PUA <- PEUA <-c(0,0,0,0,0,0,0,0)
                                 IU <- c(1,1,1,1,1,1,0,0)
                                 Use <- c(0,0,0,0,0,0,1,0)
                                 wear_path = rbind(PEW, PUW, PEUW, PEA, PUA, PEUA, IU, Use)
                                 wear_blocks = list(1:4,5:8,9:11,12:15,16:19,20:22,23:25,26:28)
                                 wear_modes = c('A','A','A','A','A','A','A','B')
                                 wear_pls = plspm(dat, wear_path, wear_blocks, modes = wear_modes)
                                 wear_pls", 
                                 height = "200px")
                  ),
                  box( width = 4,
                       h3("Select Measurement Variables"),
                       uiOutput("Variablespls"))
                  )
                  ),
              
              
              # Conditional Panel to show up if doing CBSEM
              conditionalPanel(
                condition = "input.anal == 'sem' || input.anal == 'growth' || input.anal == 'cfa'",
                fluidRow(
                  h3("Estimator Options"),
                  box("Estimator Options", icon = icon("table", lib = "font-awesome"),
                      
                      radioButtons("estimatoroptions", strong("Estimator Options"),
                                   c("Maximum likelihood" = "ML",
                                     "Generalized least squares" = "GLS",
                                     "Weighted least squares (sometimes called ADF estimation)" = "WLS",
                                     "Unweighted least squares" = "ULS",
                                     "Diagonally weighted least squares" = "DWLS"
                                   ), selected = "ML")
                      
                  ),
                  box("Model Options", icon = icon("list", lib = "font-awesome"),
                      
                      radioButtons("orthogonaloptions", strong("Orthogonal"),
                                   c("True" = "TRUE",
                                     "False" = "FALSE"
                                   ), selected = "FALSE"),
                      br(),
                      p("If TRUE is selected the exogenous latent variables are assumed to be uncorrelated, by defualt lavaan sets this to false"),
                      br()
                      
                  ),
                  
                  box("Standard Errors Options", icon = icon("cog", lib = "font-awesome"),
                      
                      radioButtons("seoptions", strong("Method for Computing Standard Errors"),
                                   c("Conventional Standard Errors" = "standard",
                                     "First-order Derivatives" = "first.order",
                                     "Conventional Robust" = "robust.sem",
                                     "Bootstrap" = "bootstrap",
                                     "None" = "none"
                                   ), selected = "standard"),
                      numericInput("bootstrapoptions", label = "Number of bootstrap draws, if bootstrapping is used." , value = 1000),
                      p("Using bootstrap will take some awhile even when hosted locally")
                  )
                  
                  
                )
              )
              
              
              
              
              
              ),
      
      
      # Fourth tab content
      tabItem(tabName = "regresult",
              
              h2("Result"),
              
              conditionalPanel(
                condition = "input.anal == 'efa' && input.efalysisopt == 'pca'",
                fluidRow(
                  box(
                    h3("Output"),
                    verbatimTextOutput("resultefapca")
                    ,width = 12
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.anal == 'efa' && input.efalysisopt != 'pca'",
                fluidRow(
                  box(
                    h3("Output"),
                    verbatimTextOutput("resultefa")
                    ,width = 12
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.anal == 'pls'",
                fluidRow(
                  box(
                    h3("Output"),
                    verbatimTextOutput("result")
                    ,width = 12
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.anal == 'sem' || input.anal == 'growth' || input.anal == 'cfa'",
                fluidRow(
                  box(
                    h3("Standardized Estimates"),
                    formattableOutput("stdsol"),
                    h3("Regression Summary"),
                    verbatimTextOutput("result1")
                    ,width = 12
                  )
                )
              )
              
              
      ),
      
      # fifth tab: Plot submenu of result
      tabItem(tabName = "plot",
              
              conditionalPanel(
                condition = "input.anal == 'efa' && input.efalysisopt == 'pca'",
                fluidRow(
                  box(width = 12,
                      h2("Principal Component Plot"),
                      fluidRow(
                        box(
                          plotOutput("plot4")
                        )
                      ),
                      fluidRow(
                        box(
                          plotOutput("plot3", height = 700), width = 12
                        )
                      ),
                      fluidRow(
                        box(
                          width = 4,
                          h3("Select Grouping Variable"),
                          uiOutput("Variablepcagroup")
                        )
                      )
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.anal == 'efa' && input.efalysisopt != 'pca'",
                fluidRow(
                  box(width = 12,
                      h2("EFA Factor Plot"),
                      fluidRow(
                        box(
                          plotOutput("plot2", height = 700), width = 12
                        )
                      ))
                )
              ),
              
              conditionalPanel(
                condition = "input.anal != 'pls' && input.anal != 'efa'",
                fluidRow(
                  box(width = 12,
                      h2("Plot"),
                      h3("Plot Options"),
                      fluidRow(
                        box(
                          selectInput("pltopt", label = "Standardized or Non Standardized Parameters:",
                                      choices = c("No weight" = "path",
                                                  "Standardized" = "std",
                                                  "Unstandardized" = "est",
                                                  "Equality Cont" = "eq",
                                                  "Mixed Colors" = "col"))
                        ),
                        box(
                          selectInput("pltstyl", label = "Residual Variance:",
                                      choices = c("RAM" = "ram",
                                                  "MX" = "mx",
                                                  "OpenMX" = "OpenMx",
                                                  "LISREL" = "lisrel"))
                        ),
                        box(
                          radioButtons("lay", strong("Plot Layout"),
                                       c("Circle" = "circle",
                                         "Circle2" = "circles",
                                         "Tree" = "tree",
                                         "Tree2" = "tree2",
                                         "Spring" = "spring",
                                         "Spring2" = "spring2"
                                       ), selected = "tree2", inline = TRUE)
                        )
                      )
                      
                      
                      
                  )
                  
                ),
                fluidRow(
                  
                  box(
                    h3("Diagram"), plotOutput("plot", height = 700), width = 12
                  )
                ),
                fluidRow(
                  box(
                    p("Tree"),
                    p("The integrated tree-like layout. Places exogenous variables at the top and endogenous variables at the bottom. See 'details' for more details."),
                    p("Tree2"),
                    p("Calls the layout.reingold.tilford function from the igraph package (Csardi & Nepusz, 2006), which uses the Reingold-Tilford algorithm (Reingold & Tilford, 1981). Before calling the algorithm roots are chosen and a slightly modified version of the graph is used to produce consistent results. See 'details'."),
                    p("Circle"),
                    p("The same layout as tree, except that afterwards the horizontal levels of the layout are placed in circles. Especially useful for models with a large number of manifest variables and a relatively small number of latent variables."),
                    p("Circle2"),
                    p("The same layout as tree2, except that afterwards the horizontal levels of the layout are placed in circles"),
                    p("Spring"),
                    p("Calls the spring layout in qgraph, which uses the Fruchterman-reingold algorithm (Fruchterman & Reingold, 1991)."), width = 12) 
                )
              ),
              conditionalPanel(
                condition = "input.anal == 'pls'",
                fluidRow(
                  box(width = 12,
                      h3("Structural Model"),
                      box(width=12,
                          plotOutput("plsinner", height = 700)
                      )
                  ),
                  box(width = 12,
                      h3("Measurement Model"),
                      plotOutput("plsouter", height = 700))
                )
                
              )
              
      )
      
      
      
      
                )
                )
                )

