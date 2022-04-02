# Adrian Cueto (CMSC 150 Exer 10)
# This serves as the UI of the RShiny app

library(shiny) # this imports the package R Shiny
library(shinyMatrix) # this imports the package for the matrixInput() function to be used in the Simplex Method
# imports the QSI and Simplex Method program to be integrated in UI
source("www/CuetoEx08.R")
source("www/CuetoEx09.R")

# UI
fluidPage(
  navbarPage(title = "CMSC 150 Lab Exercise 10: Integration",  #navbarPage() is used to link the two programs (QSI, Simplex) in one instance of the shinyApp()
             # this is the homepage
             tabPanel("Homepage", 
                      headerPanel("Welcome!"),
                      mainPanel(
                        h3("Introduction"),
                        h4("Hello! I am Adrian B. Cueto, a BS Computer Science student from UPLB."),
                        h4("This is the web application that I've created using RShiny to integrate my last 2 exercises (Exer 8 & 9) for Exercise # 10."),
                        h4("Thank you for checking my web app!"),
                        hr(),
                        h3("Navigation"),
                        h4("You can navigate the web app by clicking the panels above."),
                        img(src = "navbar.png", height = 100, width = 1500, align = "center"),
                        p("Click the", strong("Exercise 8: Quadratic Spline Interpolation Calculator"), "panel if you want to compute for Quadratic Spline Interpolation."),
                        img(src = "qsi.png", height = 100, width = 1500, align = "center"),
                        p("Click the", strong("Exercise 9: Simplex Method Calculator") , "panel if you want to compute for maximization and minimization for Simplex Method."),
                        img(src = "simplex.png", height = 95, width = 1500, align = "center"),
                        p("Click the", strong("Homepage"), "panel to return to this page."),
                        img(src = "navbar.png", height = 100, width = 1500, align = "center")
                      )
             ),
             # page for Quadratic Spline Interpolation
             tabPanel("Exercise 8: Quadratic Spline Interpolation Calculator",
                      #runApp("shinymatrix.R"),
                      headerPanel("Quadratic Spline Interpolation Calculator"),
                      sidebarLayout(
                        sidebarPanel( # Input panel on the left side of screen
                          h3("Instructions"),
                          helpText("Inputs must be separated by a comma ','", "eg. 1.0,2.0,3.0. ", "Also, the length of the values entered in the two variables must be the same"),
                          helpText("Click the 'Calculate' button once all the values are inputted correctly"),
                          helpText("NOTE: Enter at least 3 values for each variable."),
                          textInput(inputId = "x_values",  #this will take the x values from the user
                                    label = "Enter x values (independent variables)",
                                    value = "",
                                    placeholder = "eg. 1,2,3"
                          ),
                          textInput(inputId = "y_values", #this will take the y values from the user
                                    label = "Enter y values (dependent variables)",
                                    value = "",
                                    placeholder = "eg. 4.0,5.0,6.0"
                          ),
                          numericInput(inputId = "est_val", #this will only accept a numeric value for the approximated x value
                                       label = "Enter x value to be approximated (Must be in the range of the given x values)",
                                       value = 1
                          ),
                          actionButton(inputId = "calculate", label = "Calculate") #Calculates for QSI once this is clicked
                        ),
                        mainPanel( # Shows the results panel on the right
                          h3("Input Values:"),
                          verbatimTextOutput("qsi_input"), # this prints the data gathered from the output$qsi_input from the server
                          hr(),
                          h3("Functions Gathered per interval:"),
                          verbatimTextOutput("qsi_funct"), # this prints the data gathered from the output$qsi_funct from the server
                          hr(),
                          h3("Estimated y value for x input:"),
                          verbatimTextOutput("qsi_result") # this prints the data gathered from the output$qsi_result from the server
                        )
                      )),
             # page for Simplex Method 
             tabPanel("Exercise 9: Simplex Method Calculator",
                      headerPanel("Simplex Method Calculator"),
                      sidebarLayout(
                        sidebarPanel( # Input panel on the left side of the screen
                          h3("Instructions"),
                          helpText("On the first input line, select the number of equations that are gathered from the problem. This includes the objective function Z."),
                          helpText("On the second line, select the number of variables that are needed to be solved. (Disregard the Slack Variables)"),
                          helpText("After clicking the 'Generate Matrix' button, a matrix will then be generated for the user to input the values in it (Initial Tableau)"),
                          numericInput(inputId = "equations",  #numeric input for the number of constraints
                                       label = "Enter the number of equations/constraints",
                                       value = 4, min = 4
                          ),
                          numericInput(inputId = "variables",  #numeric input for the number of constraints
                                       label = "Enter the number of variables needed to be solved including Z",
                                       value = 3, min = 3
                          ),
                          selectInput(inputId = "choice",    #numeric input for the number of unknowns
                                       label = "Maximization or  Minimization?",
                                       c("Maximization" = "max",
                                         "Minimization" = "min")
                          ),
                          checkboxInput(inputId = "problem", #boolean value to check if the shipping.num will be added
                                       label = "If Minimization is selected, check this box if this will be used for a specific problem. (Minimum Shipping Cost)",
                                       value = FALSE
                          ),
                          actionButton(inputId = "generate", label = "Generate Matrix"), #generates a matrix based on the given values when clicked
                          helpText("NOTE: Always click the 'Generate Matrix' Button whenever you change something on the input lines above")
                        ),
                        mainPanel( # Shows the results panel on the right
                          h3("Input Matrix elements here:"),
                          uiOutput("coeffmatrix"),
                          hr(),
                          h3("Results:"),
                          verbatimTextOutput("simplex_result"),
                          hr(),
                          h4("Number of Items to ship from plant to warehouse:"),
                          p("(For Specific Problem only)"),
                          tableOutput("shipping_num")
                        )
                      )
                )
  )
)