# Adrian Cueto (CMSC 150 Exer 10)
# This serves as the UI of the RShiny app

library(shiny)
library(shinyMatrix)
library(bslib)  # for theming

# Load server-side logic
source("www/CuetoEx08.R")
source("www/CuetoEx09.R")

# Custom theme
my_theme <- bs_theme(bootswatch = "flatly", base_font = font_google("Roboto"))

# UI
fluidPage(
  theme = my_theme,
  navbarPage("CMSC 150 Lab Exercise 10: Integration",
             
             tabPanel("Homepage", 
                      fluidRow(
                        column(12,
                               div(
                                 class = "mt-4 mb-4 p-4",
                                 h2("Welcome!"),
                                 p("Hello! I am ", strong("Adrian B. Cueto"), ", a BS Computer Science student from UPLB."),
                                 p("This web application integrates my previous exercises (Exer 8 & 9) into one Shiny app."),
                                 hr(),
                                 h3("Navigation"),
                                 p("Click a panel above to get started:"),
                                 br(),
                                 fluidRow(
                                   column(4,
                                          #img(src = "qsi.png", height = "150px", style = "display:block; margin: 0 auto;"),
                                          p(strong("Exercise 8:"), " Quadratic Spline Interpolation")
                                   ),
                                   column(4,
                                          #img(src = "simplex.png", height = "150px", style = "display:block; margin: 0 auto;"),
                                          p(strong("Exercise 9:"), " Simplex Method Calculator")
                                   ),
                                   column(4,
                                          #img(src = "navbar.png", height = "150px", style = "display:block; margin: 0 auto;"),
                                          p(strong("Return here via the Homepage tab"))
                                   )
                                 )
                               )
                        )
                      )
             ),
             
             tabPanel("Exercise 8: Quadratic Spline Interpolation Calculator",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Instructions"),
                          HTML("<ul>
            <li>Enter at least 3 comma-separated values for x and y.</li>
            <li>Example: <code>1.0, 2.0, 3.0</code></li>
            <li>Ensure x and y vectors have the same length.</li>
          </ul>"),
                          textInput("x_values", "Enter x values (independent)", placeholder = "e.g. 1,2,3"),
                          textInput("y_values", "Enter y values (dependent)", placeholder = "e.g. 4.0,5.0,6.0"),
                          numericInput("est_val", "X to approximate (must be within range)", 1),
                          actionButton("calculate", "Calculate", class = "btn-primary")
                        ),
                        mainPanel(
                          h3("Input Values:"),
                          verbatimTextOutput("qsi_input"),
                          hr(),
                          h3("Functions per Interval:"),
                          verbatimTextOutput("qsi_funct"),
                          hr(),
                          h3("Estimated Y value:"),
                          verbatimTextOutput("qsi_result")
                        )
                      )
             ),
             
             tabPanel("Exercise 9: Simplex Method Calculator",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Instructions"),
                          HTML("<ul>
            <li>Specify the number of equations (including objective Z).</li>
            <li>Select the number of variables (ignore slack vars).</li>
            <li>Generate matrix to input initial tableau.</li>
            <li>For minimization with specific problems, tick the checkbox.</li>
          </ul>"),
                          numericInput("equations", "Number of equations/constraints", 4, min = 4),
                          numericInput("variables", "Number of variables (incl. Z)", 3, min = 3),
                          selectInput("choice", "Optimization Type", choices = c("Maximization" = "max", "Minimization" = "min")),
                          checkboxInput("problem", "Minimization: Shipping Cost Problem?", FALSE),
                          actionButton("generate", "Generate Matrix", class = "btn-success")
                        ),
                        mainPanel(
                          h3("Matrix Input:"),
                          uiOutput("coeffmatrix"),
                          hr(),
                          h3("Results:"),
                          verbatimTextOutput("simplex_result"),
                          hr(),
                          h4("Shipping Items (if applicable):"),
                          tableOutput("shipping_num")
                        )
                      )
             )
  )
)
