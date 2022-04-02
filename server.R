# Adrian Cueto (CMSC 150 Exer 10)
# This serves as the server of the RShiny App

library(shiny) # this imports the package R Shiny
library(shinyMatrix) # this imports the package for the matrixInput() function to be used in the Simplex Method
# imports the QSI and Simplex Method program to be integrated in UI
source("www/CuetoEx08.R")
source("www/CuetoEx09.R")

# Server
function(input, output) {
  # QSI Calculator
  observeEvent(input$calculate, { # this runs the code once the user clicks the "Calculate" button
    if (input$x_values != "" && input$y_values != ""){ # checks if there is an input for both the x and y values
      xvector = strsplit(input$x_values, ",") # turns the text input into a character vector 
      yvector = strsplit(input$y_values, ",")
      
      textinput <- eventReactive(input$calculate, { # this will run once the user clicks the "Calculate" button
        data1 = list(x_values = as.numeric(unlist(xvector)), y_values = as.numeric(unlist(yvector)))
        print(data1)
        print(paste("Approximate x =", input$est_val))
      })
      
      computation <- eventReactive(input$calculate, { # this will run once the user clicks the "Calculate" button
        data1 = list(x_values = as.numeric(unlist(xvector)), y_values = as.numeric(unlist(yvector)))
        result = poly.qsi(data1,input$est_val) # function from Exercise 8
        return(result)
      })
      
      output$qsi_input <- renderPrint({
        textinput() # prints the xvector, yvector, and the x value to be approximated from the user input 
        })
      
      output$qsi_funct <- renderPrint({
          result = computation() # calculates for the QSI and returns the list to be printed
          print(result$qsi.fxns) # prints the list that contains the functions between intervals
      })
      
      output$qsi_result <- renderPrint({
          result = computation() # calculates for the QSI and returns the list to be printed
          print(result$y) # prints the approximated y value for the corresponding x value from the user input
      })
      
    } else { # if the user did not enter an input in either the two input lines (x and y values)
      output$qsi_input <- renderPrint({
        print("Incomplete input/s!")
      })
      output$qsi_funct <- renderPrint({
        print("Incomplete input/s!")
      })
      output$qsi_result <- renderPrint({
        print("Incomplete input/s!")
      })
    }
  })
  #Simplex Method
  observeEvent(input$generate, { #runs the code once the user clicks "Generate" button
    if (input$generate > 0 && input$choice == "max") { #maximization
      
      #create dimnames for matrix
      variable_names = character(input$equations+input$variables)
      for (i in 1:(input$variables-1)){
        variable_names[i] = paste("x", i, sep="")
      }
      for (j in 1:(input$equations-1)){
        variable_names[j+(input$variables-1)] = paste("s", j, sep="")
      }
      variable_names[length(variable_names)-1] = "Z"
      variable_names[length(variable_names)] = "Solution"
      
      inputmatrix = reactive({ #this will generate a matrix based on the input of the user
        matrix(
          "",
          nrow = input$equations, ncol = (input$equations+input$variables), byrow = T, 
          dimnames = list(1:input$equations, variable_names)
        )})
      
      create_matrix <- eventReactive(input$generate, { # this will show a matrix to the user once the "Generate" button has been clicked
        div(
          helpText("Note: Do not leave a blank on this table"),
          "Maximize:",
          matrixInput(inputId = "tableau", value = inputmatrix(), class = "numeric"), # generates a blank matrix with dimnames
          helpText("Click 'Calculate' to compute for the data"),
          actionButton(inputId = "simplex", label =  "Calculate") #this will run the simplex method once the user clicks this
        )
      })
      
      output$coeffmatrix <- renderUI({ #this outputs to the main UI
        create_matrix()
      })
      
      simplex_maximum <- eventReactive(input$simplex, { #this computes for the maximization simplex method once the user clicks the "Calculate" button
        return(simplex(input$tableau, T, F)) #function call from Exercise 9
      })
      
      output$simplex_result <- renderPrint({ #outputs the computed result to the UI
        simplex_list = simplex_maximum() 
        print(simplex_list)
      })
      
    } else if (input$generate > 0 && input$choice == "min") { #minimization
      
      #create dimnames for matrix
      variable_names = character(input$variables)
      for (i in 1:(input$variables-1)){
        variable_names[i] = paste("x", i, sep="")
      }
      variable_names[length(variable_names)] = "RHS"
      
      inputmatrix = reactive({ #this will generate a matrix based on the input of the user
        matrix(
          "",
          nrow = input$equations, ncol = input$variables, byrow = T,
          dimnames = list(1:input$equations, variable_names)
        )})
      
      create_matrix <- eventReactive(input$generate, {  # this will show a matrix to the user once the "Generate" button has been clicked
        div(
          helpText("Note: Do not leave a blank on this table"),
          "Minimize:",
          matrixInput(inputId = "tableau", value = inputmatrix(), class = "numeric"),
          helpText("Click 'Calculate' to compute for the data"),
          actionButton(inputId = "simplex", label =  "Calculate") 
        )
      })
      
      output$coeffmatrix <- renderUI({ #this outputs to the main UI
        create_matrix()
      })
      
      simplex_minimum <- eventReactive(input$simplex, { #this computes for the minimization simplex method once the user clicks the "Calculate" button
        initial_tableau = minimization_tableau(input$tableau, input$equations, input$variables) # function call from Exer 9
        return(simplex(initial_tableau, F, input$problem))
      })
      
      output$simplex_result <- renderPrint({ #outputs the computed result to the UI
        simplex_list = simplex_minimum()
        print(simplex_list)
      })
      
      output$shipping_num <- renderTable({ #this outputs a table once the user checks the checkbox
        simplex_list = simplex_minimum()
        simplex_list$shipping.num
      }, rownames =  TRUE)
    }
  })
}