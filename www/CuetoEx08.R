# Adrian B. Cueto
# BS Computer Science
# CMSC 150 Lab Exercise # 8 (Code starts at line 49)

options(scipen = 1000) #removes the scientific notation

#is used to find the unknowns (from Exer 4)
GaussianElimination = function(AugCoeffMatrix){ #Gaussian Elimination
  n_of_variable = length(AugCoeffMatrix$variables) #this will take the number of the variables present in the equation
  value_matrix = AugCoeffMatrix$augcoeffmatrix #used to access the matrix easier
  solution_vector = c(numeric(n_of_variable)) #this will store the solution set
  
  for (i in 1:(n_of_variable-1)){ #for loop to iterate the process in the method
    pivot_element = max(abs(value_matrix[i:n_of_variable,i])) #this will take the largest value in the specific column
    
    if (pivot_element == 0 || is.nan(pivot_element))
      return(0) #returns when all the values in the column are zero
    #partial pivoting
    for (row_num in 1:n_of_variable){ #this finds where the equation with the largest number in the column is included
      if (pivot_element == value_matrix[row_num,i]){
        temp_vector = value_matrix[i,] #this will store the equation that will be swapped with the other equation
        value_matrix[i,] = value_matrix[row_num,]
        value_matrix[row_num,] = temp_vector
        break
      }
    }
    #for loop that will execute the multiplier method
    for (j in (i+1):n_of_variable){
      pivot_element = value_matrix[i,i]
      multiplier = value_matrix[j,i] / pivot_element
      normalized_row = multiplier * value_matrix[i,]
      value_matrix[j,] = value_matrix[j,] - normalized_row 
    }
  }
  #backward substitution
  for (i in n_of_variable:1){
    solution_vector[i] = (value_matrix[i,(n_of_variable+1)] - sum(value_matrix[i, i:n_of_variable] * solution_vector[i:n_of_variable])) / value_matrix[i,i]
  }
  #labeled list will be created to store all the values needed to be returned
  gaussian_list = list(
    solutionSet = solution_vector,
    Variables = AugCoeffMatrix$variables,
    matrix = value_matrix
  )
  
  return(gaussian_list)
}

poly.qsi = function(data, x){ #main function
  x_values = data[[1]] #this takes the values of x in the "data" list
  y_values = data[[2]] #this takes the values of y in the "data" list
  if (length(x_values) != length(y_values))
    return (0)
  else
    n = length(x_values)
  intervals = n-1 #this stores the number of intervals based on the data points
  num_unknowns = 3*intervals # this calculates how many variables are needed based on the intervals
  matrix_length = num_unknowns * (num_unknowns+1) # this will store the number of elements that will be placed in the matrix
  
  variable_names = c() # this will store the names of the variables
  for (unknown in 1:intervals){ # for loop to store the variables in the vector
    variable_names = append(variable_names, paste("a", unknown, sep=""))
    variable_names = append(variable_names, paste("b", unknown, sep=""))
    variable_names = append(variable_names, paste("c", unknown, sep=""))
  }
  
  #setting up the matrix
  equation_matrix = matrix(numeric(matrix_length),nrow = num_unknowns, ncol = (num_unknowns+1), dimnames = list(1:num_unknowns, append(variable_names, ("RHS"))))
  current_eq = 2 # this is added to track the total equations added in the matrix
  
  #interior knots
  iteration = 1 # this variable is used to align the position of the values to be placed alongside their respective variables
  for (i in 3:n){ #for loop for the interior knots (the range is from i=3 to n since R uses 0-indexing)
    column = i #this will store the value of i to be used to correctly align the values to their respective variables in the matrix
    if (i > 3) { # for the next iterations
      column = column + (2*iteration) # this will be added to align the values properly 
      iteration = iteration + 1
    }
    #a_i-1 * x_i-1^2 + b_i-1 * x_i-1 + c_i-1 = f(x_i-1)
    equation_matrix[current_eq, column-2] = x_values[i-1] ^ 2
    equation_matrix[current_eq, column-1] = x_values[i-1]
    equation_matrix[current_eq, column] = 1
    equation_matrix[current_eq, num_unknowns+1] = y_values[i-1] # for the RHS
    current_eq = current_eq+1
    #a_i * x_i-1^2 + b_i * x_i-1 + c_i = f(x_i-1)
    equation_matrix[current_eq, column+1] = x_values[i-1] ^ 2
    equation_matrix[current_eq, column+2] = x_values[i-1]
    equation_matrix[current_eq, column+3] = 1
    equation_matrix[current_eq, num_unknowns+1] = y_values[i-1] # for the RHS
    current_eq = current_eq+1
  }
  
  #end points
  #first x value (a_i * x_0^2 + b_1 * x_0 + c_1 = f(x_0))
  equation_matrix[current_eq, 1] = x_values[1] ^ 2
  equation_matrix[current_eq, 2] = x_values[1]
  equation_matrix[current_eq, 3] = 1
  equation_matrix[current_eq, num_unknowns+1] = y_values[1]
  current_eq = current_eq+1
  #last x value (a_n * x_n^2 + b_n * x_n + c_n = f(x_n))
  equation_matrix[current_eq, num_unknowns-2] = x_values[n] ^ 2
  equation_matrix[current_eq, num_unknowns-1] = x_values[n]
  equation_matrix[current_eq, num_unknowns] = 1
  equation_matrix[current_eq, num_unknowns+1] = y_values[n]
  current_eq = current_eq+1
  
  #first derivative at interior 
  iteration = 1 #this has almost the same implementation as with Condition 1
  for (i in 3:n){
    column = i
    if (i > 3) {
      column = column + (2*iteration)
      iteration = iteration + 1
    }
    #2a_i-1 * x_i-1 - b_i-1 = 2a_i * x_i-1 + b_i
    equation_matrix[current_eq, column-2] = x_values[i-1] * 2
    equation_matrix[current_eq, column-1] = 1
    equation_matrix[current_eq, column+1] = x_values[i-1] * -2
    equation_matrix[current_eq, column+2] = -1
    current_eq = current_eq+1
  }
  
  #final condition (a_1 = 0)
  equation_matrix[1,] = 0
  equation_matrix[,1] = 0
  equation_matrix[1,1] = 1
  
  augcoefflist = list(variables = variable_names, augcoeffmatrix = equation_matrix) #this turns the matrix and vector into the augcoefflist to be passed in the Gaussian Elimination function
  result = GaussianElimination(augcoefflist) #this takes the list given by the GaussianElimination function
  if (is.numeric(result) && result == 0){ #if the method used cannot find the solution for the matrix
    print("The solution cannot be found!")
    return (NA)
  }
  #this will take the solutions from the "result" list
  solution_vector = result$solutionSet
  qsi.fxns = list() # creates a list to store the functions 
  #function builder
  counter = 1 # this is used to track the values that will be placed in the function
  for (i in 1:intervals){
    interval_vector = c(numeric(3)) # creates a vector that will contain the a, b, and c values for the interval function
    for (j in 1:3){
      interval_vector[j] = solution_vector[counter]
      counter = counter + 1
    }
    #string manipulation
    equation_i = "function (x)"
    equation_i = paste(equation_i, interval_vector[1], sep = " ")
    equation_i = paste(equation_i, "x^2", sep = " * ")
    equation_i = paste(equation_i, interval_vector[2], sep = " + ")
    equation_i = paste(equation_i, "x", sep = " * ")
    equation_i = paste(equation_i, interval_vector[3], sep = " + ")
    
    equation_i = eval(parse(text = equation_i)) #this will eval-parse the equation
    qsi.fxns = append(qsi.fxns, equation_i) #appends the obtained equation to the list
  }
  
  #finding y value
  x_counter = 1 # used to determine the interval between the equations
  y = NA
  for (i in 1:intervals){
    if (x >= x_values[x_counter] && x <= x_values[x_counter+1]){ # if the x value satisfies the interval condition
      y = qsi.fxns[[i]](x)
    } 
    x_counter = x_counter + 1
  }
  
  qsi_list = list(qsi.fxns = qsi.fxns, y = y) #creates the final list to be returned
  return(qsi_list)
}

#test cases
#x = c(3.0,4.5,7.0,9.0)
#y = c(2.5,1.0,2.5,0.5)
#data1 = list(x,y)

#poly.qsi(data1,5) #0.66

#poly.qsi(data1,8) #3.1

#poly.qsi(data1,8.5) #2.2

#poly.qsi(data1,20) #NA

#poly.qsi(data1,3.0) #2.5, the given data