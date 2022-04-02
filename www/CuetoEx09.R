# Adrian B. Cueto
# BS Computer Science
# CMSC 150 Lab Exercise # 9 Modified for Exer 10

minimization_tableau <- function(tableau2, numof_equations2, numof_unknowns){
  # transpose the tableau
  transposed_tableau = t(tableau2)
  
  # takes the values from the last column in the matrix and separates it from the transposed matrix
  rhs_matrix = matrix(transposed_tableau[,numof_equations2])
  transposed_tableau = transposed_tableau[,-numof_equations2]
  
  # this creates an identity matrix based on how many columns are in the transposed matrix
  identity_matrix = diag(numof_unknowns)
  
  # concatenates the transposed matrix to the identity matrix and then the values from the rhs_matrix
  initial_tableau = cbind(transposed_tableau, identity_matrix)
  initial_tableau = cbind(initial_tableau, rhs_matrix)
  
  # creates the initial tableau to be passed in the simplex() function and added the variable names
  colnames(initial_tableau) = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "Z", "Solution")
  # this turns the values in the last row into negative aside from the "Z" and the "RHS"
  initial_tableau[numof_unknowns,1:(numof_unknowns+numof_equations2-2)] = initial_tableau[numof_unknowns,1:(numof_unknowns+numof_equations2-2)] * -1 
  initial_tableau[numof_unknowns,(numof_unknowns+numof_equations2)] = 0 #adjusts the Z value from the Solution column to the actual Z column
  return(initial_tableau)
}

simplex <- function(tableau, isMax, problem){ #main function
  numof_equations = length(tableau[,1]) #this takes how many rows are in the matrix
  numof_variables = length(tableau[1,]) #this takes how many columns are in the matrix
  while (TRUE){ #Simplex Method Implementation
    #pivot column and element
    minimum_lastrow = min(tableau[numof_equations,]) #this will take the minimum value in the last row (objective function)
    if (minimum_lastrow >= 0) # this will break the while loop if there is no negative values left in the objective function
      break
    for (i in 1:numof_variables){ # takes the index of the column with the minimum value in the objective function
      if (tableau[numof_equations, i] == minimum_lastrow)
        pivot_column_index = i
    }
    test_ratio = NA #initialize the test ratio
    for (i in 1:(numof_equations-1)){ # selects the pivot element
      if (tableau[i, pivot_column_index] <= 0) # disregards zero and negative values for selecting pivot element
        next
      else if (is.na(test_ratio)){ # if the test ratio does not have a value yet
        test_ratio = tableau[i, numof_variables] / tableau[i, pivot_column_index]
        pivot_element_index = i
        next
      }
      #comparison of the test ratio
      temp_testratio = tableau[i, numof_variables] / tableau[i, pivot_column_index]
      if (temp_testratio < test_ratio && temp_testratio > 0){
        test_ratio = temp_testratio
        pivot_element_index = i
      }
    }
    if (is.na(test_ratio)){ #if all the values in the pivot column are either zeroes or negative
      print("There is no solution")
      return (NA)
    }
    #From Gauss-Jordan Elimination Method
    tableau[pivot_element_index,] = tableau[pivot_element_index,] / tableau[pivot_element_index, pivot_column_index] #normalizes the row with the pivot element
    for (j in 1:numof_equations){
      if (pivot_element_index == j) #this is to make sure that the row with the normalized value will not be changed
        next
      normalized_row = tableau[j,pivot_column_index] * tableau[pivot_element_index,] #the normalized row will then be multiplied to the element of the row that will be cancelled (scalar multiplication)
      tableau[j,] = tableau[j,] - normalized_row #this will subtract the values of the normalized row to the jth equation
    }
  }
  
  if (isMax == T) { #for maximization
    solution_value = c(numeric(numof_variables-1)) # creates a vector that stores the values of the basic solution
    for (i in 1:numof_variables){ #checks where the "1" is located in a specific variable
      for (j in 1:numof_equations){ #checks the row where the "1" is located in the variable
        if (tableau[j,i] == 1){
          solution_value[i] = tableau[j, numof_variables] #this gives the variable its respective value based on where its 1 is located 
          break
        }
      }
    }
    variable_names = list("[1]", dimnames(tableau)[[2]][1:(numof_variables-1)]) # this will serve as the dimnames of the basic solution matrix
    solutionset = matrix(solution_value, nrow = 1, byrow = T, dimnames = variable_names) # creates the basic solution set
    #initializes the list to be returned
    simplex_list = list(final.tableau = tableau, 
                        basic.solution = solutionset, 
                        opt.val = solutionset[length(solutionset)])
    
  } else if (isMax == F) { #for minimization
    solutionset = tableau[numof_equations,] #takes the values in the last row
    solutionset[length(solutionset)-1] = solutionset[length(solutionset)] #moves the value in the very last index to the "Z" index
    solutionset = solutionset[1:(length(solutionset)-1)] #removes the last element of the solutionset
    if (problem == T){
      shipping_matrix = matrix(solutionset[9:23], nrow = 3, ncol = 5, byrow = T, dimnames = list(c("DEN","PHO","DAL"), c("SAC","SL","ALB","CHI","NYC")))
      simplex_list = list(final.tableau = tableau, 
                          basic.solution = solutionset,
                          opt.val = as.numeric(solutionset[length(solutionset)]),
                          shipping.num = shipping_matrix)
    } else
    simplex_list = list(final.tableau = tableau, 
                        basic.solution = solutionset,
                        opt.val = as.numeric(solutionset[length(solutionset)])) 
  } else {
    print("The parameter 'isMax' is not a boolean value")
    return(NA)
  }
  
  return(simplex_list) #returns the list
}

# for maximization (from the Exer05)
variables = c("x1", "x2", "s1", "s2", "s3", "Z", "Solution") #change this based on the number of variables and slack variables needed
numof_equations = c(1:4) # change this based on how many constraints given including the objective function

tableau1 = matrix(c(2,3,1,0,0,0,66,
                   4,9,0,1,0,0,180,
                   10,2,0,0,1,0,200,
                   -75,-90,0,0,0,1,0),
                 nrow = 4, ncol = 7, byrow = T, dimnames = list(numof_equations, variables))
result1 = simplex(tableau1, T, F) # function call

# for minimization (hardcoded for the problem)
numof_equations2 = 9 # number of constraints and the objective function
numof_unknowns = 16 # all variables that needs solution including Z
tableau2 = matrix(c(-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,-310,
                    0,0,0,0,0,-1,-1,-1,-1,-1,0,0,0,0,0,-260,
                    0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-280,
                    1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,180,
                    0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,80,
                    0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,200,
                    0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,160,
                    0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,220,
                    10,8,6,5,4,6,5,4,3,6,3,4,5,5,9,1),
                  nrow = numof_equations2, ncol = numof_unknowns, byrow = T) #input tableau from the test cases
#function calls
initial_tableau = minimization_tableau(tableau2, numof_equations2, numof_unknowns)
result2 = simplex(initial_tableau, F, T)