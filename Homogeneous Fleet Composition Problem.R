#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Homogeneous Fleet Composition Problem")

# Import lpSolve package
library(lpSolve)

#Import required packages (ompr)
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)

#Set fixed cost 
cf <- 350

#Set variable cost 
cv <- 150

#Set hiring cost 
ch <- 800

#Set number of required vehicles
v_ <- c(12,	15,	16,	17,	17,	18,	20,	20,	21,	22,	24,	22,	20,	18,	17,	16,	14,	13,	13,	14,	15,	16,	17,	19,	21,	22,	23,	22,	24,	26,	27,	28,	30,	32,	32,	30,	29,	28,	26,	25,	25,	24,	22,	22,	19,	20,	18,	17,	16,	16,	14,	13)

#Get max value of v
v_max <- max(v_)

#Set problem's size
n <- length(v_)
N <- n

if(cf + cv < ch) {
  print("Cost condition holds")
  
  #Build Model
  Model <- MIPModel() %>%
    add_variable(x[t], t = 1:n, type = "binary") %>% #define variables
    add_variable(y[t], t = 1:n, type = "integer", lb = 0, ub = v_max) %>%
    add_variable(z, type = "integer", lb = 0, ub = v_max) %>%
    add_variable(dummy, type = "integer", lb = 1, ub = 1) %>% #dummy variable
    set_objective(N*cf*z + N*z*cv -N*z*ch +
                  sum_expr(v_[t]*x[t]*cv, t = 1:n) +
                  sum_expr(-y[t]*cv, t = 1:n) +
                  sum_expr(v_[t]*ch*dummy, t = 1:n) + 
                  sum_expr(-v_[t]*x[t]*ch, t = 1:n) +
                  sum_expr(y[t]*ch, t = 1:n), "min") %>% #define objective
    add_constraint(z >= v_[t] -v_max + v_max*x[t], t = 1:n) %>% #define constraints
    add_constraint(z <= v_[t] +v_max*x[t], t = 1:n) %>%
    add_constraint(y[t] <= z, t = 1:n) %>%
    add_constraint(y[t] <= v_max*x[t], t = 1:n) %>%
    add_constraint(y[t] >= z -v_max + v_max*x[t], t = 1:n) %>%
    solve_model(with_ROI(solver = "symphony", verbosity = 1))
  
  #Model summary
  ##Status
  print(paste("Model status is:", Model$status))
  
  ##Objective Function
  print(paste("Objective value:", objective_value(Model)))
  
  ##Solution
  print(paste("Variable v:", get_solution(Model, z)))
  
      
}else{
  print("Cost condition does not hold")
}
