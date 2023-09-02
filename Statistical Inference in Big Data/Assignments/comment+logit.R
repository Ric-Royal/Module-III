# Load the tidyverse package for data manipulation and visualization
library(tidyverse)

# Read the data from the CSV file
dat <- read_csv("Challenger.csv")

# Define a function to calculate the coefficients of a logistic regression model
getCoefficients <- function(design_matrix, response_vector, epsilon = .0001) {
  
  # Convert the design matrix and response vector to matrix format
  X <- as.matrix(design_matrix)
  y <- as.matrix(response_vector)
  
  # Define the logistic function used for Scoring calculations
  pi_i <- function(v) return(exp(v) / (1 + exp(v)))
  
  # Initialize beta_0, p_0, W_0, I_0 & U_0
  beta_0 <- matrix(rep(0, ncol(X)), nrow = ncol(X), ncol = 1, byrow = FALSE, dimnames = NULL)
  p_0 <- pi_i(X %*% beta_0)  # The estimate of mu
  W_0 <- diag(as.vector(p_0 * (1 - p_0)))  # The weights matrix
  I_0 <- t(X) %*% W_0 %*% X  # The Information matrix
  U_0 <- t(X) %*% (y - p_0)  # The score function
  
  # Initialize variables for iteration
  beta_old <- beta_0
  iter_I <- I_0
  iter_U <- U_0
  iter_p <- p_0
  iter_W <- W_0
  
  # Initialize the iteration counter
  fisher_scoring_iterations <- 0
  
  # Iterate until the difference between beta_new and beta_old is less than epsilon
  while(TRUE) {
    # Update the iteration counter
    fisher_scoring_iterations <- fisher_scoring_iterations + 1
    
    # Calculate the new beta values
    beta_new <- beta_old + solve(iter_I) %*% iter_U
    
    # Check if the difference between beta_new and beta_old is less than epsilon
    if (all(abs(beta_new - beta_old) < epsilon)) {
      # If it is, then the model parameters have converged and we can stop iterating
      model_parameters <- beta_new
      fitted_values <- pi_i(X %*% model_parameters)
      covariance_matrix <- solve(iter_I)
      break
    } else {
      # If not, then we need to update the variables for the next iteration
      iter_p <- pi_i(X %*% beta_new)
      iter_W <- diag(as.vector(iter_p * (1 - iter_p)))
      iter_I <- t(X) %*% iter_W %*% X
      iter_U <- t(X) %*% (y - iter_p)
    }
    
    # Update beta_old for the next iteration
    beta_old <- beta_new
  }
  
  # Return a list containing the model parameters, covariance matrix, fitted values, and number of iterations
  summaryList <- list(
    'model_parameters' = model_parameters,
    'covariance_matrix' = covariance_matrix,
    'fitted_values' = fitted_values,
    'number_iterations' = fisher_scoring_iterations
  )
  
  return(summaryList)
}

# Convert the response variable to matrix format
y <- as.matrix(dat$O_RING_FAILURE)

# Create a design matrix with an intercept term and the TEMPERATURE variable
X <- cbind(rep(1, 23), dat$TEMPERATURE)

# Call the getCoefficients function to fit the logistic regression model
results <- getCoefficients(X, y)

# Print the results
print(results)