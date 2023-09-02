'''
In this code, we first create a data frame with the data you provided. 
We then fit a Poisson regression model to the data using the glm function from 
the MASS library. 
The offset(log(person_years)) term is used to account for the different numbers 
of person-years in each group. 
The summary function is then used to print a summary of the fitted model, 
which includes the estimated regression coefficients and their standard errors, 
the z-values, and the p-values.
'''

# Load the necessary library
library(tidyverse)

# Create the data frame
data <- tibble(
  age_group = factor(rep(c("35-44", "45-54", "55-64", "65-74", "75-84"), 2), levels = c("35-44", "45-54", "55-64", "65-74", "75-84")),
  smoker = c(rep("Yes", 5), rep("No", 5)),
  deaths = c(32, 104, 206, 186, 102, 2, 12, 28, 28, 31),
  person_years = c(52407, 43248, 28612, 12663, 5317, 18790, 10673, 5710, 2585, 1462)
)
print(data)
# Load the necessary library for Poisson regression
library(MASS)

# Fit the Poisson regression model
model <- glm(deaths ~ smoker + age_group + offset(log(person_years)), family = poisson, data = data)

# Print the summary of the model
summary(model)
