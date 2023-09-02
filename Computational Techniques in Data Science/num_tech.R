# Load necessary library
library(readr)
library(dplyr)
library(stats)

# Read the data
amhc <- read_csv("C:\\Users\\Rick-Royal\\Documents\\Strathmore University Data Science and Analytics\\Module 3\\Module-III\\Computational Techniques in Data Science\\amhc.csv")

# Get a vector of column names excluding 'Aanc'
predictor_vars <- setdiff(names(amhc), "Aanc")

# Initialize an empty list to store the model fits
model_list <- list()

# Loop through each predictor variable to fit the model
for (var in predictor_vars) {
  # Create the model formula
  model_formula <- as.formula(paste("Aanc ~", var))
  
  # Fit the model and store it in the list
  model_list[[var]] <- glm(model_formula, data = amhc, family = binomial(link = "logit"))
}

# Print the summary of each model
for (var in predictor_vars) {
  print(paste("Summary for model with predictor variable:", var))
  print(summary(model_list[[var]]))
}


# Specify the variables to include in the model
variables <- c("province", "county", "Adc", "peduc", "educ", "wealth", "insurance", "mediaexpo", "mage", "ethinicdiv", "NpropHig", "comhospdel", "desirepreg", "pregcomplication", 
               "mage1", "hospdelivery", "blast5yrs", "blastyr", "childunder5", "decisionm", "occupation", "nchilddead", "childeverborn", 
               "Rhospdel", "AMHC", "age", "region", "residence", "religion", "ethnic", "sexhhead", "mstatus", "NPhighme", "Nethinic", "Npfemale", 
               "Nmedianh", "Npmoved", "Phighmedia", "pfemaleheaded", "propHighwealth", "medianhhsize", "pmoved", "ethnicity")

# Subset the data to include only these variables
amhc <- amhc[ , variables]

# Adjusted model with all specified predictors
fit_all <- glm(Adc ~ ., data = amhc, family = binomial(link = "logit"))
summary(fit_all)

