# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Read the data into R
body <- read.csv("https://github.com/byuistats/data/raw/master/BodyMeasurementsCorrected/BodyMeasurementsCorrected.csv", header = TRUE, stringsAsFactors = FALSE)

# View the data
head(body)

# Scatterplot with linear regression line
ggplot(body, aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Height (inches)", y = "Weight (lbs)", title = "Scatterplot of Height vs. Weight")

# Fit the linear regression model
model <- lm(Weight ~ Height, data = body)

# Display the model summary
summary(model)

# Display the coefficients
model$coefficients

# Predict the weight using the equation
height <- 76
intercept <- model$coefficients[1]
slope <- model$coefficients[2]
predicted_weight <- intercept + slope * height
predicted_weight

# Predict the weight using R
new_data <- data.frame(Height = 76)
predict(model, newdata = new_data)
