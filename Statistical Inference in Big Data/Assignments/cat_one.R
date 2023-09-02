# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Read the data into R
body <- read.csv("https://github.com/byuistats/data/raw/master/BodyMeasurementsCorrected/BodyMeasurementsCorrected.csv", header = TRUE, stringsAsFactors = FALSE)

# View the data
head(body)

# Scatterplot with linear regression line
ggplot(body, aes(x = Neck, y = Height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Neck (cm)", y = "Height (inches)", title = "Scatterplot of Neck length vs. Height")

# Fit the linear regression model
model <- lm(Height ~ Neck, data = body)

# Display the model summary
summary(model)

# Display the coefficients
model$coefficients

# Predict the Height using the equation
Neck <- 35
intercept <- model$coefficients[1]
slope <- model$coefficients[2]
predicted_height <- intercept + slope * Neck
predicted_height