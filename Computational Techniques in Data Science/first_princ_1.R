# Load the necessary library
library(tidyverse)

# Create the data frame
data <- tibble(
  year = rep(1984:1988, each = 4),
  quarter = rep(1:4, times = 5),
  cases = c(1, 6, 16, 23, 27, 39, 31, 30, 43, 51, 63, 70, 88, 97, 91, 104, 110, 113, 149, 159)
)

# Create a new variable for the time period
data <- data %>% mutate(time_period = 1:n())

# Plot the number of cases against time period
ggplot(data, aes(x = time_period, y = cases)) +
  geom_point() +
  geom_line() +
  labs(x = "Time Period", y = "Number of Cases", title = "Number of AIDS Cases in Australia (1984-1988)")

# Create a new variable for the time period
data <- data %>% mutate(time_period = 1:n())

# Calculate the natural logarithm of the number of cases and the time period
data <- data %>% mutate(ln_cases = log(cases), ln_time_period = log(time_period))

# Plot ln(cases) against ln(time_period)
ggplot(data, aes(x = ln_time_period, y = ln_cases)) +
  geom_point() +
  labs(x = "ln(Time Period)", y = "ln(Number of Cases)", title = "Log-Log Plot of Number of AIDS Cases in Australia (1984-1988)")

# Fit the GLM
model <- glm(cases ~ ln_time_period, family = poisson(link = "log"), data = data)

# Print the summary of the model
summary(model)