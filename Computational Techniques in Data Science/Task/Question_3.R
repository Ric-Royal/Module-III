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


# Calculate ln(y_i) and ln(i)
data <- data %>% mutate(ln_cases = log(cases), ln_time_period = log(time_period))

# Plot ln(y_i) against ln(i)
ggplot(data, aes(x = ln_time_period, y = ln_cases)) +
  geom_point() +
  labs(x = "ln(Time Period)", y = "ln(Number of Cases)", title = "ln(Number of Cases) vs. ln(Time Period)")


# Initialize the parameters
beta <- c(0, 0)

# Set a tolerance level
tol <- 1e-8

# Initialize the difference
diff <- Inf

# Fisher's scoring algorithm
while (diff > tol) {
  # Calculate the expected values
  mu <- exp(beta[1] + beta[2] * data$ln_time_period)
  
  # Calculate the score function
  S <- c(sum(data$cases - mu), sum((data$cases - mu) * data$ln_time_period))
  
  # Calculate the information matrix
  I <- matrix(c(sum(mu), sum(mu * data$ln_time_period), sum(mu * data$ln_time_period), sum(mu * data$ln_time_period^2)), nrow = 2)
  
  # Update the parameters
  beta_new <- beta + solve(I, S)
  
  # Calculate the difference
  diff <- sum(abs(beta_new - beta))
  
  # Update the parameters
  beta <- beta_new
}

# Print the estimated parameters
print(beta)

# Fit the GLM
model <- glm(cases ~ ln_time_period, family = poisson(link = "log"), data = data)

# Print the summary of the model
summary(model)