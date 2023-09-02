# Load the dataset
data(cars)

# View the first few rows of the dataset
head(cars)

# Fit the linear model
model <- lm(dist ~ speed, data = cars)

# Print the summary of the model
summary(model)

# Print the ANOVA table
anova(model)

# Calculate the probability of Type I error
alpha <- 1 - pbinom(7, size = 20, prob = 0.25)
print(alpha)

# Calculate the power of the test
power <- 1 - pbinom(7, size = 20, prob = 0.3)
print(power)

# Calculate the probability of Type I error for n = 50 and X >= 17
alpha_50 <- 1 - pbinom(16, size = 50, prob = 0.25)
print(alpha_50)

# Calculate the power of the test for n = 50 and X >= 17
power_50 <- 1 - pbinom(16, size = 50, prob = 0.3)
print(power_50)

