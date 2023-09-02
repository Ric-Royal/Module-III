# Load the necessary library
library(tidyverse)

# Create the data frame
data <- tibble(
  yi = c(65, 156, 100, 134, 16, 108, 121, 4, 39, 143, 56, 26, 22, 1, 1, 5, 65),
  xi = c(3.36, 2.88, 3.63, 3.41, 3.78, 4.02, 4.00, 4.23, 3.73, 3.85, 3.97, 4.51, 4.54, 5.00, 5.00, 4.72, 5.00)
)

# Plot yi against xi
ggplot(data, aes(x = xi, y = yi)) +
  geom_point() +
  labs(x = "log10(Initial White Blood Cell Count)", y = "Survival Time (weeks)", title = "Survival Time vs. Initial White Blood Cell Count")

# Fit the model
model <- glm(yi ~ xi, family = Gamma(link = "log"), data = data)

# Print the summary of the model
summary(model)