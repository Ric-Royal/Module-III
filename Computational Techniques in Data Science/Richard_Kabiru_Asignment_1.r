#Install r libraries
install.packages("tidyverse")
install.packages("readr")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("ggpubr")
install.packages("ggrepel")

# import r libraries for data analysis
library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)


# import r libraries for data visualization
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(ggrepel)

# Load csv file
sal <- read.csv('C:\\Users\\Rick-Royal\\Documents\\Strathmore University Data Science and Analytics\\Module 3\\Computational Techniques in Data Science\\table_203b.csv')

# View the first 6 rows of the data
head(sal)

# View the last 6 rows of the data
tail(sal)

# View the structure of the data
str(sal)

# View the summary of the data
summary(sal)

# View the dimensions of the data
dim(sal)

# View the column names of the data
colnames(sal)

# plot a cerrelation matrix
cor(sal)

# plot a cerrelation matrix
cor(sal, method = "spearman")

# plot a histogram of LCURRENT with 10 bins and title Current Salary and x label Current Salary and y label Frequency
hist(sal$LCURRENT, breaks = 10, main = "Current Salary", xlab = "Current Salary", ylab = "Frequency")

# Plot a histogram of LSTART with 10 bins and title Starting Salary and x label Starting Salary and y label Frequency
hist(sal$LSTART, breaks = 10, main = "Starting Salary", xlab = "Starting Salary", ylab = "Frequency")

# Plot a histogram of sex with title as Sex and x label as sex and y label as Frequency
hist(sal)

# Using ggplot2, plot a scatter plot for lstart and lcurrent
ggplot(sal, aes(x = LSTART, y = LCURRENT)) + geom_point()

# Using ggplot2, plot a scatter plot for lstart and lcurrent with a linear regression line
ggplot(sal, aes(x = LSTART, y = LCURRENT)) + geom_point() + geom_smooth(method = "lm")


# Using ggplot2, plot a scatter plot for CURRENT and START columns with a linear regression line
ggplot(sal, aes(x = START, y = CURRENT)) + geom_point() + geom_smooth(method = "lm")

# Using ggplot2, plot a scatter plot for CURRENT and START columns with a linear regression line and title as Starting Salary vs Current Salary
ggplot(sal, aes(x = START, y = CURRENT)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Starting Salary vs Current Salary")

# Using ggplot2, plot a scatter plot for CURRENT and START columns with a linear regression line and title as Starting Salary vs Current Salary and x label as Starting Salary and y label as Current Salary
ggplot(sal, aes(x = START, y = CURRENT)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Starting Salary vs Current Salary") + xlab("Starting Salary") + ylab("Current Salary")

# using ggplot2 and geom_col, plot x = AGE and y = CURRENT with xlabel as Age and ylabel as Current Salary
ggplot(sal, aes(x = SEX, y = CURRENT)) + geom_col() + xlab("Sex") + ylab("Current Salary")

# using ggplot2 and geom_col, plot x = JOBCAT and y = CURRENT with xlabel as Job Category and ylabel as Current Salary
ggplot(sal, aes(x = JOBCAT, y = CURRENT)) + geom_col() + xlab("Job Category") + ylab("Current Salary")

# using ggplot2 and geom_point, plot x = START and y = current with xlabel as Starting Salary and ylabel as Current Salary, hue = SEX
ggplot(sal, aes(x = START, y = CURRENT, color = SEX)) + geom_point() + xlab("Starting Salary") + ylab("Current Salary")

# using ggplot2 and geom_point, plot x = LSTART and y = Lcurrent with xlabel as Starting Salary and ylabel as Current Salary, hue = JOBCAT



