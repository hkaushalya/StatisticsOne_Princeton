rm(list=ls())

library(psych)

PE <- read.table("../Lab4/stats1-datafiles-Stats1.13.Lab.04.txt" , header = T)

describe(PE)

#Correlation Analysis
cor(PE[2:4])  # Omit column 1 because it contains participant id numbers

round(cor(PE[2:4]), 2)

cor.test(PE$age, PE$activeyears)
cor.test(PE$endurance, PE$activeyears)
cor.test(PE$endurance, PE$age)

# Histograms
layout(matrix(c(1,2,3,4), 2, 2, byrow =TRUE))
hist(PE$age)
hist(PE$activeyears)
hist(PE$endurance)

layout(matrix(c(1,1), 1, 1, byrow =TRUE))

# Regression analysis, unstandardize
model.1 <- lm(PE$endurance ~ PE$age)
summary(model.1)
plot(PE$endurance ~ PE$age)
abline(lm(PE$endurance ~ PE$age), col='blue')

model.2 <- lm(PE$endurance ~ PE$activeyears)
summary(model.2)
plot(PE$endurance ~ PE$activeyears)
abline(lm(PE$endurance ~ PE$activeyears), col='blue')

# multiple regression
model.3 <- lm(PE$endurance ~ PE$age + PE$activeyears)
summary(model.3)
plot(PE$endurance ~ PE$activeyears)
abline(lm(PE$endurance ~ PE$activeyears), col='blue')

# to visualize model3
PE$predicted <- fitted(model.3)
plot(PE$endurance ~ PE$predicted)
abline(lm(PE$endurance ~ PE$predicted), col='blue')

# The function residuals
PE$e <- resid(model.3)
hist(PE$e)
plot(PE$predicted ~ PE$e)
abline(lm(PE$predicted ~ PE$e), col='blue')

# Regression analyses, standardized
# In simple regression, the standardized regression coeffcient will be the same
# as the correlation coefficient

round(cor(PE[2:4]), 2)

model1.z <- lm (scale(PE$endurance) ~ scale(PE$age))
summary(model1.z)

model2.z <- lm (scale(PE$endurance) ~ scale(PE$activeyears))
summary(model2.z)

model3.z <- lm (scale(PE$endurance) ~ scale(PE$age) + scale(PE$activeyears))
summary(model3.z)
