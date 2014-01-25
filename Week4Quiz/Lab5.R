#Lab 5
rm(list=ls())
library(psych)
library(ggplot2)

PE <- read.table("stats1-datafiles-Stats1.13.Lab.05.txt", header = T)

describe(PE)
# Standard error calculation
# Standard error = Standard deviation / sqrt(sample size)
table1 <- describe(PE)
table1
age.sd <- table1[2,4]
age.sd
age.n  <- table1[2,2]
age.n
age.se <- age.sd / sqrt(age.n)
age.se


#Correlation Analsysis
cor(PE[2:4])

# NHST for each correlation coefficient
cor.test(PE$age, PE$activeyears)
cor.test(PE$endurance, PE$activeyears)
cor.test(PE$endurance, PE$age)


# save the correlation in a table to illutrate caculation of regression
# coefficients
table2 <- cor(PE[2:4])

# Regression analyses, unstandardized
model1 <- lm(PE$endurance ~ PE$age)
summary(model1)

# Regression coefficent
# B = r * (sdy / sdx)

# Standard error of Regression coeffiecent
# se.B = sqrt [ (SS.resid / (N-2) ) / SS.X ]
# N == dof
table3 <- anova(model1)
table3
SS.resid <- table3[2,2]
df <- table3[2,1]
SS.X <- table3[1,2] + table3[2,2]  # sum of both model + resid
se.B <- sqrt ( (SS.resid / df ) / SS.X) 
se.B

# Print 95% conf. in for the regression coeff.
confint(model1)

# Illustration of calculation of conf. int.
# Upper value = B + (tcrit * se.B) and Lower value = B - (tcrit * se.B)

# few more stuff here


# plotting with conf. int. aorund regression line
ggplot(PE, aes(x=age, y=endurance)) + geom_smooth(method = 'lm') +
    geom_point()


