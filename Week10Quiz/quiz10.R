rm(list=ls())
library(psych)
df <- read.table('stats1_datafiles_Stats1.13.HW.10.txt', header=T)
describe(df)
str(df)

# Question 1
# What is the median population age for the countries which voted to 
# take action against global warming? (round to 2 decimal places)

describeBy(df$age, df$change=="1")


# Question 2
# Run a logistic regression including all predictor variables. 
# Which predictors are significant in this model?

# Binary logistic regression
#lrfit <- glm(df$change ~ df$country + df$age + df$educ + df$gdp + df$co2, family = binomial)
lrfit <- glm(df$change ~ df$age + df$educ + df$gdp + df$co2, family = binomial)
summary(lrfit)
confint(lrfit) #using log likelyhood
confint.default(lrfit)  #using standard errors

# Model fit (using chi2)
with(lrfit, null.deviance - deviance)
with(lrfit, df.null - df.residual)  #df = dof
with(lrfit, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail=FALSE)) #p-values

# Question 3
# What does the negative value for the estimate of educ means?


# Question 4
# What is the confidence interval for educ, 
# using profiled log-likelihood? (round to 2 decimal places, 
# and give the lower bound first and the upper bound second, separated by a space)
confint(lrfit)

# Question 5
# What is the confidence interval for age, 
# using standard errors? (round to 2 decimal places, and give the lower bound 
# first and the upper bound second, separated by a space)
confint.default(lrfit)

#Question 6
# Compare the present model with a null model. 
# What is the difference in deviance for the two models? 
# (round to 2 decimal places)
model.dev <- with(lrfit, null.deviance-deviance)
round(model.dev,2)
model.df <- with(lrfit, df.null - df.residual)
model.df
with(lrfit, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE)) #p-value



# Question 9
# Do chi-squared values differ significantly if you 
# drop educ as a predictor in the model?
lrfit2 <- glm(df$change ~ df$age + df$gdp + df$co2, family = binomial)
summary(lrfit2)
with(lrfit2, null.deviance-deviance)
with(lrfit2, df.null - df.residual)
with(lrfit2, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE)) #p-value

#Question 10
# What is the percentage of cases that can be classified 
# correctly based on our model?
library(aod)
library(QuantPsyc)
ClassLog(lrfit, df$change)