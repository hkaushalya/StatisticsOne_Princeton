#Quiz 4
rm(list=ls())
library(lattice)
df <- read.table('Stats1.13.HW.04.txt')
df
str(df)
summary(df)
xyplot(df$salary~df$years)
cor(df$salary, df$years)

xyplot(df$salary~df$courses)
cor <- cor(df$salary, df$courses)
str(cor)

#regression
model.1 <- lm ( formula = df$salary ~ df$years)
summary(model.1)
confint(model.1)

model.2 <- lm ( formula = df$salary ~ df$courses)
summary(model.2)
confint(model.2)

model.3 <- lm ( formula = df$salary ~ df$years + df$courses)
summary(model.3)
confint(model.3)
df$mod3.predicted <- predict(model.3)
mod3.pred.mean <- mean(df$mod3.predicted)
mod3.pred.mean

model.4 <- lm ( formula = scale(df$salary) ~ scale(df$years) + scale(df$courses))
summary(model.4)
mod4.resd <-resid(model.4)
mean(mod4.resd)
hist(mod4.resd)

#standardized
model.11 <- lm ( formula = scale(df$salary) ~ scale(df$years))
summary(model.11)
cov(scale(df$salary),scale(df$years))

model.21 <- lm ( formula = scale(df$salary) ~ scale(df$courses))
summary(model.21)
cov(scale(df$salary),scale(df$courses))

model.31 <- lm ( formula = scale(df$salary) ~ scale(df$years) + scale(df$courses))
summary(model.31)
confint(model.31)
df$mod31.predicted <- predict(model.31)
mod31.pred.mean <- mean(df$mod31.predicted)
mod31.pred.mean





df$predicted <- fitted(model.3)
plot(x=df$predicted, y=df$salary, main='Salary vs. Predicted')
abline( lm(df$salary ~ df$predicted), col='blue')

plot(scale(resid(model.3)))
#fitting a lm to residuals
df$resid <- resid(model.3)
plot(df$predicted ~ df$resid)
abline(lm(df$predicted ~ df$resid))


#
set.seed(1)
df.sample15 <- df[sample(nrow(df), size=15),]
cor(df.sample15$salary, df.sample15$years)
cor(df$salary, df$years)

df.sample51 <- df[c(51:70),]
cor(df.sample51$salary, df.sample51$years)
model.51 <- lm ( formula = scale(df.sample51$salary) ~ scale(df.sample51$years))
summary(model.51)
model.52 <- lm ( formula = scale(df.sample51$salary) ~ scale(df.sample51$courses))
summary(model.52)
model.53 <- lm ( formula = scale(df.sample51$salary) ~ scale(df.sample51$years) + scale(df.sample51$courses))
summary(model.53)
pred <- predict(model.53)
pred
cor(pred, df.sample51$salary)

res <- resid(model.53)
cor(pred, res)
cor.test(pred, res)
