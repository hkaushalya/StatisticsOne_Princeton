library(psych)

df <- read.table('stats1_datafiles_Stats1.13.HW.07.txt', header=T)
describe(df)
summary(df)
str(df)

#What is the correlation between extraversion and happiness?
cor(df$happy, df$extra)
#What is the correlation between extraversion and diversity of life experience?
cor(df$diverse, df$extra)

#What is the correlation between diversity of life experience and happiness?
cor(df$diverse, df$happy)



#What percentage of variance in happiness is explained by extraversion?
#Answer for Question 4

mod1 <- lm(happy ~ extra, data=df)
mod1
summary(mod1)
confint(mod1)

#What percentage of variance in happiness is explained by a model with both extraversion and diversity of life experience as predictors?
mod2 <- lm(happy ~ extra + diverse, data=df)
mod2
summary(mod2)
confint(mod2)

library(multilevel)
sobel(df$extra,df$diverse,df$happy)
