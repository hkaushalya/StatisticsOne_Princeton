#lawyer/teacher/doctor salary comparison

library(psych)

df <- read.table('stats1-datafiles-Stats1.13.HW.06.txt' ,header =T)
str(df)
cor(df[2:4])

model1 <- lm(salary ~ years, data = df)
summary(model1)
confint(model1)

model2 <- lm(salary ~ years + courses, data = df)
summary(model2)
confint(model2)

#dummy code
prof.code <- C(df$profession, treatment)
prof.code

model3 <- lm(salary ~ years + courses + prof.code , data = df)
summary(model3)
confint(model3)

anova(model1, model2)
anova(model1, model2)
anova(model3, model2)


model.x <- lm(salary ~ prof.code, data=df)
summary(model.x)
anova(model.law, model.doc)

tapply(df$salary, df$profession, mean)




