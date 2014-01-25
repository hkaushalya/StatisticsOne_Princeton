#week 9 quiz

# This week, we turn to a classic study on helping behavior by Darley
# and Batson (1973). Simulated data are provided here. The study 
# demonstrates that people’s likelihood of helping a person in distress 
# depends largely on their level of haste—whether they were running early, 
# on time, or late for an appointment—when they encounter him, rather than 
# on whether they have been asked to reflect on a pro-helping message (the
# parable of the Good Samaritan) as opposed to a neutral message (occupational
# effectiveness). In this dataset, independent variables include
# Prime (1 = parable of the Good Samaritan; 2 = occupational effectiveness)
# and Haste (1 = early, 2 = on time, 3 = late). On their way to a nearby 
# location, participants encounter a moaning individual in distress. 
# The Helping variable provides a measure of how much they help, ranging 
# from 0 to 6 with higher scores indicating greater helping.

rm(list=ls())
library(psych)
df <- read.table('Stats1.13.HW.09.txt', header=T)
str(df)
summary(df)
head(df)

# Question 1
# What is the class of Haste and Prime in R?
class(df$Prime)
class(df$Haste)

# Question 2
# After converting Haste and Prime to factors, run an ANOVA with both Haste
# and Prime as independent variables. Is the effect of Haste significant?
df$Prime <- factor(df$Prime)
df$Haste <- factor(df$Haste)

# to check if the variances of difference groups are not significantly
# different and can be pooled.
# Homogeneity of Variance should hold
leveneTest(df$Helping~df$Prime * df$Haste)

mod1.aov <- aov(df$Helping~df$Prime + df$Haste)   #Prime and Haste as ID vars
mod2.aov <- aov(df$Helping~df$Prime * df$Haste)   #Including combination effect
summary(mod1.aov)
summary(mod2.aov)
confint(mod1.aov)
confint(mod2.aov)


# Question 3
# Is the effect of Prime significant?
#yes

# Question 4
# Is the interaction significant?
#yes

# Question 5
# Save the ANOVA summary in a table and run Tukey's pairwise comparison on all
# group means. Do each level of Haste significantly differ from one another?
library(car)
TukeyHSD(mod1.aov)
TukeyHSD(mod2.aov)

# Question 6
# What is the partial eta-squared value for the effect of Haste? (round to 2
# decimal places).
library(lsr)
etaSquared(mod1.aov, anova=T)
etaSquared(mod2.aov, anova=T)

# Question 7
# What is the partial eta-squared value for the interaction? (round to 2 decimal
# places).

# Question 8
# Let's now run simple effects of Prime at each level of Haste. At which level
# of Haste is the effect of Prime significant?

AB1 <- subset(df, Haste==1)
AB2 <- subset(df, Haste==2)
AB3 <- subset(df, Haste==3)

#effect size
etaSquared(mod1.aov, anova=T)
etaSquared(mod2.aov, anova=T)


# Question 9
# What is the partial eta-squared value for the effect of Prime when people
# were early? (round to 2 decimal places).

TukeyHSD()


# Question 10
# Which one of the following statements best illustrates the main finding
# of the study?