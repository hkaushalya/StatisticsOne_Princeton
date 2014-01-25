# We now return to our cognitive training example. 
# In this week's dataset, we compare the impact of three training 
# conditions (Working Memory training, Physical Exercise, and 
# Designed Sport) on Spatial Reasoning (SR), measured before (pre) 
# and after (post) training.

library(psych)
df <- read.table('Stats1.13.HW.02.txt',header=T)
df
str(df)

#1:Using a dependent t-test, is the difference
# between pre and post-test scores significant?
preSR <- subset(df, time=="pre")
postSR <- subset(df, time=="post")
describeBy(df,group=df$time)

t.test(preSR$SR,postSR$SR, paired=T)
library(lsr)
cohensD(preSR$SR,postSR$SR, method='paired')


#2: Create subsets for each training condition. Which group
#   shows no difference between pre and post-test scores?
levels(df$condition)
DS.preSR <-subset(preSR, preSR$condition=='DS')
PE.preSR <-subset(preSR, preSR$condition=='PE')
WM.preSR <-subset(preSR, preSR$condition=='WM')

DS.postSR <-subset(postSR, postSR$condition=='DS')
PE.postSR <-subset(postSR, postSR$condition=='PE')
WM.postSR <-subset(postSR, postSR$condition=='WM')

t.test(DS.preSR$SR, DS.postSR$SR, paired=T)
t.test(PE.preSR$SR, PE.postSR$SR, paired=T)
t.test(WM.preSR$SR, WM.postSR$SR, paired=T)

#Question 3
# Which training group shows the largest effect size for the 
# difference pre-test to post-test?
cohensD(DS.preSR$SR,DS.postSR$SR, method='paired')
cohensD(PE.preSR$SR,PE.postSR$SR, method='paired')
cohensD(WM.preSR$SR,WM.postSR$SR, method='paired')

#Question 4
# Reshape the data into a wide format, and create a new variable
# for gain score. Now subset the new dataframe based on the 
# training conditions. Which comparison between training conditions 
# does not show a significant difference?
libary(reshape)
df.wide <- cast(df, subject+condition ~ time, value="SR")
df.wide[,"gain"] <- df.wide$post - df.wide$pre
df.wide

#subset the data
df.wide.WM <- subset(df.wide, condition=="WM")
df.wide.PE <- subset(df.wide, condition=="PE")
df.wide.DS <- subset(df.wide, condition=="DS")
t.test(df.wide.DS$gain, df.wide.WM$gain, var.equla=T)
t.test(df.wide.PE$gain, df.wide.DS$gain, var.equla=T)
t.test(df.wide.WM$gain, df.wide.PE$gain, var.equla=T)

#Question 5
#To compare the gain scores across all groups, we now turn to ANOVA.
#Is the homogeneity of variance assumption violated?
library(psych)
describeBy(df.wide$gain,df.wide$condition)
library(car)
leveneTest(df.wide$gain~df.wide$condition)

aov1<-aov(df.wide$gain~df.wide$condition)
summary(aov1)

# Question 7
# What is the corresponding eta-squared value? (round to 2 decimal places)
etaSquared(aov1, anova=T)


# Question 9
# Let's now run post-hoc comparisons (Tukey HSD). 
# Which two groups do not significantly differ from 
# one another when considering gain scores?

TukeyHSD(aov1)


