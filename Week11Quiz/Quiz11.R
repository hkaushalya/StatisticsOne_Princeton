library(psych)
library(lsr)
data <- read.table('stats1_datafiles_Stats1.13.HW.11.txt', header=T)
head(data)
describe(data)
describeBy(data,data$cond)



# Question 1
# Using a t-test, compare verbal scores before and after 
# training in the fixed condition. Is the difference
# pre-test to post-test significant?
data.fixedcond <- subset(data, cond=='fixed')
data.mallecond <- subset(data, cond=='malleable')

t.test(data.fixedcond$verbal.post, data.fixedcond$verbal.pre, paired=T)

# Question 2
# What are the degrees of freedom for the comparison 
# between pre-test and post-test for the spatial scores?
t.test(data.fixedcond$spatial.post, data.fixedcond$spatial.pre, paired=T)
cohensD(data.fixedcond$spatial.post, data.fixedcond$spatial.pre, method='paired')

# Question 3
# Run a Wilcoxon test for the same comparison (pre-test to 
# post-test on spatial scores, fixed condition). Which of 
# the two tests gives the highest p-value for the comparison?
wilcox.test(data.fixedcond$spatial.post, data.fixedcond$spatial.pre, paired=T)


# Question 4
# What is the effect size (Cohen's d) for the difference 
# between pre-test and post-test spatial scores for the 
# malleable condition? (round to two decimal places)
t.test(data.mallecond$spatial.post, data.mallecond$spatial.pre, paired=T)
wilcox.test(data.mallecond$spatial.post, data.mallecond$spatial.pre, paired=T)

cohensD(data.mallecond$spatial.post, data.mallecond$spatial.pre, method='paired')


# Question 5
# Which of the three tasks shows the largest improvements 
# from pre-test to post-test, in the fixed condition?
t.test(data.fixedcond$spatial.post, data.fixedcond$spatial.pre, paired=T)
t.test(data.fixedcond$verbal.post, data.fixedcond$verbal.pre, paired=T)
t.test(data.fixedcond$intel.post, data.fixedcond$intel.pre, paired=T)
cohensD(data.fixedcond$spatial.post, data.fixedcond$spatial.pre, method='paired')
cohensD(data.fixedcond$verbal.post, data.fixedcond$verbal.pre, method='paired')
cohensD(data.fixedcond$intel.post, data.fixedcond$intel.pre, method='paired')

# Question 6
# Which of the three tasks shows the largest improvements 
# from pre-test to post-test, in the malleable condition?
t.test(data.mallecond$spatial.post, data.mallecond$spatial.pre, paired=T)
t.test(data.mallecond$verbal.post, data.mallecond$verbal.pre, paired=T)
t.test(data.mallecond$intel.post, data.mallecond$intel.pre, paired=T)
cohensD(data.mallecond$spatial.post, data.mallecond$spatial.pre, method='paired')
cohensD(data.mallecond$verbal.post, data.mallecond$verbal.pre, method='paired')
cohensD(data.mallecond$intel.post, data.mallecond$intel.pre, method='paired')

# Question 7
# Conduct Mann-Whitney comparisons between all tasks at 
# pre-test. Which task(s) differ significantly from the other 
# two in pre-test scores?
wilcox.test(data$verbal.pre, data$spatial.pre)
wilcox.test(data$verbal.pre, data$intel.pre)
wilcox.test(data$spatial.pre, data$intel.pre)
