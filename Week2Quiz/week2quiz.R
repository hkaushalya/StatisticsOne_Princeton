#Week 2 Quiz on Memory Training

rm(list=ls())

setwd("/Users/samantha/Documents/Personal/Samantha/LearningMaterials/OnlineCourses_Coursera/StatisticsOne_Princeton/Week2Quiz")
list.files()

df<-read.table("Stats1.13.HW.02.txt",header=T)
print(df)
print("Original dimensions of the dataset:")
print(str(df))
#print(class(df))
#mean SR across all subjects
print("Mean SR across all subjects=")
print(round(mean(df$SR),2))
#variance SR across all subjects
print("Varince SR across all subjects=")
print(round(var(df$SR),2))

#subjects pretest conditions
df.pretest<-subset(df,time=="pre")
df.pretest.wm<-subset(df.pretest,condition=="WM")
df.pretest.pe<-subset(df.pretest,condition=="PE")
df.pretest.ds<-subset(df.pretest,condition=="DS")


print("Pretest Mean SR across all subjects=")
print(round(mean(df.pretest$SR),2))
#Pretest Variance SR across all subjects
print("Pretest Varince SR across all subjects=")
print(round(var(df.pretest$SR),2))


#subjects posttest conditions
df.posttest<-subset(df,time=="post")
print("Posttest Mean SR across all subjects=")
print(round(mean(df.posttest$SR),2))
#Posttest Variance SR across all subjects

var.posttest <- round(var(df.posttest$SR),2)
sd2.posttest <- round(sd(df.posttest$SR),2)
sd.posttest<-sqrt(var.posttest)
print("Posttest Varince SR across all subjects=")
print(var.posttest)
print("Posttest SD SR across all subjects=")
print(sd.posttest)
med.posttest<-round(median(df.posttest$SR),2)
print("Posttest Median across all subjects=")
print(med.posttest)

#Which group has the highest mean at posttest?
df.posttest.wm<-subset(df.posttest,condition=="WM")
df.posttest.pe<-subset(df.posttest,condition=="PE")
df.posttest.ds<-subset(df.posttest,condition=="DS")
print("Posttest Mean of condition==WM")
print(round(mean(df.posttest.wm$SR),2))
print("Posttest Mean of condition==PE")
print(round(mean(df.posttest.pe$SR),2))
print("Posttest Mean of condition==DS")
print(round(mean(df.posttest.ds$SR),2))



#Which one best approximates a normal distribution?
par(mfrow=c(2,3))
hist(df.posttest.ds$SR)
hist(df.posttest.pe$SR)
hist(df.posttest.wm$SR)
hist(df.pretest.ds$SR)
hist(df.pretest.pe$SR)
hist(df.pretest.wm$SR)
pretest.means<-c(mean(df.pretest.ds$SR),mean(df.pretest.pe$SR),mean(df.pretest.wm$SR))
posttest.means<-c(mean(df.posttest.ds$SR),mean(df.posttest.pe$SR),mean(df.posttest.wm$SR))
par(mfrow=c(1,2))
plot(pretest.means,col=3,ylim=c(10,15))
plot(posttest.means,col=5,ylim=c(10,15))
