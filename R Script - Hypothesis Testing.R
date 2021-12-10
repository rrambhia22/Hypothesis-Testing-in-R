#MODULE 4 - R PRACTICE



#installing packages
install.packages("MASS")
install.packages("ggpubr")


#importing libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(scales)
library(psych)
library(MASS)
library(ggpubr)



#PART 1


#reading the cats data set into a variable
cats_dataset <- cats
cats_dataset


#descriptive analysis
colnames(cats_dataset)

mean(cats_dataset$Bwt)
sd(cats_dataset$Bwt)

mean(cats_dataset$Hwt)
sd(cats_dataset$Hwt)

summary(cats_dataset)



#creating subset of the data set
male_sample <- subset(cats, subset=(cats$Sex=="M"))
male_sample

female_sample <- subset(cats, subset=(cats$Sex=="F"))
female_sample



#creating new variables
bodyweight_male <- c(male_sample$Bwt)
bodyweight_male

bodyweight_female <- c(female_sample$Bwt)
bodyweight_female


height_male <- c(male_sample$Hwt)
height_male

height_female <- c(female_sample$Hwt)
height_female


#descriptive analysis
mean(bodyweight_male)
mean(bodyweight_female)
summary(bodyweight_male)
summary(bodyweight_female)


#data visualization

par(mfrow=c(2,2))
boxplot(bodyweight_male, col = "lightblue", main = "Bodyweight of Male Sample")
boxplot(height_male, col = "cadetblue", main = "Height of Male Sample")
boxplot(bodyweight_female, col = "cadetblue", main = "Bodyweight of Female Sample")
boxplot(height_female, col = "lightblue", main = "Height of Female Sample")



#testing
t.test(bodyweight_male,bodyweight_female,var.equal = FALSE)   #unequal variance


#output
#t-test statistic: 8.7095
#degrees of freedom: 136.84
#p-value: 8.831e-15
#95% confidence interval : [0.4177242 0.6631268]
#mean of x : 2.900000
#mean of y : 2.359574
#Since the p-value of the test is less than 0.05, we reject the null hypothesis
#these results provide the means of both samples and so it is seen that the bodyweight of sample A(male) is higher/different than bodyweight of sample B(female)
#also there is no difference between the means 
#Answer : No, male and female cat samples do not have the same bodyweight. The bodyweight of sample A which is the male bodyweight sample is higher/different than the bodyweight of sample B which is the female cat bodyweight with a p-value of 8.831e-15


#visualization for testing

#graph1
qqnorm(bodyweight_male, main = "Normal Q-Q Plot for Male Cat Bodyweight",col = "blue")
qqline(bodyweight_male, lty=2)

qqnorm(bodyweight_female, main = "Normal Q-Q Plot for Female Cat Bodyweight", col = "green")
qqline(bodyweight_female, lty=2)


#graph2
ggboxplot(
  cats_dataset, x = "Sex", y = "Bwt", 
  ylab = "Bodyweight", xlab = "Groups", add = "jitter", col = "blue", main = "Bodyweight for Male and Female Cat groups"
) +
  theme(plot.title = element_text(hjust = 0.5))



#---------------------------------------------------------------------------------------------------------------#



#PART 2

#creating new variables for testing
sleeping_quality_before_workshop <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
sleeping_quality_before_workshop

sleeping_quality_during_workshop <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)
sleeping_quality_during_workshop 


dataset <- data.frame(Before = sleeping_quality_before_workshop, During = sleeping_quality_during_workshop)
dataset


#descriptive analysis
mean(sleeping_quality_before_workshop)
mean(sleeping_quality_during_workshop)

summary(sleeping_quality_before_workshop)
summary(sleeping_quality_during_workshop)


#data visualizations
boxplot(sleeping_quality_before_workshop, main = "Boxplot for sleeping quality before workshop", col = "lightblue")
boxplot(sleeping_quality_during_workshop, main = "Boxplot for sleeping quality during workshop", col = "pink")

barplot(sleeping_quality_before_workshop, horiz = TRUE, col = "blue", main = "Sleeping Quality before workshop", xlab = "Scale of Sleeping Quality")
barplot(sleeping_quality_during_workshop, horiz = TRUE, col = "cadetblue", main = "Sleeping Quality during workshop", xlab = "Scale of Sleeping Quality")



#two-sample t-test
t.test(sleeping_quality_before_workshop,sleeping_quality_during_workshop) #reject the alternative hypothesis

#output
#t-test statistic: -0.84663
#degrees of freedom: 15.641
#p-value: 0.41
#95% confidence interval : [-2.1753439  0.9353439]
#mean of x : 6.25
#mean of y : 6.87
#Since the p-value of the test is greater than 0.05, we reject the alternative hypothesis
#Answer : Yes, it is true that meditation improves sleeping quality



#Now, since the p-value of the test is greater than 0.05, we fail to reject the null hypothesis that our data is normally distributed. And so we can proceed with the paired t-test



#paired test for two-sample t-test
t.test(sleeping_quality_before_workshop,sleeping_quality_during_workshop,paired = TRUE)


#output
#we reject the alternative hypothesis as p-value is greater than 0.05
#conclude that, the sleeping quality before meditation is different than that from the sleeping quality after meditation
#Answer : It should be a paired test, because:
#If two samples are given, then the observation of one sample can be paired with the observation of the other sample. 
#This test can be used in making observations on the same sample before and after an event.



#visualization for testing
qqnorm(sleeping_quality_before_workshop, main = "Normal Q-Q Plot for sleeping quality before workshop")
qqline(sleeping_quality_before_workshop, lty=2)

qqnorm(sleeping_quality_during_workshop, main = "Normal Q-Q Plot for sleeping quality during workshop")
qqline(sleeping_quality_during_workshop, lty=2)



#level of significance changes from 0.05 to 0.1

#1. two-sample t-test
t.test(sleeping_quality_before_workshop,sleeping_quality_during_workshop) 

#output
#Since the p-value of the test is greater than 0.1, we reject the alternative hypothesis
#Hence, the conclusion does not change if the level of significance changes from 0.05 to 0.1



#2. paired t-test
t.test(sleeping_quality_before_workshop,sleeping_quality_during_workshop,paired = TRUE)

#output
#Since the p-value of the test is less than 0.1, we reject the null hypothesis
#Hence, the conclusion here changes when the level of significance changes from 0.05 to 0.1





