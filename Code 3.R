# Question 5.1:

rm = (list = ls())

set.seed(123)

# Install Packages "outliers"

#install.packages("outliers")
library(outliers)
library(ggplot2)
# Upload US Crime data into R:

setwd("~/Desktop/Georgia Tech Classes/ISyE 6501/Week 3 - Basic Data Prep - Change Detection/Homework 3/Data")
data_uscrime = data.frame(read.csv("uscrime.csv", sep=""))
head(data_uscrime)
summary(data_uscrime)

# Data Visualization - Histogram:
summary(data_uscrime$Crime)
ggplot(data_uscrime, aes(x=Crime)) + geom_histogram(bins = 50, color="black", fill="white")+theme_bw()+geom_density()

# Data Visualization - Density:
ggplot(data_uscrime, aes(x=Crime)) + geom_density()

# Data Visualization - Box-and-whisker plot:
crime = data_uscrime$Crime
crime <- data.frame(x = rep(1, nrow(data_uscrime)), y = crime)
ggplot(crime, aes(x=x, y=y)) + 
  geom_boxplot(fill="gray")+
  labs(title="Plot of Crime",x="boxplot", y = "point")+
  theme_classic()+ scale_x_discrete(limits=c("1"))
# Based on the data visualization, we observe that data is normally distributed with  
# skew and 3 data points which are outliers. 

# Draw the qq-plot of the normally distributed
qqnorm(data_uscrime$Crime,main="QQ plot of normal data",pch=10)

# Add a line where x = y to help assess how closely the scatter fits the line.
qqline(data_uscrime$Crime)
# Based on the Q-Q plot, the data is normally distributed, and we can run the Grubbs' test. However, let's run the hypothesis test for a test of normality
shapiro.test(data_uscrime$Crime)
# We obtain a p-value of 0.001882. The test rejects the normal hypothesis of the data. 
# This outcome is expected as the data is influenced by outliers that would bias its distribution towards non-normality. 
# It is the goal of this question as we need to test to see whether there are any outliers in the data.

# Let's run the Grubbs' test for outliers on left and right tail.

grubbs.results.low = grubbs.test(data_uscrime$Crime, opposite = TRUE)
grubbs.results.low
# We obtain a p-value of 1, so the lowest-crime city does not seem to be an outlier.

grubbs.results.high = grubbs.test(data_uscrime$Crime)
grubbs.results.high
# We obtain a p-value of 0.07887, so the highest-crime city (1993) seems to be an outlier.
# Let's remove the highest-crime data and run again the grubbs test till we obtain a p-value that won't conclude that highest-crime city seems to be an outlier.
data_uscrime.2 = data_uscrime$Crime[!data_uscrime$Crime %in% 1993]
grubbs.results.high.2 = grubbs.test(data_uscrime.2)
grubbs.results.high.2
# We obtain a p-value of 0.02848, so the highest-crime city (1969) seems to be an outlier.
data_uscrime.3 = data_uscrime.2[!data_uscrime.2 %in% 1969]
grubbs.results.high.3 = grubbs.test(data_uscrime.3)
grubbs.results.high.3

# We obtain a p-value of 0.1781, so the highest-crime city (1674) seems to be an outlier.
data_uscrime.4 = data_uscrime.3[!data_uscrime.3 %in% 1674]
grubbs.results.high.4 = grubbs.test(data_uscrime.4)
grubbs.results.high.4

# We obtain a p-value of 0.1139, so the highest-crime city (1635) seems to be an outlier.
data_uscrime.5 = data_uscrime.4[!data_uscrime.4 %in% 1635]
grubbs.results.high.5 = grubbs.test(data_uscrime.5)
grubbs.results.high.5

# We obtain a p-value of 0.1082, so the highest-crime city (1555) seems to be an outlier.
data_uscrime.6 = data_uscrime.5[!data_uscrime.5 %in% 1555]
grubbs.results.high.6 = grubbs.test(data_uscrime.6)
grubbs.results.high.6

# We obtain a p-value of 1, so the highest-crime city (1272) is not an outlier.
# Based on the grubbs test, we conclude that data has 5 outliers which are 1993, 1969, 1674, 1635, 1555.

