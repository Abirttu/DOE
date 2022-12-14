dat <- read.csv("https://raw.githubusercontent.com/tmatis12/datafiles/main/normtemp.csv")
dat1 <- dat[,-c(1)]
print(dat1)
# For Males data descriptive Statistics
dat_males <- dat1[c(1:65),2]
print(dat_males)
summary(dat_males)
var(dat_males)
sd(dat_males)
IQR(dat_males)
# Comments on Descriptive Statistics: 
# For males mean is 73.37 median is 73, max is 86, min is 58,standard deviation is 5.875184, Variance 
# is 34.51779, 1st quartile 70, 3rd quartile 78 and Inter quartile Range 8.

# Males Histogram Plot
hist(dat_males,main="Histogram of the Males Heartbeat",col="blue", xlab = "Resting Heart rate")
plot(density(dat_males),main="smoothed histogram of males")
# Comments on the Males histogram Plot
# The above Histogram looks similar to Normal Distribution, with cluster 
# at center and with thin tails on both ends.
# The heart rate data spread from 55 to 90. where the range is 35. Most of the 
# Males resting heart rate falls in a particular Bin 70-75

# Males Normal Probability Plot
qqnorm(dat_males,main="Males Normal Probability Plot",col="blue")
# Comments on the Males Normal Probability Plot
# The Probability plot looks fairly straight diagonal line, when the outliers are 
# ignored, hence we can say that its a normally distributed data. 
# As the plot is normally distributed, so the skewness is about Zero.

# For Females data descriptive Statistics
dat_females <- dat1[c(66:130),2]
print(dat_females)
summary(dat_females)
var(dat_females)
sd(dat_females)
IQR(dat_females)
# Comments on Descriptive Statistics: 
# For females mean is 74.15 median is 76, max is 89, min is 57,standard deviation is 8.105227, Variance 
# is 65.69471, 1st quartile 68, 3rd quartile 80 and Inter quartile Range 12.

# Females Histogram Plot
hist(dat_females,main="Histogram of the Females Heartbeat",col="pink", xlab = "Resting Heart rate")
plot(density(dat_females),main="smoothed histogram of female")

# Comments on Females Histogram Plot
# The above Histogram is Left Skewed, with cluster of data towards the right side.
# Most of the Females resting heart rate falls in a particular Bin 75-80
# Females Normal Probability Plot
qqnorm(dat_females,main="Females Normal Probability Plot",col="pink")
# Comments on the Females Normal Probability Plot
# The Probability plot looks left skewed. Hence not normally distributed.

# Plot Side by side Box plot 
c <- boxplot(dat_males,dat_females,main ="Resting Heart rates for Males and Females",names = c("Males", "Females"),ylab="Resting Heart Rate")
# Comments on the Box plot
# Compare median-Median heart rate of females (76) which is the center of female data set is more than that of males(73) which is the center of male data set 
# Compare IQR- IQR for Female is higher(12) than Male(8)
# Compare Maximum-Maximum heart rate for male((87)) is less than female(89)
# Compare minimum-Minimum heart rate for male((58)) is more than female(57)
#The maximum and minimum values of both datas are about similar hence the range of data spread is similar.
# Dispersion of Data: From the box plot we can observe that the variance of datas of females is more than that of male.
# Skewness: Also from the box plot we can observe that the upper quartile for male is more than the lower quartile,
#Hence the datas are right skewed
#For females the lower quartile is more than the upper quartile, hence the datas are left skewed. 
# Potential Outliers:Since there are not many data points so there are no outliers in the box plot 
#Overall Conclusion: From the recorded data set the female heartrate is more dispersed than that of male and it seems that female heartrate is somewhat more than that of male because the median is more for female.

