library(car)
library(tidyr)
library(dplyr)
library(readxl)
#Question 3.2 (part 02)
getwd()
library(readxl)
dat <- read_excel(file.choose("/Users/abir/Desktop/FA/zoombies.xlsx"))
dat
Basic <- dat$Basic
Conehead <- dat$Conehead
Buckethead <- dat$Buckethead
library(tidyr)
library(dplyr)
dat <- pivot_longer(dat,c(Basic,Conehead,Buckethead))
dat
dat <- dat[,-c(1)]
dat
colnames(dat) <- c("Zombies","Obs")
dat
dat$Zombies <- as.factor(dat$Zombies)
dat$Obs <- as.numeric(dat$Obs)
model <- aov(Obs~Zombies,data=dat)
summary(model)
# Hypothesis: H0: mu_1 = mu_2 = mu_3 = mu
# Ha: at least one of the mu_i differs. (i=1,2,3)
# Conclusions: p value (1.79e-14) < alpha = 0.05 level of significance Thus the null
# hypothesis H0 is rejected so at least one of the mean differs.
#Question 3.3 (part 03)
plot(model)
# Residual vs. fitted plot shows that the variance is constant.
# Normal Q-Q plot indicates that the data's are approximately normally distributed.
# since ANOVA is not sensitive from the deviation of the normality but to the 
# constant variance.
#Question 3.4 (part 04)
library(car)
TukeyHSD(model)
plot(TukeyHSD(model))
# From the 95% family-wise confidence level, we see that zero is not within the 
# confidence interval so we reject the null hypothesis H0 which indicates that
# the three pair of means are significantly different. Also from the Tukey test
# we see that p values of each individual pair's are less than the alpha = 0.05
# level of significance which means that the H0 is rejected and so the three
# pair of means are significantly different.
#Question 08
dat <- read.csv(file.choose("/Users/abir/Desktop/FA/Test5342.csv"))
dat
Levels <- dat$levels
Levels <- as.factor(Levels)
Response <- dat$response
Response <- as.numeric(Response)
dat <- data.frame(Levels,Response)
dat
str(dat)
model <- aov(Response~Levels,data=dat)
summary(model)
# Hypothesis: H0: mu_1 = mu_2 = mu_3 = mu
# Ha: at least one of the mu_i differs. (i=1,2,3)
# Since p value (0.000544) < alpha (0.1) level of significance so Null hypothesis
# is rejected. Hence, at least one of the mean differs.
#Question 10
dat1 <- read.csv(file.choose("/Users/abir/Desktop/FA/c/US_Japanese_Cars.csv"))
dat1
t.test(dat1$USCars,dat1$JapaneseCars,alternative=c("greater"),var.equal = TRUE)
# Hypothesis: H0: mean mpg of US cars (mu_1) = mean mpg of Japanese Cars (mu_2)
# Ha: at least one of the mean mpg (mu_i) differs. (i=1,2)
# Hence p value (1) is larger than alpha = 0.05 level of significance so the 
# null hypothesis is not rejected thus the Japanese cars are more efficient than the
# US cars