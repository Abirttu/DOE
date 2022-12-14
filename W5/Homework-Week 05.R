# HW-Week 05
# Problem: 3.7
library(agricolae)
Mix_Tech <- c("1","1","1","1","2","2","2","2","3","3","3","3","4","4","4","4")
Ten_Str <- c(3129,3000,2865,2890,3200,3300,2975,3150,2800,2900,2985,3050,2600,2700,2600,2765)
dat <- data.frame(Mix_Tech,Ten_Str)
dat$Ten_Str <- as.numeric(dat$Ten_Str)
dat$Mix_Tech <- as.factor(Mix_Tech)
str(dat)
model <- aov(Ten_Str~Mix_Tech,data=dat)
summary(model)
LSD.test(model,"Mix_Tech",console = TRUE)
# Answer to the problem: 3.7(c)
# Hypothesis: 
# Null Hypothesis (Ho): u1 = u2, u1 = u3, u1 = u4, u2 = u3, u2 = u4, u3 = u4.
# Alternative Hypothesis (Ha): at least one of the mean (ui) differs.
# µ1 and µ3 are similar ,
# µ2 differs from µ1,µ3 and µ4
# µ4 differs from µ1,µ2 and µ3
plot(model)
#Answer to The Problem: 3.7(d)
# The normal probability plot of the residuals show that there is nothing unusual
# in the normality assumption.
#Answer to The Problem: 3.7(e)
# The residuals vs. the predicted tensile strength plot looks almost 
# rectangular, which indicates the constant variance. 
# In the analysis of variance we also see that the plot's minimum and maximum points 
# of all treatments are in a straight line.
#Answer to The Problem: 3.7(f)
library(car)
scatterplot(Ten_Str ~ Mix_Tech, data = dat, smoother = FALSE, grid = FALSE, frame = FALSE)
# The plot also shows the sample average for each treatment and the 
# 95% confidence interval on the treatment mean.
# Problem: 3.10
library(agricolae)
Cotton_Wt_Percent <- c("15","15","15","15","15","20","20","20","20","20","25","25","25","25","25","30","30","30","30","30","35","35","35","35","35")
Obs <- c(7,7,15,11,9,12,17,12,18,18,14,19,19,18,18,19,25,22,19,23,7,10,11,15,11)
dat1 <- data.frame(Cotton_Wt_Percent,Obs)
dat1
dat1$Cotton_Wt_Percent <- as.factor(dat1$Cotton_Wt_Percent)
dat1$Obs <- as.numeric(dat1$Obs)
str(dat1)
model2 <- aov(Obs~Cotton_Wt_Percent,data=dat1)
summary(model2)
LSD.test(model2,"Cotton_Wt_Percent",console = TRUE)
#Answer to The Problem: 3.10(b)
# Hypothesis: 
# Null Hypothesis (Ho): u1 = u2, u1 = u3, u1 = u4, u1 = u5, u2 = u3, u2 = u4, u2 = u5, u3 = u4, u3 = u5, u4 = u5.
# Alternative Hypothesis (Ha): at least one of the (ui) differs.
# From the above fishers test we see that mean of 30% is different than 25%,20%,35% 
# and 15%. mean of 25% is similar to mean 20% but different than 30%,35% and 15%
# mean of 20% is similar to mean 25% but different than 30%,35% and 15%
# mean of 35% is similar to mean 15% but different than 20%,25% and 30%
# mean of 15% is similar to mean 35% but different than 20%,25% and 30%
plot(model2)
#Answer to The Problem: 3.10(c)
# The normal probability plot of the residuals show that there is nothing unusual
# in the normality assumption.
# Also from residual to fitted values plot we see that points fairly lie in 
# rectangular shape, which shows the assumption of constant variance.
# Hence the model is adequate.
# Problem: 3.44
library(pwr)
d <- (60-50)/5 #variance is 25, sd=5
d
f <- c(d/2)
f
pwr.anova.test(k=4,n=NULL,f,sig.level = 0.05 , power=0.90)

#Answer to The Problem: 3.44
# Hence we need 5 observations from each population.
# Problem: 3.45
library(pwr)
d <- (60-50)/6  #variance is 36, sd=6
d
f <- c(d/2)
f
pwr.anova.test(k=4,n=NULL,f,sig.level = 0.05 , power=0.90)

#Answer to The Problem: 3.45
# Hence we need 7 observations from each population.

# Problem: 3.46
library(pwr)
d <- (60-50)/7  #variance is 49, sd=7
d
f <- c(d/2)
f
pwr.anova.test(k=4,n=NULL,f=0.71,sig.level = 0.05 , power=0.90)
# Hence we need 9 observations from each population.

#Answer to The Problem: 3.45(a)
# It did increase the sample number in fraction wise compared to the previous 
# problem, but since it is 2.518782 and  after rounded up to the next integer value
# we need 3 observations from each population.
pwr.anova.test(k=4,n=NULL,f=sqrt(((10)^2)/49) ,sig.level = 0.05 , power=0.90)
#Answer to The Problem: 3.45(b)
# It did increase the sample number in fraction wise compared to the previous 
# problem, but since it is 2.939789 and  after rounded up to the next integer value
# we need 3 observations from each population.
#Answer to The Problem: 3.45(c)
# As the estimate of variability increases the sample size also increases
# to ensure the same power of the test
#Answer to The Problem: 3.45(d)
# When there is no prior estimate of variability, sometimes we will generate 
# sample sizes for a range of possible variances to see what effect this has 
# on the size of the experiment. Or to bound the variability in the response, 
# such as “the standard deviation is going to be at least…” or 
# “the standard deviation shouldn’t be larger than…”.

f <- c(0.71)
pwr.anova.test(k=4,n=NULL,f,sig.level = 0.05 , power=0.90)
