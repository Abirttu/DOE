m1 <- c(.34,.12,1.23,.70,1.75,.12)
m2 <- c(.91,2.94,2.14,2.36,2.86,4.55)
m3 <- c(6.31,8.37,9.75,6.09,9.82,7.24)
m4 <- c(17.15,11.82,10.97,17.20,14.35,16.82)
dat <- data.frame(m1,m2,m3,m4)
dat
library(tidyr)
library(dplyr)
dat1 <- pivot_longer(dat,c(m1,m2,m3,m4))
dat1
colnames(dat1)<-c("Method","Observations")
dat1
dat1$Method <- as.factor(dat1$Method)
dat1$Observations <- as.numeric(dat1$Observations)
str(dat1)
# Answer to the ques (A)
# Linear effects equation Yij = y_bar.. + Ti + Eij
# where Yij = jth observations from the ith population (Estimation Method)
# y_bar..  = grand mean
# Ti = Fixed Effect of estimation method
# Eij = random error ~ N(0,sigma^2)
# Hypothesis 
# H0 = u1=u2=u3=u4
# Ha = at leat one of the u(i) differs
# u(i) = mean of the estimation method (i=1,2,3,4)
obs <- c(dat1$Observations)
str(obs)
x <- c(rep(1,6),rep(2,6),rep(3,6),rep(4,6))
boxplot(obs~x,xlab="estimation method",ylab="observations",main="Boxplot of Observations")
meanx<-c(rep(mean(m1),6),rep(mean(m2),6),rep(mean(m3),6),rep(mean(m4),6))
methods<-c(m1,m2,m3,m4)
res<-methods-meanx
qqnorm(res)
qqline(res)
plot(meanx,res,xlab="Method average",ylab="residual",
     main="constant variance check")
# Answer to the ques (B)
# From the qq plot, we see that the data appears to be approximately 
# normally distributed
# From the constant variance check plot, we see that the residuals have 
# the different spread/disperse. Hence the variance is not constant.
# Answer to the ques (C)
kruskal.test(Observations~Method,data=dat1)
# P value is highly significant at alpha = 0.05 so we reject H0 that is at 
# least one of the mean estimation method differs
library(MASS)
m1 <- c(.34,.12,1.23,.70,1.75,.12)
m2 <- c(.91,2.94,2.14,2.36,2.86,4.55)
m3 <- c(6.31,8.37,9.75,6.09,9.82,7.24)
m4 <- c(17.15,11.82,10.97,17.20,14.35,16.82)
obs <- c(m1,m2,m3,m4)
x <- c(rep(1,6),rep(2,6),rep(3,6),rep(4,6))
boxplot(obs~x,xlab="Method",ylab="observation",main="Boxplot of Observations")
boxcox(obs~x)
# lambda=1 is not within 95% CI so we can go ahead to do boxcox transformations
obs1 <- obs^(0.5)
boxplot(obs1~x,xlab="method",ylab="observation",main="Boxplot of Observations")
anova_mod <- aov(obs1~x)
summary(anova_mod)
## Since the p value is highly significant at alpha=0.05, we reject H0
# atleast one of the mean estimation method differs