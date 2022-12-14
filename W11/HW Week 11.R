#6.12
A <- c(rep(-1,4),rep(1,4))
B <- c(rep(-1,8),rep(1,8))
obs <- c(14.037,16.165,13.972,13.907,13.880,13.860,14.032,13.914,14.821,14.757,14.843,14.878,14.888,14.921,14.415,14.932)
A <- as.factor(A)
B <- as.factor(B)
dat <- data.frame(A,B,obs)
dat
# Finding Corner Points:
one <- sum(dat$obs[1:4])
one
a <- sum(dat$obs[5:8])
a
b <- sum(dat$obs[9:12])
b
ab <- sum(dat$obs[13:16])
ab
# Estimate the Factor Effects
n <- c(4)
fact_A <- 2*(a+ab-b-one)/(4*n)
fact_A
fact_B <- 2*(b+ab-a-one)/(4*n)
fact_B
fact_AB <- 2*(ab+one-a-b)/(4*n)
fact_AB
# Analysis of variance
A <- c(rep(-1,4),rep(1,4))
B <- c(rep(-1,8),rep(1,8))
obs <- c(14.037,16.165,13.972,13.907,13.880,13.860,14.032,13.914,14.821,14.757,14.843,14.878,14.888,14.921,14.415,14.932)
A <- as.factor(A)
B <- as.factor(B)
dat <- data.frame(A,B,obs)
dat
model <- aov(obs~A*B,data=dat)
summary(model)
# p values of the main effect A and the interaction effect (A:B) is greater than alpha = 0.05 level of significance.
# Hence the main effect A and the interaction effect (A:B) are not significant as we fail to reject H_0.
# On the other hand p value of the main effect B is less than alpha = 0.1 level of significance. Hence the main effect B
# is significant as we reject H_0.
# Regression equation:
# 



#book example
A <- c(rep(-1,3),rep(1,3))
B <- c(rep(-1,6),rep(1,6))
obs <- c(28,25,27,36,32,32,18,19,23,31,30,29)
A <- as.factor(A)
B <- as.factor(B)
dat <- data.frame(A,B,obs)
dat
model <- aov(obs~A*B,data=dat)
summary(model)
plot(model)

A <- c(rep(-1,3),rep(1,3))
B <- c(rep(-1,6),rep(1,6))
obs <- c(28,25,27,36,32,32,18,19,23,31,30,29)
A <- as.factor(A)
B <- as.factor(B)
dat <- data.frame(A,B,obs)
dat
model1 <- lm(obs~A*B,data=dat)
coef(model1)
summary(model1)
plot(model1)



#6.8
time<-c(rep(-1, 12),rep(1, 12) )
cul_med<-c(rep(-1, 2), rep(1, 2))
obs <- c(21,22,25,26,23,28,24,25,20,26,29,27,37,39,31,34,38,38,29,33,35,36,30,35)
time <- as.factor(time)
cul_med <- as.factor(cul_med)
dat <- data.frame(time,cul_med,obs)
dat
model <- aov(obs~time*cul_med,data=dat)
summary(model) 
plot(model)






#6.21
