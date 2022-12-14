library(pwr)
sqrt(1^2/4.5)
pwr.anova.test(k=4,n=NULL,f=0.471,sig.level = 0.05,power =0.80)

# Ans to the ques 1.(a).
# 14 samples of each fluid we will need to be collected 

sqrt(.5^2/4.5)
pwr.anova.test(k=4,n=NULL,f=0.2357,sig.level = 0.05,power =0.80)

# Ans to the ques 1.(b).
# 51 samples of each fluid we will need to be collected 


type1 <- c(17.6,	18.9,	16.3,	17.4,	20.1,	21.6)
type2	<- c(16.9,	15.3,	18.6,	17.1,	19.5,	20.3)
type3	<- c(21.4,	23.6,	19.4,	18.5,	20.5,	22.3)
type4 <- c(19.3,	21.1,	16.9,	17.5,	18.3,	19.8)

dat <- data.frame(type1,type2,type3,type4)
dat
library(tidyr)
library(dplyr)
dat <- pivot_longer(dat,c(type1,type2,type3,type4))
dat
colnames(dat) <- c("type","obs")
dat
s <- sd(dat$obs)
s
v <- var(dat$obs)
v
f <- sqrt(1^2/v)
f
#OR
f2 <- sqrt(1^2)/sd(dat$obs)
f2


pwr.anova.test(k=4,n=6,f,sig.level = .1,power =NULL)

pwr.anova.test(k=4,n=6,f2,sig.level = .1,power =NULL)

# Ans to the ques 2.(a).
# The power will be 0.5618141 for a hypothesis test with an alpha = 0.10 level 
# of significance 


model<-aov(dat$obs~dat$type, data = dat)
summary(model)

# Ans to the ques 2.(b).
# Since p value = 0.0525 is less than alpha = 0.1, so we reject null hypothesis.

plot(model)

# Ans to the ques 2.(c).
# From the plot, it seems like Normality assumption and the variance is constant is satisfied.
# The model is adequate.


TukeyHSD(model,conf.level = 0.9)
plot(TukeyHSD(model,conf.level = 0.9))

# Ans to the ques 2.(d).
# Type 2 and Type 3 fluid significantly differs 