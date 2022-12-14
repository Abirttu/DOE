library(GAD)
dop <- c(rep(1,6),rep(2,6))
temp <- c(rep(seq(900,1000,50),4))
obs <- c(4.60,10.15,11.01,4.40,10.20,10.58,3.20,9.38,10.81,3.50,10.02,10.60)
dop <- as.fixed(dop)
temp <- as.fixed(temp)
dat <- data.frame(dop,temp,obs)
dat
model <- aov(obs~dop*temp,data=dat)
gad(model)
interaction.plot(temp,dop,obs)

#critical value:
qf(0.9,2,6)

