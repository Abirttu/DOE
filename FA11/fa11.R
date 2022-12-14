#Ques 01
install.packages(GAD)
library(GAD)
chemical<-c(rep(1,5),rep(2,5),rep(3,5),rep(4,5))
chemical
bolt<-c(seq(1,5),seq(1,5),seq(1,5),seq(1,5))
bolt
obs <- c(73,68,74,71,67,73,67,75,72,70,75,68,78,73,68,73,71,75,75,69)
obs
chemical<-as.fixed(chemical)
bolt<-as.fixed(bolt)
obs <- as.numeric(obs)
dat <- data.frame(chemical,bolt,obs)
dat
model<-lm(obs~chemical+bolt,data=dat) 
gad(model)

#Ques 02
