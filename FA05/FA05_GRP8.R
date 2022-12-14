#Question1
#Installing Library
install.packages("pwr")
library(pwr)

#Using Power Package to find n
#For d we have (mean1-mean2)/SD and we have given sd=0.03.
pwr.t.test(n= NULL, d= 0.5, sig.level = 0.05, power = 0.75, type = c("paired"), alternative = c("greater"))
#Answer We will need 23 paired samples.

#Question2
pwr.t.test(n= NULL, d= 0.5, sig.level = 0.1, power = 0.85, type = c("two.sample"), alternative = c("greater"))
#Answer In each group we will need 44 infants and in total 88.