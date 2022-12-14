RoddingLevel <- c("10","10","10","15","15","15","20","20","20","25","25","25")
CompressStrength <- c(1530,1530,1440,1610,1650,1500,1560,1730,1530,1500,1490,1510)
str(RoddingLevel)
RoddingLevel <-as.factor(RoddingLevel)
str(RoddingLevel)
str(CompressStrength)
dat <- data.frame(RoddingLevel,CompressStrength)
dat
aov.model <- aov(CompressStrength~RoddingLevel,data=dat)
summary(aov.model)
# Ans to the ques 3.20.(b)
# P value for the F-statistic is 0.214


