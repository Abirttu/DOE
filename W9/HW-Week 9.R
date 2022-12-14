
# Problem 5.9
library(GAD)
drill_speed <- c(rep(125,8),rep(200,8))
feed_rate <- c(rep(seq(0.015,0.060,0.015),4))
response <- c(2.70,2.45,2.60,2.75,2.78,2.49,2.72,2.86,2.83,2.85,2.86,2.94,2.86,
              2.80,2.87,2.88)
drill_speed <- as.fixed(drill_speed)
feed_rate <- as.fixed(feed_rate)
dat <- data.frame(drill_speed,feed_rate,response)
dat
model <- aov(response~drill_speed+feed_rate+drill_speed*feed_rate,data=dat)
summary(model)
interaction.plot(drill_speed,feed_rate,response)

interaction.plot(feed_rate,drill_speed,response) ##this is the correct interaction plot
qf(0.95,3,8)
