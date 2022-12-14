fluid_type <- c(rep(1,6),rep(2,6),rep(3,6),rep(4,6))
life <- c(17.6,18.9,16.3,17.4,20.1,21.6,16.9,15.3,18.6,17.1,19.5,
          20.3,21.4,23.6,19.4,18.5,20.5,22.3,19.3,21.1,16.9,17.5,18.3,19.8)
length(life)
fluid_type <- as.factor(fluid_type)
life <- as.numeric(life)
library(tidyr)
library(dplyr)
dat <- data.frame(fluid_type,life)
dat
mod <- aov(life~fluid_type,data=dat)
summary(mod)
library(agricolae)
LSD.test(mod,"fluid_type",console = TRUE)

library(car)
TukeyHSD(mod)
plot(TukeyHSD(mod))
