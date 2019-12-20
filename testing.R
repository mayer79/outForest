#=====================================================================================
#  TESTING THE PACKAGE "outRanger"
#=====================================================================================

library(ranger)
library(FNN)
lapply(list.files("r", full.names = TRUE), source)
# E) Diamonds 
# setwd("Q:/mma/abgeschlossen/2014_IAZI_Outlier_detection")
# load("diamonds.RData")
# head(diamonds); dim(diamonds); precheck(diamonds)
# diamonds$Color <- ordered(diamonds$Color)
# outd <- check(diamonds)
# object.size(outd) / object.size(diamonds)
# repd <- summary(outd, limit = 6, text = TRUE)
# head(repd, 10)

# F) Time series data 
# set.seed(3)
# t <- seq(0, 10, by = 0.01)
# y <- sin(t) + runif(length(t), 0, 1)
# y[500] <- 1
# plot(ts(y), xlab = "t", ylab = "y")
# ch <- check(data.frame(t, y))
# summary(ch, text = TRUE)

# G) Bathtube
# set.seed(3)
# t <- seq(0, pi, by = 0.01)
# dat <- transform(data.frame(t), x = cos(t), y = sin(t) + runif(length(t), -0.1, 0.1))
# dat[150, 2:3] <- c(0, 0.4)
# plot(y ~ x, data = dat, cex.lab = 1.3, cex.axis = 1.3)
# ch <- check(dat[-1])
# summary(ch)
# summary(ch, text = TRUE)