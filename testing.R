#=====================================================================================
#  TESTING THE PACKAGE "outRanger"
#=====================================================================================

library(ranger)
library(FNN)
lapply(list.files("r", full.names = TRUE), source)
out <- outRanger(as.data.frame(ggplot2::diamonds), num.trees = 10, max_n_outliers = 10)
outliers(out)
head(Data(out))

# F) Time series data
set.seed(3)
t <- seq(0, 10, by = 0.01)
y <- sin(t) + runif(length(t), 0, 0.3)
y[500] <- 0
plot(y ~ t, type = "l")
ch <- outRanger(data.frame(t, y), min.node.size = 40)
outliers(ch)

# G) Bathtube
set.seed(3)
t <- seq(0, pi, by = 0.01)
dat <- transform(data.frame(t), x = cos(t), y = sin(t) + runif(length(t), -0.1, 0.1))
dat[150, 2:3] <- c(0, 0.4)
plot(y ~ x, data = dat, cex.lab = 1.3, cex.axis = 1.3)
ch <- outRanger(dat[-1])
outliers(ch)
