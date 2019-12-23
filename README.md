# outRanger

`outRanger` is a random forest regression based multivariate anomaly detection method. Each numeric variable is regressed onto all other variables using a random forest. If the scaled absolute difference between observed value and out-of-bag prediction is larger than a prespecified threshold, then a value is considered an outlier. After identification of outliers, they can be replaced e.g. by predictive mean matching from the non-outliers.

The method can be viewed as a multivariate extension of a basic univariate outlier detection method where a value is considered an outlier if it is more than e.g. three times the standard deviation away from its mean. In the multivariate case, instead of comparing a value with the overall mean, rather the difference to the conditional mean is considered. The `outRanger` package estimates this conditional mean by a random forest.

## Installation
From CRAN:
``` r
install.packages("outRanger")
```

Latest version from github:
``` r
# library(devtools)
install_github("mayer79/outRanger")
```

## Examples

We first generate a data set with about 5% outliers values in each column. Then, we try to identify them.

``` r
library(outRanger)
 
# Generate data with outliers in numeric columns
irisWithOutliers <- generateOutliers(iris, seed = 34)
head(irisWithOutliers)
 
# Find outliers by random forest regressions and replace them by predictive mean matching
(out <- outRanger(irisWithOutliers, allow_predictions = TRUE))

# Plot the number of outliers per numeric variable
plot(out)

# Information on all outliers
outliers(out)

# Resulting data set with replaced outliers
head(Data(out))

# Out-of-sample application
iris1 <- iris[1, ]
iris1$Sepal.Length <- -1
pred <- predict(out, newdata = iris1)
outliers(pred)
Data(pred)
```
