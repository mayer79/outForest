# outRanger

`outRanger` is a random forest regression based multivariate anomaly detection method. Each numeric variable is regressed onto all other variables using a random forest. If the scaled absolute difference between observed value and out-of-bag prediction is larger than a prespecified z-score, then a value is considered an outlier. After identification of outliers, they can be replaced e.g. by predictive mean matching from the non-outliers.

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
irisWithOutliers <- generateOutlier(iris, seed = 34)
head(irisWithOutliers)
 
# Find outliers by random forest regressions and replace them by predictive mean matching.
(out <- outRanger(irisWithOutliers))

# Plot the number of outliers per numeric variable
plot(out)

# Information on all outliers
out$info

# Resulting data set with replaced outliers
head(out$data)

```
