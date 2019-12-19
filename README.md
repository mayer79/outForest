# outRanger

`outRanger` is a random forest regression based anomaly detection method. Each numeric variable to be checked for outliers is regressed onto all other variables using a random forest. If the absolute difference between observed value and out-of-bag prediction is too large, then a value is considered an outlier. After identification of outliers, they can be replaced e.g. by predictive mean matching from the non-outliers.

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
iris2 <- outRanger(irisWithOutliers)
 
# Check results
head(iris2)

# The signed outlier scores
head(attr(iris2, "scores"))

```
