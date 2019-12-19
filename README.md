# outRanger

Uses the 'ranger' package [1] to identify outliers in numeric variables. Each numeric variable to be checked is regressed onto all other variables using a random forest. If the absolute difference between observed value and out-of-bag prediction is too large, then a value is considered an outlier. Since the random forest algorithm [1] does not allow for missing values, they are first imputed by chained random forests, see e.g. [2] or [3].

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

We first generate a data set with about 10% outliers values in each column. Then, we try to identify them.

``` r
library(outRanger)
 
# Generate data with outliers in numeric columns
irisWithOutliers <- generateOutlier(iris, seed = 34)
head(irisWithOutliers)
 
# Find and replace outliers by random forest regressions
iris2 <- outRanger(irisWithOutliers, replace = "predictions")
 
# Check results
head(iris2)

# The signed outlier scores
head(attr(iris2, "scores"))

```

## References
[1]  Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. http://arxiv.org/abs/1508.04409. 

[2]  Stekhoven, D.J. and Buehlmann, P. (2012). MissForest - nonparametric missing value imputation for mixed-type data. Bioinformatics, 28(1), 112-118, doi: 10.1093/bioinformatics/btr597

[3]  Van Buuren, S. and Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. http://www.jstatsoft.org/v45/i03/

