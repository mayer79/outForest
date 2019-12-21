#=====================================================================================
# BUILD THE PACKAGE
#=====================================================================================

library(ranger)
library(missRanger)
library(FNN)
lapply(list.files("R", full.names = TRUE), source)

library(usethis)
library(devtools)

# Create a new package
dir.create(file.path("release"))
pkg <- file.path("release", "outRanger")

create_package(
  pkg,
  fields = list(
    Title = "Multivariate Outlier Detection and Replacement by Random Forest Predictions",
    Type = "Package",
    Version = "0.1.0",
    Date = Sys.Date(),
    Description = "Provides a random forest based implementation of the method described in Chapter 7.1.2 ('Regression Model Based Anomaly detection') of Chandola et al. (2009) <dx.doi.org/10.1145/1541880.1541882>. It works as follows: Each numeric variable is regressed onto all other variables. If the absolute difference between observed value and out-of-bag prediction of the corresponding random forest regression is suspiciously large (e.g. three times the RMSE of the random forest), then a value is considered an outlier. Our package offers different options to replace such outliers, e.g. by realistic values found via preditive mean matching. The method can be viewed as a multivariate extension of a basic univariate outlier detection method where a value is considered an outlier if it is more than e.g. three times the standard deviation away from its mean. In the multivariate case, instead of comparing a value with the overall mean, rather the difference to the conditional mean is considered. The 'outRanger' package estimates this conditional mean by a random forest.",

    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre', 'cph'))",
    Depends = "R (>= 3.5.0)",
    VignetteBuilder = "knitr",
    License = "GPL(>= 2)",
    Maintainer = "Michael Mayer <mayermichael79@gmail.com>"))

file.copy(file.path(pkg, "DESCRIPTION"), to = getwd(), overwrite = TRUE)
# Use package has no option to look for pkg, so we first copy description from pkg, modify it and move back
use_package("stats", "Imports")
use_package("graphics", "Imports")
use_package("FNN", "imports")
use_package("ranger", "Imports")
use_package("missRanger", "Imports", min_version = "2.1.0")
use_package("dplyr", "Suggests")
use_package("knitr", "Suggests")

# Set up other files -------------------------------------------------
# use_readme_md()
# use_news_md()
# use_cran_comments()

# Copy readme etc.
file.copy(c(".Rbuildignore", "NEWS.md", "README.md", "cran-comments.md", "DESCRIPTION"), pkg, overwrite = TRUE)

# Copy R scripts and document them
file.copy(list.files("R", full.names = TRUE), file.path(pkg, "R"), overwrite = TRUE)
devtools::document(pkg)

# Copy vignette
# use_vignette(name = "outRanger", title = "outRanger")
dir.create(file.path(pkg, "vignettes"))
dir.create(file.path(pkg, "doc"))
dir.create(file.path(pkg, "Meta"))
file.copy(list.files("vignettes", full.names = TRUE),
          file.path(pkg, "vignettes"), overwrite = TRUE)

devtools::build_vignettes(pkg)

# Check
check(pkg, manual = TRUE)

# Create
build(pkg)
build(pkg, binary = TRUE)

# Install
install(pkg)

# modify .Rbuildignore in build project to ignore the proj file.

check_win_devel(pkg)

check_rhub(pkg)

devtools::release(pkg)
