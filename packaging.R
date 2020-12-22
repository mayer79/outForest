#=====================================================================================
# BUILD THE PACKAGE
#=====================================================================================

if (FALSE) {
  library(ranger)
  library(missRanger)
  library(FNN)
  lapply(list.files("R", full.names = TRUE), source)
}

library(usethis)
library(devtools)

# Create a new package
dir.create(file.path("release"))
pkg <- file.path("release", "outForest")

create_package(
  pkg,
  fields = list(
    Title = "Multivariate Outlier Detection and Replacement",
    Type = "Package",
    Version = "0.1.1",
    Date = Sys.Date(),
    Description = "Provides a random forest based implementation of the method described in Chapter 7.1.2 (Regression model based anomaly detection) of Chandola et al. (2009) <doi:10.1145/1541880.1541882>. It works as follows: Each numeric variable is regressed onto all other variables by a random forest. If the scaled absolute difference between observed value and out-of-bag prediction of the corresponding random forest is suspiciously large, then a value is considered an outlier. The package offers different options to replace such outliers, e.g. by realistic values found via predictive mean matching. Once the method is trained on a reference data, it can be applied to new data.",
    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre'))",
    URL = "https://github.com/mayer79/outForest",
    BugReports = "https://github.com/mayer79/outForest/issues",
    Depends = "R (>= 3.5.0)",
    VignetteBuilder = "knitr",
    License = "GPL(>= 2)",
    Maintainer = "Michael Mayer <mayermichael79@gmail.com>"),
  open = FALSE)

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
if (!dir.exists(file.path(pkg, "R"))) {
  dir.create(file.path(pkg, "R"))
}
file.copy(list.files("R", full.names = TRUE), file.path(pkg, "R"), overwrite = TRUE)
devtools::document(pkg)

if (TRUE) {
  # Copy vignette
  # use_vignette(name = "outForest", title = "outForest")
  dir.create(file.path(pkg, "vignettes"), showWarnings = FALSE)
  dir.create(file.path(pkg, "doc"), showWarnings = FALSE)
  dir.create(file.path(pkg, "Meta"), showWarnings = FALSE)
  file.copy(list.files("vignettes", full.names = TRUE),
            file.path(pkg, "vignettes"), overwrite = TRUE)

  devtools::build_vignettes(pkg)
}

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

usethis::use_pkgdown()
pkgdown::build_site(pkg)
