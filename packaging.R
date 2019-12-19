#=====================================================================================
# BUILD THE PACKAGE
#=====================================================================================

# lapply(list.files("R", full.names = TRUE), source)

library(usethis)
library(devtools)

# Create a new package
dir.create(file.path("release"))
pkg <- file.path("release", "outRanger")

create_package(
  pkg,
  fields = list(
    Title = "Outlier Detection and Replacement by Random Forest Predictions",
    Type = "Package",
    Version = "0.1.0",
    Date = Sys.Date(),
    Description = "Uses the 'ranger' package [1] to identify outliers in numeric variables. Each numeric variable to be checked is regressed onto all other variables using a random forest. If the absolute difference between observed value and out-of-bag prediction is too large, then a value is considered an outlier. Since the random forest algorithm [1] does not allow for missing values, they are first imputed by chained random forests.",

    `Authors@R` = "c(person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre', 'cph'))",
    Depends = "R (>= 3.5.0)",
    VignetteBuilder = "knitr",
    License = "GPL(>= 2)",
    Maintainer = "Michael Mayer <mayermichael79@gmail.com>"))

file.copy(file.path(pkg, "DESCRIPTION"), to = getwd(), overwrite = TRUE)
# Use package has no option to look for pkg, so we first copy description from pkg, modify it and move back
use_package("stats", "Imports")
use_package("ranger", "Imports")
use_package("missRanger", "Imports")
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
