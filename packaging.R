# =============================================================================
# Put together the package
# =============================================================================

# WORKFLOW: UPDATE EXISTING PACKAGE
# 1) Modify package content and documentation.
# 2) Increase package number in "use_description" below.
# 3) Go through this script and carefully answer "no" if a "use_*" function
#    asks to overwrite the existing files. Don't skip that function call.
# devtools::load_all()

library(usethis)

# Sketch of description file
use_description(
  fields = list(
    Title = "Multivariate Outlier Detection and Replacement",
    Version = "1.0.2",
    Description = "Provides a random forest based implementation of the method described
    in Chapter 7.1.2 (Regression model based anomaly detection) of
    Chandola et al. (2009) <doi:10.1145/1541880.1541882>.
    It works as follows: Each numeric variable is regressed onto all other variables by
    a random forest. If the scaled absolute difference between observed value and
    out-of-bag prediction of the corresponding random forest is suspiciously large,
    then a value is considered an outlier. The package offers different options to
    replace such outliers, e.g. by realistic values found via predictive mean matching.
    Once the method is trained on a reference data, it can be applied to new data.",
    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre'))",
    Depends = "R (>= 3.5.0)",
    LazyData = NULL
  ),
  roxygen = TRUE
)

use_package("stats", "Imports")
use_package("graphics", "Imports")
use_package("FNN", "imports")
use_package("ranger", "Imports")
use_package("missRanger", "Imports", min_version = "2.1.0")

use_gpl_license(2)

use_github_links() # use this if this project is on github

# Your files that do not belong to the package itself (others are added by "use_* function")
use_build_ignore(c(
  "^packaging.R$", "[.]Rproj$", "^backlog$",
  "^cran-comments.md$", "^logo.png$"
), escape = FALSE)

# If your code uses the pipe operator %>%
# use_pipe()

# If your package contains data. Google how to document
# use_data()

# Add short docu in Markdown (without running R code)
use_readme_md()

# Longer docu in RMarkdown (with running R code). Often quite similar to readme.
use_vignette("outForest")

# If you want to add unit tests
use_testthat()
use_test("outForest.R")

# On top of NEWS.md, describe changes made to the package
use_news_md()

# Add logo
use_logo("logo.png")

# If package goes to CRAN: infos (check results etc.) for CRAN
use_cran_comments()

# Github actions
use_github_action("check-standard")
use_github_action("test-coverage")
use_github_action("pkgdown")

# =============================================================================
# Finish package building (can use fresh session)
# =============================================================================

library(devtools)

document()
test()
check(manual = TRUE, cran = TRUE)
build()
# build(binary = TRUE)
install()

# Run only if package is public(!) and should go to CRAN
if (FALSE) {
  check_win_devel()
  check_rhub()

  # Wait until above checks are passed without relevant notes/warnings
  # then submit to CRAN
  release()
}
