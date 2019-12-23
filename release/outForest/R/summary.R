#' Summarizes outForest
#'
#' Summary method for an object of class \code{outForest}. Besides the number of outliers per variables, it shows the worst outliers.
#'
#' @importFrom utils head
#' @param object A on object of class \code{outForest}.
#' @param ... Further arguments passed from other methods.
#' @return A list of summary statistics.
#' @method summary outForest
#' @export
#' @examples
#' out <- outForest(iris, seed = 34, verbose = 0)
#' summary(out)
summary.outForest <- function(object, ...) {
   if (nrow(outliers(object)) == 0L) {
    cat("Congratulations, no outliers found.")
  } else {
    cat("The following outlier counts have been detected:\n\n")
    print(cbind(`Number of outliers` = object$n_outliers))

    cat("\nThese are the worst outliers:\n\n")
    print(head(outliers(object)))
  }
}
