#' Summarizes outRanger
#'
#' Summary method for an object of class \code{outRanger}. Besides the number of outliers per variables, it shows univariate distributions of the values considered as outliers.
#'
#' @param object A on object of class \code{outRanger}.
#' @param ... Further arguments passed from other methods.
#' @return A list of summary statistics.
#' @method summary outRanger
#' @export
#' @examples
#' out <- outRanger(iris, seed = 34, verbose = 0)
#' summary(out)
summary.outRanger <- function(object, ...) {
  print(object)
  if (nrow(outliers(object))) {
    cat("\nDistribution of outliers:\n\n")
    do.call(rbind, with(outliers(object), by(observed, INDICES = col, FUN = summary)))
  }
}
