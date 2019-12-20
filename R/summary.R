#' Summary outRanger
#'
#' Summary method for an object of class \code{outRanger}.
#'
#' @param x A on object of class \code{outRanger}.
#' @param ... Further arguments passed from other methods.
#' @return A list of summary statistics.
#' @method summary outRanger
#' @export
#' @examples
#' a <- outRanger(iris, replace = "NA", seed = 34)
#' summary(a)
summary.outRanger <- function(x, ...) {
  print(x)
}