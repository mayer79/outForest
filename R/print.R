#' Prints outRanger
#'
#' Print method for an object of class \code{outRanger}.
#'
#' @param x A on object of class \code{outRanger}.
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @method print outRanger
#' @export
#' @examples
#' x <- outRanger(iris)
#' x
print.outRanger <- function(x, ...) {
  cat("\nI am an object with class(es)", paste(class(x), collapse = " and "), "\n\n")
  cat("The following number of outliers have been identified:\n\n")
  print(cbind(`Number of outliers` = x$n_outliers))
  invisible(x)
}
