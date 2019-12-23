#' Prints outForest
#'
#' Print method for an object of class \code{outForest}.
#'
#' @param x A on object of class \code{outForest}.
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @method print outForest
#' @export
#' @examples
#' x <- outForest(iris)
#' x
print.outForest <- function(x, ...) {
  cat("I am an object of class(es)", paste(class(x), collapse = " and "), "\n\n")
  cat("The following number of outliers have been identified:\n\n")
  print(cbind(`Number of outliers` = x$n_outliers))
  invisible(x)
}
