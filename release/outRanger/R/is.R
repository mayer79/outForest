#' Check function
#'
#' Checks if an object inherits class \code{outRanger}.
#'
#' @param x Any object.
#' @return A logical vector of length one.
#' @export
#' @examples
#' a <- outRanger(iris)
#' is.outRanger(a)
#' is.outRanger("a")
is.outRanger <- function(x) {
  inherits(x, "outRanger")
}