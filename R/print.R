#' Prints outRanger
#'
#' Print method for an object of class \code{outRanger}.
#'
#' @importFrom utils head
#' @param x A on object of class \code{outRanger}.
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @method print outRanger
#' @export
#' @examples
#' a <- outRanger(iris)
#' a
print.outRanger <- function(x, ...) {
  cat("\nI am an object with class(es)", paste(class(x), collapse = " and "), "\n\n")
  print(head(x$data))
  invisible(x)
}