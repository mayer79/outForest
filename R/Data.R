#' Data of outForest
#'
#' Extracts data with replaced outliers from object of class \code{outForest}.
#'
#' @param object An object of class \code{outForest}.
#' @param ... Arguments passed from or to other methods.
#' @return A \code{data.frame}.
#' @export
#' @examples
#' x <- outForest(iris)
#' head(Data(x))
Data <- function(object, ...) {
  UseMethod("Data")
}

#' @describeIn Data Default method not implemented yet.
#' @export
Data.default <- function(object, ...) {
  stop("No default method available yet.")
}

#' @describeIn Data Extract data from outForest object.
#' @export
Data.outForest <- function(object, ...) {
  object$Data
}
