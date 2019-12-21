#' Data of outRanger
#'
#' Extracts data with replaced outliers from object of class \code{outRanger}.
#'
#' @param object An object of class \code{outRanger}.
#' @param ... Arguments passed from or to other methods.
#' @return A \code{data.frame}.
#' @export
#' @examples
#' x <- outRanger(iris)
#' head(Data(x))
Data <- function(object, ...) {
  UseMethod("Data")
}

#' @describeIn Data Default method not implemented yet.
#' @export
Data.default <- function(object, ...) {
  stop("No default method available yet.")
}

#' @describeIn Data Extract data from outRanger object.
#' @export
Data.outRanger <- function(object, ...) {
  object$Data
}