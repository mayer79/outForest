#' outliers of outRanger
#'
#' Extracts outliers from object of class \code{outRanger}.
#'
#' @param object An object of class \code{outRanger}.
#' @param ... Arguments passed from or to other methods.
#' @return A \code{data.frame}. Each row describes an outlier. The columns are as follows:
#' \itemize{
#'   \item \code{row}, \code{col} Row and column in original data with outlier.
#'   \item \code{observed} Observed value.
#'   \item \code{predicted} Predicted value.
#'   \item \code{rmse} Scaling factor used to normalize the difference between observed and predicted.
#'   \item \code{score} Outlier score defined as (observed-predicted)/rmse.
#'   \item \code{replacement} Value used to replace observed value.
#' }
#' @export
#' @examples
#' x <- outRanger(iris)
#' outliers(x)
outliers <- function(object, ...) {
  UseMethod("outliers")
}

#' @describeIn outliers Default method not implemented yet.
#' @export
outliers.default <- function(object, ...) {
  stop("No default method available yet.")
}

#' @describeIn outliers Extract outliers from outRanger object.
#' @export
outliers.outRanger <- function(object, ...) {
  object$outliers
}