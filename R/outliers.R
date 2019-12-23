#' outliers of outForest
#'
#' Extracts outliers from object of class \code{outForest}. The outliers are sorted by there absolute score in descending fashion.
#'
#' @param object An object of class \code{outForest}.
#' @param ... Arguments passed from or to other methods.
#' @return A \code{data.frame} with one row per outlier. The columns are as follows:
#' \itemize{
#'   \item \code{row}, \code{col} Row and column in original data with outlier.
#'   \item \code{observed} Observed value.
#'   \item \code{predicted} Predicted value.
#'   \item \code{rmse} Scaling factor used to normalize the difference between observed and predicted.
#'   \item \code{score} Outlier score defined as (observed-predicted)/rmse.
#'   \item \code{threshold} Threshold above which an outlier score counts as outlier.
#'   \item \code{replacement} Value used to replace observed value.
#' }
#' @export
#' @examples
#' x <- outForest(iris)
#' outliers(x)
outliers <- function(object, ...) {
  UseMethod("outliers")
}

#' @describeIn outliers Default method not implemented yet.
#' @export
outliers.default <- function(object, ...) {
  stop("No default method available yet.")
}

#' @describeIn outliers Extract outliers from outForest object.
#' @export
outliers.outForest <- function(object, ...) {
  object$outliers
}
