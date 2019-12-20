#' Extracts Data from outRanger
#'
#' Extracts the "data" element in an "outRanger" object.
#' 
#' @param x An object of type "outRanger".
#' @return A \code{data.frame}.
#' @export
#' @examples 
#' generateOutlier(1:10, seed = 3345)
#' generateOutlier(cbind(1:10, 10:1), p = 0.2)
#' head(generateOutlier(iris))
#' head(generateOutlier(iris, p = 0.2))
#' head(generateOutlier(iris, p = c(0, 0, 0.5, 0.5, 0.5)))
#' head(generateOutlier(iris, p = list(Sepal.Length = 0.2)))
#' @seealso \code{\link{outRanger}}.
extractData <- function(x) {
  stopifnot(is.outRanger(x))
  x$data
}
