#' Visualize Outliers
#'
#' blabla
#'
#' @import ggplot2
#' @importFrom stats reformulate
#' @method plot outRanger
#' @param x An object of class \code{outRanger}.
#' @param ... Further arguments passed to \code{geom_bar}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' @seealso \code{\link{light_performance}}.
plot.outRanger <- function(x, ...) {
  data <- x$data
 
  ggplot(data, aes(x = x, y = y)) +
         geom_bar(stat = "identity", ...)
}
