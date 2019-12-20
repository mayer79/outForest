#' Plot for outRanger
#'
#' This function can plot different aspects of an outRanger object. For \code{what = "counts"}, the number of outliers per variable is visualized as a barplot. For \code{what = "scores"}, normalized outlier scores (i.e. the difference between predicted and observed value) are shown as scatter plot per variable.
#'
#' @importFrom graphics text barplot stripchart abline
#' @method plot outRanger
#' @param x An object of class \code{outRanger}.
#' @param what What should be plotted? One of "counts" (the default) or "scores". 
#' @param ... Further arguments passed to \code{graphics::barplot} or \code{graphics::stripchart}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' x <- outRanger(iris, verbose = 0)
#' plot(x)
#' plot(x, what = "scores")
#' @seealso \code{\link{light_performance}}.
plot.outRanger <- function(x, what = c("counts", "scores"), ...) {
  what <- match.arg(what)
  
  if (what == "counts") {
    yy <- barplot(x$n_outliers, horiz = TRUE, yaxt = "n")
    text(0.1, yy, names(x$n_outliers), adj = 0)
  } else {
    stripchart(score ~ col, data = x$info, vertical = TRUE, pch = 4, jitter = 0.05)
    abline(h = 0, lty = 2)
  }
}
