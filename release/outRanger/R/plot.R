#' Plot for outRanger
#'
#' This function can plot aspects of an outRanger object. For \code{what = "counts"}, the number of outliers per variable is visualized as a barplot. For \code{what = "scores"}, outlier scores (i.e. the scaled difference between predicted and observed value) are shown as scatter plot per variable.
#'
#' @importFrom graphics text barplot stripchart abline
#' @method plot outRanger
#' @param x An object of class \code{outRanger}.
#' @param what What should be plotted? One of "counts" (the default) or "scores".
#' @param ... Further arguments passed to \code{graphics::barplot} or \code{graphics::stripchart}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' irisWithOutliers <- generateOutliers(iris, seed = 345)
#' x <- outRanger(irisWithOutliers, verbose = 0)
#' plot(x)
#' plot(x, what = "scores")
plot.outRanger <- function(x, what = c("counts", "scores"), ...) {
  what <- match.arg(what)

  if (what == "counts") {
    yy <- barplot(x$n_outliers, horiz = TRUE, yaxt = "n",
                  main = "Number of outliers per variable", xlab = "Count", ...)
    text(0.1, yy, names(x$n_outliers), adj = 0)
  } else {
    stripchart(score ~ col, data = outliers(x), vertical = TRUE,
               pch = 4, las = 2, cex.axis = 0.7, ...)
    abline(h = c(-1, 1) * outliers(x)$threshold[1], lty = 2)
  }
}
