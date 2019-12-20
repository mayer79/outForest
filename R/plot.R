#' Visualize Outliers
#'
#' "counts" are represented by a bar plot, while the other options are visualized as scatter plots.
#'
#' @importFrom graphics text barplot stripchart abline
#' @method plot outRanger
#' @param x An object of class \code{outRanger}.
#' @param what What should be plotted? One of "counts" (the default) or "scores". 
#' @param ... Further arguments passed to \code{geom_bar}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' x <- outRanger(iris, verbose = 0)
#' plot(x, fill = "darkred")
#' plot(x, fill = "darkred", what = "scores")
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
