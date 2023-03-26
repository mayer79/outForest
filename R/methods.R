#' Prints outForest
#'
#' Print method for an object of class "outForest".
#'
#' @param x A on object of class "outForest".
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @export
#' @examples
#' x <- outForest(iris)
#' x
print.outForest <- function(x, ...) {
  cat("I am an object of class(es)", paste(class(x), collapse = " and "), "\n\n")
  cat("The following number of outliers have been identified:\n\n")
  print(cbind(`Number of outliers` = x$n_outliers))
  invisible(x)
}

#' Summarizes outForest
#'
#' Summary method for an object of class "outForest".
#' Besides the number of outliers per variables, it also shows the worst outliers.
#'
#' @param object A on object of class "outForest".
#' @param ... Further arguments passed from other methods.
#' @return A list of summary statistics.
#' @export
#' @examples
#' out <- outForest(iris, seed = 34, verbose = 0)
#' summary(out)
summary.outForest <- function(object, ...) {
   if (nrow(outliers(object)) == 0L) {
    cat("Congratulations, no outliers found.")
  } else {
    cat("The following outlier counts have been detected:\n\n")
    print(cbind(`Number of outliers` = object$n_outliers))

    cat("\nThese are the worst outliers:\n\n")
    print(utils::head(outliers(object)))
  }
}

#' Plots outForest
#'
#' This function can plot aspects of an "outForest" object.
#' For \code{what = "counts"}, the number of outliers per variable is visualized as a barplot.
#' For \code{what = "scores"}, outlier scores (i.e. the scaled difference between
#' predicted and observed value) are shown as scatter plot per variable.
#'
#' @param x An object of class "outForest".
#' @param what What should be plotted? One of "counts" (the default) or "scores".
#' @param ... Further arguments passed to \code{graphics::barplot()} or
#' \code{graphics::stripchart()}.
#' @return A list.
#' @export
#' @examples
#' irisWithOutliers <- generateOutliers(iris, seed = 345)
#' x <- outForest(irisWithOutliers, verbose = 0)
#' plot(x)
#' plot(x, what = "scores")
plot.outForest <- function(x, what = c("counts", "scores"), ...) {
  what <- match.arg(what)

  if (what == "counts") {
    yy <- graphics::barplot(
      x$n_outliers,
      horiz = TRUE,
      yaxt = "n",
      main = "Number of outliers per variable",
      xlab = "Count",
      ...
    )
    graphics::text(0.1, yy, names(x$n_outliers), adj = 0)
  } else {
    if (nrow(outliers(x)) == 0L) {
      stop("No outlier to plot")
    }
    graphics::stripchart(
      score ~ col,
      data = outliers(x),
      vertical = TRUE,
      pch = 4,
      las = 2,
      cex.axis = 0.7,
      ...
    )
    graphics::abline(h = c(-1, 1) * outliers(x)$threshold[1], lty = 2)
  }
}

#' Type Check
#'
#' Checks if an object inherits class "outForest".
#'
#' @param x Any object.
#' @return A logical vector of length one.
#' @export
#' @examples
#' a <- outForest(iris)
#' is.outForest(a)
#' is.outForest("a")
is.outForest <- function(x) {
  inherits(x, "outForest")
}


#' Extracts Data
#'
#' Extracts data with optionally replaced outliers from object of class "outForest".
#'
#' @param object An object of class "outForest".
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

#' @describeIn Data Extract data from "outForest" object.
#' @export
Data.outForest <- function(object, ...) {
  object$Data
}
