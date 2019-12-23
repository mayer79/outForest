#' Out-of-Sample Application
#'
#' Identify outliers in new data set based on previously fitted "outRanger" object. The result of \code{predict} is again an object of type "outRanger". All its methods can be applied to it.
#'
#' @importFrom stats predict
#' @param object An object of class "outRanger".
#' @param newdata A new \code{data.frame} to be assessed for numeric outliers.
#' @param replace Should outliers be replaced by predicting mean matching (from the original non-outliers) on the predictions ("pmm", the default), by predictions ("predictions"), by \code{NA} ("NA"). Use "no" to keep outliers as they are.
#' @param pmm.k For \code{replace = "pmm"}, how many nearest prediction neighbours (from the original non-outliers) be considered to sample observed values from?
#' @param threshold Threshold above which an outlier score is considered an outlier.
#' @param max_n_outliers Maximal number of outliers to identify. Will be used in combination with \code{threshold} and \code{max_prop_outliers}.
#' @param max_prop_outliers Maximal relative count of outliers. Will be used in combination with \code{threshold} and \code{max_n_outliers}.
#' @param seed Integer random seed.
#' @param ... Further arguments passed from other methods.
#' @return An object of type \code{outRanger}.
#' @export
#' @method predict outRanger
#' @examples
#' (out <- outRanger(iris, allow_predictions = TRUE))
#' iris1 <- iris[1, ]
#' iris1$Sepal.Length <- -1
#' pred <- predict(out, newdata = iris1)
#' outliers(pred)
#' Data(pred)
#' plot(pred)
#' plot(pred, what = "scores")
#' @seealso \code{\link{outRanger}}, \code{\link{outliers}}, \code{\link{Data}}.
predict.outRanger <- function(object, newdata, replace = c("pmm", "predictions", "NA", "no"),
                              pmm.k = 3, threshold = object$threshold, max_n_outliers = Inf,
                              max_prop_outliers = 1, seed = NULL, ...) {
  replace <- match.arg(replace)
  newdata <- as.data.frame(newdata)

  # Initial check
  stopifnot(is.data.frame(newdata),
            (n <- nrow(newdata)) >= 1L)
  if (!object$allow_predictions) {
    stop("Use 'allow_predictions = TRUE' when creating 'outRanger' object.")
  }

  # Initialization
  if (!is.null(seed)) {
    set.seed(seed)
  }
  v <- object$v
  predData <- newdata[, v, drop = FALSE]

  # Currently, can deal only with NA in one single v
  all_vars <- union(v, object$used_to_check)
  any_NA <- names(which(0 < colSums(is.na(newdata[, all_vars, drop = FALSE]))))
  if (length(any_NA) <= 1L && all(any_NA %in% v)) {
    if (length(any_NA) == 1L) {
      .s <- is.na(newdata[[any_NA]])
      newdata[.s, any_NA] <- predict(object$forests[[any_NA]], data = newdata[.s, ])$predictions
    }
  } else {
    stop("Missing values only in one v variable allowed.")
  }

  # Check each variable
  for (vv in v) {
    if (length(setdiff(object$used_to_check, vv))) {
      predData[[vv]] <- predict(object$forests[[vv]], data = newdata)$predictions
    } else {
      predData[[vv]] <- object$mu[vv]
    }
  }
  # Calculate outlier scores and status
  scores <- scale(newdata[, v, drop = FALSE] - predData, center = FALSE, scale = object$rmse)
  if (length(any_NA)) {
    scores[.s, any_NA] <- 0
  }
  out <- process_scores(data = newdata, scores = scores, predData = predData,
                        v = v, rmse = object$rmse, replace = replace, pmm.k = pmm.k,
                        threshold = threshold, max_n_outliers = max_n_outliers,
                        max_prop_outliers = max_prop_outliers,
                        allow_predictions = FALSE, obj = object)
  out <- c(out, list(forests = NULL), object[c("used_to_check", "mu")])
  class(out) <- c("outRanger", "list")
  out
}




