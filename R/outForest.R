#' Multivariate Outlier Detection and Replacement by Random Forest Predictions
#'
#' This function provides a random forest based implementation of the method described in Chapter 7.1.2 ("Regression Model Based Anomaly detection") of [1]. Each numeric variable to be checked for outliers is regressed onto all other variables using a random forest. If the scaled absolute difference between observed value and out-of-bag prediction is larger than some predefined threshold (default is 3), then a value is considered an outlier, see Details below. After identification of outliers, they can be replaced e.g. by predictive mean matching from the non-outliers. Since the random forest algorithm 'ranger' [2] does not allow for missing values, any missing value is first being imputed by chained random forests. The method can be viewed as a multivariate extension of a basic univariate outlier detection method where a value is considered an outlier if it is more than e.g. three times the standard deviation away from its mean. In the multivariate case, instead of comparing a value with the overall mean, rather the difference to the conditional mean is considered. The 'outForest' function estimates this conditional mean by a random forest. If the method is trained on a reference data with option \code{allow_predictions}, it can be applied to new data.
#'
#' The outlier score of the i-th value x_ij of the j-th variable is defined as s_ij = (x_ij - pred_ij) / rmse_j, where pred_ij is the corresponding out-of-bag prediction of the j-th random forest and rmse_j its RMSE. If |s_ij| > L with threshold L, then x_ij is considered an outlier.
#' For large data sets, just by chance, many values can surpass the default threshold of 3. To reduce the number of outliers, the threshold can be increased. Alternatively, the number of outliers can be limited by the two arguments \code{max_n_outliers} and \code{max_prop_outliers}. E.g. if at most ten outliers are to be identified, set \code{max_n_outliers = 10}.
#'
#' @importFrom stats reformulate terms.formula as.formula predict var
#' @importFrom ranger ranger
#' @importFrom missRanger missRanger imputeUnivariate
#' @param data A \code{data.frame} to be assessed for numeric outliers.
#' @param formula A two-sided formula specifying variables to be checked (left hand side) and variables used to check (right hand side). Defaults to . ~ ., i.e. use all variables to check all (numeric) variables.
#' @param replace Should outliers be replaced by predicting mean matching on the OOB predictions ("pmm", the default), by OOB predictions ("predictions"), by \code{NA} ("NA"). Use "no" to keep outliers as they are.
#' @param pmm.k For \code{replace = "pmm"}, how many nearest prediction neighbours (without outliers) be considered to sample observed values from?
#' @param threshold Threshold above which an outlier score is considered an outlier. The default is 3.
#' @param max_n_outliers Maximal number of outliers to identify. Will be used in combination with \code{threshold} and \code{max_prop_outliers}.
#' @param max_prop_outliers Maximal relative count of outliers. Will be used in combination with \code{threshold} and \code{max_n_outliers}.
#' @param min.node.size Minimal node size of the random forests. With 40, the value is relatively high. This reduces the impact of outliers.
#' @param allow_predictions Should the resulting outForest be used on new data? Default is \code{FALSE} as fitted random forests can be huge.
#' @param impute_multivariate If \code{TRUE} (default), missing values are imputed by \code{missRanger::missRanger}. Otherwise, by univariate sampling.
#' @param impute_multivariate_control Parameters passed to \code{missRanger::missRanger} if data contains missing values.
#' @param seed Integer random seed.
#' @param verbose Controls how much outliers is printed to screen. 0 to print nothing, 1 prints information.
#' @param ... Arguments passed to \code{ranger}. If the data set is large, use less trees (e.g. \code{num.trees = 20}) and/or a low value of \code{mtry}.
#' @return An object of type 'outForest' and a list with the following elements.
#' \itemize{
#'   \item \code{Data}: Original data set in unchanged row order but optionally with outliers replaced. Can be extracted with the \code{Data} function.
#'   \item \code{outliers}: Compact representation of outliers, for details see the \code{outliers} function used to extract them.
#'   \item \code{n_outliers}: Number of outliers per \code{v}.
#'   \item \code{is_outlier}: Logical matrix with outlier status. NULL if \code{allow_predictions = FALSE}.
#'   \item \code{predData}: \code{data.frame} with OOB predictions. NULL if \code{allow_predictions = FALSE}.
#'   \item \code{allow_predictions}: Same as \code{allow_predictions}.
#'   \item \code{v}: Variables checked.
#'   \item \code{threshold}: The threshold used.
#'   \item \code{rmse}: Named vector of RMSE of the random forests. Used for scaling the difference between observed values and predicted.
#'   \item \code{forests}: Named list of fitted random forests. NULL if \code{allow_predictions = FALSE}.
#'   \item \code{used_to_check}: Variables used for checking \code{v}.
#'   \item \code{mu}: Named vector of sample means of the original v (incl. outliers).
#' }
#'
#' @references
#' [1] Chandola V., Banerjee A., and Kumar V. (2009). Anomaly detection: A survey. ACM Comput. Surv. 41, 3, Article 15 <dx.doi.org/10.1145/1541880.1541882>.
#'
#' [2] Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. <arxiv.org/abs/1508.04409>.
#' @export
#' @examples
#' head(irisWithOut <- generateOutliers(iris, seed = 345))
#' (out <- outForest(irisWithOut))
#' outliers(out)
#' head(Data(out))
#' plot(out)
#' plot(out, what = "scores")
#' @seealso \code{\link{outliers}}, \code{\link{Data}}, \code{\link{plot.outForest}}, \code{\link{summary.outForest}}, \code{\link{predict.outForest}}.
outForest <- function(data, formula = . ~ .,
                      replace = c("pmm", "predictions", "NA", "no"), pmm.k = 3,
                      threshold = 3, max_n_outliers = Inf, max_prop_outliers = 1,
                      min.node.size = 40, allow_predictions = FALSE,
                      impute_multivariate = TRUE,
                      impute_multivariate_control = list(pmm.k = 3, num.trees = 50, maxiter = 3L),
                      seed = NULL, verbose = 1, ...) {
  replace <- match.arg(replace)
  data <- as.data.frame(data)

  # Initial check
  stopifnot((n <- nrow(data)) >= 1L,
            inherits(formula, "formula"),
            length(formula <- as.character.default(formula)) == 3L)
  if (min.node.size > n / 3) {
    min.node.size <- ceiling(n / 3)
    message("Due to small sample size, reduced 'min.node.size' to ", min.node.size)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Extract lhs and rhs from formula
  relevantVars <- lapply(formula[2:3], function(z) attr(terms.formula(
    reformulate(z), data = data[1, ]), "term.labels"))

  # Fill missing values
  all_relevant <- unique(unlist(relevantVars))
  data_imp <- data[, all_relevant, drop = FALSE]
  if (anyNA(data_imp)) {
    if (impute_multivariate) {
      ff <- paste(all_relevant, collapse = "+")
      missRanger_args <- c(list(data = data_imp, formula = as.formula(paste(ff, ff, sep = "~")),
                                verbose = verbose), impute_multivariate_control)
      data_imp <- do.call(missRanger, missRanger_args)
    } else {
      data_imp <- imputeUnivariate(data_imp)
    }
  }

  if (verbose) {
    cat("\nOutlier identification by random forests\n")
  }

  # Pick numeric variables from lhs and determine variable names v to check
  predData <- Filter(function(z) is.numeric(z) && (var(z) > 0),
                     data_imp[, relevantVars[[1]], drop = FALSE])
  v <- colnames(predData)
  m <- length(v)
  if (m == 0L) {
    stop("Nothing to check.")
  }
  # Vector with means (incl. outliers, excl. NAs)
  mu <- colMeans(data[, v, drop = FALSE], na.rm = TRUE)

  # Keep missingness outliers in original data
  if ((was_any_NA <- anyNA(data[, v, drop = FALSE]))) {
    wasNAData <- is.na(data[, v, drop = FALSE])
  }
  # Initialize forests
  if (allow_predictions) {
    forests <- vector(mode = "list", length = m)
    names(forests) <- v
  }

  # Check each variable
  if (verbose) {
    cat("\n  Variables to check:\t\t")
    cat(v, sep = ", ")
    cat("\n  Variables used to check:\t")
    cat(relevantVars[[2]], sep = ", ")
    cat("\n\n  Checking: ")
  }
  for (vv in v) {
    if (verbose) {
      cat(vv, " ")
    }
    covariables <- setdiff(relevantVars[[2]], vv)
    if (length(covariables)) {
      fit <- ranger(formula = reformulate(covariables, response = vv),
                    data = data_imp, min.node.size = min.node.size, ...)
      predData[[vv]] <- fit$predictions
      if (any(is_na <- is.na(predData[[vv]]))) {
        predData[is_na, vv] <- predict(fit, data_imp[is_na, ])$predictions
      }
      if (allow_predictions) {
        forests[[vv]] <- fit
      }
    } else {
      # If no covariables, then prediction is simply the mean (including outliers)
      predData[[vv]] <- mu[vv]
    }
  }

  # Calculate outlier scores and status
  scores <- scale(data_imp[, v, drop = FALSE] - predData, center = FALSE)
  rmse <- attributes(scores)$`scaled:scale`
  if (was_any_NA) {
    scores[wasNAData] <- 0
  }
  out <- process_scores(data = data, scores = scores, predData = predData,
                        v = v, rmse = rmse, replace = replace, pmm.k = pmm.k,
                        threshold = threshold, max_n_outliers = max_n_outliers,
                        max_prop_outliers = max_prop_outliers,
                        allow_predictions = allow_predictions)
  out <- c(out, list(
    forests = if (allow_predictions) forests,
    used_to_check = relevantVars[[2]],
    mu = mu))
  class(out) <- c("outForest", "list")
  out
}




