#' Multivariate Outlier Detection and Replacement
#'
#' This function provides a random forest based implementation of the method described
#' in Chapter 7.1.2 ("Regression Model Based Anomaly detection") of Chandola et al.
#' Each numeric variable to be checked for outliers is regressed onto all other
#' variables using a random forest. If the scaled absolute difference between observed
#' value and out-of-bag prediction is larger than some predefined threshold
#' (default is 3), then a value is considered an outlier, see Details below.
#' After identification of outliers, they can be replaced, e.g., by
#' predictive mean matching from the non-outliers.
#'
#' The method can be viewed as a multivariate extension of a basic univariate outlier
#' detection method where a value is considered an outlier if it is more than, e.g.,
#' three times the standard deviation away from its mean. In the multivariate case,
#' instead of comparing a value with the overall mean, rather the difference to the
#' conditional mean is considered. `outForest()` estimates this conditional
#' mean by a random forest. If the method is trained on a reference data with option
#' `allow_predictions = TRUE`, it can even be applied to new data.
#'
#' The outlier score of the ith value \eqn{x_{ij}} of the jth variable is defined as
#' \eqn{s_{ij} = (x_{ij} - p_{ij}) / \textrm{rmse}_j}, where \eqn{p_{ij}}
#' is the corresponding out-of-bag prediction of the jth random forest and
#' \eqn{\textrm{rmse}_j} its RMSE. If \eqn{|s_{ij}| > L} with
#' threshold \eqn{L}, then \eqn{x_{ij}} is considered an outlier.
#'
#' For large data sets, just by chance, many values can surpass the default threshold
#' of 3. To reduce the number of outliers, the threshold can be increased.
#' Alternatively, the number of outliers can be limited by the two arguments
#' `max_n_outliers` and `max_prop_outliers`. For instance, if at most ten outliers
#' are to be identified, set `max_n_outliers = 10`.
#'
#' Since the random forest algorithm "ranger" does not allow for missing values,
#' any missing value is first being imputed by chained random forests.
#'
#' @param data A `data.frame` to be assessed for numeric outliers.
#' @param formula A two-sided formula specifying variables to be checked
#'   (left hand side) and variables used to check (right hand side).
#'   Defaults to `. ~ .`, i.e., use all variables to check all (numeric) variables.
#' @param replace Should outliers be replaced via predictive mean matching "pmm"
#'   (default), by "predictions", or by `NA` ("NA").
#'   Use "no" to keep outliers as they are.
#' @param pmm.k For `replace = "pmm"`, from how many nearest OOB prediction neighbours
#'   (from the original non-outliers) to sample?
#' @param threshold Threshold above which an outlier score is considered an outlier.
#'   The default is 3.
#' @param max_n_outliers Maximal number of outliers to identify.
#'   Will be used in combination with `threshold` and `max_prop_outliers`.
#' @param max_prop_outliers Maximal relative count of outliers.
#'   Will be used in combination with `threshold` and `max_n_outliers`.
#' @param min.node.size Minimal node size of the random forests.
#'   With 40, the value is relatively high. This reduces the impact of outliers.
#' @param allow_predictions Should the resulting "outForest" object be applied to
#'   new data? Default is `FALSE`.
#' @param impute_multivariate If `TRUE` (default), missing values are imputed
#'   by [missRanger::missRanger()]. Otherwise, by univariate sampling.
#' @param impute_multivariate_control Parameters passed to [missRanger::missRanger()]
#'   (only if data contains missing values).
#' @param seed Integer random seed.
#' @param verbose Controls how much outliers is printed to screen.
#'   0 to print nothing, 1 prints information.
#' @param ... Arguments passed to [ranger::ranger()]. If the data set is large, use
#'   less trees (e.g. `num.trees = 20`) and/or a low value of `mtry`.
#' @returns
#'   An object of class "outForest" and a list with the following elements.
#'   - `Data`: Original data set in unchanged row order but optionally with
#'     outliers replaced. Can be extracted with the [Data()] function.
#'   - `outliers`: Compact representation of outliers, for details see the [outliers()]
#'     function used to extract them.
#'   - `n_outliers`: Number of outliers per `v`.
#'   - `is_outlier`: Logical matrix with outlier status.
#'     `NULL` if `allow_predictions = FALSE`.
#'   - `predData`: `data.frame` with OOB predictions.
#'     `NULL` if `allow_predictions = FALSE`.
#'   - `allow_predictions`: Same as `allow_predictions`.
#'   - `v`: Variables checked.
#'   - `threshold`: The threshold used.
#'   - `rmse`: Named vector of RMSEs of the random forests. Used for scaling the
#'     difference between observed values and predicted.
#'   - `forests`: Named list of fitted random forests.
#'     `NULL` if `allow_predictions = FALSE`.
#'   - `used_to_check`: Variables used for checking `v`.
#'   - `mu`: Named vector of sample means of the original `v` (incl. outliers).
#' @references
#'   1. Chandola V., Banerjee A., and Kumar V. (2009). Anomaly detection: A survey.
#'     ACM Comput. Surv. 41, 3, Article 15 <dx.doi.org/10.1145/1541880.1541882>.
#'   2. Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random
#'     Forests for High Dimensional Data in C++ and R. Journal of Statistical Software,
#'     in press. <arxiv.org/abs/1508.04409>.
#' @export
#' @examples
#' head(irisWithOut <- generateOutliers(iris, seed = 345))
#' (out <- outForest(irisWithOut))
#' outliers(out)
#' head(Data(out))
#' plot(out)
#' plot(out, what = "scores")
#' @seealso [outliers()], [Data()] [plot.outForest()], [summary.outForest()],
#'   [predict.outForest()]
outForest <- function(data, formula = . ~ .,
                      replace = c("pmm", "predictions", "NA", "no"), pmm.k = 3L,
                      threshold = 3, max_n_outliers = Inf, max_prop_outliers = 1,
                      min.node.size = 40L, allow_predictions = FALSE,
                      impute_multivariate = TRUE,
                      impute_multivariate_control = list(pmm.k = 3L, num.trees = 50L, maxiter = 3L),
                      seed = NULL, verbose = 1, ...) {
  replace <- match.arg(replace)
  data <- as.data.frame(data)

  # Initial check
  stopifnot(
    (n <- nrow(data)) >= 1L,
    inherits(formula, "formula"),
    length(formula <- as.character.default(formula)) == 3L
  )
  if (min.node.size > n / 3) {
    min.node.size <- ceiling(n / 3)
    message("Due to small sample size, reduced 'min.node.size' to ", min.node.size)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Extract lhs and rhs from formula
  relevantVars <- lapply(
    formula[2:3],
    function(z) attr(
      stats::terms.formula(stats::reformulate(z), data = data[1, ]), "term.labels"
    )
  )

  # Fill missing values
  all_relevant <- unique(unlist(relevantVars))
  if (!all(all_relevant == make.names(all_relevant))) {
    stop("Currently, non-syntactic column names are not supported in the core of 'outRanger'.
         You can rename them or drop them via formula interface by writing something like
         '. - `bad name` ~ . - `bad name`'.")
  }
  data_imp <- data[, all_relevant, drop = FALSE]
  if (anyNA(data_imp)) {
    if (impute_multivariate) {
      ff <- paste(all_relevant, collapse = "+")
      missRanger_args <- c(
        list(
          data = data_imp,
          formula = stats::as.formula(paste(ff, ff, sep = "~")),
          verbose = verbose
        ),
        impute_multivariate_control
      )
      data_imp <- do.call(missRanger::missRanger, missRanger_args)
    } else {
      data_imp <- missRanger::imputeUnivariate(data_imp)
    }
  }

  if (verbose) {
    cat("\nOutlier identification by random forests\n")
  }

  # Pick numeric variables from lhs and determine variable names v to check
  predData <- Filter(
    function(z) is.numeric(z) && (stats::var(z) > 0),
    data_imp[, relevantVars[[1L]], drop = FALSE]
  )
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
    cat(relevantVars[[2L]], sep = ", ")
    cat("\n\n  Checking: ")
  }
  for (vv in v) {
    if (verbose) {
      cat(vv, " ")
    }
    covariables <- setdiff(relevantVars[[2L]], vv)
    if (length(covariables)) {
      fit <- ranger::ranger(
        formula = stats::reformulate(covariables, response = vv),
        data = data_imp,
        min.node.size = min.node.size,
        ...
      )
      predData[[vv]] <- fit$predictions
      if (any(is_na <- is.na(predData[[vv]]))) {
        predData[is_na, vv] <- stats::predict(fit, data_imp[is_na, ])$predictions
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
  out <- process_scores(
    data = data,
    scores = scores,
    predData = predData,
    v = v,
    rmse = rmse,
    replace = replace,
    pmm.k = pmm.k,
    threshold = threshold,
    max_n_outliers = max_n_outliers,
    max_prop_outliers = max_prop_outliers,
    allow_predictions = allow_predictions
  )
  out <- c(
    out,
    list(
      forests = if (allow_predictions) forests, used_to_check = relevantVars[[2L]],
      mu = mu
    )
  )
  class(out) <- c("outForest", "list")
  out
}




