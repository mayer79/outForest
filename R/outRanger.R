#' Multivariate Outlier Detection and Replacement by Random Forest Predictions
#' 
#' This function provides a random forest based implementation of the method described in Chapter 7.1.2 ("Regression Model Based Anomaly detection") of [1]. Each numeric variable to be checked for outliers is regressed onto all other variables using a random forest. If the absolute difference between observed value and out-of-bag prediction is too large, then a value is considered an outlier. After identification of outliers, they can be replaced e.g. by predictive mean matching from the non-outliers. Since the random forest algorithm `ranger` [2] does not allow for missing values, any missing value is first being imputed by chained random forests.
#' 
#' @importFrom stats scale reformulate terms.formula predict
#' @importFrom ranger ranger
#' @importFrom missRanger missRanger imputeUnivariate
#' @importFrom stats rmultinom
#' @importFrom FNN knnx.index
#' @param data A \code{data.frame} or \code{tibble} to be assessed for numeric outliers.
#' @param formula A two-sided formula specifying variables to be checked (left hand side) and variables used to check (right hand side). Defaults to . ~ ., i.e. use all variables to check all (numeric) variables. 
#' @param replace Should outliers be replaced by predicting mean matching on the OOB predictions ("pmm", the default), by OOB predictions ("predictions"), by \code{NA} ("NA"). Use "no" to keep outliers as they are.
#' @param pmm.k For \code{replace = "pmm"}, how many nearest neighbours (without outliers) be considered to sample values from?
#' @param z_score Z score used to identify outliers. The default is 3.
#' @param max_n_outlier Maximal number of outliers to identify. Will be used on top of the z score criterion.
#' @param max_prop_outlier Maximal relative count of outliers. Will be used on top of the z score criterion.
#' @param summary If TRUE (default), different attributes are added to the output.
#' @param impute_multivariate If \code{TRUE} (default), missing values are imputed by \code{missRanger::missRanger}. Otherwise, by univariate sampling.
#' @param impute_multivariate_control Parameters passed to \code{missRanger::missRanger} if data contains missing values.
#' @param seed Integer random seed.
#' @param verbose Controls how much info is printed to screen. 0 to print nothing, 1 prints information.
#' @param ... Arguments passed to \code{ranger}. If the data set is large, use less trees (e.g. \code{num.trees = 20}) and/or a low value of \code{mtry}. 
#' The following arguments are e.g. incompatible with \code{ranger}: \code{write.forest}, \code{probability}, \code{dependent.variable.name}, and \code{classification}. 
#' @return An object of type \code{outRanger} and a list. The data set with replaced values can be accessed by \code{$data}.
#' @references
#' [1] Chandola V., Banerjee A., and Kumar V. (2009). Anomaly detection: A survey. ACM Comput. Surv. 41, 3, Article 15 <dx.doi.org/10.1145/1541880.1541882>.
#' [2] Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. <arxiv.org/abs/1508.04409>.
#' @export
#' @examples
#' head(irisWithOut <- generateOutlier(iris, seed = 345))
#' head(irisReplaced <- outRanger(irisWithOut))
#' head(irisReplaced <- outRanger(irisWithOut))
outRanger <- function(data, formula = . ~ ., 
                      replace = c("pmm", "predictions", "NA", "no"), pmm.k = 3,
                      z_score = 3, max_n_outlier = Inf, max_prop_outlier = 1, summary = TRUE,
                      impute_multivariate = TRUE,
                      impute_multivariate_control = list(pmm.k = 3, num.trees = 20, maxiter = 2L), 
                      seed = NULL, verbose = 1, ...) {
  replace <- match.arg(replace)
  
  # Initial check
  stopifnot(is.data.frame(data), 
            (n <- nrow(data)) >= 1L, 
            inherits(formula, "formula"), 
            length(formula <- as.character(formula)) == 3L,
            !(c("write.forest", "probability", "dependent.variable.name", 
                "classification") %in% names(list(...))))
  
  if (!is.null(seed)) {
    set.seed(seed)
  }  
  
  # Extract lhs and rhs from formula
  relevantVars <- lapply(formula[2:3], function(z) attr(terms.formula(
    reformulate(z), data = data[1, ]), "term.labels"))
  
  # Fill missing values
  all_relevant <- unique(unlist(relevantVars))
  data_rel <- data[, all_relevant, drop = FALSE]
  if (anyNA(data_rel)) {
    if (impute_multivariate) {
      ff <- paste(all_relevant, collapse = "+")
      missRanger_args <- c(list(data = data_rel, formula = as.formula(paste(ff, ff, sep = "~")), 
                                verbose = verbose), impute_multivariate_control)
      data_rel <- do.call(missRanger, missRanger_args)
    } else {
      data_rel <- imputeUnivariate(data_rel)
    }
  }

  if (verbose) {
    cat("\nOutlier identification by random forests\n")
  }
  
  # Pick numeric variables from lhs and determine variable names v to check
  predData <- Filter(is.numeric, data_rel[, relevantVars[[1]], drop = FALSE])
  v <- colnames(predData)
  m <- length(v)
  if (m == 0L) {
    return(data)
  }
  # Keep missingness info in original data
  if ((was_any_NA <- anyNA(data[, v, drop = FALSE]))) {
    wasNAData <- is.na(data[, v, drop = FALSE])
  }
  if (verbose) {
    cat("\n  Variables to check:\t\t")
    cat(v, sep = ", ")
    cat("\n  Variables used to check:\t")
    cat(relevantVars[[2]], sep = ", ")
    cat("\n\n  Checking: ")
  }

  # Check each variable
  for (vv in v) { # 1 <- 1
    if (verbose) {
      cat(vv, " ")
    }
    covariables <- setdiff(relevantVars[[2]], vv)
    if (length(covariables)) {
      fit <- ranger(formula = reformulate(covariables, response = vv), data = data_rel)#, ...)
      predData[[vv]] <- fit$predictions  
    } else {
      predData[[vv]] <- mean(data_rel[[vv]], na.rm = TRUE) 
    }
  }
  
  # Calculate outlier scores and status
  scores <- scale(data_rel[, v, drop = FALSE] - predData)
  if (was_any_NA) {
    scores[wasNAData] <- 0 
  }
  outlier <- abs(scores) > z_score
  
  # Bound outlier count
  max_n_outlier <- min(max_n_outlier, max_prop_outlier * n * m)
  if (sum(outlier) > max_n_outlier) {
    outlier <- abs(scores) >= sort(abs(scores[outlier]), decreasing = TRUE)[max_n_outlier]
  }
  
  # Collect info on outliers (one row per outlier)
  info <- data.frame(which(outlier, arr.ind = TRUE))
  info[["col"]] <- v[info[["col"]]]
  info[["observed"]] <- data[, v][outlier]
  info[["predicted"]] <- predData[outlier]
  info[["scale_center"]] <- attributes(scores)$`scaled:center`[info[["col"]]] 
  info[["scale_scale"]] <- attributes(scores)$`scaled:scale`[info[["col"]]] 
  info[["score"]] <- scores[outlier]
  
  # Replace values
  if (replace != "no") {
    if (replace == "pmm") {
      for (vv in v) { # v <- "Sepal.Length"
        if (any(is_out <- outlier[, vv])) {
          nn <- knnx.index(predData[[vv]][!is_out], query = predData[[vv]][is_out], k = pmm.k)
          take <- t(rmultinom(sum(is_out), 1L, rep(1L, pmm.k)))
          data[, vv][is_out] <- data_rel[[vv]][!is_out][rowSums(nn * take)]
        }
      } 
    } else {
      data[, v][outlier] <- if (replace == "predictions") predData[outlier] else NA 
    }
  }
  info[["replacement"]] <- data[, v][outlier]

  out <- list(data = data,
              v = v,
#              scores = scores,
#              predData = predData,
#              outlier = outlier,
              n_outliers = colSums(outlier, na.rm = TRUE),
              info = info)
  class(out) <- c("outRanger", "list")
  
  if (verbose) {
    cat("\n")
  }
  
  out
}




