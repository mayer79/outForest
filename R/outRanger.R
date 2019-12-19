#' Outlier Detection and Replacement by Random Forest Predictions
#' 
#' Uses the "ranger" package [1] to identify outliers in numeric variables. Each numeric variable to be checked is regressed onto all other variables using a random forest. If the absolute difference between observed value and out-of-bag prediction is too large, then a value is considered an outlier. Since the random forest algorithm [1] does not allow for missing values, they are first imputed by chained random forests. If the data sets are too large for multivariate imputation, try univariate imputation.
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
#' @param n_max Maximal number of outliers to identify. Will be used on top of the z score criterion.
#' @param summary If TRUE (default), different attributes are added to the output.
#' @param impute_multivariate If \code{TRUE} (default), missing values are imputed by \code{missRanger::missRanger}. Otherwise, by univariate sampling.
#' @param impute_multivariate_control Parameters passed to \code{missRanger::missRanger} if data contains missing values.
#' @param seed Integer random seed.
#' @param verbose Controls how much info is printed to screen. 0 to print nothing, 1 prints information.
#' @param ... Arguments passed to \code{ranger}. If the data set is large, use less trees (e.g. \code{num.trees = 20}) and/or a low value of \code{mtry}. 
#' The following arguments are e.g. incompatible with \code{ranger}: \code{write.forest}, \code{probability}, \code{dependent.variable.name}, and \code{classification}. 
#' @return A \code{data.frame}.
#' @references
#' [1] Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. 
#' http://arxiv.org/abs/1508.04409.
#' @export
#' @examples
#' head(irisWithOut <- generateOutlier(iris, seed = 345))
#' head(irisReplaced <- outRanger(irisWithOut))
#' head(irisReplaced <- outRanger(irisWithOut))
outRanger <- function(data, formula = . ~ ., 
                      replace = c("pmm", "predictions", "NA", "no"), pmm.k = 3,
                      z_score = 3, n_max = Inf, summary = TRUE,
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
  
  dataOrig <- data
  
  # Extract lhs and rhs from formula
  relevantVars <- lapply(formula[2:3], function(z) attr(terms.formula(
    reformulate(z), data = data[1, ]), "term.labels"))
  
  # Fill missing values
  all_relevant <- unique(unlist(relevantVars))
  if (anyNA(data[, all_relevant, drop = FALSE])) {
    if (impute_multivariate) {
      ff <- paste(all_relevant, collapse = "+")
      missRanger_args <- c(list(data = data, formula = as.formula(paste(ff, ff, sep = "~")), 
                                verbose = verbose), impute_multivariate_control)
      data <- do.call(missRanger, missRanger_args)
    } else {
      data[, all_relevant] <- imputeUnivariate(data[, all_relevant, drop = FALSE])
    }
  }

  if (verbose) {
    cat("\nOutlier identification by random forests\n")
  }
  
  # Pick numeric variables from lhs
  predData <- outData <- Filter(is.numeric, data[, relevantVars[[1]], drop = FALSE])
  v <- colnames(outData)
  if (!(m <- length(v))) {
    return(dataOrig)
  }
  if ((was_any_NA <- anyNA(outData))) {
    wasNAData <- is.na(outData)
  }
  if (verbose) {
    cat("\n  Variables to check:\t\t")
    cat(v, sep = ", ")
    cat("\n  Variables used to check:\t")
    cat(relevantVars[[2]], sep = ", ")
    if (m >= 2L) {
      pb <- txtProgressBar(1, length(v), style = 3)  
    }
  }

  # Check each variable
  for (i in seq_len(m)) { # 1 <- 1
    v <- v[i]
    fit <- ranger(formula = reformulate(setdiff(relevantVars[[2]], v), response = v), data = data)#, ...)
    predData[[v]] <- fit$predictions
    if (verbose && m > 2L) {
      setTxtProgressBar(pb, i)
    }
  }
  
  # Calculate outlier scores and status
  scores <- scale(outData - predData)
  if (was_any_NA) {
    scores[wasNAData] <- 0 
  }
  outlier <- abs(scores) > z_score
  if (sum(outlier) > n_max) {
    outlier <- abs(scores) >= sort(abs(scores[outlier]), decreasing = TRUE)[n_max]
  }
  
  # Replace values
  if (replace != "no") {
    if (replace == "pmm") {
      for (v in v) { # v <- "Sepal.Length"
        if (any(is_out <- outlier[, v])) {
          nn <- knnx.index(predData[[v]][!is_out], query = predData[[v]][is_out], k = pmm.k)
          take <- t(rmultinom(sum(is_out), 1L, rep(1L, pmm.k)))
          dataOrig[, v][is_out] <- data[[v]][!is_out][rowSums(nn * take)]
        }
      } 
    } else {
      dataOrig[, v][outlier] <- if (replace == "predictions") predData[outlier] else NA 
    }
  }

  if (verbose) {
    cat("\n")
  }
  if (summary) {
    attr(dataOrig, "v") <- v
    attr(dataOrig, "scores") <- scores
    attr(dataOrig, "outlier") <- outlier
  }
  dataOrig
}




