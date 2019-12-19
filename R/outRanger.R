#' Outlier Detection and Replacement by Random Forest Predictions
#' 
#' @importFrom stats scale reformulate terms.formula predict
#' @importFrom ranger ranger
#' @importFrom missRanger missRanger imputeUnivariate
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @description Uses the "ranger" package [1] to identify outliers in numeric variables. Each numeric variable to be checked is regressed onto all other variables using a random forest. If the absolute difference between observed value and out-of-bag prediction is too large, then a value is considered an outlier. Since the random forest algorithm [1] does not allow for missing values, they are first imputed by chained random forests, see e.g. [2] or [3]. If the data sets are so large that multivariate imputation is too slow, try univariate imputation.
#' 
#' @param data A \code{data.frame} or \code{tibble} to be assessed for numeric outliers.
#' @param formula A two-sided formula specifying variables to be checked (left hand side) and variables used to check (right hand side). Defaults to . ~ ., i.e. use all variables to check all (numeric) variables. 
#' @param replace Should outliers be replaced by out-of-bag "predictions" (default), missing values ("NA") or not ("no")?
#' @param z_score Z score used to identify outliers. The default is 4.
#' @param n_max Maximal number of outliers to identify.
#' @param add_differences If TRUE (default), logical matrix is added as attribute "outlier" to the resulting data set, where \code{TRUE} means outlier.
#' @param impute_multivariate If \code{TRUE} (default), missing values are imputed by \code{missRanger::missRanger}. Otherwise, by univariate sampling.
#' @param impute_multivariate_control Parameters passed to \code{missRanger::missRanger} if data contains missing values.
#' @param seed Integer random seed.
#' @param verbose Controls how much info is printed to screen. 0 to print nothing, 1 prints useful information.
#' @param ... Arguments passed to \code{ranger}. If the data set is large, use less trees (e.g. \code{num.trees = 20}) and/or a low value of \code{mtry}. 
#' The following arguments are e.g. incompatible with \code{ranger}: \code{write.forest}, \code{probability}, \code{dependent.variable.name}, and \code{classification}. 
#'
#' @return A \code{data.frame}.
#' 
#' @title outRanger
#' @author Michael Mayer
#' 
#' @references
#' [1] Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. 
#' http://arxiv.org/abs/1508.04409.
#'
#' [2] Stekhoven, D.J. and Buehlmann, P. (2012). 'MissForest - nonparametric missing value imputation for mixed-type data', Bioinformatics, 28(1) 2012, 112-118. 
#' https://doi.org/10.1093/bioinformatics/btr597.
#'
#' [3] Van Buuren, S., Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. 
#' http://www.jstatsoft.org/v45/i03/
#' @export
#'
#' @examples
#' irisWithNA <- generateNA(iris, seed = 34)
#' irisWithOut <- generateOutlier(iris)
#' irisImputed <- outRanger(irisWithOut)
#' head(irisImputed)
#' head(irisWithNA)
#' }
outRanger <- function(data, formula = . ~ ., replace = c("predictions", "NA", "no"), 
                      z_score = 3, n_max = Inf, add_differences = TRUE,
                      impute_multivariate = TRUE,
                      impute_multivariate_control = list(pmm.k = 3, num.trees = 20, maxiter = 2L), 
                      seed = NULL, verbose = 1, ...) {
  if (verbose) {
    cat("\nOutlier identification by random forests\n")
  }
  replace <- match.arg(replace)
  
  # 1) INITIAL CHECKS
  stopifnot(is.data.frame(data), dim(data) >= 1L, 
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
  
  # 2) NEED IMPUTED VALUES
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

  # Pick numeric variables from lhs
  predData <- outData <- Filter(is.numeric, data[, relevantVars[[1]], drop = FALSE])
  
  visitSeq <- colnames(outData)
  
  if (verbose) {
    cat("\n  Variables to check:\t\t")
    cat(visitSeq, sep = ", ")
  }
  
  if (!length(visitSeq)) {
    if (verbose) {
      cat("\n")
    }
    return(dataOrig)
  }
  
  # 3) SELECT VARIABLES USED TO CHECK
  
  # Variables on the rhs should either appear in "visitSeq" or do not contain any missings
 
  if (verbose) {
    cat("\n  Variables used to check:\t")
    cat(relevantVars[[2]], sep = ", ")
  }

  # 4) Check each variable
  if (verbose) {
    pb <- txtProgressBar(1, length(visitSeq), style = 3)
  }
  for (i in seq_along(visitSeq)) { # 1 <- 1
    v <- visitSeq[i]
    fit <- ranger(formula = reformulate(setdiff(relevantVars[[2]], v), response = v), data = data)#, ...)
    predData[[v]] <- fit$predictions
    if (verbose) {
      setTxtProgressBar(pb, i)
    }
  }
  scores <- scale(outData - predData)
  outlier <- abs(scores) > z_score
  if (sum(outlier) > n_max) {
    outlier <- abs(scores) >= sort(abs(scores[outlier]), decreasing = TRUE)[n_max]
  }
  
  if (replace != "no") {
    dataOrig[, visitSeq][outlier] <- if (replace == "predictions") predData[outlier] else NA
  }

  if (verbose) {
    cat("\n")
  }
  if (add_differences) {
    attr(dataOrig, "scores") <- scores
  }
  dataOrig
}
