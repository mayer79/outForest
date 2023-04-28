#' Process Scores
#'
#' Internal function used to process scores and replace outliers.
#'
#' @param data Data set.
#' @param scores Matrix with outlier scores.
#' @param predData Prediction data.frame.
#' @param v Variables checked.
#' @param rmse rmse.
#' @param replace replace.
#' @param pmm.k pmm.k.
#' @param threshold threshold.
#' @param max_n_outliers max_n_outliers.
#' @param max_prop_outliers max_prop_outliers.
#' @param allow_predictions allow_predictions.
#' @param obj outForest object.
#' @return A list.
process_scores <- function(data, scores, predData, v, rmse, replace, pmm.k, threshold,
                           max_n_outliers, max_prop_outliers, allow_predictions,
                           obj = NULL) {
  is_outlier <- (abs(scores) > threshold)

  if (any(is_outlier)) {
    # Bound outlier count
    max_n_outliers <- min(max_n_outliers, max_prop_outliers * nrow(data) * length(v))
    if (sum(is_outlier) > max_n_outliers) {
      threshold <- sort(abs(scores[is_outlier]), decreasing = TRUE)[max_n_outliers + 1L]
      is_outlier <- abs(scores) > threshold
    }

    # Collect outliers on outliers (one row per outlier)
    outliers <- data.frame(which(is_outlier, arr.ind = TRUE))
    outliers[["col"]] <- factor(v[outliers[["col"]]], levels = v)
    outliers[["observed"]] <- data[, v][is_outlier]
    outliers[["predicted"]] <- predData[is_outlier]
    outliers[["rmse"]] <- rmse[outliers[["col"]]]
    outliers[["score"]] <- scores[is_outlier]
    outliers[["threshold"]] <- threshold

    # Replace values
    if (replace != "no") {
      if (replace == "pmm") {
        # Distinguish in- and out-of-sample situation
        data_ref <- if (is.null(obj)) data else Data(obj)
        predData_ref <- if (is.null(obj)) predData else obj$predData
        is_outlier_ref <- if (is.null(obj)) is_outlier else obj$is_outlier
        for (vv in v) {
          if (any(is_out <- is_outlier[, vv])) {
            orig_ok <- !is.na(data_ref[[vv]]) & !is_outlier_ref[, vv]
            nn <- FNN::knnx.index(
              predData_ref[[vv]][orig_ok], query = predData[[vv]][is_out], k = pmm.k
            )
            take <- t(stats::rmultinom(sum(is_out), 1L, rep(1L, pmm.k)))
            data[, vv][is_out] <- data_ref[[vv]][orig_ok][rowSums(nn * take)]
          }
        }
      } else {
        data[, v][is_outlier] <- if (replace == "predictions") predData[is_outlier] else NA
      }
    }
    outliers[["replacement"]] <- data[, v][is_outlier]
    outliers <- outliers[order(abs(outliers$score), decreasing = TRUE), , drop = FALSE]
  } else {
    outliers <- data.frame(row = integer(), col = factor(character(), levels = v))
    nc <- c("observed", "predicted", "rmse", "score", "threshold", "replacement")
    outliers <- cbind(
      outliers,
      matrix(NA_real_, ncol = length(nc), nrow = 0L, dimnames = list(NULL, nc))
    )
  }
  list(
    Data = data,
    outliers = outliers,
    n_outliers = colSums(is_outlier, na.rm = TRUE),
    is_outlier = if (allow_predictions) is_outlier,
    predData = if (allow_predictions) predData,
    allow_predictions = allow_predictions,
    v = v,
    threshold = threshold,
    rmse = rmse
  )
}


